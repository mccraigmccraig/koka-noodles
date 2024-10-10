module logger

import std/core/string 
import std/core/undiv
import std/core/vector
import std/data/json
import std/pretty/pprint
import steps/types


///////////////////////////////////////////////////////////////////////////////
//                                  public types
///////////////////////////////////////////////////////////////////////////////


// delayed serialization for step values
pub alias json-src = () -> total json

pub value struct json-fns<a>
  to-json: (a) -> total json
  from-json: (json: json, path: string) -> pure a

// matches the run-step-result
pub value type outcome<a,b,x>
  ReasonFail(a: a)
  ReasonPause(b: b)
  ReasonExec(x: x)
  ReasonResume(x: x)

pub alias log-outcome = outcome<json-src, json-src, json-src>

// RunLog is built by the logger effect, ThunkLog by
// the step-ctl.log operation
pub type step-log
  RunLog(rl: run-log)
  ThunkLog(c-src: json-src)

pub struct run-log
  name: string
  pauses: int // # of pauses detected inside this step
  children: list<step-log>
  outcome: log-outcome
  
pub struct computation-log
  name: string
  pauses: int // total # of pauses
  resume-values: list<json-src>
  step-logs: list<step-log>

// capture execution structure of a computation
pub effect logger
  // open a step-log. If following a log, returns the corresponding step-log
  fun log-enter(name: string) : maybe<run-log>
  // close a step-log - must provide a log-outcome param because
  // the logger can't distinguish Exec from Resume (split this into -leave-exec and -leave-resume ?)
  fun log-leave(outcome: log-outcome) : ()


///////////////////////////////////////////////////////////////////////////////
//                                 private types
///////////////////////////////////////////////////////////////////////////////


// follows the log as a step is run
value struct step-cursor
  steps: vector<run-log>
  next-idx: int

// enables navigation through a computation-log tree, 
// for as long as the log tree matches the computation tree - 
// so errored computations can be continued from the point of error
value struct log-cursor
  step-cursors: list<step-cursor>

// accumulating a new step-log
value struct open-step
  name: string
  pauses: int
  children: list<step-log>

// all the logger state
struct logger-state
  resume-cursor: maybe<log-cursor>
  pause-ignores: int
  completed-steps: list<step-log>
  open-steps: list<open-step>


///////////////////////////////////////////////////////////////////////////////
//                      logger / step-ctl effect handlers
///////////////////////////////////////////////////////////////////////////////

// update a log with a resume-value, which will later be retrieved by the 
// pause operation
pub fun push-resume-value<c>(
    log: computation-log,
    resume-value: c,
    ?c/json-fns: json-fns<c>
  )
  log(resume-values = log.resume-values ++ [{?c/json-fns.to-json()(resume-value)}])

// collect and return a computation-log from a computation
// this handler also handles any exn effect
// TODO currently using exn internally for fatal programming errors - is that ok?
pub fun log-collector<a,b,c,d,x>(
    previous-log: maybe<computation-log>, 
    computation-name: string, 
    handle-fatal-error: (string) -> a,
    a-json-fns: json-fns<a>,
    b-json-fns: json-fns<b>,
    c-json-fns: json-fns<c>,
    d-json-fns: json-fns<d>,
    action: () -> <div,logger,step-ctl<a,b,c,d>> x
  ) : div (outcome<a,b,x>, computation-log)

  var state := make-logger-state(previous-log)

  // we ignore as many pauses as the previous run had
  val max-pause-ignores = match previous-log
    Just(pl) -> pl.pauses
    _ -> 0

  with return(v) 
    val (pauses, resume-values) = match previous-log
      Just(pl) -> (max(pl.pauses, state.pause-ignores), pl.resume-values)
      _ -> (state.pause-ignores, [])

    (v, Computation-Log(computation-name, pauses, resume-values, state.completed-steps.reverse))

  with handler
    final ctl throw-exn(ex)
      state := state/close-open-steps(state, ReasonFail({a-json-fns.to-json()(ex.show.handle-fatal-error)}), handle-fatal-error, a-json-fns)
      ReasonFail(handle-fatal-error(ex.show))

  with handler
    final ctl fail(a)
      state := state/close-open-steps(state, ReasonFail({a-json-fns.to-json()(a)}), handle-fatal-error, a-json-fns)
      ReasonFail(a)
    ctl pause(b)
      if state.pause-ignores < max-pause-ignores then
        // it's a pause from a previous run - return the recorded resume value
        val resume-val = match previous-log
          Just(pl) -> match @index(pl.resume-values, state.pause-ignores)
            Just(vjs) -> c-json-fns.from-json()(vjs(), "") 
            _ -> throw("missing resume value")
          _ -> throw("missing resume value")
        state := state(pause-ignores=state.pause-ignores + 1)
        resume(resume-val)
      else
        // pausing for realz
        val state' = state/close-open-steps(state, ReasonPause({b-json-fns.to-json()(b)}), handle-fatal-error, a-json-fns)
        state := state'(pause-ignores=state.pause-ignores + 1)
        ReasonPause(b)
    fun log(d)
      val state' = state/add-thunk-log(state, {d-json-fns.to-json()(d)})
      state := state'
      ()

  // the handler implementation fns are all total, but can return errors via either. 
  // Such errors will be thrown to the throw-exn handler, to immediately terminate 
  // the computation
  with handler
    fun log-enter(name)
      match state/log-enter(state, name)
        (state', resumed-step) -> 
          state := state'
          resumed-step
    fun log-leave(success-reason)
      // TODO can we get rid of this throw - it's the last remaining internal throw
      // perhaps turn it into a log-fail ?      
      match state/log-leave(state, success-reason)
        Left(err) -> throw(err)
        Right(state') -> state := state'

  ReasonExec(action())


///////////////////////////////////////////////////////////////////////////////
//                                 state helpers
///////////////////////////////////////////////////////////////////////////////


fun make-step-cursor(sls: list<step-log>): step-cursor
  // filter out ThunkLogs here - they aren't needed for following the computation
  val run-logs = sls.filter-map(fn (el) ->
    match el 
      RunLog(rl) -> Just(rl)
      _ -> Nothing
  )
  Step-cursor(steps = vector(run-logs), next-idx = 0)

fun make-log-cursor(log: computation-log): log-cursor
  Log-cursor(step-cursors = Cons(make-step-cursor(log.step-logs), Nil))

fun make-logger-state(previous-log: maybe<computation-log>): logger-state
  match previous-log
    Just(pl) -> Logger-state(Just(make-log-cursor(pl)), 0, [], [])
    _ -> Logger-state(Nothing, 0, [], [])

fun state/add-thunk-log(state: logger-state, c-src: json-src): total logger-state 
  match state.open-steps
    [] -> 
      state
    Cons(first, rest) ->
      val step-log = ThunkLog(c-src)
      val first' = first(children = Cons(step-log, first.children))
      state(open-steps=(Cons(first', rest)))

// propagate an outcome through the open-steps, closing them all and 
// returning a single step-log representing the whole closed tree. Add a
// fatal error if there are no open-steps
// TODO this is broken when effect type parameters are mapped with pentamap - 
// although the breakage doesn't affect anything, other than setting observably
// wrong result types
fun state/close-open-steps<a>(state: logger-state, outcome: log-outcome, handle-fatal-error: (string)-> a, a-json-fns: json-fns<a>): total logger-state
  match state.open-steps
    [] -> 
      val fatal-step = RunLog(Run-Log("error", 0, [], ReasonFail({a-json-fns.to-json()("no open steps".handle-fatal-error)})))
      val completed-steps' = Cons(fatal-step, state.completed-steps)
      state(completed-steps=completed-steps')
    Cons(first, Nil) -> 
      val step-log = RunLog(Run-Log(first.name, 0, first.children.reverse, outcome))
      val completed-steps' = Cons(step-log, state.completed-steps)
      state(completed-steps=completed-steps', open-steps=[])
    Cons(first, Cons(next, rest)) -> 
      val step-log = RunLog(Run-Log(first.name, 0, first.children.reverse, outcome))
      val next' = next(children = Cons(step-log, next.children))
      state/close-open-steps(pretend-decreasing(state(open-steps=Cons(next', rest))), outcome, handle-fatal-error, a-json-fns)

// if we are following a log and have a step to enter, update the log cursor
// and return the logged step. either way, create a new `open-step` to 
// record the outcome
fun state/log-enter(state: logger-state, step-name: string): total (logger-state, maybe<run-log>)
  // follow the resumed log, if it matches
  val state-resumed-step = match state.resume-cursor
    Just(lc) -> 
      match lc.step-cursors
        Cons(Step-cursor(steps, next-idx), rest) | next-idx < length(steps) ->
          val maybe-sl = at(steps, next-idx)
          match maybe-sl 
            Just(sl) ->
              // inc the next-idx counter on the current step-cursor, push a new step-cursor
              (state(resume-cursor=Just(Log-cursor(Cons(make-step-cursor(sl.children), 
                                                        Cons(Step-cursor(steps, next-idx + 1), 
                                                             rest))))), 
               Just(sl))
            _ -> (state(resume-cursor=Nothing), Nothing)
        _ -> (state(resume-cursor=Nothing), Nothing)
    _ -> (state(resume-cursor=Nothing), Nothing)
    
  // always accumulate a new open-step
  match state-resumed-step
    (state', resumed-step) -> 
      // copy the pause count from any resumed-step... it may be incremented
      // by further pauses
      val pauses = match resumed-step
        Just(rl) -> rl.pauses()
        _ -> 0
      val open-steps' = match state.open-steps
        [] -> [Open-Step(step-name, pauses, [])]
        rest -> Cons(Open-Step(step-name, pauses, []), rest)
      (state'(open-steps=open-steps'), resumed-step)
  
fun state/log-leave(state: logger-state, outcome: log-outcome): total either<string,logger-state>
  // first pop a step off the cursor
  val state' = match state.resume-cursor
    Just(lc) ->
      match lc.step-cursors
        Cons(Step-cursor(steps, next-idx), rest) | next-idx == length(steps) ->
          state(resume-cursor=Just(Log-cursor(rest)))
        _ -> state(resume-cursor=Nothing)
    _ -> state(resume-cursor=Nothing)

  // then update the completed-steps / open-steps
  val error-completed-steps-open-steps = match state.open-steps
    Cons(first, Nil) -> 
      Right((Cons(RunLog(Run-Log(first.name, 0, first.children.reverse, outcome)), state.completed-steps),
            []))
    Cons(first, Cons(next, rest)) -> 
      val step-log = RunLog(Run-Log(first.name, 0, first.children.reverse, outcome))
      val next' = next(children = Cons(step-log, next.children))
      val open-steps' = Cons(next', rest)
      Right((state.completed-steps, open-steps'))
    [] -> Left("log-leave: no open-step")

  match error-completed-steps-open-steps
    Right((completed-steps', open-steps')) ->
      Right(state'(completed-steps=completed-steps', open-steps=open-steps'))
    Left(err) -> Left(err)


///////////////////////////////////////////////////////////////////////////////
//                       printing and JSON serialisation
///////////////////////////////////////////////////////////////////////////////


// parse json, converting exn into an either
pub fun jsond/parse-json<a>(j: json, path: string, p: (j: json, path: string) -> pure a): either<string,a>
  with pretend-no-div
  match try({p(j, path)})
    Ok(a) -> Right(a)
    Error(e) -> Left(e.message)

pub fun json-src/show(a: json-src): total string
  a().show
pub fun json-src/to-json(s: json-src): total json
  s()
pub fun json-src/from-json(j: json, path: string): exn json-src
  fn () -> j

pub fun either/to-json<a,b>(ea: either<a,b>, ?a/to-json: (a)->total json, ?b/to-json: (b)->total json): total json 
  match ea
    Left(a) -> JSObj([("Left", a.?a/to-json)])
    Right(b) -> JSObj([("Right", b.?b/to-json)])
pub fun either/from-json<a,b>(j: json, path: string, ?a/from-json: (json,string)-> pure a, ?b/from-json: (json,string)-> pure b): pure either<a,b>
  match j
    JSObj(l) -> 
      match l.lookup(fn (k) k=="Left")
        Just(ja) -> Left(ja.?a/from-json(path ++ ".Left"))
        _ -> 
          match l.lookup(fn (k) k=="Right")
            Just(jb) -> Right(jb.?b/from-json(path ++ ".Right"))
            _ -> throw("either/dejson: expected Left or Right")
    _ -> throw("either/dejson: expected object")
pub fun either/make-json-fns<a,b>(a-json-fns: json-fns<a>, b-json-fns: json-fns<b>): total json-fns<either<a,b>>
  Json-fns(
    fn(eab) -> either/to-json(eab, ?a/to-json=a-json-fns.to-json, ?b/to-json=b-json-fns.to-json),
    fn(j, path) -> either/from-json(j, path, ?a/from-json=a-json-fns.from-json, ?b/from-json=b-json-fns.from-json)
  )

pub fun outcome/show<a,b,c>(outcome: outcome<a,b,c>, ?a/show: (a)->string, ?b/show: (b)->string, ?c/show: (c)->string): total string
  match outcome
    ReasonFail(a) -> "error: " ++ a.?a/show
    ReasonPause(b) -> "paused: " ++ b.?b/show
    ReasonExec(c) -> "exec: " ++ c.?c/show
    ReasonResume(c) -> "resume: " ++ c.?c/show

pub fun log-outcome/show(outcome: log-outcome): total string
  match outcome
    ReasonFail(a) -> "error: " ++ a().show
    ReasonPause(b) -> "paused: " ++ b().show
    ReasonExec(c) -> "exec: " ++ c().show
    ReasonResume(c) -> "resume: " ++ c().show
pub fun log-outcome/to-json(o: log-outcome): total json
  match o
    ReasonFail(a) -> JSObj([("ReasonFail", a().to-json)])
    ReasonPause(b) -> JSObj([("ReasonPause", b().to-json)])
    ReasonExec(c) -> JSObj([("ReasonExec", c().to-json)])
    ReasonResume(c) -> JSObj([("ReasonResume", c().to-json)])
pub fun log-outcome/from-json(j: json, path: string): exn log-outcome
  match j
    JSObj(l) -> 
      match l.lookup(fn (k) k=="ReasonFail")
        Just(fa) -> ReasonFail(json-src/from-json(fa, path ++ "->ReasonFail"))
        _ -> match l.lookup(fn (k) k=="ReasonPause")
          Just(pa) -> ReasonPause(json-src/from-json(pa, path ++ "->ReasonPause"))
          _ -> match l.lookup(fn (k) k=="ReasonExec")
            Just(ea) -> ReasonExec(json-src/from-json(ea, path ++ "->ReasonExec"))
            _ -> match l.lookup(fn (k) k=="ReasonResume")
              Just(ra) -> ReasonResume(json-src/from-json(ra, path ++ "->ReasonResume"))
              _ -> throw("Expecting log-outcome, got " ++ l.show, info=JsonDeserializationException(JSObj(l), path))
    _ -> throw("Expecting log-outcome, got " ++ j.show, info=JsonDeserializationException(j, path))

pub fun run-log/to-json'(rl: run-log): div json
  JSObj([("name", JSString(rl.name)),
         ("pauses", JSInt(rl.pauses)),
         ("children", JSList(rl.children.map(fn(csl) csl.to-json'))), 
         ("outcome", rl.outcome.log-outcome/to-json)])
pub fun run-log/from-json(j: json, path: string): pure run-log 
  match j
    JSObj(l) -> 
      Run-Log(
        l.from-json("name", path),
        l.from-json("pauses", path),
        l.from-json("children", path),
        l.from-json("outcome", path)
      )
    _ -> throw("Expecting step-log, got " ++ j.show, info=JsonDeserializationException(j, path))


pub fun step-log/show(a: step-log): total string
  with pretend-no-div
  a.step-log/pretty.show
pub fun step-log/to-json'(sl: step-log): div json
  match sl 
    RunLog(rl) -> 
      JSObj([("RunLog", run-log/to-json'(rl))])
    ThunkLog(c-src) -> 
      JSObj([("ThunkLog", c-src())])      
pub fun step-log/to-json(sl: step-log): total json
  with pretend-no-div 
  step-log/to-json'(sl)
pub fun step-log/from-json(j: json, path: string): pure step-log
  match j
    JSObj(l) -> 
      match l.lookup(fn (k) k=="ThunkLog")
        Just(tl) -> ThunkLog(json-src/from-json(tl, path ++ "->ThunkLog"))
        _ -> match l.lookup(fn (k) k=="RunLog")
          Just(rl) -> rl.run-log/from-json(path ++ "->RunLog").RunLog
          _ -> throw("Expecting run-log, got " ++ j.show, info=JsonDeserializationException(j, path))

pub fun computation-log/show(a: computation-log): total string
  with pretend-no-div
  a.pretty.show
pub fun computation-log/to-json(cl: computation-log): total json
  JSObj([("name", JSString(cl.name)),
         ("pauses", JSInt(cl.pauses)),
         ("resume-values", JSList(cl.resume-values.map(to-json))),
         ("step-logs", JSList(cl.step-logs.map(to-json)))])
pub fun computation-log/from-json(j: json, path: string): pure computation-log
  match j
    JSObj(l) -> 
      Computation-log(
        l.from-json("name", path),
        l.from-json("pauses", path),
        l.from-json("resume-values", path),
        l.from-json("step-logs", path)
      )
    _ -> throw("Expecting computation-log, got " ++ j.show, info=JsonDeserializationException(j, path))

// TODO it would be good if we could indent with something other than spaces...
// but I can't see how to do that with the pretty-printer as it stands - this
// fn only indents the first line of a doc with "| " and the rest get spaces
// pub fun pipe-indent(d: doc, i: int): doc
//   val pad = repeat("| ", i)
//   (pad.text |.| d).hang(i * 2)

pub fun step-log-children/pretty(log: step-log): div doc
  match log 
    RunLog(rl) ->
      if length(rl.children)>0 then 
        linebreak |.| "children:".text |--| 
        vcat(rl.children.map(fn (sl) sl.step-log/pretty').punctuate(linebreak |.| "--".text)).indent(2)
      else "".text
    ThunkLog(_c) ->
      "".text

pub fun step-log/pretty'(log: step-log): div doc
  match log
    RunLog(rl) ->
      "run: ".text |.| rl.name.text |.|  
      (if rl.pauses>0 then linebreak |.| "pauses: ".text |.| rl.pauses.show.text else "".text) |.|
      log.step-log-children/pretty |--|
      "outcome: ".text |.| rl.outcome.show.text
    ThunkLog(c-src) -> 
      "log:".text |.| c-src().show.text

pub fun step-log/pretty(log: step-log): total doc
  // pretend-decreasing doesn't seem to help with mutually-recursive fns
  with pretend-no-div
  step-log/pretty'(log)

fun computation-log-resume-values/pretty(resume-values: list<json-src>): total doc
  if length(resume-values)>0 then
    linebreak |.| "resume-values:".text |--|
    vcat(resume-values.map(fn(rv-src) rv-src().show.text )).indent(2)
  else "".text 

pub fun computation-log/pretty(log: computation-log): total doc
  "computation: ".text |.| log.name.text |--|
  "pauses: ".text |.| log.pauses.show.text |.|
  computation-log-resume-values/pretty(log.resume-values) |--|
  "steps: ".text |--|
  vcat(log.step-logs.map(fn(sl) sl.step-log/pretty).punctuate("----".text)).indent(2)

pub val string/json-fns = Json-fns(string/to-json, string/from-json)
pub val int/json-fns = Json-fns(int/to-json, int/from-json)


///////////////////////////////////////////////////////////////////////////////
//                       examples
///////////////////////////////////////////////////////////////////////////////


// pub fun stuff(i: int)
//   log-enter("stuff")
//   val o = i + i
//   // log-fail({o.show})
//   log-leave(ReasonExec({o.to-json}))
//   0

// // note: need to annotate fn to set the right outcome type for the log-collector
// pub fun thingz(i: int): <logger,step-ctl<string,string,string>> int
//   log-enter("thingz")
//   val o = i * i
//   // throw("oops: " ++ o.show)
//   log-leave(ReasonExec({o.to-json}))
//   o

// pub fun collect-test-logs()
//   with log-collector(
//     Nothing, 
//     "test", 
//     fn (s: string) -> s,
//     string/json-fns,
//     string/json-fns,
//     string/json-fns
//     )
//   // log-pause("foo", string/json-fns)
//   val v = stuff(5)
//   val w = thingz(v)
//   w

// pub fun logs-an-error()
//   log-enter("logs-an-error")
//   fail("boo!")

// pub fun collect-logs-with-error()
//   with log-collector(
//     Nothing, 
//     "test-with-error",
//     fn (s: string) -> s,
//     string/json-fns,
//     string/json-fns,
//     string/json-fns
//   )
//   val v = stuff(5)
//   val w = thingz(v)
//   logs-an-error()
//   w

// pub fun throws-an-exception()
//   log-enter("throws-an-exception")
//   throw("oops")

// pub fun test-collect-logs()
//   match collect-test-logs()
//     (v, log) -> 
//       log.pretty.show.println
//       println("Result: " ++ v.show)
//   match collect-logs-with-error()
//     (v, log) -> 
//       log.pretty.show.println
//       println("Result: " ++ v.show)
