module logger

import std/core/string 
import std/core/undiv
import std/core/vector
import std/data/json

// a thunk for step values - we don't always need to 
// serialize or deserialise all values, e.g. we might have an
// "only log final values if there are no errors, but 
//  log all values if there are errors" policy
pub alias json-src = () -> total json
pub alias string-src = () -> total string

pub value struct json-fns<a>
  to-json: (a) -> total json
  from-json: (json: json, path: string) -> pure a

// parse json, converting exn into an either
pub fun jsond/parse-json<a>(j: json, path: string, p: (j: json, path: string) -> pure a): either<string,a>
  with pretend-no-div
  match try({p(j, path)})
    Ok(a) -> Right(a)
    Error(e) -> Left(e.message)

// matches the run-step-result
pub value type outcome<a,b,c>
  ReasonFail(a: a)
  ReasonPause(b: b)
  ReasonExec(c: c)
  ReasonResume(c: c)

pub alias log-outcome = outcome<json-src, json-src, json-src>

pub struct step-log
  name: string
  children: list<step-log>
  outcome: log-outcome

pub struct computation-log
  name: string
  pauses: int
  step-logs: list<step-log>

pub effect logger
  // open a step-log. If following a log, returns the corresponding step-log
  fun log-enter(name: string) : maybe<step-log>
  // close a step-log - must provide a log-outcome param because
  // the logger can't distinguish Exec from Resume (split this into -leave-exec and -leave-resume ?)
  fun log-leave(outcome: log-outcome) : ()

pub effect log-ctl<a,b>
  // error the computation with a value, returning a log which can be used to retry later
  final ctl log-fail(a: a): ()
  // pause a computation with a value, returning a log which can be used to resume later
  ctl log-pause(b: b): ()

value struct step-cursor
  steps: vector<step-log>
  next-idx: int

// enables navigation through a computation-log tree, 
// for as long as the log tree matches the computation tree - 
// so errored computations can be continued from the point of error
value struct log-cursor
  step-cursors: list<step-cursor>

// accumulating a single step
value struct open-step
  name: string
  children: list<step-log>

// all the logger state
struct logger-state
  resume-cursor: maybe<log-cursor>
  pause-ignores: int
  completed-steps: list<step-log>
  open-steps: list<open-step>

// collect and return a computation-log from a computation
// this handler also handles any exn effect
// TODO currently using exn internally for fatal programming errors - is that ok?
pub fun log-collector<a,b,c>(
    previous-log: maybe<computation-log>, 
    computation-name: string, 
    handle-fatal-error: (string) -> a,
    a-json-fns: json-fns<a>,
    b-json-fns: json-fns<b>,
    action: () -> <div,logger,log-ctl<a,b>> c
  ) : div (outcome<a,b,c>, computation-log)

  var state := make-logger-state(previous-log)

  with return(v) 
    val pauses = match previous-log
      Just(pl) -> pl.pauses - state.pause-ignores
      _ -> 0 - state.pause-ignores

    (v, Computation-Log(computation-name, pauses, state.completed-steps.reverse))

  with handler
    final ctl throw-exn(ex)
      state := state/close-open-steps(state, ReasonFail({a-json-fns.to-json()(ex.show.handle-fatal-error)}), handle-fatal-error, a-json-fns)
      ReasonFail(handle-fatal-error(ex.show))

  with handler
    final ctl log-fail(a)
      state := state/close-open-steps(state, ReasonFail({a-json-fns.to-json()(a)}), handle-fatal-error, a-json-fns)
      ReasonFail(a)
    ctl log-pause(b)
      if state.pause-ignores >= 1 then
        state := state(pause-ignores=state.pause-ignores - 1)
        resume(())
      else
        val state' = state/close-open-steps(state, ReasonPause({b-json-fns.to-json()(b)}), handle-fatal-error, a-json-fns)
        state := state'(pause-ignores=state.pause-ignores - 1)
        ReasonPause(b)

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

// state helpers

fun make-step-cursor(sls: list<step-log>): step-cursor
  Step-cursor(steps = vector(sls), next-idx = 0)

fun make-log-cursor(log: computation-log): log-cursor
  Log-cursor(step-cursors = Cons(make-step-cursor(log.step-logs), Nil))

fun make-logger-state(previous-log: maybe<computation-log>)
  match previous-log
    Just(pl) -> Logger-state(Just(make-log-cursor(pl)), pl.pauses, [], [])
    _ -> Logger-state(Nothing, 0, [], [])

// propagate an outcome through the open-steps, closing them all and 
// returning a single step-log representing the whole closed tree. Add a
// fatal error if there are no open-steps
fun state/close-open-steps<a>(state: logger-state, outcome: log-outcome, handle-fatal-error: (string)-> a, a-json-fns: json-fns<a>): total logger-state
  match state.open-steps
    [] -> 
      val fatal-step = Step-Log("error", [], ReasonFail({a-json-fns.to-json()("no open steps".handle-fatal-error)}))
      val completed-steps' = Cons(fatal-step, state.completed-steps)
      state(completed-steps=completed-steps')
    Cons(first, Nil) -> 
      val step-log = Step-Log(first.name, first.children.reverse, outcome)
      val completed-steps' = Cons(step-log, state.completed-steps)
      state(completed-steps=completed-steps', open-steps=[])
    Cons(first, Cons(next, rest)) -> 
      val step-log = Step-Log(first.name, first.children.reverse, outcome)
      val next' = next(children = Cons(step-log, next.children))
      state/close-open-steps(pretend-decreasing(state(open-steps=Cons(next', rest))), outcome, handle-fatal-error, a-json-fns)

// if we are following a log and have a step to enter, update the log cursor
// and return the logged step. either way, create a new `open-step` to 
// record the outcome
fun state/log-enter(state: logger-state, step-name: string): total (logger-state, maybe<step-log>)
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
      val open-steps' = match state.open-steps
        [] -> [Open-Step(step-name, [])]
        rest -> Cons(Open-Step(step-name, []), rest)
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
      Right((Cons(Step-Log(first.name, first.children.reverse, outcome), state.completed-steps),
            []))
    Cons(first, Cons(next, rest)) -> 
      val step-log = Step-Log(first.name, first.children.reverse, outcome)
      val next' = next(children = Cons(step-log, next.children))
      val open-steps' = Cons(next', rest)
      Right((state.completed-steps, open-steps'))
    [] -> Left("log-leave: no open-step")

  match error-completed-steps-open-steps
    Right((completed-steps', open-steps')) ->
      Right(state'(completed-steps=completed-steps', open-steps=open-steps'))
    Left(err) -> Left(err)

// printing and JSON serialisation

pub fun string-src/show(s: string-src): string
  s()
pub fun string-src/to-json(s: string-src): json
  JSString(s())
pub fun string-src/from-json(j: json, path: string): pure string-src
  match j
    JSString(s) -> fn () -> s
    _ -> throw("Expecting string-src, got " ++ j.show, info=JsonDeserializationException(j, path))

pub fun json-src/show(a: json-src): string
  a().show
pub fun json-src/to-json(s: json-src): json
  s()
pub fun json-src/from-json(j: json, path: string): pure json-src
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
pub fun either/make-json-fns<a,b>(a-json-fns: json-fns<a>, b-json-fns: json-fns<b>): json-fns<either<a,b>>
  Json-fns(
    fn(eab) -> either/to-json(eab, ?a/to-json=a-json-fns.to-json, ?b/to-json=b-json-fns.to-json),
    fn(j, path) -> either/from-json(j, path, ?a/from-json=a-json-fns.from-json, ?b/from-json=b-json-fns.from-json)
  )

pub fun outcome/show<a,b,c>(outcome: outcome<a,b,c>, ?a/show: (a)->string, ?b/show: (b)->string, ?c/show: (c)->string): string
  match outcome
    ReasonFail(a) -> "error: " ++ a.?a/show
    ReasonPause(b) -> "paused: " ++ b.?b/show
    ReasonExec(c) -> "exec: " ++ c.?c/show
    ReasonResume(c) -> "resume: " ++ c.?c/show

pub fun log-outcome/show(outcome: log-outcome): string
  match outcome
    ReasonFail(a) -> "error: " ++ a().show
    ReasonPause(b) -> "paused: " ++ b().show
    ReasonExec(c) -> "exec: " ++ c().show
    ReasonResume(c) -> "resume: " ++ c().show
pub fun log-outcome/to-json(o: log-outcome): pure json
  match o
    ReasonFail(a) -> JSObj([("ReasonFail", a().to-json)])
    ReasonPause(b) -> JSObj([("ReasonPause", b().to-json)])
    ReasonExec(c) -> JSObj([("ReasonExec", c().to-json)])
    ReasonResume(c) -> JSObj([("ReasonResume", c().to-json)])
pub fun log-outcome/from-json(j: json, path: string): pure log-outcome
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

pub fun step-log/show(a: step-log): string
  with apply-indent(0)
  a.pp
pub fun step-log/to-json(sl: step-log): pure json
  JSObj([("name", JSString(sl.name)),
         ("children", JSList(sl.children.map(to-json))), 
         ("outcome", sl.outcome.log-outcome/to-json)])
pub fun step-log/from-json(j: json, path: string): pure step-log
  match j
    JSObj(l) -> 
      Step-log(
        l.from-json("name", path),
        l.from-json("children", path),
        l.from-json("outcome", path)
      )
    _ -> throw("Expecting step-log, got " ++ j.show, info=JsonDeserializationException(j, path))

pub fun computation-log/show(a: computation-log): string
  with apply-indent(0)
  a.pp
pub fun computation-log/to-json(cl: computation-log): pure json
  JSObj([("name", JSString(cl.name)),
         ("pauses", JSInt(cl.pauses)),
         ("step-logs", JSList(cl.step-logs.map(to-json)))])
pub fun computation-log/from-json(j: json, path: string): pure computation-log
  match j
    JSObj(l) -> 
      Computation-log(
        l.from-json("name", path),
        l.from-json("pauses", path),
        l.from-json("step-logs", path)
      )
    _ -> throw("Expecting computation-log, got " ++ j.show, info=JsonDeserializationException(j, path))

effect val indent: int
pub fun apply-indent<e>(n: int, action: () -> <indent|e> a) : e a
  with val indent = n
  action()

fun pad()
  match indent 
    0 -> ""
    _ -> " " ++ repeat(" |", (indent - 1)) ++ " "

pub fun log-outcome/pp(outcome: log-outcome): string
  log-outcome/show(outcome)

pub fun step-log/pp'(log: step-log): <div,indent> string
  val p = pad()

  val children-output = apply-indent(indent + 1)
   log.children.map(step-log/pp').joinsep/join("\n" ++ pad() ++ "--\n")

  p ++ "run: " ++ log.name ++
  (if length(log.children)>0 then "\n" ++ pad() ++ "children:\n" ++ children-output else "") ++
  "\n" ++ p ++ "outcome: " ++ log.outcome.pp
pub fun step-log/pp(log: step-log): indent string
  with pretend-no-div
  log.pp'

pub fun computation-log/pp(log: computation-log): total string
  with apply-indent(1)
  "computation: " ++ log.name ++ "\n" ++
  "pauses: " ++ log.pauses.show ++ "\n" ++
  log.step-logs.map(step-log/pp).joinsep/join("\n" ++ pad() ++ "--\n")

pub val string/json-fns = Json-fns(string/to-json, string/from-json)
pub val int/json-fns = Json-fns(int/to-json, int/from-json)

// test/example

pub fun stuff(i: int)
  log-enter("stuff")
  val o = i + i
  // log-fail({o.show})
  log-leave(ReasonExec({o.to-json}))
  0

// note: need to annotate fn to set the right outcome type for the log-collector
pub fun thingz(i: int): <logger,log-ctl<string,string>> int
  log-enter("thingz")
  val o = i * i
  // throw("oops: " ++ o.show)
  log-leave(ReasonExec({o.to-json}))
  o

pub fun collect-test-logs()
  with log-collector(
    Nothing, 
    "test", 
    fn (s: string) -> s,
    string/json-fns,
    string/json-fns
    )
  // log-pause("foo", string/json-fns)
  val v = stuff(5)
  val w = thingz(v)
  w

pub fun logs-an-error()
  log-enter("logs-an-error")
  log-fail("boo!")

pub fun collect-logs-with-error()
  with log-collector(
    Nothing, 
    "test-with-error",
    fn (s: string) -> s,
    string/json-fns,
    string/json-fns
  )
  val v = stuff(5)
  val w = thingz(v)
  logs-an-error()
  w

pub fun throws-an-exception()
  log-enter("throws-an-exception")
  throw("oops")

pub fun test-collect-logs()
  match collect-test-logs()
    (v, log) -> 
      log.pp.println
      println("Result: " ++ v.show)
  match collect-logs-with-error()
    (v, log) -> 
      log.pp.println
      println("Result: " ++ v.show)
