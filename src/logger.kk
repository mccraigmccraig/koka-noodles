module logger

import std/core/exn
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

pub value struct jsonde<a>
  json: (a) -> total json
  de: (json) -> total either<string,a>

// matches the run-step-result
pub value type outcome<a,b,c>
  ReasonFatal(msg: string)
  ReasonFail(a: a)
  ReasonPaused(b: b)
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

pub effect logger<a,b,c>
  // open a step-log. If following a log, returns the corresponding step-log
  fun log-enter(name: string) : maybe<step-log>
  // close a step-log - must provide a log-outcome param because
  // the logger can't distinguish Exec from Resume (split this into -leave-exec and -leave-resume ?)
  fun log-leave(outcome: log-outcome) : ()
  // error the computation with a value, returning a log which can be used to retry later
  ctl log-fail(a: a, jsonde: jsonde<a>) : ()
  // pause a computation with a value, returning a log which can be used to resume later
  ctl log-pause(b: b, jsonde: jsonde<b>) : ()
  // succeed a computation with a value, also returning a log
  ctl log-ok(c: c, jsonde: jsonde<c>): ()

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
    action: () -> <div,logger<a,b,c>> outcome<a,b,c>
  ) : div (outcome<a,b,c>, computation-log)

  var state := make-logger-state(previous-log)

  with return(v) 
    val pauses = match previous-log
      Just(pl) -> pl.pauses - state.pause-ignores
      _ -> 0 - state.pause-ignores

    (v, Computation-Log(computation-name, pauses, state.completed-steps.reverse))

  with handler
    final ctl throw-exn(ex)
      state := state/close-open-steps(state, ReasonFatal(ex.show))
      ReasonFail(handle-fatal-error(ex.show))

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
    ctl log-fail(a, a-jsonde)
      state := state/close-open-steps(state, ReasonFail({a-jsonde.json()(a)}))
      ReasonFail(a)
    ctl log-pause(b, b-jsonde)
      val state' = state/close-open-steps(state, ReasonPaused({b-jsonde.json()(b)}))
      val state'' = state'(pause-ignores=state.pause-ignores - 1)
      state := state''
      if state''.pause-ignores >= 0 then
        resume(())
      else
        ReasonPaused(b)
    ctl log-ok(c, c-jsonde)
      state := state/close-open-steps(state, ReasonExec({c-jsonde.json()(c)}))
      ReasonExec(c)

  action()

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
// step with ReasonFatal if there are no open steps
fun state/close-open-steps(state: logger-state, outcome: log-outcome): total logger-state
  match state.open-steps
    [] -> 
      val fatal-step = Step-Log("error", [], ReasonFatal("no open steps"))
      val completed-steps' = Cons(fatal-step, state.completed-steps)
      state(completed-steps=completed-steps')
    Cons(first, Nil) -> 
      val step-log = Step-Log(first.name, first.children.reverse, outcome)
      val completed-steps' = Cons(step-log, state.completed-steps)
      state(completed-steps=completed-steps', open-steps=[])
    Cons(first, Cons(next, rest)) -> 
      val step-log = Step-Log(first.name, first.children.reverse, outcome)
      val next' = next(children = Cons(step-log, next.children))
      state/close-open-steps(pretend-decreasing(state(open-steps=Cons(next', rest))), outcome)

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

// // printing and JSON serialisation

// pub fun string-src/show(s: string-src): string
//   s()
// pub fun string-src/to-json(s: string-src): json
//   JSString(s())
// pub fun string-src/from-json(j: json, path: string): pure string-src
//   match j
//     JSString(s) -> fn () -> s
//     _ -> throw("Expecting string-src, got " ++ j.show, info=JsonDeserializationException(j, path))

// pub fun json-src/show(a: json-src): string
//   a().show
// pub fun json-src/to-json(s: json-src): json
//   s()
// pub fun json-src/from-json(j: json, path: string): pure json-src
//   fn () -> j

// pub fun fail-reason/show<a>(reason: fail-reason<a>, ?a/show: (a)->string): string
//   match reason
//     ReasonFail(a) -> "ReasonFail: " ++ a.show
//     ReasonPaused -> "ReasonPaused"
// pub fun fail-reason/to-json(fr: fail-reason<a>, ?a/to-json: a -> e json): e json
//   match fr
//     ReasonFail(a) -> JSObj([("ReasonFail", a/to-json(a))])
//     ReasonPaused -> JSObj([("ReasonPaused", JSNull)])
// pub fun fail-reason/from-json(j: json, path: string, ?a/from-json: (j: json, path: string) -> pure a): pure fail-reason<a>
//   match j
//     JSObj(l) ->
//       match l.lookup(fn (k) k=="ReasonFail")
//         Just(jj) -> ReasonFail(?a/from-json(jj, path ++ "->ReasonFail"))
//         _ -> match l.lookup(fn (k) k=="ReasonPaused")
//           Just(JSNull) -> ReasonPaused
//           _ -> throw("Expecting fail-reason, got " ++ l.show, info=JsonDeserializationException(JSObj(l), path))
//     _ -> throw("Expecting fail-reason, got " ++ j.show, info=JsonDeserializationException(j, path))

// pub fun success-reason/show<a>(reason: success-reason<a>, ?a/show: (a)->string): string
//   match reason
//     ReasonExec(a) -> "ReasonExec("  ++ ?a/show(a) ++ ")"
//     ReasonResume(a) -> "ReasonResume(" ++ ?a/show(a) ++ ")"
// pub fun success-reason/to-json(sr: success-reason<a>, ?a/to-json: a -> e json): e json
//   match sr
//     ReasonExec(a) -> JSObj([("ReasonExec", a/to-json(a))])
//     ReasonResume(a) -> JSObj([("ReasonResume", a/to-json(a))])
// pub fun success-reason/from-json(j: json, path: string, ?a/from-json: (j: json, path: string) -> pure a): pure success-reason<a>
//   match j
//     JSObj(l) ->
//       match l.lookup(fn (k) k=="ReasonExec")
//         Just(jj) -> ReasonExec(?a/from-json(jj, path ++ "->ReasonExec"))
//         _ -> match l.lookup(fn (k) k=="ReasonResume")
//           Just(jjj) -> ReasonResume(?a/from-json(jjj, path ++ "->ReasonResume"))
//           _ -> throw("Expecting success-reason, got " ++ l.show, info=JsonDeserializationException(JSObj(l), path))
//     _ -> throw("Expecting success-reason, got " ++ j.show, info=JsonDeserializationException(j, path))

// pub fun outcome/show<a,b>(outcome: outcome<a,b>, ?a/show: (a)->string, ?b/show: (b)->string): string
//   match outcome
//     Left(ReasonFail(a)) -> "Left(ReasonFail(" ++ a.show ++ "))"
//     Left(ReasonPaused) -> "Left(ReasonPaused)"
//     Right(b) -> "Right(" ++ b.show ++ ")"
// pub fun outcome/to-json<a,b>(o: outcome<a,b>, ?a/to-json: a -> e json, ?b/to-json: b -> e json): e json
//   match o
//     Left(fa) -> JSObj([("Left", fail-reason/to-json(fa))])
//     Right(b) -> JSObj([("Right", b/to-json(b))])
// pub fun outcome/from-json<a,b>(j: json, path: string, ?a/from-json: (j: json, path: string) -> pure a, ?b/from-json: (j: json, path: string) -> pure b): pure outcome<a,b>
//   match j
//     JSObj(l) ->
//       match l.lookup(fn (k) k=="Left")
//         Just(fr) -> Left(fail-reason/from-json(fr, path ++ "->Left"))
//         _ -> match l.lookup(fn (k) k=="Right")
//           Just(jj) -> Right(?b/from-json(jj, path ++ "->Right"))
//           _ -> throw("Expecting outcome, got " ++ l.show, info=JsonDeserializationException(JSObj(l), path))
//     _ -> throw("Expecting outcome, got " ++ j.show, info=JsonDeserializationException(j, path))

// pub fun log-outcome/show(outcome: log-outcome): string
//   outcome.outcome/show(?a/show=string-src/show, ?b/show=fn (sr) -> sr.show)
// pub fun log-outcome/to-json(o: log-outcome): pure json
//   match o
//     Left(fa) -> JSObj([("Left", fa.fail-reason/to-json)])
//     Right(sb) -> JSObj([("Right", sb.success-reason/to-json)])
// pub fun log-outcome/from-json(j: json, path: string): pure log-outcome
//   match j
//     JSObj(l) ->
//       match l.lookup(fn (k) k=="Left")
//         Just(fr) -> Left(fail-reason/from-json(fr, path ++ "->Left"))
//         _ -> match l.lookup(fn (k) k=="Right")
//           Just(sr) -> Right(success-reason/from-json(sr, path ++ "->Right"))
//           _ -> throw("Expecting log-outcome, got " ++ l.show, info=JsonDeserializationException(JSObj(l), path))
//     _ -> throw("Expecting log-outcome, got " ++ j.show, info=JsonDeserializationException(j, path))

// pub fun step-log/show(a: step-log): string
//   with apply-indent(0)
//   a.pp
// pub fun step-log/to-json(sl: step-log): pure json
//   JSObj([("name", JSString(sl.name)),
//          ("children", JSList(sl.children.map(to-json))), 
//          ("outcome", sl.outcome.log-outcome/to-json)])
// pub fun step-log/from-json(j: json, path: string): pure step-log
//   match j
//     JSObj(l) -> 
//       Step-log(
//         l.from-json("name", path),
//         l.from-json("children", path),
//         l.from-json("outcome", path)
//       )
//     _ -> throw("Expecting step-log, got " ++ j.show, info=JsonDeserializationException(j, path))

// pub fun computation-log/show(a: computation-log): string
//   with apply-indent(0)
//   a.pp
// pub fun computation-log/to-json(cl: computation-log): pure json
//   JSObj([("name", JSString(cl.name)),
//          ("pauses", JSInt(cl.pauses)),
//          ("step-logs", JSList(cl.step-logs.map(to-json)))])
// pub fun computation-log/from-json(j: json, path: string): pure computation-log
//   match j
//     JSObj(l) -> 
//       Computation-log(
//         l.from-json("name", path),
//         l.from-json("pauses", path),
//         l.from-json("step-logs", path)
//       )
//     _ -> throw("Expecting computation-log, got " ++ j.show, info=JsonDeserializationException(j, path))

// effect val indent: int
// pub fun apply-indent<e>(n: int, action: () -> <indent|e> a) : e a
//   with val indent = n
//   action()

// fun pad()
//   match indent 
//     0 -> ""
//     _ -> " " ++ repeat(" |", (indent - 1)) ++ " "

// pub fun log-outcome/pp(outcome: log-outcome): string
//   match outcome
//     Left(fail-reason) -> match fail-reason
//       ReasonFail(v) -> "error: " ++ v().show
//       ReasonPaused -> "paused"
//     Right(success-reason) -> match success-reason
//       ReasonExec(v) -> "exec: " ++ v().show
//       ReasonResume(v) -> "resume: " ++ v().show

// pub fun step-log/pp'(log: step-log): <div,indent> string
//   val p = pad()

//   val children-output = apply-indent(indent + 1)
//    log.children.map(step-log/pp').joinsep/join("\n" ++ pad() ++ "--\n")

//   p ++ "run: " ++ log.name ++
//   (if length(log.children)>0 then "\n" ++ pad() ++ "children:\n" ++ children-output else "") ++
//   "\n" ++ p ++ "outcome: " ++ log.outcome.pp
// pub fun step-log/pp(log: step-log): indent string
//   with pretend-no-div
//   log.pp'

// pub fun computation-log/pp(log: computation-log): total string
//   with apply-indent(1)
//   "computation: " ++ log.name ++ "\n" ++
//   "pauses: " ++ log.pauses.show ++ "\n" ++
//   log.step-logs.map(step-log/pp).joinsep/join("\n" ++ pad() ++ "--\n")





// // test/example

// pub fun stuff(i: int)
//   log-enter("stuff")
//   val o = i + i
//   // log-fail({o.show})
//   log-leave(ReasonExec({o.to-json}))
//   o

// pub fun thingz(i: int)
//   log-enter("thingz")
//   val o = i * i
//   // throw("oops: " ++ o.show)
//   log-leave(ReasonExec({o.to-json}))
//   o

// pub fun collect-test-logs()
//   with log-collector(Nothing, "test")
//   val v = stuff(5)
//   val w = thingz(v)
//   w

// pub fun logs-an-error()
//   log-enter("logs-an-error")
//   log-fail({"boo!"})

// pub fun collect-logs-with-error()
//   with log-collector(Nothing, "test-with-error")
//   val v = stuff(5)
//   logs-an-error()
//   v

// pub fun throws-an-exception()
//   log-enter("throws-an-exception")
//   throw("oops")

// pub fun collect-logs-with-exception()
//   with log-collector(Nothing, "test-with-exception")
//   val v = stuff(5)
//   throws-an-exception()
//   v

// pub fun test-collect-logs()
//   match collect-test-logs()
//     (v, log) -> 
//       log.pp.println
//       println("Result: " ++ v.show)
//   match collect-logs-with-error()
//     (v, log) -> 
//       log.pp.println
//       println("Result: " ++ v.show)
//   match collect-logs-with-exception()
//     (v, log) -> 
//       log.pp.println
//       println("Result: " ++ v.show)
