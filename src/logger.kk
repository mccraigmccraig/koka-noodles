module logger

import parsers/serjson/serjson
import std/core/exn
import std/core/string 
import std/core/undiv
import std/core/vector
import std/text/parse

pub extend type exception-info {
  ExnPauseComputation
}

// a thunk for step values - we don't always need to 
// serialize or deserialise all values, e.g. we might have an
// "only log final values if there are no errors, but 
//  log all values if there are errors" policy
pub alias stringsrc = () -> total string

pub fun stringsrc/show(a: stringsrc): string
  a()
pub fun stringsrc/pfn(v: serjson/value): parse stringsrc
  match v
    String(s) -> fn () -> s
    _ -> fail("invalid json: stringsrc/pfn")
pub fun stringsrc/vfn(s: stringsrc): total serjson/value
  String(s())

pub value type outcome-fail-reason<a>
  ReasonError(a: a)
  ReasonPaused

pub fun outcome-fail-reason/show<a>(reason: outcome-fail-reason<a>, ?a/show: (a)->string): string
  match reason
    ReasonError(a) -> "ReasonError: " ++ a.show
    ReasonPaused -> "ReasonPaused"
pub fun outcome-fail-reason/make-vfn<a>(a-vfn: pvfn<a,e>): pvfn<outcome-fail-reason<a>,e>
  fn(reason)
    match reason
      ReasonError(a) -> Object([Object-entry("ReasonError", a-vfn(a))])
      ReasonPaused -> Object([Object-entry("ReasonPaused", Null)])
pub fun outcome-fail-reason/make-pfn<a>(a-pfn: ppfn<a,e>): ppfn<outcome-fail-reason<a>,e>
  fn (v)
    match v
      Object([Object-entry("ReasonError", a)]) -> ReasonError(a-pfn(a))
      Object([Object-entry("ReasonPaused", Null)]) -> ReasonPaused
      _ -> fail("invalid json: outcome-fail-reason/pfn")

pub value type outcome-success-reason<a>
  ReasonExec(a: a)
  ReasonResume(a: a)

pub fun outcome-success-reason/show<a>(reason: outcome-success-reason<a>, ?a/show: (a)->string): string
  match reason
    ReasonExec(a) -> "ReasonExec("  ++ ?a/show(a) ++ ")"
    ReasonResume(a) -> "ReasonResume(" ++ ?a/show(a) ++ ")"
pub fun outcome-success-reason/make-vfn<a>(a-vfn: pvfn<a,e>): pvfn<outcome-success-reason<a>,e>
  fn(reason)
    match reason
      ReasonExec(a) -> Object([Object-entry("ReasonExec", a-vfn(a))])
      ReasonResume(a) -> Object([Object-entry("ReasonResume", a-vfn(a))])
pub fun outcome-success-reason/make-pfn<a>(a-pfn: ppfn<a,e>): ppfn<outcome-success-reason<a>,e>
  fn(v)
    match v
      Object([Object-entry("ReasonExec", a)]) -> ReasonExec(a-pfn(a))
      Object([Object-entry("ReasonResume", a)]) -> ReasonResume(a-pfn(a))
      _ -> fail("invalid json: outcome-success-reason/pfn")


pub alias outcome<a,b> = either<outcome-fail-reason<a>,b>

pub fun outcome/show<a,b>(outcome: outcome<a,b>, ?a/show: (a)->string, ?b/show: (b)->string): string
  match outcome
    Left(ReasonError(a)) -> "Left(ReasonError(" ++ a.show ++ "))"
    Left(ReasonPaused) -> "Left(ReasonPaused)"
    Right(b) -> "Right(" ++ b.show ++ ")"
pub fun outcome/make-vfn<a,b>(a-vfn: pvfn<a,e>, b-vfn: pvfn<b,e>): pvfn<outcome<a,b>,e>
  either/make-vfn(outcome-fail-reason/make-vfn(a-vfn), b-vfn)
pub fun outcome/make-pfn<a,b>(a-pfn: ppfn<a,e>, b-pfn: ppfn<b,e>): ppfn<outcome<a,b>,e>
  either/make-pfn(outcome-fail-reason/make-pfn(a-pfn), b-pfn)

pub alias log-outcome = either<outcome-fail-reason<stringsrc>, outcome-success-reason<stringsrc>>

pub fun log-outcome/show(outcome: log-outcome): string
  outcome/show(outcome)
pub fun log-outcome/vfn(a: log-outcome): total serjson/value
  either/make-vfn(outcome-fail-reason/make-vfn(stringsrc/vfn), outcome-success-reason/make-vfn(stringsrc/vfn))(a)
pub fun log-outcome/pfn(v: serjson/value): parse log-outcome
  either/make-pfn(outcome-fail-reason/make-pfn(stringsrc/pfn), outcome-success-reason/make-pfn(stringsrc/pfn))(v)

pub struct step-log
  name: string
  children: list<step-log>
  outcome: log-outcome

pub fun step-log/show(a: step-log): string
  serjson/json(a, ?vfn=logger/step-log/vfn)
fun step-log/vfn'(sl: step-log): div serjson/value
  Object([Object-entry("name", String(sl.name)), 
          Object-entry("children", list/make-vfn(step-log/vfn')(sl.children)), 
          Object-entry("outcome", log-outcome/vfn(sl.outcome))])
pub fun step-log/vfn(sl: step-log): total serjson/value
  with pretend-no-div
  step-log/vfn'(sl)
fun step-log/pfn'(v: serjson/value): <div,parse> step-log
  match v
    Object([Object-entry("name", String(name)), 
            Object-entry("children", list-children-value), 
            Object-entry("outcome", outcome-object)]) -> 
        val list-children = list/make-pfn(step-log/pfn')(list-children-value)
        val outcome = log-outcome/pfn(outcome-object)
        Step-log(name, list-children, outcome)
    _ -> fail("invalid json: step-log/pfn")
pub fun step-log/pfn(v: serjson/value): parse step-log
  with pretend-no-div
  step-log/pfn'(v)

pub struct computation-log
  name: string
  pauses: int
  step-logs: list<step-log>
  outcome: log-outcome

pub fun computation-log/show(a: computation-log): string
  serjson/json(a, ?vfn=computation-log/vfn)
pub fun computation-log/vfn(cl: computation-log): total serjson/value
  Object([Object-entry("name", String(cl.name)), 
          Object-entry("pauses", Int(cl.pauses)),
          Object-entry("step-logs", list/make-vfn(step-log/vfn)(cl.step-logs)),
          Object-entry("outcome", log-outcome/vfn(cl.outcome))])
pub fun computation-log/pfn(v: serjson/value): parse computation-log
  match v
    Object([Object-entry("name", String(name)), 
            Object-entry("pauses", Int(pauses)),
            Object-entry("step-logs", list-step-logs-value),
            Object-entry("outcome", outcome-value)]) -> 
        val list-step-logs = list/make-pfn(step-log/pfn)(list-step-logs-value)
        val outcome = log-outcome/pfn(outcome-value)
        Computation-log(name, pauses, list-step-logs, outcome)
    _ -> fail("invalid json: computation-log/pfn")

pub effect logger
  // open a step-log. If following a log, returns the corresponding step-log
  fun log-enter(name: string) : maybe<step-log>
  // close a step-log successfully
  fun log-complete(outcome: log-outcome) : ()
  // if following a log, skip the next entry (because it's not going to be run)
  fun log-skip() : ()
  // close a step log with an error and immediately terminate
  ctl log-error(error-ser: stringsrc) : ()
  // pause a computation, returning a log which can be used to resume later
  ctl pause() : ()

effect val indent: int
pub fun apply-indent<e>(n: int, action: () -> <indent|e> a) : e a
  with val indent = n
  action()

fun pad()
  match indent 
    0 -> ""
    _ -> " " ++ repeat(" |", (indent - 1)) ++ " "
  
pub fun step-log/pp(log: step-log): <div,indent> string
  val p = pad()

  val children-output = apply-indent(indent + 1)
   log.children.map(step-log/pp).joinsep/join("\n" ++ pad() ++ "--\n")

  val outcome-output = match log.outcome
    Left(outcome-fail-reason) -> match outcome-fail-reason
      ReasonError(v) -> "error: " ++ v()
      ReasonPaused -> "paused"
    Right(outcome-success-reason) -> match outcome-success-reason
      ReasonExec(v) -> "exec: " ++ v()
      ReasonResume(v) -> "resume: " ++ v()

  p ++ "run: " ++ log.name ++
  (if length(log.children)>0 then "\n" ++ pad() ++ "children:\n" ++ children-output else "") ++
  "\n" ++ p ++ "outcome: " ++ outcome-output

pub fun computation-log/pp(log: computation-log)
  with apply-indent(1)
  "computation: " ++ log.name ++ "\n" ++
  log.step-logs.map(step-log/pp).joinsep/join("\n" ++ pad() ++ "--\n")

pub fun print-computation-log(log: computation-log)
  println(log.pp)

pub fun computation-log/json(log: computation-log): total string
  serjson/json(log, ?vfn=computation-log/vfn)

pub fun computation-log/dejson(s: string): total either<string,computation-log>
  serjson/dejson(s)

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

fun make-step-cursor(sls: list<step-log>): step-cursor
  Step-cursor(steps = vector(sls), next-idx = 0)

fun make-log-cursor(log: computation-log): log-cursor
  Log-cursor(step-cursors = Cons(make-step-cursor(log.step-logs), Nil))

fun make-logger-state(previous-log: maybe<computation-log>)
  match previous-log
    Just(pl) -> Logger-state(Just(make-log-cursor(pl)), pl.pauses, [], [])
    _ -> Logger-state(Nothing, 0, [], [])

// // propagate an error through the open-steps, closing them all and 
// // returning a single step-log representing the whole closed tree
// // propagate an error to a state
// fun state/error-open-steps(state: logger-state, error-ser: stringsrc): total either<string,logger-state>
//   match state.open-steps
//     [] -> Left("error-open-steps: no open-step")
//     Cons(first, Nil) -> 
//       val step-log = Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, StepError, Left(error-ser))
//       val completed-steps' = Cons(step-log, state.completed-steps)
//       Right(state(completed-steps=completed-steps', open-steps=[]))
//     Cons(first, Cons(next, rest)) -> 
//       val step-log = Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, StepError, Left(error-ser))
//       val next' = next(children = Cons(step-log, next.children))
//       state/error-open-steps(pretend-decreasing(state(open-steps=Cons(next', rest))), error-ser)


// // if we are following a log and have a step to enter, update the log cursor
// // and return the logged step. either way, create a new `open-step` to 
// // record the outcome
// fun state/log-enter(state: logger-state, step-name: string, step-type: step-type): total (logger-state, resumed-step)
//   // follow the resumed log, if it matches
//   val state-resumed-step = match state.resume-cursor
//     Just(lc) -> 
//       match lc.step-cursors
//         Cons(Step-cursor(steps, next-idx), rest) | next-idx < length(steps) ->
//           val maybe-sl = at(steps, next-idx)
//           match maybe-sl 
//             // Just(Step-log(_, _, _, StepError, _)) -> (state(resume-cursor=Nothing), NoLog)
//             Just(sl) ->
//               // inc the next-idx counter on the current step-cursor, push a new step-cursor
//               (state(resume-cursor=Just(Log-cursor(Cons(make-step-cursor(sl.children), 
//                                                         Cons(Step-cursor(steps, next-idx + 1), 
//                                                              rest))))), 
//                Log(sl))
//             _ -> 
//               (state(resume-cursor=Nothing), NoLog)
//         _ -> (state(resume-cursor=Nothing), NoLog)
//     _ -> (state(resume-cursor=Nothing), NoLog)
    
//   // always accumulate a new open-step
//   match state-resumed-step
//     (state', resumed-step) -> 
//       val open-steps' = match state.open-steps
//         [] -> [Open-Step(step-name, step-type, Nothing, [])]
//         rest -> Cons(Open-Step(step-name, step-type, Nothing, []), rest)
//       (state'(open-steps=open-steps'), resumed-step)

// // returns Left if there has been a programming error and there are no open steps
// fun state/log-bound-value(state: logger-state, value-ser: stringsrc): total either<string,logger-state>
//   match state.open-steps
//     Cons(first, rest) -> 
//       val open-steps' = Cons(first(bound-value=Just(value-ser)), rest)
//       Right(state(open-steps=open-steps'))
//     [] -> 
//       Left("log-bound-value: no open-step")
  
// fun state/log-complete(state: logger-state, outcome : step-outcome, result-ser : stringsrc): total either<string,logger-state>
//   val state' = match state.resume-cursor
//     Just(lc) ->
//       match lc.step-cursors
//         Cons(Step-cursor(steps, next-idx), rest) | next-idx == length(steps) ->
//           state(resume-cursor=Just(Log-cursor(rest)))
//         _ -> state(resume-cursor=Nothing)
//     _ -> state(resume-cursor=Nothing)

//   val error-completed-steps-open-steps = match state.open-steps
//     Cons(first, Nil) -> 
//       Right((Cons(Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, outcome, Right(result-ser)), state.completed-steps),
//             []))
//     Cons(first, Cons(next, rest)) -> 
//       val step-log = Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, outcome, Right(result-ser))
//       val next' = next(children = Cons(step-log, next.children))
//       val open-steps' = Cons(next', rest)
//       Right((state.completed-steps, open-steps'))
//     [] -> Left("log-complete: no open-step")

//   match error-completed-steps-open-steps
//     Right((completed-steps', open-steps')) ->
//       Right(state(completed-steps=completed-steps', open-steps=open-steps'))
//     Left(err) -> Left(err)

// fun state/log-skip(state: logger-state): total either<string,logger-state>
//   match state.resume-cursor
//     Just(lc) ->
//       match lc.step-cursors
//         Cons(Step-cursor(steps, next-idx), rest-step-cursors) | next-idx < length(steps) ->
//           val maybe-sl = at(steps, next-idx)
//           match maybe-sl
//             Just(sl) ->
//               val sl' = Step-Log(sl.name, sl.step-type, sl.bound-value, sl.children, Resume, sl.result)
//               match state.open-steps
//                 Nil -> 
//                   Right(state(resume-cursor=Just(Log-cursor(step-cursors=Cons(Step-cursor(steps, next-idx + 1), Nil))),
//                               completed-steps=Cons(sl', state.completed-steps)))
//                 Cons(first, rest) ->
//                   Right(state(resume-cursor=Just(Log-cursor(step-cursors=Cons(Step-cursor(steps, next-idx + 1), rest-step-cursors))),
//                               open-steps=Cons(first(children=Cons(sl', first.children)), rest)))
//             _ -> Left("log-skip: fell off the resume-log")
//         _ -> Right(state(resume-cursor=Nothing))
//     _ -> Right(state(resume-cursor=Nothing))

// fun state/log-error(state: logger-state, error-ser: stringsrc): total either<string,logger-state>
//   match state.open-steps
//     Cons(first, Nil) -> 
//       val completed-steps' = Cons(Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, StepError, Left(error-ser)), state.completed-steps)
//       Right(state(completed-steps=completed-steps'))
//     Cons(first, Cons(next, rest)) ->
//       val step-log = Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, StepError, Left(error-ser))
//       val next' = next(children = Cons(step-log, next.children))
//       Right(state(open-steps=Cons(next', rest)))
//     [] -> Left("log-error: no open-step")

// fun state/pause(state: logger-state): total logger-state
//   state(pause-ignores=state.pause-ignores - 1)

// // collect and return a computation-log from a computation
// // this handler also handles any exn effect, so the 
// // return is div
// pub fun log-collector<v>(
//     previous-log: maybe<computation-log>, 
//     computation-name: string, 
//     opts: log-options,
//     action: () -> <div,exn,logger> v
//   ) : div (outcome<string,v>, computation-log)

//   var state := make-logger-state(previous-log)

//   with return(v) 
//     val pauses = match previous-log
//       Just(pl) -> pl.pauses - state.pause-ignores
//       _ -> 0 - state.pause-ignores
//     (v, Computation-Log(computation-name, opts, pauses, state.completed-steps.reverse))

//   // close out all open steps with an error, then return
//   with handler
//     final ctl throw-exn(ex)
//       match ex
//         Exception(_, ExnPauseComputation) -> 
//           match state/error-open-steps(state, {ex.show})
//             Left(err) -> OutcomeError("Fatal: " ++ err.show)
//             Right(state') -> 
//               state := state'
//               OutcomePaused
          
//         _ ->
//           match state/error-open-steps(state, {ex.show})
//             Left(err) -> OutcomeError("Fatal: " ++ err.show)
//             Right(state') -> 
//               state := state'
//               OutcomeError(ex.show)

//   // the handler implementation fns are all total, but can return errors via either. 
//   // Such errors will be thrown to the throw-exn handler, to immediately terminate 
//   // the computation
//   with handler
//     val log-options = opts
//     fun log-enter(name, step-type)
//       match state/log-enter(state, name, step-type)
//         (state', resumed-step) -> 
//           state := state'
//           resumed-step
//     fun log-bound-value(value-ser)
//       match state/log-bound-value(state, value-ser)
//         Left(err) -> throw(err)
//         Right(state') -> state := state'
//     fun log-complete(outcome, result-ser)
//       match state/log-complete(state, outcome, result-ser)
//         Left(err) -> throw(err)
//         Right(state') -> state := state'
//     fun log-skip()
//       match state/log-skip(state)
//         Left(err) -> throw(err)
//         Right(state') -> state := state'
//     ctl log-error(error-ser)
//       match state/log-error(state, error-ser)
//         Left(err) -> throw(err)
//         Right(state') -> state := state'
//       throw(error-ser())
//     ctl pause()
//       state := state/pause(state)
//       if state.pause-ignores >= 0 then
//         resume(())
//       else
//         throw("Paused", ExnPauseComputation)

//   OutcomeOk(action())

// // test/example

// pub fun stuff(i: int)
//   log-enter("stuff", RunStep)
//   val o = i + i
//   // log-error({o.show})
//   log-complete(Exec, {o.show})
//   o

// pub fun thingz(i: int)
//   log-enter("thingz", BindStep)
//   log-bound-value({i.show})
//   val o = i * i
//   // throw("oops: " ++ o.show)
//   log-complete(Exec, {o.show})
//   o

// pub fun collect-test-logs()
//   with log-collector(Nothing, "test", default-log-options)
//   val v = stuff(5)
//   val w = thingz(v)
//   w

// pub fun logs-an-error()
//   log-enter("logs-an-error", RunStep)
//   log-error({"boo!"})

// pub fun collect-logs-with-error()
//   with log-collector(Nothing, "test-with-error", default-log-options)
//   val v = stuff(5)
//   logs-an-error()
//   v

// pub fun throws-an-exception()
//   log-enter("throws-an-exception", RunStep)
//   throw("oops")

// pub fun collect-logs-with-exception()
//   with log-collector(Nothing, "test-with-exception", default-log-options)
//   val v = stuff(5)
//   throws-an-exception()
//   v

// pub fun test-collect-logs()
//   match collect-test-logs()
//     (v, log) -> 
//       print-computation-log(log)
//       println("Result: " ++ v.show)
//   match collect-logs-with-error()
//     (v, log) -> 
//       print-computation-log(log)
//       println("Result: " ++ v.show)
//   match collect-logs-with-exception()
//     (v, log) -> 
//       print-computation-log(log)
//       println("Result: " ++ v.show)
