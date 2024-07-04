module logger

import std/core/string 
import std/core/vector

// a thunk for step values - we don't always need to 
// serialize or deserialise all values, e.g. we might have an
// "only log final values if there are no errors, but 
//  log all values if there are errors" policy
pub alias stringsrc = () -> total string

pub type step-outcome
  Run
  Resume
  StepError
  UnhandledStepError

pub fun step-outcome/show(outcome: step-outcome)
  match outcome
    Run -> "run"
    Resume -> "resume"
    StepError -> "error"
    UnhandledStepError -> "unhandled error"

pub struct step-log
  name: string
  bound-value: maybe<stringsrc>
  children: list<step-log>
  outcome: step-outcome
  result: either<stringsrc,stringsrc>

// When resuming a computation, an existing step-log will be
// given back as the result of log-enter
pub value type resumed-step
  Log(l: step-log)
  NoLog

pub effect logger
  // open a step-log
  fun log-enter(name: string) : resumed-step
  fun log-bound-value(value-ser: stringsrc) : ()
  // close a step log successfully
  fun log-complete(outcome: step-outcome, result-ser: stringsrc) : ()
  // close a step log with an error and immediately terminate
  ctl log-error(error-ser: stringsrc) : ()

effect val indent: int
fun apply-indent<e>(n: int, action: () -> <indent|e> a) : e a
  with val indent = n
  action()

fun pad()
  match indent 
    0 -> ""
    _ -> " " ++ repeat(" |", (indent - 1)) ++ " "
  
pub fun step-log/show(log: step-log): <div,indent> string
  val p = pad()

  val bound-value-output = match log.bound-value
    Just(v) -> "bound-value: " ++ v()
    Nothing -> "bound-value: Nothing"

  val step-output = match log.result
    Left(err) -> "error: " ++ err()
    Right(v) -> "result: " ++ v()

  val children-output = apply-indent(indent + 1)
   log.children.map(step-log/show).joinsep/join("\n" ++ pad() ++ "--\n")

  p ++ "step: " ++ log.name ++
  "\n" ++ p ++ bound-value-output ++
  (if length(log.children)>0 then "\n" ++ pad() ++ "children:\n" ++ children-output else "") ++
  "\n" ++ p ++ "outcome: " ++ log.outcome.show ++
  "\n" ++ p ++ step-output

pub value struct computation-log
  name: string
  step-logs: list<step-log>

pub fun computation-log/show(log: computation-log)
  with apply-indent(1)
  "computation: " ++ log.name ++ "\n" ++
  log.step-logs.map(step-log/show).joinsep/join("\n" ++ pad() ++ "--\n")

pub fun print-computation-log(log: computation-log)
  println(log.show)

value struct step-cursor
  steps: vector<step-log>
  next-idx: int

// enables navigation through a computation-log tree, 
// for as long as the log tree matches the computation tree - 
// so errored computations can be continued from the point of error
value struct log-cursor
  step-cursors: list<step-cursor>

value type log-cursor-nav-result
  // a matching step with a completed log was found
  RunStep(sl: step-log) 
  // a matching step with an error log was found
  ErrorStep(sl: step-log)
  // the leave operation was matched
  NoStep
  // there was a tree-shape mismatch
  Mismatch

// accumulating a single step
value struct open-step
  name: string
  bound-value: maybe<stringsrc>
  children: list<step-log>

// all the logger state
struct logger-state
  resume-cursor: maybe<log-cursor>
  completed-steps: list<step-log>
  open-steps: list<open-step>

fun make-step-cursor(sls: list<step-log>): step-cursor
  Step-cursor(steps = vector(sls), next-idx = 0)

fun make-log-cursor(log: computation-log): log-cursor
  Log-cursor(step-cursors = Cons(make-step-cursor(log.step-logs), Nil))

fun make-logger-state(previous-log: maybe<computation-log>)
  match previous-log
    Just(pl) -> Logger-state(Just(make-log-cursor(pl)), [], [])
    _ -> Logger-state(Nothing, [], [])

// if we are following a log and have a step to enter, update the log cursor
// and return the logged step. either way, create a new `open-step` to 
// record the outcome
fun state/enter(state: logger-state, step-name: string): total (logger-state, resumed-step)
  // follow the resumed log, if it matches
  val state-resumed-step = match state.resume-cursor
    Just(lc) -> 
      match lc.step-cursors
        Cons(Step-cursor(steps, next-idx), rest) | next-idx < length(steps) ->
          val maybe-sl = at(steps, next-idx)
          match maybe-sl 
            Just(Step-log(_, _, _, StepError, _)) -> (state(resume-cursor=Nothing), NoLog)
            Just(Step-log(_, _, _, UnhandledStepError, _)) -> (state(resume-cursor=Nothing), NoLog)
            Just(sl) ->
              // inc the next-idx counter on the current step-cursor, push a new step-cursor
              (state(resume-cursor=Just(Log-cursor(Cons(make-step-cursor(sl.children), 
                                                        Cons(Step-cursor(steps, next-idx + 1), 
                                                             rest))))), 
               Log(sl))
            _ -> (state(resume-cursor=Nothing), NoLog)
        _ -> (state(resume-cursor=Nothing), NoLog)
    _ -> (state(resume-cursor=Nothing), NoLog)
    
  // always accumulate a new open-step
  match state-resumed-step
    (state', resumed-step) -> 
      val open-steps' = match state.open-steps
        [] -> [Open-Step(step-name, Nothing, [])]
        rest -> Cons(Open-Step(step-name, Nothing, []), rest)
      (state'(open-steps=open-steps'), resumed-step)

fun state/log-bound-value(state: logger-state, value-ser: stringsrc): exn logger-state
  val open-steps' = match state.open-steps
    Cons(first, rest) -> Cons(first(bound-value=Just(value-ser)), rest)
    [] -> throw("Unexpected log-bound-value")
  state(open-steps=open-steps')    


fun state/leave(state: logger-state, outcome : step-outcome, result-ser : stringsrc): exn logger-state
  val state' = match state.resume-cursor
    Just(lc) ->
      match lc.step-cursors
        Cons(Step-cursor(steps, next-idx), rest) | next-idx == length(steps) ->
          state(resume-cursor=Just(Log-cursor(rest)))
        _ -> state(resume-cursor=Nothing)
    _ -> state(resume-cursor=Nothing)

  val completed-steps-open-steps = match state.open-steps
    Cons(first, Nil) -> 
      (Cons(Step-Log(first.name, first.bound-value, first.children.reverse, outcome, Right(result-ser)), state.completed-steps),
       [])
    Cons(first, Cons(next, rest)) -> 
      val step-log = Step-Log(first.name, first.bound-value, first.children.reverse, outcome, Right(result-ser))
      val next' = next(children = Cons(step-log, next.children))
      val open-steps' = Cons(next', rest)
      (state.completed-steps, open-steps')
    [] -> throw("Unexpected log-complete")

  match completed-steps-open-steps
    (completed-steps', open-steps') ->
      state(completed-steps=completed-steps', open-steps=open-steps')



// collect and return a list of step-logs from a computation
// this handler should also handle any exn effect, so the 
// return is div (would be nice for it to be total, but there's
// no stdlib for timeouts yet afaics)

// how to do replay ... we have:
// step-log
//   name: string
//   bound-value: maybe<stringsrc>
//   children: list<step-log>
//   outcome: step-outcome
//   result: either<stringsrc,stringsrc>
// computation-log
//   step-logs: list<step-log>
// 
// want to follow the computation-log tree, but as soon as the computation diverges - 
// e.g. there was an error, or there are too many or too few logged steps, or too deep,
// then we switch to making real calls and recording new results
//
// so, as we're traversing the computation-log tree we should be accumulating all the results
// as if it was a new computation, so when the log diverges we can carry on with the new log
//
// to follow the tree, need something like a zipper
pub fun log-collector<v>(
    previous-log: maybe<computation-log>, 
    computation-name, 
    action: () -> <div,exn,logger> v
  ) : div (either<string,v>, computation-log)

  var state := make-logger-state(previous-log)

  // if we got a previous log, follow it
  var cursor := match previous-log
    Just(pl) -> Just(make-log-cursor(pl))
    _ -> Nothing

  // list of completed top-level steps
  var steps: list<step-log> := []
  // steps can have children, which we will prepend to the next-step list
  var next-step: list<open-step> := []

  with return(v) (v, Computation-Log(computation-name, steps.reverse))

  with handler
    final ctl throw-exn(ex)
      match ex
        Exception(_, ExnTodo) ->
          Left("Error logged: " ++ ex.show)
        _ ->
          match next-step
            Cons(first,_rest) ->
              steps := Cons(Step-log(first.name, first.bound-value, first.children, UnhandledStepError, Left({"Unhandled Exception: " ++ ex.show})), steps)
              Left("Unhandled Exception: " ++ ex.show)
            [] -> Left("Unhandled Exception outside step:" ++ ex.show)

  with handler
    fun log-enter(name)
      val state_resumed_step = state/enter(state, name)
      match state_resumed_step
        (state', resumed-step) -> 
          state := state'
          resumed-step
    fun log-bound-value(value-ser)
      match next-step
        Cons(first, rest) -> next-step := Cons(Open-Step(first.name, Just(value-ser), first.children), rest)
        [] -> throw("Unexpected log-bound-value")
    fun log-complete(outcome, result-ser)
      match next-step
        Cons(first, Nil) -> 
          steps := Cons(Step-Log(first.name, first.bound-value, first.children.reverse, outcome, Right(result-ser)), steps)
          next-step := []
        Cons(first, Cons(next, rest)) -> 
          val step-log = Step-Log(first.name, first.bound-value, first.children.reverse, outcome, Right(result-ser))
          val next' = next(children = Cons(step-log, next.children))
          next-step := Cons(next', rest)
        [] -> throw("Unexpected log-complete")
    ctl log-error(error-ser)
      match next-step
        Cons(first, Nil) -> 
          steps := Cons(Step-Log(first.name, first.bound-value, first.children.reverse, StepError, Left(error-ser)), steps)
          throw(error-ser(), ExnTodo)
        Cons(first, Cons(next, rest)) ->
          val step-log = Step-Log(first.name, first.bound-value, first.children.reverse, StepError, Left(error-ser))
          val next' = next(children = Cons(step-log, next.children))
          next-step := Cons(next', rest)
          throw(error-ser(), ExnTodo)
        [] -> throw("Unexpected log-error")

  Right(action())

// test/example

pub fun stuff(i: int)
  log-enter("stuff")
  val o = i + i
  // log-error({o.show})
  log-complete(Run, {o.show})
  o

pub fun thingz(i: int)
  log-enter("thingz")
  log-bound-value({i.show})
  val o = i * i
  // throw("oops: " ++ o.show)
  log-complete(Run, {o.show})
  o

pub fun collect-test-logs()
  with log-collector(Nothing, "test")
  val v = stuff(5)
  val w = thingz(v)
  w

pub fun logs-an-error()
  log-enter("logs-an-error")
  log-error({"boo!"})

pub fun collect-logs-with-error()
  with log-collector(Nothing, "test-with-error")
  val v = stuff(5)
  logs-an-error()
  v

pub fun throws-an-exception()
  log-enter("throws-an-exception")
  throw("oops")

pub fun collect-logs-with-exception()
  with log-collector(Nothing, "test-with-exception")
  val v = stuff(5)
  throws-an-exception()
  v

pub fun test-collect-logs()
  match collect-test-logs()
    (v, log) -> 
      print-computation-log(log)
      println("Result: " ++ v.show)
  match collect-logs-with-error()
    (v, log) -> 
      print-computation-log(log)
      println("Result: " ++ v.show)
  match collect-logs-with-exception()
    (v, log) -> 
      print-computation-log(log)
      println("Result: " ++ v.show)
