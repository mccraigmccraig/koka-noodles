module logger

import std/core/string 

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

pub effect logger
  // open a step-log
  fun log-enter(name: string) : ()
  // close a step log successfully
  fun log-complete(outcome: step-outcome, output-ser: stringsrc) : ()
  // close a step log with an error and immediately terminate
  ctl log-error(error-ser: stringsrc) : ()

pub fun step-outcome/show(outcome: step-outcome)
  match outcome
    Run -> "run"
    Resume -> "resume"
    StepError -> "error"
    UnhandledStepError -> "unhandled error"

pub struct step-log
  name: string
  outcome: step-outcome
  output: either<stringsrc,stringsrc>

effect val indent: int
pub fun apply-indent<e>(n: int, action: () -> <indent|e> a) : e a
  with val indent = n
  action()

pub fun pad()
  match indent 
    0 -> ""
    _ -> repeat(" ", indent)
  
pub fun step-log/show(log: step-log)
  val p = pad()
  val step-output = match log.output
    Left(err) -> "error: " ++ err() ++ "\n"
    Right(v) -> "value: " ++ v() ++ "\n"

  p ++ "step: " ++ log.name ++
  "\n" ++ p ++ "outcome: " ++ log.outcome.show ++
  "\n" ++ p ++ step-output

// NB: step-logs are a reversed list, to match chain steps
pub value struct computation-log
  name: string
  step-logs: list<step-log>

pub fun computation-log/show(log: computation-log)
  with apply-indent(2)
  "computation: " ++ log.name ++ "\n" ++
  log.step-logs.reverse.map(step-log/show).joinsep/join(pad() ++ "--\n")

pub fun print-computation-log(log: computation-log)
  println(log.show)

// a de/serializable log of a single step
// names should be unique, but it won't matter if not,
// since chain position is the true unique identifier
value struct open-step
  name: string

// collect and return a list of step-logs from a computation
// this handler should also handle any exn effect, so the 
// return is div (would be nice for it to be total, but there's
// no stdlib for timeouts yet afaics)
pub fun log-collector<v>(computation-name, action: () -> <div,exn,logger> v) : div (either<string,v>, computation-log)
  var steps: list<step-log> := []
  var next-step: maybe<open-step> := Nothing
  with return(v) (v, Computation-Log(computation-name, steps))
  with handler
    final ctl throw-exn(ex)
      match ex
        Exception(_, ExnTodo) ->
          Left("Error logged: " ++ ex.show)
        _ ->
          match next-step
            Just(Open-step(name)) ->
              steps := Cons(Step-log(name, UnhandledStepError, Left({"Unhandled Exception: " ++ ex.show})), steps)
              Left("Unhandled Exception: " ++ ex.show)
            Nothing -> Left("Unhandled Exception outside step:" ++ ex.show)
  with handler
    fun log-enter(name)
      match next-step
        Nothing -> next-step := Just(Open-Step(name))
        Just(_) -> throw("Unexpected log-enter")
    fun log-complete(outcome, output-ser)
      match next-step
        Just(Open-step(name)) -> 
          steps := Cons(Step-Log(name, outcome, Right(output-ser)), steps)
          next-step := Nothing
        Nothing ->
          throw("Unexpected log-complete")
    ctl log-error(error-ser)
      match next-step
        Just(Open-step(name)) ->
          steps := Cons(Step-Log(name, StepError, Left(error-ser)), steps)
          next-step := Nothing
          // logging an error immediately terminates any computation
          throw(error-ser(), ExnTodo)
        Nothing ->
          throw("Unexpected log-error")
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
  val o = i * i
  // throw("oops: " ++ o.show)
  log-complete(Run, {o.show})
  o

pub fun collect-test-logs()
  with log-collector("test")
  val v = stuff(5)
  val w = thingz(v)
  w

pub fun test-collect-logs()
  match collect-test-logs()
    (v, log) -> 
      print-computation-log(log)
      println("Result: " ++ v.show)
