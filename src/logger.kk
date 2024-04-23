module logger

import std/core/string 

// a thunk for step inputs and outputs - we don't always need to 
// serialize or deserialise all inputs and outputs, e.g. we might have an
// "only log input and final-step output if there are no errors, but 
//  log full step inputs+outputs if there are errors" policy
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
  outcome: step-outcome
  input: stringsrc
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
    Right(out) -> "output: " ++ out() ++ "\n"

  p ++ "step: " ++ log.name ++
  "\n" ++ p ++ "outcome: " ++ log.outcome.show ++
  "\n" ++ p ++ "input: " ++ log.input()() ++
  "\n" ++ p ++ step-output

// NB: step-logs are a reversed list, to match chain steps
pub value struct computation-log
  name: string
  step-logs: list<step-log>

pub effect logger
  // open a step-log
  fun log-enter(name: string, input-ser: stringsrc) : ()
  // close a step log successfully
  fun log-complete(outcome: step-outcome, output-ser: stringsrc) : ()
  // close a step log with an error and immediately terminate
  ctl log-error(error-ser: stringsrc) : ()

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
  input: stringsrc 

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
            Just(Open-step(name, input-str)) ->
              steps := Cons(Step-Log(name, UnhandledStepError, input-str, Left({"Unhandled Exception: " ++ ex.show})), steps)
              Left("Unhandled Exception: " ++ ex.show)
            Nothing -> Left("Unhandled Exception outside step:" ++ ex.show)
  with handler
    fun log-enter(name, input-ser)
      match next-step
        Nothing -> next-step := Just(Open-Step(name, input-ser))
        Just(_) -> throw("Unexpected log-enter")
    fun log-complete(outcome, output-ser)
      match next-step
        Just(Open-step(name, input-ser)) -> 
          steps := Cons(Step-Log(name, outcome, input-ser, Right(output-ser)), steps)
          next-step := Nothing
        Nothing ->
          throw("Unexpected log-complete")
    ctl log-error(error-ser)
      match next-step
        Just(Open-step(name, input-ser)) ->
          steps := Cons(Step-Log(name, StepError, input-ser, Left(error-ser)), steps)
          next-step := Nothing
          // logging an error immediately terminates any computation
          throw(error-ser(), ExnTodo)
        Nothing ->
          throw("Unexpected log-error")
  Right(action())

// test/example

pub fun stuff(i: int)
  log-enter("stuff", {i.show})
  val o = i + i
  // log-error({o.show})
  log-complete(Run, {o.show})
  o

pub fun thingz(i: int)
  log-enter("thingz", {i.show})
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
