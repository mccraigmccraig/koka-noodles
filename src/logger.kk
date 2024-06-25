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
  children: list<step-log>
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
  
pub fun step-log/show(log: step-log): <div,indent> string
  val p = pad()
  val step-output = match log.output
    Left(err) -> "error: " ++ err() ++ "\n"
    Right(v) -> "value: " ++ v() ++ "\n"

  val children-output = apply-indent(indent + 2)
   log.children.reverse.map(step-log/show).joinsep/join(pad() ++ "--\n")

  p ++ "step: " ++ log.name ++
  (if length(log.children)>0 then "\n" ++ pad() ++ "children:\n" ++ children-output else "") ++
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
value struct open-step
  name: string
  children: list<step-log>

// collect and return a list of step-logs from a computation
// this handler should also handle any exn effect, so the 
// return is div (would be nice for it to be total, but there's
// no stdlib for timeouts yet afaics)
pub fun log-collector<v>(computation-name, action: () -> <div,exn,logger> v) : div (either<string,v>, computation-log)
  // list of completed top-level steps
  var steps: list<step-log> := []
  // steps can have children, which we will prepend to the next-step list
  var next-step: list<open-step> := []
  with return(v) (v, Computation-Log(computation-name, steps))
  with handler
    final ctl throw-exn(ex)
      match ex
        Exception(_, ExnTodo) ->
          Left("Error logged: " ++ ex.show)
        _ ->
          match next-step
            Cons(Open-step(name),_rest) ->
              steps := Cons(Step-log(name, [], UnhandledStepError, Left({"Unhandled Exception: " ++ ex.show})), steps)
              Left("Unhandled Exception: " ++ ex.show)
            [] -> Left("Unhandled Exception outside step:" ++ ex.show)
  with handler
    fun log-enter(name)
      match next-step
        [] -> next-step := [Open-Step(name, [])]
        rest -> next-step := Cons(Open-Step(name, []), rest)
    fun log-complete(outcome, output-ser)
      match next-step
        Cons(first, Nil) -> 
          steps := Cons(Step-Log(first.name, first.children, outcome, Right(output-ser)), steps)
          next-step := []
        Cons(first, Cons(next, rest)) -> 
          val step-log = Step-Log(first.name, first.children, outcome, Right(output-ser))
          val next' = next(children = Cons(step-log, next.children))
          next-step := Cons(next', rest)
        [] -> throw("Unexpected log-complete")
    ctl log-error(error-ser)
      match next-step
        Cons(first, Nil) -> 
          steps := Cons(Step-Log(first.name, first.children, StepError, Left(error-ser)), steps)
          throw(error-ser(), ExnTodo)
        Cons(first, Cons(next, rest)) ->
          val step-log = Step-Log(first.name, first.children, StepError, Left(error-ser))
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
