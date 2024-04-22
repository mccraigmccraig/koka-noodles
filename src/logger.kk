module logger

pub alias stringsrc = () -> div string

pub type step-outcome
  Run
  Resume
  StepError
  UnhandledStepError

pub struct step-log
  name: string
  outcome: step-outcome
  input: stringsrc
  output: either<stringsrc,stringsrc>

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

pub fun print-computation-log(log: computation-log)
  println("computation: " ++ log.name ++ "\n--")
  log.step-logs.reverse.foreach( fn (step-log)
    println("  step: " ++ step-log.name)
    match(step-log.outcome)
      Run -> println("  outcome: run")
      Resume -> println("  outcome: resume")
      StepError -> println("  outcome: error")
      UnhandledStepError -> println("  outcome: unhandled error")
    println("  input: " ++ step-log.input()())
    match step-log.output
      Left(err) -> println("  error: " ++ err())
      Right(out) -> println("  output: " ++ out())
    println("--")
  )  

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
