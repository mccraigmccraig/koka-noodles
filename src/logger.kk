module logger

import std/core/exn
import std/core/string 
import std/core/undiv
import std/core/vector

// a thunk for step values - we don't always need to 
// serialize or deserialise all values, e.g. we might have an
// "only log final values if there are no errors, but 
//  log all values if there are errors" policy
pub alias stringsrc = () -> total string

pub type step-outcome
  Run
  Resume
  ResumeBind
  StepError

pub fun step-outcome/show(outcome: step-outcome)
  match outcome
    Run -> "run"
    Resume -> "resume"
    ResumeBind -> "resume-bind"
    StepError -> "error"

pub struct step-log
  name: string
  bound-value: maybe<stringsrc>
  children: list<step-log>
  outcome: step-outcome
  result: either<stringsrc,stringsrc>

pub value struct computation-log
  name: string
  step-logs: list<step-log>

// When resuming a computation, an existing step-log will be
// given back as the result of log-enter
pub value type resumed-step
  Log(l: step-log)
  NoLog

// TODO distinguish between bind-steps and run-steps
pub effect logger
  val log-detail: bool
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

  val children-output = apply-indent(indent + 1)
   log.children.map(step-log/show).joinsep/join("\n" ++ pad() ++ "--\n")

  val step-output = match log.result
    Left(err) -> "error: " ++ err()
    Right(v) -> "result: " ++ v()

  p ++ "step: " ++ log.name ++
  "\n" ++ p ++ bound-value-output ++
  (if length(log.children)>0 then "\n" ++ pad() ++ "children:\n" ++ children-output else "") ++
  "\n" ++ p ++ "outcome: " ++ log.outcome.show ++
  "\n" ++ p ++ step-output

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

// propagate an error through the open-steps, closing them all and 
// returning a single step-log representing the whole closed tree
// propagate an error to a state
fun state/error-open-steps(state: logger-state, error-ser: stringsrc): total either<string,logger-state>
  match state.open-steps
    [] -> Left("error-open-steps: no open-step")
    Cons(first, Nil) -> 
      val step-log = Step-Log(first.name, first.bound-value, first.children.reverse, StepError, Left(error-ser))
      val completed-steps' = Cons(step-log, state.completed-steps)
      Right(state(completed-steps=completed-steps', open-steps=[]))
    Cons(first, Cons(next, rest)) -> 
      val step-log = Step-Log(first.name, first.bound-value, first.children.reverse, StepError, Left(error-ser))
      val next' = next(children = Cons(step-log, next.children))
      state/error-open-steps(pretend-decreasing(state(open-steps=Cons(next', rest))), error-ser)


// if we are following a log and have a step to enter, update the log cursor
// and return the logged step. either way, create a new `open-step` to 
// record the outcome
fun state/log-enter(state: logger-state, step-name: string): total (logger-state, resumed-step)
  // follow the resumed log, if it matches
  val state-resumed-step = match state.resume-cursor
    Just(lc) -> 
      match lc.step-cursors
        Cons(Step-cursor(steps, next-idx), rest) | next-idx < length(steps) ->
          val maybe-sl = at(steps, next-idx)
          match maybe-sl 
            // Just(Step-log(_, _, _, StepError, _)) -> (state(resume-cursor=Nothing), NoLog)
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

// returns Left if there has been a programming error and there are no open steps
fun state/log-bound-value(state: logger-state, value-ser: stringsrc): total either<string,logger-state>
  match state.open-steps
    Cons(first, rest) -> 
      val open-steps' = Cons(first(bound-value=Just(value-ser)), rest)
      Right(state(open-steps=open-steps'))
    [] -> 
      Left("log-bound-value: no open-step")
  
fun state/log-complete(state: logger-state, outcome : step-outcome, result-ser : stringsrc): total either<string,logger-state>
  val state' = match state.resume-cursor
    Just(lc) ->
      match lc.step-cursors
        Cons(Step-cursor(steps, next-idx), rest) | next-idx == length(steps) ->
          state(resume-cursor=Just(Log-cursor(rest)))
        _ -> state(resume-cursor=Nothing)
    _ -> state(resume-cursor=Nothing)

  val error-completed-steps-open-steps = match state.open-steps
    Cons(first, Nil) -> 
      Right((Cons(Step-Log(first.name, first.bound-value, first.children.reverse, outcome, Right(result-ser)), state.completed-steps),
            []))
    Cons(first, Cons(next, rest)) -> 
      val step-log = Step-Log(first.name, first.bound-value, first.children.reverse, outcome, Right(result-ser))
      val next' = next(children = Cons(step-log, next.children))
      val open-steps' = Cons(next', rest)
      Right((state.completed-steps, open-steps'))
    [] -> Left("log-complete: no open-step")

  match error-completed-steps-open-steps
    Right((completed-steps', open-steps')) ->
      Right(state(completed-steps=completed-steps', open-steps=open-steps'))
    Left(err) -> Left(err)

fun state/log-error(state: logger-state, error-ser: stringsrc): total either<string,logger-state>
  match state.open-steps
    Cons(first, Nil) -> 
      val completed-steps' = Cons(Step-Log(first.name, first.bound-value, first.children.reverse, StepError, Left(error-ser)), state.completed-steps)
      Right(state(completed-steps=completed-steps'))
    Cons(first, Cons(next, rest)) ->
      val step-log = Step-Log(first.name, first.bound-value, first.children.reverse, StepError, Left(error-ser))
      val next' = next(children = Cons(step-log, next.children))
      Right(state(open-steps=Cons(next', rest)))
    [] -> Left("log-error: no open-step")

// collect and return a computation-log from a computation
// this handler also handles any exn effect, so the 
// return is div
pub fun log-collector<v>(
    previous-log: maybe<computation-log>, 
    computation-name, 
    action: () -> <div,exn,logger> v
  ) : div (either<string,v>, computation-log)

  var state := make-logger-state(previous-log)

  with return(v) (v, Computation-Log(computation-name, state.completed-steps.reverse))

  // try and close out all open steps with an error, then return the error
  with handler
    final ctl throw-exn(ex)
      val error-state = state/error-open-steps(state, {ex.show})
      match error-state
        Left(err) -> Left("Fatal: " ++ err.show)
        Right(state') -> 
          state := state'
          Left(ex.show)

  // the handler implementation fns are all total, but can return errors via either. 
  // Such errors will be thrown to the throw-exn handler, to immediately terminate 
  // the computation
  with handler
    val log-detail = True
    fun log-enter(name)
      val state-resumed-step = state/log-enter(state, name)
      match state-resumed-step
        (state', resumed-step) -> 
          state := state'
          resumed-step
    fun log-bound-value(value-ser)
      val error-state = state/log-bound-value(state, value-ser)
      match error-state
        Left(err) -> throw(err)
        Right(state') -> state := state'
    fun log-complete(outcome, result-ser)
      val error-state = state/log-complete(state, outcome, result-ser)
      match error-state
        Left(err) -> throw(err)
        Right(state') -> state := state'
    ctl log-error(error-ser)
      val error-state = state/log-error(state, error-ser)
      match error-state
        Left(err) -> throw(err)
        Right(state') -> state := state'
      throw(error-ser(), ExnTodo)

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
