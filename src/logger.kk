module logger

import parsers/serjson/serjson
import std/core/exn
import std/core/string 
import std/core/undiv
import std/core/vector
import std/text/parse

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

pub type step-type
  BindStep
  RunStep

pub fun step-type/show(a: step-type): string  
  match a 
    BindStep -> "BindStep"
    RunStep -> "RunStep"
pub fun step-type/pfn(v: serjson/value): parse step-type
  match v
    String("BindStep") -> BindStep
    String("RunStep") -> RunStep
    _ -> fail("invalid json: step-type/pfn")
pub fun step-type/vfn(st: step-type): total serjson/value
  String(st.show)

pub type step-outcome
  Exec
  Resume
  ResumeBind
  StepError

pub fun step-outcome/show(outcome: step-outcome) 
  match outcome
    Exec -> "Exec"
    Resume -> "Resume"
    ResumeBind -> "ResumeBind"
    StepError -> "StepError"
pub fun step-outcome/pfn(v: serjson/value): parse step-outcome
  match v
    String("Exec") -> Exec
    String("Resume") -> Resume
    String("ResumeBind") -> ResumeBind
    String("StepError") -> StepError
    _ -> fail("invalid json: step-outcome/pfn")
pub fun step-outcome/vfn(so: step-outcome): total serjson/value
  String(so.show)

pub struct step-log
  name: string
  step-type: step-type
  bound-value: maybe<stringsrc>
  children: list<step-log>
  outcome: step-outcome
  result: either<stringsrc,stringsrc>

pub fun step-log/show(a: step-log): string
  serjson/json(a, ?vfn=logger/step-log/vfn)
  
fun step-log/pfn'(v: serjson/value): <div,parse> step-log
  match v
    Object([Object-entry("name", String(name)), 
            Object-entry("step-type", step-type-value), 
            Object-entry("bound-value", maybe-bound-value-value), 
            Object-entry("children", list-children-value), 
            Object-entry("outcome", outcome-object), 
            Object-entry("result", either-result-value)]) -> 
        val step-type = step-type/pfn(step-type-value)
        val maybe-bound-value = maybe/make-pfn(stringsrc/pfn)(maybe-bound-value-value)
        val list-children = list/make-pfn(step-log/pfn')(list-children-value)
        val outcome = step-outcome/pfn(outcome-object)
        val either-result = either/make-pfn(stringsrc/pfn,stringsrc/pfn)(either-result-value)
        Step-log(name, step-type, maybe-bound-value, list-children, outcome, either-result)
    _ -> fail("invalid json: step-log/pfn")
pub fun step-log/pfn(v: serjson/value): parse step-log
  with pretend-no-div
  step-log/pfn'(v)

fun step-log/vfn'(sl: step-log): div serjson/value
  Object([Object-entry("name", String(sl.name)), 
          Object-entry("step-type", step-type/vfn(sl.step-type)), 
          Object-entry("bound-value", maybe/make-vfn(stringsrc/vfn)(sl.bound-value)), 
          Object-entry("children", list/make-vfn(step-log/vfn')(sl.children)), 
          Object-entry("outcome", step-outcome/vfn(sl.outcome)), 
          Object-entry("result", either/make-vfn(stringsrc/vfn,stringsrc/vfn)(sl.result))])
pub fun step-log/vfn(sl: step-log): total serjson/value
  with pretend-no-div
  step-log/vfn'(sl)
  
pub struct log-options
  bind-detail: bool

pub fun log-options/show(a: log-options): string
  serjson/json(a, ?vfn=log-options/vfn)
pub fun log-options/pfn(v: serjson/value): parse log-options
  match v
    Object([Object-entry("bind-detail", Bool(bind-detail))]) -> Log-options(bind-detail)
    _ -> fail("invalid json: log-options/pfn")
pub fun log-options/vfn(lo: log-options): total serjson/value
  Object([Object-entry("bind-detail", Bool(lo.bind-detail))])

pub value struct computation-log
  name: string
  options: log-options
  step-logs: list<step-log>

pub fun computation-log/show(a: computation-log): string
  serjson/json(a, ?vfn=computation-log/vfn)
pub fun computation-log/pfn(v: serjson/value): parse computation-log
  match v
    Object([Object-entry("name", String(name)), 
            Object-entry("options", log-options-value), 
            Object-entry("step-logs", list-step-logs-value)]) -> 
        val log-options = log-options/pfn(log-options-value)
        val list-step-logs = list/make-pfn(step-log/pfn)(list-step-logs-value)
        Computation-log(name, log-options, list-step-logs)
    _ -> fail("invalid json: computation-log/pfn")
pub fun computation-log/vfn(cl: computation-log): total serjson/value
  Object([Object-entry("name", String(cl.name)), 
          Object-entry("options", log-options/vfn(cl.options)), 
          Object-entry("step-logs", list/make-vfn(step-log/vfn)(cl.step-logs))])

// When resuming a computation, an existing step-log will be
// given back as the result of log-enter
pub value type resumed-step
  Log(l: step-log)
  NoLog

pub val default-log-options = Log-options(bind-detail=False)
pub val detailed-log-options = Log-options(bind-detail=True)

// TODO distinguish between bind-steps and run-steps
pub effect logger
  val log-options: log-options
  // open a step-log
  fun log-enter(name: string, step-type: step-type) : resumed-step
  // capture a bound value
  fun log-bound-value(value-ser: stringsrc) : ()
  // close a step log successfully
  fun log-complete(outcome: step-outcome, result-ser: stringsrc) : ()
  // if following a log, skip the next entry (because it's not going to be run)
  fun log-skip() : ()
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
  
pub fun step-log/pp(log: step-log): <div,indent> string
  val p = pad()

  val step-type-output = match log.step-type
    BindStep -> "bind: "
    RunStep -> "run: "

  val bound-value-output = match log.step-type 
    BindStep -> match log.bound-value
      Just(v) -> "\n" ++ p ++ "bound-value: " ++ v()
      Nothing -> "\n" ++ p ++ "bound-value: Nothing"
    _ -> ""

  val children-output = apply-indent(indent + 1)
   log.children.map(step-log/pp).joinsep/join("\n" ++ pad() ++ "--\n")

  val step-output = match log.result
    Left(err) -> "error: " ++ err()
    Right(v) -> "result: " ++ v()

  p ++ step-type-output ++ log.name ++
  bound-value-output ++
  (if length(log.children)>0 then "\n" ++ pad() ++ "children:\n" ++ children-output else "") ++
  "\n" ++ p ++ "outcome: " ++ log.outcome.show ++
  "\n" ++ p ++ step-output

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
struct open-step
  name: string
  step-type: step-type
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
      val step-log = Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, StepError, Left(error-ser))
      val completed-steps' = Cons(step-log, state.completed-steps)
      Right(state(completed-steps=completed-steps', open-steps=[]))
    Cons(first, Cons(next, rest)) -> 
      val step-log = Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, StepError, Left(error-ser))
      val next' = next(children = Cons(step-log, next.children))
      state/error-open-steps(pretend-decreasing(state(open-steps=Cons(next', rest))), error-ser)


// if we are following a log and have a step to enter, update the log cursor
// and return the logged step. either way, create a new `open-step` to 
// record the outcome
fun state/log-enter(state: logger-state, step-name: string, step-type: step-type): total (logger-state, resumed-step)
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
            _ -> 
              (state(resume-cursor=Nothing), NoLog)
        _ -> (state(resume-cursor=Nothing), NoLog)
    _ -> (state(resume-cursor=Nothing), NoLog)
    
  // always accumulate a new open-step
  match state-resumed-step
    (state', resumed-step) -> 
      val open-steps' = match state.open-steps
        [] -> [Open-Step(step-name, step-type, Nothing, [])]
        rest -> Cons(Open-Step(step-name, step-type, Nothing, []), rest)
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
      Right((Cons(Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, outcome, Right(result-ser)), state.completed-steps),
            []))
    Cons(first, Cons(next, rest)) -> 
      val step-log = Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, outcome, Right(result-ser))
      val next' = next(children = Cons(step-log, next.children))
      val open-steps' = Cons(next', rest)
      Right((state.completed-steps, open-steps'))
    [] -> Left("log-complete: no open-step")

  match error-completed-steps-open-steps
    Right((completed-steps', open-steps')) ->
      Right(state(completed-steps=completed-steps', open-steps=open-steps'))
    Left(err) -> Left(err)

fun state/log-skip(state: logger-state): total either<string,logger-state>
  match state.resume-cursor
    Just(lc) ->
      match lc.step-cursors
        Cons(Step-cursor(steps, next-idx), rest-step-cursors) | next-idx < length(steps) ->
          val maybe-sl = at(steps, next-idx)
          match maybe-sl
            Just(sl) ->
              val sl' = Step-Log(sl.name, sl.step-type, sl.bound-value, sl.children, Resume, sl.result)
              match state.open-steps
                Nil -> 
                  Right(state(resume-cursor=Just(Log-cursor(step-cursors=Cons(Step-cursor(steps, next-idx + 1), Nil))),
                              completed-steps=Cons(sl', state.completed-steps)))
                Cons(first, rest) ->
                  Right(state(resume-cursor=Just(Log-cursor(step-cursors=Cons(Step-cursor(steps, next-idx + 1), rest-step-cursors))),
                              open-steps=Cons(first(children=Cons(sl', first.children)), rest)))
            _ -> Left("log-skip: fell off the resume-log")
        _ -> Right(state(resume-cursor=Nothing))
    _ -> Right(state(resume-cursor=Nothing))

fun state/log-error(state: logger-state, error-ser: stringsrc): total either<string,logger-state>
  match state.open-steps
    Cons(first, Nil) -> 
      val completed-steps' = Cons(Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, StepError, Left(error-ser)), state.completed-steps)
      Right(state(completed-steps=completed-steps'))
    Cons(first, Cons(next, rest)) ->
      val step-log = Step-Log(first.name, first.step-type, first.bound-value, first.children.reverse, StepError, Left(error-ser))
      val next' = next(children = Cons(step-log, next.children))
      Right(state(open-steps=Cons(next', rest)))
    [] -> Left("log-error: no open-step")

// collect and return a computation-log from a computation
// this handler also handles any exn effect, so the 
// return is div
pub fun log-collector<v>(
    previous-log: maybe<computation-log>, 
    computation-name: string, 
    opts: log-options,
    action: () -> <div,exn,logger> v
  ) : div (either<string,v>, computation-log)

  var state := make-logger-state(previous-log)

  with return(v) (v, Computation-Log(computation-name, opts, state.completed-steps.reverse))

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
    val log-options = opts
    fun log-enter(name, step-type)
      val state-resumed-step = state/log-enter(state, name, step-type)
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
    fun log-skip()
      val error-state = state/log-skip(state)
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
  log-enter("stuff", RunStep)
  val o = i + i
  // log-error({o.show})
  log-complete(Exec, {o.show})
  o

pub fun thingz(i: int)
  log-enter("thingz", BindStep)
  log-bound-value({i.show})
  val o = i * i
  // throw("oops: " ++ o.show)
  log-complete(Exec, {o.show})
  o

pub fun collect-test-logs()
  with log-collector(Nothing, "test", default-log-options)
  val v = stuff(5)
  val w = thingz(v)
  w

pub fun logs-an-error()
  log-enter("logs-an-error", RunStep)
  log-error({"boo!"})

pub fun collect-logs-with-error()
  with log-collector(Nothing, "test-with-error", default-log-options)
  val v = stuff(5)
  logs-an-error()
  v

pub fun throws-an-exception()
  log-enter("throws-an-exception", RunStep)
  throw("oops")

pub fun collect-logs-with-exception()
  with log-collector(Nothing, "test-with-exception", default-log-options)
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
