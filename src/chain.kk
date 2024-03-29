module chain

import std/core/list
import std/core/int
import std/num/random

pub alias stringsrc = () -> string

pub effect logger
  // open a step-log
  fun log-enter(name: string, input-ser: stringsrc) : ()
  // close a step log successfully
  fun log-complete(output-ser: stringsrc) : ()
  // close a step log with an error
  fun log-error(error-ser: stringsrc) : ()

pub value struct serde<a>
  ser: (a) -> string
  deser: (string) -> either<string,a>

pub alias pure-step<i,o> = (i) -> <pure,logger> o
pub alias effectful-step<i,o,e> = (i) -> <pure,logger|e> o
pub alias either-step<i,o> = (i) -> <div,logger> either<string,o>
pub alias handle-step<i,o,e> = (effectful-step<i,o,e>) -> either-step<i,o>

pub type step<i,o,e>
  Pure(pname: string, 
    pstep: pure-step<i,o>, 
    ?pi/serde: serde<i>, 
    ?po/serde: serde<o>)
  Effectful(fname: string, 
    fstep: effectful-step<i,o,e>, 
    ?fi/serde: serde<i>, 
    ?fo/serde: serde<o>)

pub fun run-step<i,o,e>(s: step<i,o,e>, i: i): <pure,logger|e> o
  match s
    Pure(nm, f, iserde, oserde) -> 
      log-enter(nm, {iserde.ser()(i)})
      val o = f(i)
      log-complete({oserde.ser()(o)})
      o
    Effectful(nm, f, iserde, oserde) -> 
      log-enter(nm, {iserde.ser()(i)})
      val o = f(i)
      log-complete({oserde.ser()(o)})
      o

fun int/ser(i: int): string
 i.show

fun int/deser(s: string): either<string,int>
  match s.parse-int()
    Just(i) -> Right(i)
    Nothing -> Left("failed to parse int: " ++ s)

val int/serde = Serde(int/ser, int/deser)

fun string/ser(s: string): string
  s
fun string/deser(s: string): either<string,string>
  Right(s)
val string/serde = Serde(string/ser, string/deser)

pub fun run-some-steps()
  val s1 = Pure("inc", fn (x: int) {x + 1}, int/serde, int/serde)
  val s2 = Effectful("double", fn (x: int) {println("double:" ++ x.show) ; x + x})
  val s3 = Effectful("throws", fn (x: int) {println("throws:" ++ x.show) ; throw("error") ; x + x})
  val s1r = run-step(s1, 1)
  println("s1r: " ++ s1r.show())
  val s2r = run-step(s2, 2)
  println("s2r: " ++ s2r.show())
  val s3r = run-step(s3, 3)
  println("s3r: " ++ s3r.show())

value type frame 
  NoFrame
  Name(n: string)

// handle the logger effect by printing to console
pub fun print-logger(action)
  var current := NoFrame
  with finally
    match current
      Name(name) -> 
        println("unhandled effect: <" ++ name ++ ">")
        current := NoFrame
  with handler
    fun log-enter(name, input-ser)
      current := Name(name)
      println("enter: <" ++ name ++ "> " ++ input-ser())
    fun log-complete(output-ser)
      match current
        Name(name) -> 
          println("complete: <" ++ name ++ "> " ++ output-ser())
          current := NoFrame
        NoFrame -> println("complete: <?>" ++ " " ++ output-ser())
    fun log-error(error-ser)
      match current
        Name(name) -> 
          println("error: <" ++ name ++ "> " ++ error-ser())
          current := NoFrame
        NoFrame -> println("error: <?>" ++ " " ++ error-ser())
  action()

pub fun test-some-steps()
  with print-logger
  run-some-steps()    

// it's a backwards cons of steps - last step first... 
pub type chain<i,o,e>
  First(first: step<i,o,e>)
  Link(last: some <j> step<j,o,e>, front: some <j> chain<i,j,e>)

pub fun add-step(c: chain<i,o,e>, s: step<o,p,e>): chain<i,p,e>
  match c
    First(fst) -> Link(s, First(fst))
    Link(lst, fr) -> Link(s, Link(lst, fr))

// run a chain of steps. blows up stack if chain too long
pub fun run-chain(c: chain<i,o,e>, i: i): <pure,logger|e> o
  match c
    First(fst) -> run-step(fst, i)
    Link(lst, fr) -> 
      val r = run-chain(fr, i)
      run-step(lst,r)

pub fun simple-chain()
  val s1 = Pure("inc", fn (x: int) {x + 1}, int/serde, int/serde)
  val s2 = Effectful("tostr", fn (x: int) {println("tostr:" ++ x.show) ; x.show}, int/serde, string/serde)
  val s3 = Effectful("concat", fn (x: string) {println("throws:" ++ x) ; throw("error") ; x ++ x})
  val c = First(s1).add-step(s2).add-step(s3)
  run-chain(c, 1)

pub fun test-simple-chain()
  with print-logger
  simple-chain()
  
// three stages to a computation
// - inputs: takes a command/event value and produces the data-inputs
//   for the pure stage, possibly with side-effects
// - pure: a pure function that transforms the data-inputs to the data-outputs
// - outputs: takes the data-outputs and performs any side-effects
// maybe the inputs/outputs stages should themselves be effects - since
// being able to change when/where the stages are performed is desirable - e.g. you might 
// want to write data-outputs to kafka (or postgresql) rather than performing 
// effects immediately

// want input and output stages to be composed of multiple steps, and to support
// - auto logging/tracing
// - resumption from error values
// - serialization of input/output values

pub struct computation<i,d,o,v,e>
  name: string // the name of the computation, and also the name of the pure step
  input-chain: chain<i,d,e>
  pure: pure-step<d,o>
  output-chain: chain<o,v,e>

// allows control of input-gathering and output-performing actions - 
// perhaps data is read directly from a serialized message, or written 
// to a kafka topic rather than performed immediately
pub effect computation-effect<i,d,o,v,e>
  ctl inputs-fx(i: i, c: computation<i,d,o,v,e>) : d
  ctl outputs-fx(o: o, c: computation<i,d,o,v,e>) : v

// run a computation - using effects to gather inputs and perform outputs
// now need to deal with de/serialization of input/output values, gathering logs
// resuming from errors and giving control over the execution process (for e.g. 
// remote logging)
pub fun run-computation-steps<i,d,o,v,e>(c: computation<i,d,o,v,e>, i: i) : <pure,logger,computation-effect<i,d,o,v,e>|e> v
  val d = i.inputs-fx(c)
  val o = c.pure()(d)
  val v = o.outputs-fx(c)
  v

// given a handler, handle the computation specific effects, leaving just the logger effect.
// a good time to inject fake services etc
pub fun handle-computation-fx<i,d,o,v,e>(
    c: computation<i,d,o,v,e>, 
    i: i, 
    h: (() -> <pure,logger,computation-effect<i,d,o,v,e>|e> v) -> <pure,logger> v
  ) : <pure,logger> v
  with h 
  run-computation-steps(c, i)

// a de/serializable log of a single step
// names should be unique, but it won't matter if not,
// since chain position is the true unique identifier
pub struct step-log
  name: string
  input-str: stringsrc
  output-str: either<stringsrc,stringsrc>

// intermediate computation state log
pub struct computation-log
  name: string
  input-str: stringsrc
  step-logs: list<step-log>

// the final state of a computation
pub struct final-computation-log
  name: string
  input-str: stringsrc
  step-logs: list<step-log>
  output-str: either<stringsrc,stringsrc>

pub value type log
  None
  Collecting(log: computation-log)
  Complete(log: final-computation-log)

// collect and return a list of step-logs from a computation
pub fun log-collector(action)
  var log := None
  with return(v) 
    val tlog = log
    log := None
    (v, tlog)
  with handler
    fun log-enter(name, input-ser)
      log := Collecting(Computation-Log(name, input-ser, []))
    fun log-complete(output-ser)
      match log
        Collecting(l) -> 
          log := Complete(Final-Computation-Log(l.name, l.input-str, l.step-logs, Right(output-ser)))
    fun log-error(error-ser)
      match log
        Collecting(l) -> 
          log := Complete(Final-Computation-Log(l.name, l.input-str, l.step-logs, Left(error-ser)))
  action()

pub fun print-log(log: final-computation-log)
  println("computation: " ++ log.name)
  println("input: " ++ log.input-str()())
  log.step-logs.foreach( fn (step-log)
    println("  step: " ++ step-log.name)
    println("  input: " ++ step-log.input-str()())
    match step-log.output-str
      Left(err) -> println("  error: " ++ err())
      Right(out) -> println("  output: " ++ out())
  )
  match log.output-str
    Left(err) -> println("error: " ++ err())
    Right(out) -> println("output: " ++ out())


// run a computation, handling all effects and returning final output and 
// logs. the logs have not been persisted at this point - a decision can be made later
// pub fun log-computation<i,d,o,v,e>(c: computation<i,d,o,v,e>, i: i) : computation-effect<i,d,o,v,e> (final-computation-log, v)
//   with handle-computation-fx(c, i)   
//   val v = run-computation-steps(c, i)

// run a computation, handling input and output effects, and returning final output
// could add something at this point to deal with e.g. remote logging successes and 
// failures, or retry policies
// need some standard handlers for simple inputs/outputs, e.g. literal data
// (can use the return override of effects to provide literal outputs alongside the log and value)
// pub fun run-computation<i,d,o,v,e>(c: computation<i,d,o,v,e>, i: i) : total (final-computation-log, v)