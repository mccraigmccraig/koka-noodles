module chain

import std/core/list
import std/core/int
import std/num/random

// a de/serializable log of a single step
// names should be unique, but it won't matter if not,
// since chain position is the true unique identifier
pub struct step-log
  name: string
  input-str: string
  output-str: either<string,string>

// a de/serializable log of a chain of steps
pub struct chain-log
  input-str: string
  step-logs: list<step-log>
  output-str: either<string,string>

pub effect logger
  // open a step-log
  fun log-enter(name: string, input-str: string) : ()
  // close a step log successfully
  fun log-complete(output-str: string) : ()
  // close a step log with an error
  fun log-error(error-str: string) : ()

pub alias pure-step<i,o> = (i) -> <pure,logger> o
pub alias effectful-step<i,o,e> = (i) -> <pure,logger|e> o
pub alias either-step<i,o> = (i) -> <div,logger> either<string,o>
pub alias handle-step<i,o,e> = (effectful-step<i,o,e>) -> either-step<i,o>

pub type step<i,o,e>
  Pure(name: string, step: pure-step<i,o>)
  Effectful(name: string, step: effectful-step<i,o,e>)

pub fun run-step<i,o,e>(s: step<i,o,e>, i: i): <pure,logger|e> o
  match s
    Pure(nm, f) -> 
      log-enter(nm, "")
      val r = f(i)
      log-complete("")
      r
    Effectful(nm, f) -> 
      log-enter(nm, "")
      val r = f(i)
      log-complete("")
      r

pub fun foo()
  val s1 = Pure("inc", fn (x) {x + 1})
  val s2 = Effectful("double", fn (x: int) {println("double:" ++ x.show()) ; x + x})
  val s1r = run-step(s1, 1)
  println("s1r: " ++ s1r.show())
  val s2r = run-step(s2, 2)
  println("s2r: " ++ s2r.show())

pub fun test-foo()
  var stack: list<string> := []
  with handler
    fun log-enter(name, input-str)
      stack := Cons(name, stack)
      println("enter: <" ++ name ++ "> " ++ input-str)
    fun log-complete(output-str)
      val h = stack.head()
      match h 
        Just(name) -> 
          stack := stack.tail()
          println("complete: <" ++ name ++ "> " ++ output-str)
        Nothing -> println("complete: <?>" ++ " " ++ output-str)
    fun log-error(output-str)
      val h = stack.head()
      match h 
        Just(name) -> 
          stack := stack.tail()
          println("error: <" ++ name ++ "> " ++ output-str)
        Nothing -> println("error: <?>" ++ " " ++ output-str)
  foo()    

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
    First(fst) -> run-step(fst,i)
    Link(lst, fr) -> 
      val r = run-chain(fr, i)
      run-step(lst,r)

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


// intermediate computation state
pub struct computation-log
  name: string
  input-str: string
  step-logs: list<step-log>

// the final state of a computation
pub struct final-computation-log
  name: string
  input-str: string
  step-logs: list<step-log>
  output-str: either<string,string>

// run a computation - using effects to gather inputs and perform outputs
// now need to deal with de/serialization of input/output values, gathering logs
// resuming from errors and giving control over the execution process (for e.g. 
// remote logging)
pub fun run-computation<i,d,o,v,e>(c: computation<i,d,o,v,e>, i: i) : <pure,logger,computation-effect<i,d,o,v,e>|e> v
  val d = i.inputs-fx(c)
  val o = c.pure()(d)
  val v = o.outputs-fx(c)
  v


  
// ok, now we've got a chain of steps... want to allow for things like:
// - resumable computation
// - automatic logging and tracing
// - splitting and distributing parts of the pipeline execution, e.g. could 
//   have a sequence of [<effectful-input-step*> <pure-step> <effectful-output-step*>]
//   and just execute the input-steps, or the output steps, or the pure step... 



// pub fun chain/while( predicate : () -> <div|e> bool, action : () -> <div|e> () ) : <div|e> ()
//   if predicate() then
//     action()
//     chain/while(predicate, action)

// pub fun test_run_while()
//   val r = chain/while(random-bool, fn() print("hello"))
//   r
