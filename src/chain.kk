module chain

import std/core/exn
import std/core/int
import std/core/list
import std/num/random
import logger

pub value struct serde<a>
  ser: (a) -> string
  deser: (string) -> either<string,a>

pub alias pure-step-fn<i,o> = (i) -> <pure,logger> o
pub alias effectful-step-fn<i,o,e> = (i) -> <pure,logger|e> o

pub struct pure-step<i,o>
  name: string
  step: pure-step-fn<i,o>
  i-serde: serde<i>
  o-serde: serde<o>

pub fun pure-step<i,o>(name: string, step: pure-step-fn<i,o>, ?i/serde: serde<i>, ?o/serde: serde<o>): pure-step<i,o>
  Pure-step(name, step, ?i/serde, ?o/serde)

pub struct effectful-step<i,o,e>
  name: string
  step: effectful-step-fn<i,o,e>
  i-serde: serde<i>
  o-serde: serde<o>

pub fun effectful-step<i,o,e>(name: string, step: effectful-step-fn<i,o,e>, ?i/serde: serde<i>, ?o/serde: serde<o>): effectful-step<i,o,e>
  Effectful-step(name, step, ?i/serde, ?o/serde)

pub value type step<i,o,e>
  Pure(step: pure-step<i,o>)
  Effectful(step: effectful-step<i,o,e>)

pub fun run-pure-step<i,o>(s: pure-step<i,o>, i: i): <pure,logger> o
  log-enter(s.name, {s.i-serde.ser()(i)})
  val o = s.step()(i)
  log-complete({s.o-serde.ser()(o)})
  o

pub fun run-effectful-step<i,o,e>(s: effectful-step<i,o,e>, i: i): <pure,logger|e> o
  log-enter(s.name, {s.i-serde.ser()(i)})
  val o = s.step()(i)
  log-complete({s.o-serde.ser()(o)})
  o

pub fun run-step<i,o,e>(s: step<i,o,e>, i: i): <pure,logger|e> o
  match s
    Pure(step) -> run-pure-step(step, i)
    Effectful(step) -> run-effectful-step(step, i)

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
  val s1 = Pure(pure-step("inc", fn (x: int) {x + 1}))
  val s2 = Effectful(effectful-step("double", fn (x: int) x + x))
  val s3 = Effectful(effectful-step("throws", fn (x: int) {throw("error") ; x + x}))
  val s1r = run-step(s1, 1)
  // println("s1r: " ++ s1r.show())
  val s2r = run-step(s2, 2)
  // println("s2r: " ++ s2r.show())
  val s3r = run-step(s3, 3)
  // println("s3r: " ++ s3r.show())
  s3r

pub fun log-some-steps()
  with log-collector("some-steps")
  run-some-steps() 

pub fun test-some-steps()
  match log-some-steps()
    (v, log) ->
      print-computation-log(log)
      println("result: " ++ v.show())

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
  val s1 = Pure(pure-step("inc", fn (x: int) {x + 1}))
  val s2 = Effectful(effectful-step("tostr", fn (x: int) x.show))
  val s3 = Effectful(effectful-step("concat", fn (x: string) {throw("error") ; x ++ x}))
  val c = First(s1).add-step(s2).add-step(s3)
  run-chain(c, 1)

pub fun log-simple-chain()
  with log-collector("simple-chain")
  simple-chain()

pub fun test-simple-chain()
  match log-simple-chain()
    (v, log) ->
      print-computation-log(log)
      println("result: " ++ v.show())
  
pub struct computation<i,d,o,v,e>
  name: string // the name of the computation, and also the name of the pure step
  input-chain: chain<i,d,e>
  pure: pure-step<d,o>
  output-chain: chain<o,v,e>

// allows control of input-gathering and output-performing actions - 
// perhaps data is read directly from a serialized message, or written 
// to a kafka topic rather than performed immediately
pub effect computation-io<i,d,o,v,e>
  ctl inputs-fx(c: computation<i,d,o,v,e>, i: i) : d
  ctl outputs-fx(c: computation<i,d,o,v,e>, o: o) : v

// the core business of running a computation - using effects to gather inputs
// and perform outputs
pub fun run-computation-steps<i,d,o,v,e>(c: computation<i,d,o,v,e>, i: i) : <computation-io<i,d,o,v,e>,pure,logger|e> v
  val d = c.inputs-fx(i)
  log-enter(c.name, {c.pure.i-serde.ser()(d)})
  val o = c.pure.step()(d)
  log-complete({c.pure.o-serde.ser()(o)})
  val v = c.outputs-fx(o)
  v

// a handler for the computation io operations
pub alias computation-io-handler<i,d,o,v,e> = (() -> <computation-io<i,d,o,v,e>,pure,logger|e> v) -> <pure,logger> v

// run a computation, handling input and output effects, and returning final
// output, and a log of the computation. the log can be used for 
// debugging and introspection, and in the case of failure can be used 
// with an io-handler to retry the computation
// TODO - can we add a timer to convert the div effect to <ndet,exn> ?
//      how would that work with truly non-terminating computations ?
//      can they be interrupted ?
pub fun run-computation<i,d,o,v,e>(
    io-handler: computation-io-handler<i,d,o,v,e>, 
    c: computation<i,d,o,v,e>, 
    i: i) : div (either<string,v>, computation-log)
    
  with log-collector(c.name)
  with io-handler
  run-computation-steps(c, i)

// handle computation-specific effects
pub alias fx-handler<v,e> = (() -> <pure,logger|e> v) -> <pure,logger> v

// basic case - handle computation io by running effectful input/output chains
pub fun run-chain-io-handler<i,d,o,v,e>(hdl: fx-handler<v,e>): computation-io-handler<i,d,o,v,e>
  fn (action)
    with hdl
    with handler 
      ctl inputs-fx(c, i)
        resume(run-chain(c.input-chain, i))
      ctl outputs-fx(c, o)
        resume(run-chain(c.output-chain, o))
    action()

// next up - a resume handler which will take a computation-log, presumably 
// from a failed computation, and resume it from the point of failure
// need proper deserialization to make this work... and probably also want 
// to tie the computation-log to the computation itself so that you can't 
// resume a computation with a log from a completely different computation

// pub fun resume-io-handler<i,d,o,v,e>(log: computation-log, hdl: fx-handler<v,e>): computation-io-handler<i,d,o,v,e>
