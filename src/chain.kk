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

pub struct effectful-step<i,o,e>
  name: string
  step: effectful-step-fn<i,o,e>
  i-serde: serde<i>
  o-serde: serde<o>

pub fun pure-step<i,o>(name: string, step: pure-step-fn<i,o>, ?i/serde: serde<i>, ?o/serde: serde<o>): pure-step<i,o>
  Pure-step(name, step, ?i/serde, ?o/serde)

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

// test/example

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
  
