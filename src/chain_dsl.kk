module chain_dsl

import std/core/console
import std/core/exn
import std/text/parse

import parsers/serjson/serjson


// using an identity monad to build a simple DSL... don't need monads
// for effects like state, 'cos we have effects, but the binding aspect
// is useful... can also do without flow-control effects like either/maybe
// because we have the exn effect
//
// step is bind is just function application here, but there's no reason it can't 
// be extended to log, resume, etc - the point is that we can bind each 
// step's value... can also forget .pure because it's an identity monad

pub value struct stringalize<a>
  ser: (a) -> total string
  deser: (string) -> total either<string,a>

pub value struct sdescr<i>
  name: string
  stringalize: stringalize<i>

pub fun desc<i>(name: string, ?stringalize: stringalize<i>): sdescr<i>
  Sdescr(name, ?stringalize)

// serialize/deserialize int and list<int>

fun int/valuefn(i: int): serjson/value
  serjson/Int(i)

fun int/pfn(v: serjson/value): parse int
  match v
    serjson/Int(i) -> i
    _ -> parse/fail("not an int")

val int/stringalize = Stringalize(
  fn(i: int) -> serjson/json(i, ?valuefn=int/valuefn),
  fn(s) -> serjson/dejson(s, ?pfn=int/pfn)
)

val listint/stringalize = Stringalize(
  fn(l: list<int>) -> serjson/list/json(l),
  fn(s) -> serjson/list/dejson(s)
)

// step is identity bind, with a name provided. all effects from f are propagated
pub fun step<i, o, e>(i: i, descr: sdescr<i>, f: (i) -> <console/console|e> o): <console/console|e> o
  println(descr.name ++ ": " ++ descr.stringalize.ser()(i))
  f(i)

pub fun test-dsl2()
  with x <- 1.step(desc("x"))
  with y <- 10.step(desc("y"))
  with z <- (x + y).step(desc("z"))
  2 * z

// can match bound variables and continue binding
pub fun make-test-dsl3(i: int)
  with x <- i.step(desc("x"))
  with y <- [10,11].step(desc("y"))
  val aa = match y
    [a, b] -> 
      with z <- (x + a + b).step(desc("z"))
      z
    _ -> 0
  with bb <- 12.step(desc("bb"))
  aa.show ++ "/" ++ bb.show

pub fun test-dsl3()
  println(make-test-dsl3(1))
  println(make-test-dsl3(10))
