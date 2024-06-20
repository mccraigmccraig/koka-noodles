module chain_dsl

import parsers/serjson/serjson
import std/core/console
import std/core/exn
import std/text/parse

import logger

pub value struct stringalize<a>
  ser: (a) -> total string
  deser: (string) -> total either<string,a>

pub struct smeta<i>
  name: string
  i-str: stringalize<i>

pub fun meta<i>(name: string, ?stringalize: stringalize<i>): smeta<i>
  Smeta(name, ?stringalize)

pub alias vfn<t> = () -> t

// it's bind for 0-args fns returning a value, with a bit of extra metadata
pub fun step<i,o,e>(ifn: vfn<i>, m: smeta<i>, f: (i) -> <console/console|e> vfn<o>): <console/console|e> vfn<o>
  val i = ifn()
  println(m.name ++ ": " ++ m.i-str.ser()(i))
  f(i)

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

// some examples

pub fun make-test-dsl2()
  with x <- ({1}).step(meta("x"))
  with y <- ({10}).step(meta("y"))
  with z <- ({x + y}).step(meta("z"))
  ({2 * z})

pub fun test-dsl2()
  val td = make-test-dsl2()
  td()

// can match bound variables and continue binding
pub fun make-test-dsl3(i: int)
  with x <- ({i}).step(meta("x"))
  with y <- ({[10,11]}).step(meta("y"))
  val aa' = match y
    [a, b] -> 
      with z <- ({(x + a + b)}).step(meta("z"))
      ({z})
    _ -> ({0})
  with aa <- aa'.step(meta("aa"))
  with bb <- ({12}).step(meta("bb"))
  ({aa.show ++ "/" ++ bb.show})

pub fun test-dsl3()
  println(make-test-dsl3(1)())
  println(make-test-dsl3(10)())
