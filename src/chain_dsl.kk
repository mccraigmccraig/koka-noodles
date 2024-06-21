module chain_dsl

import parsers/serjson/serjson
import std/core/console
import std/core/exn
import std/text/parse
import logger

pub value struct stringalize<a>
  ser: (a) -> total string
  deser: (string) -> total either<string,a>

pub alias thunk<a> = () -> a

pub struct step<a>
  name: string
  str: stringalize<a>
  thunk: thunk<a>

pub fun step<a>(thunk: thunk<a>, name: string, ?stringalize: stringalize<a>): step<a>
  Step(name, ?stringalize, thunk)

pub fun step/show(a: step<a>): string
  a.name ++ ": " ++ a.str.ser()(a.thunk()())

pub fun bind<a,b,e>(st: step<a>, f: (a) -> <console/console|e> step<b>): <console/console|e> step<b>
  // can retrieve the value of i from logs or evaluate the ifn
  val a = st.thunk()()
  println(st.name ++ ": " ++ st.str.ser()(a))
  f(a)

pub fun step/pure<a>(a: a, ?stringalize: stringalize<a>): step<a>
  step({a}, "pure", ?stringalize=?stringalize)

// some examples

pub fun make-test-dsl1()
  with x <- ({1}).step("x").bind
  with y <- step({10},"y").bind
  with z <- ({x + y}).step("z").bind
  ({2 * z}).step("result")

pub fun test-dsl1()
  val td = make-test-dsl1()
  td()

fun stuff(i: int)
  step({i}, "stuff")

// can match bound variables and continue binding
pub fun make-test-dsl2(i: int)
  with x <- ({i}).step("x").bind
  with y <- ({ if i>5 then [10,11] else [] }).step("y").bind
  val aa' = match y
    [a, b] -> 
      with z <- ({(x + a + b)}).step("z").bind
      ({z}).step("aa'-pair")
    _ -> ({0}).step("aa'-default")
  with aa <- aa'.bind
  with bb <- stuff(i).bind
  ({aa.show ++ "/" ++ bb.show}).step("result")

pub fun test-dsl2()
  println(make-test-dsl2(1)())
  println("--")
  println(make-test-dsl2(10)())

















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

val string/stringalize = Stringalize(
  fn(s: string) -> s,
  fn(s) -> Right(s)
)
