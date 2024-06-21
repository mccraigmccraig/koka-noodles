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

pub fun step<a>(name: string, ?stringalize: stringalize<a>): step<a>
  Step(name, ?stringalize)


// TODO not very happy about the bind signature - can we standardize it ?

// it's bind for thunks, with a bit of extra metadata to name the step and de/serialize the value
pub fun bind<a,b,e>(ifn: thunk<a>, st: step<a>, f: (a) -> <console/console|e> thunk<b>): <console/console|e> thunk<b>
  // can retrieve the value of i from logs or evaluate the ifn
  val i = ifn()
  println(st.name ++ ": " ++ st.str.ser()(i))
  f(i)

pub fun pure<a>(i: a): thunk<a>
  fn() -> i


// some examples

pub fun make-test-dsl1()
  with x <- 1.pure.bind(step("x"))
  with y <- 10.pure.bind(step("y"))
  with z <- ({x + y}).bind(step("z"))
  ({2 * z})

pub fun test-dsl1()
  val td = make-test-dsl1()
  td()

// can match bound variables and continue binding
pub fun make-test-dsl2(i: int)
  with x <- i.pure.bind(step("x"))
  with y <- [10,11].pure.bind(step("y"))
  val aa' = match y
    [a, b] -> 
      with z <- ({(x + a + b)}).bind(step("z"))
      z.pure
    _ -> 0.pure
  with aa <- aa'.bind(step("aa"))
  with bb <- 12.pure.bind(step("bb"))
  ({aa.show ++ "/" ++ bb.show})

pub fun test-dsl2()
  println(make-test-dsl2(1)())
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
