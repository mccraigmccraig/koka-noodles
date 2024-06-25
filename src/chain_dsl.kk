module chain_dsl

import parsers/serjson/serjson
import std/core/console
import std/core/exn
import std/text/parse
import logger

pub value struct stringalize<a>
  ser: (a) -> total string
  deser: (string) -> total either<string,a>

pub alias thunk<a,e> = () -> e a

pub struct step<a,e>
  name: string
  str: stringalize<a>
  thunk: thunk<a,e>

pub fun step<a,e>(thunk: thunk<a,e>, name: string, ?stringalize: stringalize<a>): step<a,e>
  Step(name, ?stringalize, thunk)

pub fun step/show(a: step<a,e>): string
  a.name ++ ": <unevaluated>"

pub fun bind<a,b,e>(
    st: step<a,<logger|e>>, 
    f: (a) -> <logger|e> step<b,<logger|e>>,
    ?stringalize: stringalize<b>
  ): total step<b,<logger|e>>
  Step(
    "bind: " ++ st.name,
    ?stringalize,
    fn () -> 
      logger/log-enter(st.name)
      val a = st.thunk()()
      logger/log-complete(logger/Run, {st.str.ser()(a)})
      // println(st.name ++ ": " ++ st.str.ser()(a) )
      f(a).thunk()()
  )

// run a step, returning an either result and the computation-log
// h must handle any non-logger effects from the step - the only 
// effects allowed out are div,exn
pub fun run<a>(
    st: step<a,<logger|e>>,
    h: (thunk<a,<logger|e>>) -> <pure,logger> a
  ): pure (either<string,a>, logger/computation-log)
  with logger/log-collector(st.name)
  h(st.thunk)



// pub value type resume-step-log
//   StepLog(step-log: step-log)
//   None

// pub fun resume<a,e>(resume-step-log: resume-step-log, st: step<a,e>): e a
//   st.thunk()()

// some examples

pub fun make-test-dsl1()
  with x <- ({1}).step("x").bind
  with y <- step({10},"y").bind
  with z <- ({int/(+)(x, y)}).step("z").bind
  ({2 * z}).step("result")

pub fun log-test-dsl1()
  make-test-dsl1().run(fn (t) -> t())

pub fun test-dsl1()
  val (_, log) = log-test-dsl1()
  logger/print-computation-log(log)
  

fun testdsl/stuff(i: int)
  step({i}, "stuff")

// can match bound variables and continue binding
pub fun make-test-dsl2(i: int)
  with x <- ({i}).step("x").bind
  with y <- ({ if i>5 then [10,11] else [] }).step("y").bind
  val aa' = match y
    [a, b] -> 
      with z <- ({foldl([x, a, b], 0, int/(+))}).step("z").bind
      ({z}).step("aa'-pair", ?stringalize=int/stringalize)
    _ -> ({0}).step("aa'-default")
  with aa <- aa'.bind
  with bb <- testdsl/stuff(i).bind
  ({aa.int/show ++ "/" ++ bb.int/show}).step("result")

pub fun log-test-dsl2(i: int)
  make-test-dsl2(i).run(fn (t) -> t())

pub fun print-log-test-dsl2(i: int)
  val (_, log) = log-test-dsl2(i)
  logger/print-computation-log(log)

pub fun test-dsl2()
  print-log-test-dsl2(1)
  println("--")
  print-log-test-dsl2(10)

















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
