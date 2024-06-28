module steps

import parsers/serjson/serjson
import std/core/console
import std/core/exn
import std/num/random
import std/text/parse
import logger

pub value struct strde<a>
  str: (a) -> total string
  de: (string) -> total either<string,a>

pub alias thunk<a,e> = () -> e a

pub struct step<a,e>
  name: string
  strde: strde<a>
  thunk: thunk<a,e>

pub fun step<a,e>(thunk: thunk<a,e>, name: string, ?strde: strde<a>): step<a,e>
  Step(name, ?strde, thunk)

// alternative allowing the thunk to be given as a trailing lambda - the
// desugaring puts the trailing lambda before the implicit args!
pub fun s<a,e>(name: string, thunk: thunk<a,e>, ?strde: strde<a>): step<a,e>
  Step(name, ?strde, thunk)

// another alternative - merging the step + bind into a single function
// giving a much more compact DSL syntax... the trailing lambda and 
// the with lambda are desugared into the desired positions
pub fun b<a,b,e>(
    name: string, 
    thunk: thunk<a,<logger|e>>, 
    f: (a) -> total step<b,<logger|e>>, 
    ?a/strde: strde<a>,
    ?b/strde: strde<b>
  ): step<b,<logger|e>>
  Step(name, ?a/strde, thunk).bind(f, ?b/strde=?b/strde)

pub fun step/show(a: step<a,e>): string
  a.name ++ ": <unevaluated>"

// the core binding logic - call the thunk to get a value,
// bind that value to the next step, and call that next
// thunk
fun bindcore<a,b,e>(
    st: step<a,<logger|e>>, 
    f: (a) -> total step<b,<logger|e>>,
    b-strde: strde<b>
  ): <logger|e> b

  // call the thunk to get the value to be bound
  val a = st.thunk()() 
  logger/log-bound-value({st.strde.str()(a)})
  // bind a new step, then call its thunk
  val r = f(a).thunk()() 
  logger/log-complete(logger/Run, {b-strde.str()(r)})
  r

pub fun bind<a,b,e>(
    st: step<a,<logger|e>>, 
    f: (a) -> total step<b,<logger|e>>,
    ?b/strde: strde<b>
  ): total step<b,<logger|e>>

  Step(
    "bind--" ++ st.name,
    ?b/strde,
    fn () -> 
      val rs = logger/log-enter(st.name)
      match rs
        // if we are resuming a successful step, short-circuit to the result
        Log(Step-log(_,_,_,Run,Right(r))) ->
          match ?b/strde.de()(r())
            Right(rr) -> 
              logger/log-complete(logger/Resume, r)
              rr
            Left(_err) -> 
              // deser failed - perhaps the code no longer matches the data?
              bindcore(st,f,?b/strde)
        _ -> 
          bindcore(st,f,?b/strde)
  )

// run a step, returning an either result and the computation-log
// h must handle any non-logger effects from the step - the only 
// effects allowed out are div,exn
pub fun run<a>(
    st: step<a,<logger|e>>,
    h: (thunk<a,<logger|e>>) -> <pure,logger> a
  ): div (either<string,a>, logger/computation-log)
  with logger/log-collector(st.name)
  with h 
  st.thunk()()

// convenience function for running a step with no effects
pub fun run-nofx(st: step<a,<pure,logger>>): div (either<string,a>, logger/computation-log)
  run(st, fn (th) -> th())

pub fun rerun<a>(
    st: step<a,<logger|e>>,
    h: (thunk<a,<logger|e>>) -> <pure,logger> a
  ): div (either<string,a>, logger/computation-log)
  with logger/log-collector(st.name)
  with h 
  st.thunk()()

// pub value type resume-step-log
//   StepLog(step-log: step-log)
//   None

// pub fun resume<a,e>(resume-step-log: resume-step-log, st: step<a,e>): e a
//   st.thunk()()

// some examples

pub fun make-test-dsl1()
  with x: int <- b("x") {1}
  with y <- b("y") {10}
  with z <- b("z") {x + y}
  s("result") {2 * z}

pub fun test-dsl1()
  val (_, log) = make-test-dsl1().run-nofx
  logger/print-computation-log(log)
  

fun testdsl/stuff(i: int)
  with x: int <- step({i}, "i").bind
  with y <- s("rand"){random-int()}.bind
  ({x + y}).step("stuff")

pub fun make-test-dsl2(i: int)
  with x: int <- ({i}).step("x").bind
  with y <- ({ if i>5 then [10,11] else [] }).step("y").bind
  val aa' = match y
    [a, b] -> 
      with z <- ({x + a + b}).step("z").bind
      ({z}).step("aa'-pair", ?strde=int/strde)
    _ -> ({0}).step("aa'-default")
  with aa: int <- aa'.bind
  with bb: int <- testdsl/stuff(i).bind
  ({aa.show ++ "/" ++ bb.show}).step("result")

pub fun test-dsl2()
  val (_, log) = make-test-dsl2(10).run(fn(th) -> pseudo-random(100, th))
  logger/print-computation-log(log)
  // val (_, log2) = make-test-dsl2(0).run-nofx
  // logger/print-computation-log(log2)















// serialize/deserialize int and list<int>

val int/strde = Strde(fn(i: int) -> serjson/json(i), fn(s) -> serjson/dejson(s))

val listint/strde = Strde(
  fn(l: list<int>) -> serjson/json(l, ?valuefn=list/make-valuefn(int/valuefn)),
  fn(s) -> serjson/dejson(s, ?pfn=list/make-pfn(int/pfn))
)

val string/strde = Strde(fn(s: string) -> serjson/json(s), fn(s) -> serjson/dejson(s))
