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
pub alias step-thunk<a,e> = thunk<a,<pure,logger|e>>

// steps are a name, a thunk and a de/serializer for the thunk value.
// this is enough to control execution of a computation and to 
// serialize bound values and outputs to a log so that computations 
// can be resumed and failures inspected from the log
pub struct step<a,e>
  name: string
  strde: strde<a>
  thunk: step-thunk<a,e>

pub alias pure-step<a> = step<a,<>>

pub fun step<a,e>(thunk: step-thunk<a,e>, name: string, ?strde: strde<a>): step<a,e>
  Step(name, ?strde, thunk)

// monadic fns of steps - the fns have the <pure,logger|e> effects
// since (cf fcore.compose) we often need to *run* the step thunks
// to compose the mfns
pub alias stepmfn<a,b,e> = (a: a) -> <pure,logger|e> step<b,e>
pub alias pure-stepmfn<a,b> = stepmfn<a,b,<>>

// alternative allowing the thunk to be given as a trailing lambda - the
// desugaring puts the trailing lambda before the implicit args!
pub fun s<a,e>(name: string, thunk: step-thunk<a,e>, ?strde: strde<a>): step<a,e>
  Step(name, ?strde, thunk)

// another alternative - merging the step + bind into a single function
// giving a much more compact DSL syntax... the trailing lambda and 
// the with lambda are desugared into the desired positions
pub fun sb<a,b,e>(
    name: string, 
    thunk: step-thunk<a,e>, 
    f: stepmfn<a,b,e>, 
    ?a/strde: strde<a>,
    ?b/strde: strde<b>
  ): step<b,e>
  Step(name, ?a/strde, thunk).bind(f, ?b/strde=?b/strde)

pub fun step/show(a: step<a,e>): string
  a.name ++ ": <unevaluated>"

// the core binding logic - call the thunk to get a value,
// bind that value to the next step, and call that next
// thunk
fun bindcore<a,b,e>(
    st: step<a,e>, 
    // f: (a) -> total step<b,<logger|e>>,
    f: stepmfn<a,b,e>,
    b-strde: strde<b>
  ): <pure,logger|e> b

  // call the thunk to get the value to be bound
  val a = st.thunk()() 
  logger/log-bound-value({st.strde.str()(a)})
  // bind a new step, then call its thunk
  val r = f(a).thunk()() 
  logger/log-complete(logger/Run, {b-strde.str()(r)})
  r

pub fun bind<a,b,e>(
    st: step<a,e>, 
    f: stepmfn<a,b,e>,
    ?b/strde: strde<b>
  ): total step<b,e>

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

// run a step without dismissing any effects
pub fun run'<a,e>(st: step<a, e>): <pure,logger|e> a
  st.thunk()()

// run a step, returning an either result and the computation-log
// h must handle any non-logger effects from the step - the only 
// effects allowed out are div,exn
pub fun run<a>(
    st: step<a,e>,
    h: (step-thunk<a,e>) -> <pure,logger> a
  ): div (either<string,a>, logger/computation-log)
  with logger/log-collector(Nothing, st.name)
  with h 
  st.thunk()()

pub fun pure-handler<a>(th: thunk<a,<pure,logger>>): <pure,logger> a
  th()

// convenience function for running a pure step
pub fun run-pure(st: pure-step<a>): div (either<string,a>, logger/computation-log)
  run(st, fn (th) -> th())

pub fun rerun<a>(
    st: step<a,e>,
    log: logger/computation-log,
    h: (step-thunk<a,e>) -> <pure,logger> a
  ): div (either<string,a>, logger/computation-log)
  with logger/log-collector(Just(log), st.name)
  with h 
  st.thunk()()

// pub value type resume-step-log
//   StepLog(step-log: step-log)
//   None

// pub fun resume<a,e>(resume-step-log: resume-step-log, st: step<a,e>): e a
//   st.thunk()()

// some examples

pub fun make-test-dsl0(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") {10}
  with z <- sb("z") {x + y}
  s("result") {2 * z}

pub fun test-dsl0()
  val (_, log) = make-test-dsl0(100).run-pure
  logger/print-computation-log(log)

pub fun make-test-dsl05(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") { /* throw("hoo") ; */ 10}
  with z <- sb("z") {x + y}
  throw("boo")
  s("result") {2 * z}

pub fun test-dsl05()
  val (_, log) = make-test-dsl05(100).run-pure
  logger/print-computation-log(log)

pub fun test-dsl06()
  val (_, log) = make-test-dsl0(100).run-pure
  val (_, log2) = make-test-dsl0(100).rerun(log, pure-handler)
  logger/print-computation-log(log)
  logger/print-computation-log(log2)

pub fun make-test-dsl07(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") { 10}
  with z <- sb("z") {x + y}
  s("result") {2 * z}

pub fun test-dsl07()
  val (_, log) = make-test-dsl05(100).run-pure
  val (_, log2) = make-test-dsl0(100).rerun(log, pure-handler)
  logger/print-computation-log(log)
  logger/print-computation-log(log2)


pub fun make-test-dsl1(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") {random-int()}
  with z <- sb("z") {x + y}
  s("result") {2 * z}

fun handle-random<a>(th: thunk<a,<random|e>>): e a
  with pseudo-random(100)
  th()

pub fun test-dsl1()
  val (_, log) = make-test-dsl1(100).run(handle-random)
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
  val (_, log) = make-test-dsl2(10).run(handle-random)
  logger/print-computation-log(log)
  // val (_, log2) = make-test-dsl2(0).run-nofx
  // logger/print-computation-log(log2)

pub fun make-test-dsl3(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") { if i>5 then [10,11] else [] }
  val aa' = match y
    [a, b] -> 
      with z <- sb("z") {x + a + b}
      s("aa'-pair", ?strde=int/strde) {z}
    _ -> s("aa'-default") {0}
  with aa: int <- aa'.bind
  with bb: int <- testdsl/stuff(i).bind
  s("result") {aa.show ++ "/" ++ bb.show}

pub fun test-dsl3()
  val (_, log) = make-test-dsl3(10).run(fn(th) -> pseudo-random(100, th))
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
