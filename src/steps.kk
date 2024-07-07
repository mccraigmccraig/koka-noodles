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

// full core binding logic - call the thunk to get a value,
// bind that value to the next step, and call that next
// thunk
fun full/bindcore<a,b,e>(
    st: step<a,e>, 
    f: stepmfn<a,b,e>,
    b-strde: strde<b>
  ): <pure,logger|e> b
 
  val log-bind-runs = logger/options.bind-detail()

  // call the thunk to get the value to be bound
  val a = if log-bind-runs then
    st.run'
  else
    st.run''

  logger/log-bound-value({st.strde.str()(a)})
  // bind a new step, then call its thunk
  val r = if log-bind-runs then
    f(a).run'
  else
    f(a).run''

  logger/log-complete(logger/Run, {b-strde.str()(r)})
  r

// partial resume logic - we have a bound value logged, so no
// need to call the thunk to get it, just bind the value to 
// the function
fun resume-bind/bindcore<a,b,e>(
    st: step<a,e>, 
    step-log: step-log,
    f: stepmfn<a,b,e>,
    b-strde: strde<b>
  ): <pure,logger|e> b

  val log-bind-runs = logger/options.bind-detail

  match step-log
    Step-log(_,Just(bv-strsrc),_,_,_) ->
      match st.strde.de()(bv-strsrc())
        Right(aa) -> 
          // we have a bound value - no need to re-run the thunk, just the bind
          logger/log-bound-value(bv-strsrc)
          val r = if log-bind-runs then 
            f(aa).run'
          else 
            f(aa).run''
          logger/log-complete(logger/ResumeBind, {b-strde.str()(r)})
          r
        Left(_err) -> 
          // deser failed - perhaps the code no longer matches the data?
          full/bindcore(st,f,b-strde)    
    _ -> full/bindcore(st,f,b-strde)

// full resume logic - we have a result value logged
fun resume-full/bindcore<a,b,e>(
    st: step<a,e>, 
    step-log: step-log,
    f: stepmfn<a,b,e>,
    b-strde: strde<b>
  ): <pure,logger|e> b
  match step-log
    Step-log(_,_,_,_,Right(r-strsrc)) ->
      match b-strde.de()(r-strsrc())
        Right(rr) -> 
          // we are resuming a successful step, short-circuit straight to the result
          logger/log-complete(logger/Resume, r-strsrc)
          rr
        Left(_err) -> 
          // deser failed - perhaps the code no longer matches the data?
          resume-bind/bindcore(st,step-log,f,b-strde)
    Step-log(_,Just(_bv-src),_,_,_) ->
      resume-bind/bindcore(st,step-log,f,b-strde)
    _ -> full/bindcore(st,f,b-strde)

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
        Log(step-log) ->
          resume-full/bindcore(st, step-log, f, ?b/strde)
        _ -> 
          full/bindcore(st,f,?b/strde)
  )

pub fun run''<a,e>(st: step<a, e>): <pure,logger|e> a
  st.thunk()()

// run a step without dismissing any effects
pub fun run'<a,e>(st: step<a, e>): <pure,logger|e> a
  logger/log-enter("run--" ++ st.name)
  val r = st.run''
  logger/log-complete(logger/Run, {st.strde.str()(r)})
  r

// run a pure-step 
pub fun run-pure'<a>(st: pure-step<a>): <pure,logger> a
  st.run'

// run a step, returning an either result and the computation-log
// h must handle any non-logger effects from the step - the only 
// effects allowed out are div,exn
pub fun run<a>(
    st: step<a,e>,
    log-options: logger/log-options,
    h: (step-thunk<a,e>) -> <pure,logger> a
  ): div (either<string,a>, logger/computation-log)
  with logger/log-collector(Nothing, st.name, log-options)
  with h 
  st.run'

pub fun pure-handler<a>(th: thunk<a,<pure,logger>>): <pure,logger> a
  th()

// convenience function for running a pure step
pub fun run-pure(st: pure-step<a>, log-options: logger/log-options): div (either<string,a>, logger/computation-log)
  run(st, log-options, pure-handler)

pub fun rerun<a>(
    st: step<a,e>,
    log: logger/computation-log,
    log-options: logger/log-options,
    h: (step-thunk<a,e>) -> <pure,logger> a
  ): div (either<string,a>, logger/computation-log)
  with logger/log-collector(Just(log), st.name, log-options)
  with h 
  st.run'

// some examples

pub fun make-test-dsl0(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") {10}
  with z <- sb("z") {x + y}
  s("result") {2 * z}

pub fun test-dsl0()
  val (_, log) = make-test-dsl0(100).run-pure(logger/default/log-options)
  logger/print-computation-log(log)

pub fun make-test-dsl05(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") {10}
  with z <- sb("z") {throw("hoo") ; x + y}
  throw("boo")
  s("result") {2 * z}

pub fun test-dsl05()
  val (_, log) = make-test-dsl05(100).run-pure(logger/default/log-options)
  logger/print-computation-log(log)

pub fun test-dsl06()
  val (_, log) = make-test-dsl0(100).run-pure(logger/default/log-options)
  val (_, log2) = make-test-dsl0(100).rerun(log, logger/default/log-options, pure-handler)
  logger/print-computation-log(log)
  logger/print-computation-log(log2)

pub fun test-dsl07()
  val (_, log) = make-test-dsl05(100).run-pure(logger/detailed/log-options)
  val (_, log2) = make-test-dsl0(100).rerun(log, logger/detailed/log-options, pure-handler)
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
  val (_, log) = make-test-dsl1(100).run(logger/default/log-options, handle-random)
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
  val (_, log) = make-test-dsl2(10).run(logger/default/log-options, handle-random)
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
  val (_, log) = make-test-dsl3(10).run(logger/default/log-options, fn(th) -> pseudo-random(100, th))
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
