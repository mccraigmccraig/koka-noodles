module steps

import parsers/serjson/serjson
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
 
  val log-bind-runs = logger/log-options.bind-detail

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

  logger/log-complete(logger/Exec, {b-strde.str()(r)})
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

  val log-bind-runs = logger/log-options.bind-detail

  match step-log
    Step-log(_,_,Just(bv-strsrc),_,_,_) ->
      match st.strde.de()(bv-strsrc())
        Right(aa) -> 
          // we have a bound value - no need to re-run the thunk, just the bind
          // but if we don't run a logged bind, then we must skip it to correctly
          // tracking the log
          if log-bind-runs then
            logger/log-skip()            
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

// full resume logic - if we have a result value logged, then
// return with the logged result, otherwise try for a resume-bind
fun resume-full/bindcore<a,b,e>(
    st: step<a,e>, 
    step-log: step-log,
    f: stepmfn<a,b,e>,
    b-strde: strde<b>
  ): <pure,logger|e> b
  match step-log
    Step-log(_,_,_,_,_,Right(r-strsrc)) ->
      match b-strde.de()(r-strsrc())
        Right(rr) -> 
          // we are resuming a successful step, short-circuit straight to the result
          logger/log-complete(logger/Resume, r-strsrc)
          rr
        Left(_err) -> 
          // deser failed - perhaps the code no longer matches the data?
          resume-bind/bindcore(st,step-log,f,b-strde)
    Step-log(_,_,Just(_bv-src),_,_,_) ->
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
      match logger/log-enter(st.name, BindStep)
        Log(step-log) ->
          resume-full/bindcore(st, step-log, f, ?b/strde)
        _ -> 
          full/bindcore(st,f,?b/strde)
  )

pub fun thunk/run''<a,e>(st: step<a, e>): <pure,logger|e> a
  st.thunk()()

pub fun resume/run''<a,e>(st: step<a, e>, step-log: step-log): <pure,logger|e> a
  match step-log
    Step-log(_,_,_,_,_,Right(r-strsrc)) ->
      match st.strde.de()(r-strsrc())
        Right(rr) -> 
          // we are resuming a successful step, short-circuit straight to the result
          logger/log-complete(logger/Resume, r-strsrc)
          rr
        Left(_err) -> 
          // deser failed - perhaps the code no longer matches the data?
          val r = st.thunk/run''
          logger/log-complete(logger/Exec, {st.strde.str()(r)})
          r
    _ -> 
      val r = st.thunk/run''
      logger/log-complete(logger/Exec, {st.strde.str()(r)})
      r


// run a step without dismissing any effects
pub fun run'<a,e>(st: step<a, e>): <pure,logger|e> a
  match logger/log-enter(st.name, RunStep)
    Log(step-log) ->
      resume/run''(st, step-log)
    _ -> 
      val r = st.thunk/run''
      logger/log-complete(logger/Exec, {st.strde.str()(r)})
      r

// run a pure-step 
pub fun run-pure'<a>(st: pure-step<a>): <pure,logger> a
  st.run'

pub fun compose-stepmfn(fa: stepmfn<a,b,e>, fb: stepmfn<b,c,e>): stepmfn<a,c,e>
  fn (a)
    val sb = fa(a)
    val b = sb.run'
    fb(b)

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

pub fun resume<a>(
    st: step<a,e>,
    log: logger/computation-log,
    h: (step-thunk<a,e>) -> <pure,logger> a
  ): div (either<string,a>, logger/computation-log)
  with logger/log-collector(Just(log), st.name, log.options)
  with h 
  st.run'

// some examples

pub fun make-test-dsl-trivial(i: int)
  with x: int <- sb("x") {i}
  s("result") {x.show}

pub fun test-dsl-trivial()
  val (_, log) = make-test-dsl-trivial(100).run-pure(logger/default-log-options)
  logger/print-computation-log(log)
  val logjson = computation-log/json(log)
  println(logjson)
  match computation-log/dejson(logjson)
    Right(log') -> log'.pp
    Left(err) -> err.show

pub fun make-test-dsl-simple(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") {10}
  with z <- sb("z") {x + y}
  s("result") {(2 * z).show}

pub fun test-dsl-simple()
  val (_, log) = make-test-dsl-simple(100).run-pure(logger/detailed-log-options)
  logger/print-computation-log(log)
  val logjson = computation-log/json(log)
  logjson.println
  match computation-log/dejson(logjson)
    Right(log3) -> log3.pp
    Left(err) -> err

pub fun make-test-dsl-simple-error(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") {10}
  with z <- sb("z") {throw("boo") ; x + y}
  s("result") {(2 * z).show}

pub fun make-test-dsl-pause(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") {10}
  with z <- sb("z") {pause() ; x + y}
  s("result") {(2 * z).show}


pub fun test-dsl-simple-error-fix-rerun()
  val (_, log) = make-test-dsl-simple-error(100).run-pure(logger/detailed-log-options)
  val logj = log.json
  val (_, log2) = make-test-dsl-simple(100).resume(log, pure-handler)
  val log2j = log2.json

  match logj.dejson
    Right(log') -> 
      logj.println
      log'.pp.println
    Left(err) -> err.show.println

  match log2j.dejson
    Right(log2') -> 
      log2j.println
      log2'.pp.println
    Left(err) -> err.show.println

pub fun test-dsl-simple-pause-resume()
  val (_, log) = make-test-dsl-pause(100).run-pure(logger/detailed-log-options)
  val logj = log.json
  val (_, log2) = make-test-dsl-simple(100).resume(log, pure-handler)
  val log2j = log2.json

  match logj.dejson
    Right(log') -> 
      logj.println
      log'.pp.println
    Left(err) -> err.show.println

  match log2j.dejson
    Right(log2') -> 
      log2j.println
      log2'.pp.println
    Left(err) -> err.show.println


pub fun test-dsl-simple-error()
  val (_, log) = make-test-dsl-simple-error(100).run-pure(logger/detailed-log-options)
  logger/print-computation-log(log)

pub fun test-dsl-simple-resume()
  val (_, log) = make-test-dsl-simple(100).run-pure(logger/detailed-log-options)
  val (_, log2) = make-test-dsl-simple(100).resume(log, pure-handler)
  logger/print-computation-log(log)
  logger/print-computation-log(log2)

pub fun make-test-dsl-random-effect(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") {random-int()}
  with z <- sb("z") {x + y}
  s("result") {2 * z}

fun handle-random<a>(th: thunk<a,<random|e>>): e a
  with pseudo-random(100)
  th()

pub fun test-dsl-random-effect()
  val (_, log) = make-test-dsl-random-effect(100).run(logger/default-log-options, handle-random)
  logger/print-computation-log(log)
  

fun testdsl/stuff(i: int)
  with x: int <- step({i}, "i").bind
  with y <- s("rand"){random-int()}.bind
  ({x + y}).step("stuff")

pub fun make-test-dsl-more-complex(i: int)
  with x: int <- ({i}).step("x").bind
  with y <- ({ if i>5 then [10,11] else [] }).step("y").bind
  val aa' = match y
    [a, b] -> 
      with z: int <- ({x + a + b}).step("z").bind
      ({z + 10}).step("aa'-pair")
    _ -> ({0}).step("aa'-default")
  with aa: int <- aa'.bind
  with bb: int <- testdsl/stuff(i).bind
  ({aa.show ++ "/" ++ bb.show}).step("result")

pub fun test-dsl-more-complex()
  val (_, log) = make-test-dsl-more-complex(10).run(logger/default-log-options, handle-random)
  logger/print-computation-log(log)
  // val (_, log2) = make-test-dsl-more-complex(0).run-nofx
  // logger/print-computation-log(log2)

pub fun make-test-dsl-more-complex-compact(i: int)
  with x: int <- sb("x") {i}
  with y <- sb("y") { if i>5 then [10,11] else [] }
  val aa' = match y
    [a, b] -> 
      with z: int <- sb("z") {x + a + b}
      s("aa'-pair") {z + 10}
    _ -> s("aa'-default") {0}
  with aa: int <- aa'.bind
  with bb: int <- testdsl/stuff(i).bind
  s("result") {aa.show ++ "/" ++ bb.show}

pub fun test-dsl-more-complext-compact()
  val (_, log) = make-test-dsl-more-complex-compact(10).run(logger/default-log-options, fn(th) -> pseudo-random(100, th))
  logger/print-computation-log(log)
  // val (_, log2) = make-test-dsl-more-complex(0).run-nofx
  // logger/print-computation-log(log2)














// serialize/deserialize int and list<int>

val int/strde = Strde(fn(i: int) -> serjson/json(i), fn(s) -> serjson/dejson(s))

val listint/strde = Strde(
  fn(l: list<int>) -> serjson/json(l, ?vfn=list/make-vfn(int/vfn)),
  fn(s) -> serjson/dejson(s, ?pfn=list/make-pfn(int/pfn))
)

val string/strde = Strde(fn(s: string) -> serjson/json(s), fn(s) -> serjson/dejson(s))
