module steps

import std/core/exn
import std/core/list
import std/core/string
import std/core/undiv
import std/data/json
import std/num/random
import std/pretty/pprint
import logger

// TODO think about error channels - the thunk can throw, 
// but what about a left/right channel ?
// e.g. could use a typed step-control effect so the stop
// effect can return a value to the computation left-channel
// so left-channel would have
// pause | error | stop<a>
// ...but... does this compose ? it could do... would need
// some handler combinators to modify the left channel...
// could build either into the result type too, so there are 
// options
// TODO - should pause get a data parameter to allow coordination
// code to make decisions ? step-thunks could then be:
//   step-thunk< either<a,c>, <step-ctl<a,b,c>|e> >
//     a - failure channel
//     b - pause channel
//     c - success value
//     e - polymorphic effects
// and we could offer a step-ctl effect with short-circuits for 
// failure, success and pause, with a 3-way final result, 
// like an either with an additional pause channel
//
// there will still be a polymorphic effect, but we won't allow 
// it to propagate an exn out of the caller's handler, so the 
// result will be completely-typed

// like an either, but with a typed pause... like
// either makes left-to-right wrong-to-right
pub value type run-step-result<a,b,c>
  StepFail(a: a)
  StepPause(b: b)
  StepOk(c: c)

effect no-return
  ctl no-return(): a

// consumer control effect
pub effect step-ctl<a,b>
  // Immediately fail the computation with a value. Equivalent
  // to returning a Left(a) from a thunk
  ctl fail(a: a): ()
  // Immediately pause the computation with a value
  ctl pause(b: b): ()


pub alias thunk<a,e> = () -> e a

// thunks returning an either, and with logger and step-ctl effects
pub alias step-thunk<a,b,c,e> = thunk<either<a,c>, <div,logger<a,b>,step-ctl<a,b>|e>>

// steps are a name, a thunk and a de/serializer for the thunk value.
// this is enough to control execution of a computation and to 
// serialize bound values and outputs to a log so that computations 
// can be resumed and failures inspected from the log
pub struct step<a,b,c,e>
  name: string
  a-json-tofrom: json-tofrom<a>
  b-json-tofrom: json-tofrom<b>
  c-json-tofrom: json-tofrom<c>
  thunk: step-thunk<a,b,c,e>

pub alias pure-step<a,b,c> = step<a,b,c,<>>

// monadic fns of step values
pub alias stepmfn<a,b,c,d,e> = (c: c) -> <div,logger<a,b>,step-ctl<a,b>|e> step<a,b,d,e>
pub alias pure-stepmfn<a,b,c,d> = stepmfn<a,b,c,d,<>>

// map error, pause and success values in a step
pub alias trifmap<a,b,c,x,y,z,e> = ((a)->x,(b)->y,(c)->z,step<x,y,z,e>)

// allowing the thunk to be given as a trailing lambda - the
// desugaring puts the trailing lambda before the implicit args!
pub fun step<a,b,c,e>(
  name: string, 
  thunk: step-thunk<a,b,c,e>, 
  ?a/json-tofrom: json-tofrom<a>,
  ?b/json-tofrom: json-tofrom<b>,
  ?c/json-tofrom: json-tofrom<c>): step<a,b,c,e>
  Step(name, ?a/json-tofrom, ?b/json-tofrom, ?c/json-tofrom, thunk)

// merging the step + bind into a single function
// giving a more compact DSL syntax... the trailing lambda and 
// the with lambda are desugared into the desired positions
pub fun sbind<a,b,c,d,e>(
    name: string, 
    thunk: step-thunk<a,b,c,e>, 
    f: stepmfn<a,b,c,d,e>, 
    ?a/json-tofrom: json-tofrom<a>,
    ?b/json-tofrom: json-tofrom<b>,
    ?c/json-tofrom: json-tofrom<c>,
    ?d/json-tofrom: json-tofrom<d>
  ): step<a,b,d,e>
  Step(
      name, 
      ?a/json-tofrom, 
      ?b/json-tofrom, 
      ?c/json-tofrom, 
      thunk
    ).bind(f, ?d/json-tofrom=?d/json-tofrom)

pub fun step/show(a: step<a,b,c,e>): string
  a.name ++ ": <unevaluated>"
  
pub fun bind<a,b,c,d,e>(
    st: step<a,b,c,e>, 
    f: stepmfn<a,b,c,d,e>,
    ?d/json-tofrom: json-tofrom<d>
  ): total step<a,b,d,e>

  Step(
    "bind--" ++ st.name,
    st.a-json-tofrom,
    st.b-json-tofrom,
    ?d/json-tofrom,
    fn () ->
      match st.run'
        Left(a) -> Left(a)
        Right(c) -> c.f.run'
  )

// some json-tofrom for the either result of the thunk
pub fun thunk/run''<a,b,c,e>(st: step<a,b,c,e>): <div,logger<a,b>,step-ctl<a,b>|e> either<a,c>
  st.thunk()()

fun thunk/run-core'<a,b,c,e>(
  st: step<a,b,c,e>
): <div,logger<a,b>,step-ctl<a,b>|e> either<a,c>
    val r = st.thunk/run''
    match r
      Left(f) -> 
        logger/log-leave(ReasonFail({st.a-json-tofrom.to-json()(f)}))
      Right(s) -> 
        logger/log-leave(ReasonExec({st.c-json-tofrom.to-json()(s)}))
    r

fun resume/resume-core''<a,b,c,e>(  
  st: step<a,b,c,e>, 
  c-json: json
): <div,logger<a,b>,step-ctl<a,b>|e> either<a,c>
  match parse-json(c-json, "", st.c-json-tofrom.from-json)
    Right(rr) -> 
      // we are resuming a successful step, short-circuit straight to the result
      logger/log-leave(ReasonResume({c-json}))
      Right(rr)
    Left(_err) -> st.thunk/run-core'

pub fun resume/run'<a,b,c,e>(
  st: step<a,b,c,e>, 
  step-log: step-log
): <div,logger<a,b>,step-ctl<a,b>|e> either<a,c>
  match step-log
    Step-log(_,_,ReasonExec(c-jsrc)) -> resume/resume-core''(st, c-jsrc())
    Step-log(_,_,ReasonResume(c-jsrc)) -> resume/resume-core''(st, c-jsrc())
    _ -> st.thunk/run-core'

// run a step without dismissing any effects
pub fun run'<a,e>(st: step<a,b,c,e>): <div,logger<a,b>,step-ctl<a,b>|e> either<a,c>
  match logger/log-enter(st.name)
    Just(step-log) ->
      resume/run'(st, step-log)
    _ -> st.thunk/run-core'

// run a pure-step 
pub fun run-pure'<a>(st: pure-step<a,b,c>): <div,logger<a,b>,step-ctl<a,b>> either<a,c>
  st.run'

// pub fun compose-stepmfn(fa: stepmfn<a,b,e>, fb: stepmfn<b,c,e>): stepmfn<a,c,e>
//   fn (a)
//     val sb = fa(a)
//     val b = sb.run'
//     fb(b)

// handle the step-ctl effects, using step knowledge to convert them to logger effects
pub fun step-ctl-handler<a,b,c,e>(
    st: step<a,b,c,e>,
    handle-fatal-error: (string)->a,
    th: thunk<either<a,c>, <div,logger<a,b>,step-ctl<a,b>>>
  ): <div,logger<a,b>> either<a,c>
  with handler
    ctl fail(a)
      log-fail(a, st.a-json-tofrom)
      resume(()) // never happens
    ctl pause(b)
      log-pause(b, st.b-json-tofrom)
      resume(())
  th()

// run a step, returning an either result and the computation-log
// h must handle any non-logger effects from the step - the only 
// effects allowed out are div,exn
pub fun run<a>(
    st: step<a,b,c,e>,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger<a,b>,step-ctl<a,b>> either<a,c>
  ): div (outcome<a,b,c>, logger/computation-log)
  with logger/log-collector(Nothing, st.name, handle-fatal-error)
  with st.step-ctl-handler(handle-fatal-error)
  with h 
  st.run' 

pub fun pure-handler<a,b,c>(
    th: thunk<either<a,c>,<div,logger<a,b>,step-ctl<a,b>>>
  ): <div,logger<a,b>,step-ctl<a,b>> either<a,c>
  th()

// convenience function for running a pure step
pub fun run-pure(
    st: pure-step<a,b,c>,
    handle-fatal-error: (string)->a
  ): div (outcome<a,b,c>, logger/computation-log)
  run(st, handle-fatal-error, pure-handler)

pub fun resume<a>(
    st: step<a,b,c,e>,
    log: logger/computation-log,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger<a,b>,step-ctl<a,b>> either<a,c>
  ): div (outcome<a,b,c>, logger/computation-log)
  with logger/log-collector(Just(log), st.name, handle-fatal-error)
  with st.step-ctl-handler(handle-fatal-error)
  with h 
  st.run'

pub fun resume-pure<a>(
    st: pure-step<a,b,c>,
    log: logger/computation-log,
    handle-fatal-error: (string)->a
  ): div (outcome<a,b,c>, logger/computation-log)
  resume(st, log, handle-fatal-error, pure-handler)

// some examples

pub fun make-test-dsl-trivial(i: int): pure-step<string,string,string>
  with x <- sbind("x") {i.Right}
  step("result") {x.show.Right}

pub fun test-dsl-trivial()
  val (_, log) = make-test-dsl-trivial(100).run-pure(id)
  log.pp.println
  val logjson = log.to-json
  logjson.println
  val log' = logjson.computation-log/from-json("")
  log'.pp.println

// pub fun make-test-dsl-simple(i: int)
//   with x: int <- sbind("x") {i}
//   with y <- sbind("y") {10}
//   with z <- sbind("z") {x + y}
//   step("result") {(2 * z).show}

// pub fun make-test-dsl-simple-error(i: int)
//   with x: int <- sbind("x") {i}
//   with y <- sbind("y") {10}
//   with z <- sbind("z") {throw("boo") ; x + y}
//   step("result") {(2 * z).show}

// pub fun test-dsl-simple()
//   val (_, log) = make-test-dsl-simple(100).run-pure
//   log.pp.println
//   val logjson = log.to-json
//   logjson.println
//   val log' = logjson.computation-log/from-json("")
//   log'.pp.println

// // resuming a computation after fixing an error
// pub fun test-dsl-simple-error-fix-rerun()
//   val (_, log) = make-test-dsl-simple-error(100).run-pure
//   val logj = log.to-json
//   val (_, log2) = make-test-dsl-simple(100).resume(log, pure-handler)
//   val log2j = log2.to-json

//   val log' = logj.computation-log/from-json("")
//   log'.pp.println

//   val log2' = log2j.computation-log/from-json("")
//   log2'.pp.println

// pub fun make-test-dsl-simple-pause(i: int)
//   with x: int <- sbind("x") {i}
//   pause()
//   with y <- sbind("y") {10}
//   with z <- sbind("z")  {x + y}
//   step("result") {pause() ; (2 * z).show}

// val rule = string/repeat("-", 20)

// // resuming a normal computation after it was paused
// pub fun test-dsl-simple-pause-resume()
//   val computation = make-test-dsl-simple-pause(100)
//   val (paused-v, paused-log) = computation.run-pure
//   val paused-logj = paused-log.to-json

//   paused-log.pp.println
//   rule.println
//   paused-logj.to-json.pretty.show.println
//   rule.println
//   paused-v.show.println 
//   rule.println

//   val paused-log' = paused-logj.computation-log/from-json("")
//   val (_, intermed-log) = computation.resume-pure(paused-log')
//   // intermed-log.pp.println
//   intermed-log.to-json.pretty.show.println
//   rule.println
//   val intermed-logj = intermed-log.to-json

//   val intermed-log' = intermed-logj.computation-log/from-json("")
//   val (_, final-log) = computation.resume-pure(intermed-log')
//   final-log.pp.println
//   rule.println
//   final-log.to-json.println

// pub fun test-dsl-simple-error()
//   val (_, log) = make-test-dsl-simple-error(100).run-pure
//   log.pp.println

// pub fun test-dsl-simple-resume()
//   val (_, log) = make-test-dsl-simple(100).run-pure
//   val (_, log2) = make-test-dsl-simple(100).resume(log, pure-handler)
//   log.pp.println
//   log2.pp.println

// pub fun make-test-dsl-random-effect(i: int)
//   with x: int <- sbind("x") {i}
//   with y <- sbind("y") {random-int()}
//   with z <- sbind("z") {x + y}
//   step("result") {2 * z}

// fun handle-random<a>(th: thunk<a,<random|e>>): e a
//   with pseudo-random(100)
//   th()

// pub fun test-dsl-random-effect()
//   val (_, log) = make-test-dsl-random-effect(100).run(handle-random)
//   log.pp.println
  

// fun testdsl/stuff(i: int)
//   with x: int <- step("i"){i}.bind
//   with y <- step("rand"){random-int()}.bind
//   step("stuff") {x + y}

// pub fun make-test-dsl-more-complex-compact(i: int)
//   with x: int <- sbind("x") {i}
//   with y <- sbind("y") { if i>5 then [10,11] else [] }
//   val aa' = match y
//     [a, b] -> 
//       with z: int <- sbind("z") {x + a + b}
//       step("aa'-pair") {z + 10}
//     _ -> step("aa'-default") {0}
//   with aa: int <- aa'.bind
//   with bb: int <- testdsl/stuff(i).bind
//   step("result") {aa.show ++ "/" ++ bb.show}

// pub fun test-dsl-more-complext-compact()
//   val (_, log) = make-test-dsl-more-complex-compact(10).run(fn(th) -> pseudo-random(100, th))
//   log.pp.println
//   log.to-json.show.println














// // serialize/deserialize int and list<int>

// pub fun parse-json(j: json, p: (j: json, path: string) -> pure a): either<string,a>
//   with pretend-no-div
//   match try({p(j, "")})
//     Ok(a) -> Right(a)
//     Error(e) -> Left(e.message)

// val int/json-tofrom = Jsonde(
//   int/to-json, 
//   fn (j) -> j.parse-json(int/from-json))

// val listint/json-tofrom = Jsonde(
//   fn(l: list<int>) -> l.to-json(),
//   fn(j: json) -> j.parse-json(fn (jj, path) -> jj.list/from-json(path)))

// val string/json-tofrom = Jsonde(
//   string/to-json, 
//   fn(j) -> j.parse-json(string/from-json))
