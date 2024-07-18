module steps

import std/core/exn
import std/core/undiv
import std/data/json
import std/num/random
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
pub effect step-control<a>
  ctl ctl-stop(a: a): ()
  ctl ctl-pause(): ()

pub value struct jsonde<a>
  json: (a) -> total json
  de: (json) -> total either<string,a>

pub alias thunk<a,e> = () -> e a
pub alias step-thunk<a,e> = thunk<a,<pure,logger|e>>

// steps are a name, a thunk and a de/serializer for the thunk value.
// this is enough to control execution of a computation and to 
// serialize bound values and outputs to a log so that computations 
// can be resumed and failures inspected from the log
pub struct step<a,e>
  name: string
  jsonde: jsonde<a>
  thunk: step-thunk<a,e>

pub alias pure-step<a> = step<a,<>>

// monadic fns of steps - the fns have the <pure,logger|e> effects
// since (cf fcore.compose) we often need to *run* the step thunks
// to compose the mfns
pub alias stepmfn<a,b,e> = (a: a) -> <pure,logger|e> step<b,e>
pub alias pure-stepmfn<a,b> = stepmfn<a,b,<>>

// allowing the thunk to be given as a trailing lambda - the
// desugaring puts the trailing lambda before the implicit args!
pub fun step<a,e>(name: string, thunk: step-thunk<a,e>, ?jsonde: jsonde<a>): step<a,e>
  Step(name, ?jsonde, thunk)

// merging the step + bind into a single function
// giving a much more compact DSL syntax... the trailing lambda and 
// the with lambda are desugared into the desired positions
pub fun sbind<a,b,e>(
    name: string, 
    thunk: step-thunk<a,e>, 
    f: stepmfn<a,b,e>, 
    ?a/jsonde: jsonde<a>,
    ?b/jsonde: jsonde<b>
  ): step<b,e>
  Step(name, ?a/jsonde, thunk).bind(f, ?b/jsonde=?b/jsonde)

pub fun step/show(a: step<a,e>): string
  a.name ++ ": <unevaluated>"
  
pub fun bind<a,b,e>(
    st: step<a,e>, 
    f: stepmfn<a,b,e>,
    ?b/jsonde: jsonde<b>
  ): total step<b,e>

  Step(
    "bind--" ++ st.name,
    ?b/jsonde,
    fn () -> 
      st.run'.f.run'
  )

pub fun thunk/run''<a,e>(st: step<a, e>): <pure,logger|e> a
  st.thunk()()

pub fun resume/run''<a,e>(st: step<a, e>, step-log: step-log): <pure,logger|e> a
  match step-log
    Step-log(_,_,Right(success-reason)) ->
      val ssrc = match success-reason
        ReasonExec(essrc) -> essrc
        ReasonResume(rssrc) -> rssrc

      val r-str = ssrc()
      match st.jsonde.de()(r-str)
        Right(rr) -> 
          // we are resuming a successful step, short-circuit straight to the result
          logger/log-complete(ReasonResume({r-str}))
          rr
        Left(_err) -> 
          // deser failed - perhaps the code no longer matches the data?
          val r = st.thunk/run''
          logger/log-complete(ReasonExec({st.jsonde.json()(r)}))
          r
    _ -> 
      val r = st.thunk/run''
      logger/log-complete(ReasonExec({st.jsonde.json()(r)}))
      r

// run a step without dismissing any effects
pub fun run'<a,e>(st: step<a, e>): <pure,logger|e> a
  match logger/log-enter(st.name)
    Just(step-log) ->
      resume/run''(st, step-log)
    _ -> 
      val r = st.thunk/run''
      logger/log-complete(ReasonExec({st.jsonde.json()(r)}))
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
    h: (step-thunk<a,e>) -> <pure,logger> a
  ): div (outcome<string,a>, logger/computation-log)
  with logger/log-collector(Nothing, st.name)
  with h 
  st.run' 

pub fun pure-handler<a>(th: thunk<a,<pure,logger>>): <pure,logger> a
  th()

// convenience function for running a pure step
pub fun run-pure(
    st: pure-step<a> 
  ): div (outcome<string,a>, logger/computation-log)
  run(st, pure-handler)

pub fun resume<a>(
    st: step<a,e>,
    log: logger/computation-log,
    h: (step-thunk<a,e>) -> <pure,logger> a
  ): div (outcome<string,a>, logger/computation-log)
  with logger/log-collector(Just(log), st.name)
  with h 
  st.run'

pub fun resume-pure<a>(
    st: pure-step<a>,
    log: logger/computation-log
  ): div (outcome<string,a>, logger/computation-log)
  resume(st, log, pure-handler)

// some examples

pub fun make-test-dsl-trivial(i: int)
  with x: int <- sbind("x") {i}
  step("result") {x.show}

pub fun test-dsl-trivial()
  val (_, log) = make-test-dsl-trivial(100).run-pure
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
//   val logjson = computation-log/json(log)
//   logjson.println
//   match computation-log/dejson(logjson)
//     Right(log3) -> log3.pp
//     Left(err) -> err

// // resuming a computation after fixing an error
// pub fun test-dsl-simple-error-fix-rerun()
//   val (_, log) = make-test-dsl-simple-error(100).run-pure
//   val logj = log.json
//   val (_, log2) = make-test-dsl-simple(100).resume(log, pure-handler)
//   val log2j = log2.json

//   match logj.dejson
//     Right(log') -> 
//       logj.println
//       log'.pp.println
//     Left(err) -> err.show.println

//   match log2j.dejson
//     Right(log2') -> 
//       log2j.println
//       log2'.pp.println
//     Left(err) -> err.show.println

// pub fun make-test-dsl-simple-pause(i: int)
//   with x: int <- sbind("x") {i}
//   pause()
//   with y <- sbind("y") {10}
//   with z <- sbind("z")  {x + y}
//   step("result") {pause() ; (2 * z).show}

// // resuming a normal computation after it was paused
// pub fun test-dsl-simple-pause-resume()
//   val computation = make-test-dsl-simple-pause(100)
//   val (paused-v, paused-log) = computation.run-pure
//   val paused-logj = paused-log.json

//   // serialize the paused computation
//   paused-log.pp.println
//   paused-logj.println
//   paused-v.show.println 

//   // val (_, intermed-log) = computation.resume-pure(paused-log)
//   // intermed-log.print-computation-log
//   // val (_, final-log) = computation.resume-pure(intermed-log)
//   // final-log.print-computation-log
  
//   // deserialize the paused computation and carry on
//   match paused-logj.dejson
//     Right(paused-log') -> 
//       val (_, intermed-log) = computation.resume-pure(paused-log')
//       val intermed-logj = intermed-log.json
//       intermed-log.pp.println
//       intermed-logj.println
//       match intermed-logj.dejson
//         Right(intermed-log') -> 
//           val (_, final-log) = computation.resume-pure(intermed-log')
//           final-log.pp.println
//           final-log.json
//         Left(err) -> err.show
//     Left(err) -> err.show

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
//   // val (_, log2) = make-test-dsl-more-complex(0).run-nofx
//   // logger/print-computation-log(log2)














// serialize/deserialize int and list<int>

pub fun parse-json(j: json, p: (j: json, path: string) -> pure a): either<string,a>
  with pretend-no-div
  match try({p(j, "")})
    Ok(a) -> Right(a)
    Error(e) -> Left(e.message)

val int/jsonde = Jsonde(
  int/to-json, 
  fn (j) -> j.parse-json(int/from-json))

val listint/jsonde = Jsonde(
  fn(l: list<int>) -> l.to-json(),
  fn(j: json) -> j.parse-json(fn (jj, path) -> jj.list/from-json("")))

val string/jsonde = Jsonde(
  string/to-json, 
  fn(j) -> j.parse-json(string/from-json))
