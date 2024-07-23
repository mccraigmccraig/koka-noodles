module steps

import std/core/exn
import std/core/list
import std/core/string
import std/core/undiv
import std/data/json
import std/num/random
import std/pretty/pprint
import steps/logger

// like an either, but with a typed pause channel
pub value type run-step-result<a,b,c>
  StepFail(a: a)
  StepPause(b: b)
  StepOk(c: c)

pub fun run-step-result/show<a,b,c>(r: run-step-result<a,b,c>, ?a/show: (a)->string, ?b/show: (b)->string, ?c/show: (c)->string): string
  match r
    StepFail(a) -> "Failed: " ++ a.?a/show
    StepPause(b) -> "Paused: " ++ b.?b/show
    StepOk(c) -> "Ok: " ++ c.?c/show

// consumer control effect - immediately pause or fail a computation.
// 
// There's no short-circuit returning of values - because 
// that would mean parameterising step-ctl on the value type, and 
// so would require rebinding handlers at every bind, and need
// (c)->d functions we don't have 
pub effect step-ctl<a,b>
  // Immediately fail the computation with a value - equivalent
  // to returning a Left(a) from a thunk
  ctl fail(a: a): ()
  // Immediately pause the computation with a value
  ctl pause(b: b): ()

pub alias thunk<a,e> = () -> e a

// thunks returning an either, and with logger and step-ctl effects
// TODO thunks returning an either is nice, but it seems to be giving the inference
// trouble - probably since there are two branches and with literal values only 
// one is inferrable... would it be better to switch back to single-type return 
// values and use the fail effect or an exn to return errors ?
pub alias step-thunk<a,b,c,e> = thunk<c, <div,logger,step-ctl<a,b>|e>>

// steps are a name, a thunk and json de/serializers for all the values.
pub struct step<a,b,c,e>
  name: string
  a-json-fns: json-fns<a>
  b-json-fns: json-fns<b>
  c-json-fns: json-fns<c>
  thunk: step-thunk<a,b,c,e>

pub alias div-step<a,b,c> = step<a,b,c,<>>

// monadic fns of step values
pub alias stepmfn<a,b,c,d,e> = (c: c) -> <div,logger,step-ctl<a,b>|e> step<a,b,d,e>
pub alias div-stepmfn<a,b,c,d> = stepmfn<a,b,c,d,<>>

// map error, pause and success values in a step
pub alias trifmap<a,b,c,x,y,z,e> = ((a)->x,(b)->y,(c)->z,step<a,b,c,e>) -> step<x,y,z,e>

// allowing the thunk to be given as a trailing lambda - the
// desugaring puts the trailing lambda before the implicit args!
pub fun step<a,b,c,e>(
  name: string, 
  thunk: step-thunk<a,b,c,e>, 
  ?a/json-fns: json-fns<a>,
  ?b/json-fns: json-fns<b>,
  ?c/json-fns: json-fns<c>): step<a,b,c,e>
  Step(name, ?a/json-fns, ?b/json-fns, ?c/json-fns, thunk)

// merging the step + bind into a single function
// giving a more compact DSL syntax... the trailing lambda and 
// the with lambda are desugared into the desired positions
pub fun sbind<a,b,c,d,e>(
    name: string, 
    thunk: step-thunk<a,b,c,e>, 
    f: stepmfn<a,b,c,d,e>, 
    ?a/json-fns: json-fns<a>,
    ?b/json-fns: json-fns<b>,
    ?c/json-fns: json-fns<c>,
    ?d/json-fns: json-fns<d>
  ): step<a,b,d,e>
  Step(
      name, 
      ?a/json-fns, 
      ?b/json-fns, 
      ?c/json-fns, 
      thunk
    ).bind(f, ?d/json-fns=?d/json-fns)

pub fun step/show(a: step<a,b,c,e>): string
  a.name ++ ": <unevaluated>"
  
pub fun fmap<a,b,c,d,e>(
    f: (c)->d,
    st: step<a,b,c,e>, 
    ?d/json-fns: json-fns<d>
  ): total step<a,b,d,e>

  Step(
    "bind--" ++ st.name,
    st.a-json-fns,
    st.b-json-fns,
    ?d/json-fns,
    fn () ->
      st.run'.f
  )

pub fun bind<a,b,c,d,e>(
    st: step<a,b,c,e>, 
    f: stepmfn<a,b,c,d,e>,
    ?d/json-fns: json-fns<d>
  ): total step<a,b,d,e>

  Step(
    "bind--" ++ st.name,
    st.a-json-fns,
    st.b-json-fns,
    ?d/json-fns,
    fn () ->
      st.run'.f.run'
  )

// some json-fns for the either result of the thunk
pub fun thunk/run''<a,b,c,e>(st: step<a,b,c,e>): <div,logger,step-ctl<a,b>|e> c
  st.thunk()()

fun thunk/run-core'<a,b,c,e>(
  st: step<a,b,c,e>
): <div,logger,step-ctl<a,b>|e> c
    val r = st.thunk/run''
    logger/log-leave(ReasonExec({st.c-json-fns.to-json()(r)}))
    r

fun resume/resume-core'<a,b,c,e>(  
  st: step<a,b,c,e>, 
  c-json: json
): <div,logger,step-ctl<a,b>|e> c
  match parse-json(c-json, "", st.c-json-fns.from-json)
    Right(rr) -> 
      // we are resuming a successful step, short-circuit straight to the result
      logger/log-leave(ReasonResume({c-json}))
      rr
    Left(_err) -> st.thunk/run-core'

fun resume/run'<a,b,c,e>(
  st: step<a,b,c,e>, 
  step-log: step-log
): <div,logger,step-ctl<a,b>|e> c
  match step-log
    Step-log(_,_,ReasonExec(c-jsrc)) -> resume/resume-core'(st, c-jsrc())
    Step-log(_,_,ReasonResume(c-jsrc)) -> resume/resume-core'(st, c-jsrc())
    _ -> st.thunk/run-core'

// run a step without dismissing any effects
pub fun run'<a,e>(st: step<a,b,c,e>): <div,logger,step-ctl<a,b>|e> c
  match logger/log-enter(st.name)
    Just(step-log) ->
      resume/run'(st, step-log)
    _ -> st.thunk/run-core'

// run a div-step 
pub fun run-div'<a>(st: div-step<a,b,c>): <div,logger,step-ctl<a,b>> c
  st.run'

// don't like this formulation of composing stepmfns - have to use bind rather than calling the 
// stepmfn directly, because of the left-channel - but that has a consequence of requiring the
// implicit parameter ?x/json-fns... perhaps this is another evidence-point towards
// switching to a single-type return value for thunks and only using the step-ctl/fail effect for 
// short-circuiting ... alternatives which spring to mind include monad-transformers and 
// interpreting a free-monad... both of which are complicated, so the step-ctl/fail approach 
// is sounding like an easier life
pub fun compose-stepmfn(fa: stepmfn<a,b,c,d,e>, fb: stepmfn<a,b,d,x,e>, ?x/json-fns: json-fns<x>): stepmfn<a,b,c,x,e>
  fn (c)
    val sd = fa(c)
    sd.bind(fb)

// handle the step-ctl effects, using step knowledge to convert them to logger effects
fun step-ctl-handler<a,b,c,e>(
    st: step<a,b,c,e>,
    handle-fatal-error: (string)->a,
    th: thunk<c, <div,logger,step-ctl<a,b>>>
  ): <div,logger,log-ctl<a,b>> c
  with handler
    ctl fail(a)
      log-fail(a)
      resume(()) // never happens
    ctl pause(b)
      log-pause(b)
      resume(())
  th()

// handle all the effects! (apart from div)
fun handle-effects<a>(
    st: step<a,b,c,e>,
    run-name: string,
    maybe-log: maybe<logger/computation-log>,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (outcome<a,b,c>, logger/computation-log)
  with logger/log-collector(maybe-log, run-name, handle-fatal-error, st.a-json-fns, st.b-json-fns)
  with st.step-ctl-handler(handle-fatal-error)
  with h 
  st.run'

pub fun run-resume<a>(
    st: step<a,b,c,e>,
    run-name: string,
    maybe-log: maybe<logger/computation-log>,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (run-step-result<a,b,c>, logger/computation-log)
  match handle-effects(st, run-name, maybe-log, handle-fatal-error, h)
    (outcome, log) -> 
      // convert the more detailed log-outcome to a Fail/Pause/Ok step-run-result
      match outcome
        ReasonFail(a) -> (StepFail(a), log)
        ReasonPause(b) -> (StepPause(b), log)
        ReasonExec(c) -> (StepOk(c), log)
        ReasonResume(c) -> (StepOk(c), log)

// run a step, returning an either result and the computation-log
// h must handle any non-logger effects from the step - the only 
// effects allowed out are div,exn
pub fun run<a>(
    st: step<a,b,c,e>,
    run-name: string,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (run-step-result<a,b,c>, logger/computation-log)
  run-resume(st, run-name, Nothing, handle-fatal-error, h)

// a handler which does nothing but call its thunk
pub fun div-handler<a,b,c>(
    th: thunk<c,<div,logger,step-ctl<a,b>>>
  ): <div,logger,step-ctl<a,b>> c
  th()

// convenience function for running a div step
pub fun run-div(
    st: div-step<a,b,c>,
    run-name: string,
    handle-fatal-error: (string)->a
  ): div (run-step-result<a,b,c>, logger/computation-log)
  run(st, run-name, handle-fatal-error, div-handler)

pub fun resume<a>(
    st: step<a,b,c,e>,
    run-name: string,
    log: logger/computation-log,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (run-step-result<a,b,c>, logger/computation-log)
  run-resume(st, run-name, Just(log), handle-fatal-error, h)

pub fun resume-div<a>(
    st: div-step<a,b,c>,
    run-name: string,
    log: logger/computation-log,
    handle-fatal-error: (string)->a
  ): div (run-step-result<a,b,c>, logger/computation-log)
  resume(st, run-name, log, handle-fatal-error, div-handler)

// some examples

pub fun make-test-dsl-trivial(i: int): div-step<string,string,string>
  with x <- sbind("x") {i}
  step("result") {x.show}

pub fun test-dsl-trivial()
  val (_, log) = make-test-dsl-trivial(100).run-div("trivial", id)
  log.pp.println
  val logjson = log.to-json
  logjson.println
  val log' = logjson.computation-log/from-json("")
  log'.pp.println

pub fun make-test-dsl-simple(i: int): div-step<string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- sbind("z") {x + y}
  step("result") {(2 * z).show}

pub fun make-test-dsl-simple-left(i: int): div-step<string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  // TODO - a bit awkward - if we return a Left, inference can't get the Right type without help
  with z <- step("z") {fail("foo") ; 1}.bind
  step("result") {(2 * z).show}

pub fun test-dsl-simple()
  val (_, log) = make-test-dsl-simple(100).run-div("simple",id)
  log.pp.println
  val logjson = log.to-json
  logjson.println
  val log' = logjson.computation-log/from-json("")
  log'.pp.println

// resuming a computation after fixing an error
pub fun test-dsl-simple-error-fix-rerun()
  val (_, log) = make-test-dsl-simple-left(100).run-div("will-error",id)
  val logj = log.to-json
  val (_, log2) = make-test-dsl-simple(100).resume("fixed",log, id, div-handler)
  val log2j = log2.to-json

  val log' = logj.computation-log/from-json("")
  log'.pp.println

  val log2' = log2j.computation-log/from-json("")
  log2'.pp.println

pub fun make-test-dsl-simple-pause(i: int): div-step<string,string,string>
  with x <- sbind("x") {i}
  pause("pause")
  with y <- sbind("y") {10}
  with z <- sbind("z")  {x + y}
  step("result") {pause("again") ; z.show ++ ":stuff"}

val rule = string/repeat("-", 20)

// resuming a normal computation after it was paused
pub fun test-dsl-simple-pause-resume()
  val computation = make-test-dsl-simple-pause(100)
  val (paused-v, paused-log) = computation.run-div("run-1",id)
  val paused-logj = paused-log.to-json

  paused-log.pp.println
  rule.println
  paused-logj.show.println
  rule.println
  paused-v.show.println 
  rule.println

  val paused-log' = paused-logj.computation-log/from-json("")
  val (intermed-v, intermed-log) = computation.resume-div("run-2", paused-log',id)
  val intermed-logj = intermed-log.to-json

  intermed-log.pp.println
  rule.println
  intermed-logj.show.println
  rule.println
  intermed-v.show.println
  rule.println

  val intermed-log' = intermed-logj.computation-log/from-json("")
  val (final-v, final-log) = computation.resume-div("run-3", intermed-log',id)
  val final-logj = final-log.to-json

  final-log.pp.println
  rule.println
  final-logj.show.println
  rule.println
  final-v.show.println
  rule.println

pub fun make-test-dsl-simple-fail(i: int): div-step<string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- sbind("z")  {fail("boom") ; x.show ++ "/" ++ y.show}
  step("result") {z ++ ":" ++ z}

pub fun make-test-dsl-simple-fix(i: int): div-step<string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- sbind("z")  {x.show ++ "/" ++ y.show}
  step("result") {z ++ ":" ++ z}

pub fun test-dsl-simple-fail()
  val (_, log) = make-test-dsl-simple-fail(100).run-div("will-fail", id)
  log.pp.println

pub fun test-dsl-simple-resume()
  val (_, log) = make-test-dsl-simple-fail(100).run-div("simple", id)
  val (_, log2) = make-test-dsl-simple-fix(100).resume("rerun", log, id, div-handler)
  log.pp.println
  log2.pp.println

pub fun make-test-dsl-random-effect(i: int): step<string,string,int,<random>>
  with x <- sbind("x") {i}
  with y <- sbind("y") {random-int()}
  with z <- sbind("z") {x + y}
  step("result") {2 * z}

fun handle-random<a>(th: thunk<a,<random|e>>): e a
  with pseudo-random(100)
  th()

pub fun test-dsl-random-effect()
  val (_, log) = make-test-dsl-random-effect(100).run("random-effect", id, handle-random)
  log.pp.println  

fun testdsl/stuff(i: int) : step<string,string,int,<random>>
  with x <- sbind("i"){i}
  with y <- sbind("rand"){random-int()}
  step("stuff") {x + y}

pub fun make-test-dsl-more-complex-compact(i: int): step<string,string,string,<random>>
  with x <- sbind("x") {i}
  with y <- sbind("y") { if i>5 then [10,11] else [] }
  val aa' = match y
    [a, b] -> 
      with z <- sbind("z") {x + a + b}
      // TODO inference totally loses it here
      step("aa'-pair", ?a/json-fns=string/json-fns, ?b/json-fns=string/json-fns, ?c/json-fns=int/json-fns) {fail("blagh") ; 10}
    _ -> step("aa'-default") {0}
  with aa: int <- aa'.bind
  with bb: int <- testdsl/stuff(i).bind
  step("result") {aa.show ++ "/" ++ bb.show}

pub fun test-dsl-more-complext-compact()
  val (_, log) = make-test-dsl-more-complex-compact(10).run("more-complex", id, fn(th) -> pseudo-random(100, th))
  log.pp.println
  log.to-json.show.println












val listint/json-fns = Json-fns(
  fn(l: list<int>) -> JSList(l.map(int/to-json)),
  fn(j: json, path: string) -> match j
    JSList(l) -> l.map(fn (jj: json)->int/from-json(jj, "[]." ++ path))
    _ -> throw("listint/from-json: expected list")
)
