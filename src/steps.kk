module steps

import std/core/string
import std/core/undiv
import std/data/json
import std/num/random
import std/pretty/pprint
import steps/logger


///////////////////////////////////////////////////////////////////////////////
//                                    types
///////////////////////////////////////////////////////////////////////////////


// like an either, but with a typed pause channel
pub value type result<a,b,c>
  Fail(a: a)
  Pause(b: b)
  Success(c: c)

alias thunk<a,e> = () -> e a

// the logger and step-ctl<a,b> effects will be handled by run, while 
// any polymorphic effect must be handled by the handler given to run
// 
// NB: step-thunk doesn't strictly need the logger effect, but steps 
// compose and we end up with <logger|e> everywhere that calls run'
// which is very messy - it seems generally neater and easier to give
// the step-thunk a logger effect
pub alias step-thunk<a,b,c,e> = thunk<c, <logger,step-ctl<a,b>|e>>

// step values are a name, a thunk and json de/serializers for all the values.
pub struct step<a,b,c,e>
  name: string
  a-json-fns: json-fns<a>
  b-json-fns: json-fns<b>
  c-json-fns: json-fns<c>
  thunk: step-thunk<a,b,c,e>

// allows the logger & step-ctl effects and nothing else. the run-nofx result
// is total
pub alias nofx-step<a,b,c> = step<a,b,c,<>>

// monadic fns of step values - stepmfns are allowed the step-ctl effect,
// so can pause or fail based on previous step results, but no other effects are
// allowed. this guarantees that resumption plays nicely
pub alias stepmfn<a,b,c,d,e> = (c: c) -> step-ctl<a,b> step<a,b,d,e>
pub alias nofx-stepmfn<a,b,c,d> = stepmfn<a,b,c,d,<>>

// a stepmfn and all its json de/serializers
pub struct stepmfn-json<a,b,c,d,e>
  mfn-a-json-fns: json-fns<a>
  mfn-b-json-fns: json-fns<b>
  mfn-c-json-fns: json-fns<c>
  mfn-d-json-fns: json-fns<d>
  mfn: stepmfn<a,b,c,d,e>

pub alias nofx-stepmfn-json<a,b,c,d> = stepmfn-json<a,b,c,d,<>>


///////////////////////////////////////////////////////////////////////////////
//                               constructors
///////////////////////////////////////////////////////////////////////////////


// allowing the thunk to be given as a trailing lambda - the
// desugaring puts the trailing lambda before the implicit args!
pub fun step<a,b,c,e>(
  name: string, 
  thunk: step-thunk<a,b,c,e>, 
  ?a/json-fns: json-fns<a>,
  ?b/json-fns: json-fns<b>,
  ?c/json-fns: json-fns<c>): total step<a,b,c,e>
  Step(name, ?a/json-fns, ?b/json-fns, ?c/json-fns, thunk)

pub fun stepmfn-json(
    mfn: stepmfn<a,b,c,d,e>, 
    ?a/json-fns: json-fns<a>,
    ?b/json-fns: json-fns<b>,
    ?c/json-fns: json-fns<c>,
    ?d/json-fns: json-fns<d>
  ) : total stepmfn-json<a,b,c,d,e>
  Stepmfn-json(
    ?a/json-fns,
    ?b/json-fns,
    ?c/json-fns,
    ?d/json-fns,
    mfn
  )


///////////////////////////////////////////////////////////////////////////////
//                               compose
//
// - create new steps from old
// - compose fns are total
///////////////////////////////////////////////////////////////////////////////


// bind a stepmfn to a step
pub fun bind<a,b,c,d,e>(
    st: step<a,b,c,e>, 
    mf: stepmfn<a,b,c,d,e>,
    ?d/json-fns: json-fns<d>
  ): total step<a,b,d,e>

  Step(
    "bind--" ++ st.name,
    st.a-json-fns,
    st.b-json-fns,
    ?d/json-fns,
    fn () ->
      st.run'.mf.run'
  )

// bind a stepmfn-json ... would be nice to use a qualifier like stepmfn-json/bind
// but haven't yet made that work without breaking inference
pub fun bind-mfn-json<a,b,c,d,e>(
    st: step<a,b,c,e>, 
    mf-json: stepmfn-json<a,b,c,d,e>,
    ?d/json-fns: json-fns<d>
  ): total step<a,b,d,e>
  bind(st, mf-json.mfn)

// converts an
//   step<a,b,c,e>
// into an
//   step<x,y,z,e>
// so steps can be composed with steps with 
// different fail and pause channels
pub fun trimap<a,b,c,x,y,z,e>(
    fax: (a)->x,
    fby: (b)->y,
    fcz: (c)->z,
    s: step<a,b,c,<step-ctl<x,y>|e>>,
    ?x/json-fns: json-fns<x>,
    ?y/json-fns: json-fns<y>,
    ?z/json-fns: json-fns<z>
  ): total step<x,y,z,e>
  Step(
    "trifmap--" ++ s.name,
    ?x/json-fns,
    ?y/json-fns,
    ?z/json-fns,
    fn () ->
      with handle-step-ctl-fmap(fax, fby)
      s.run'.fcz
  )

// map the fail and pause effects for trimap
fun handle-step-ctl-fmap<a,b,c,x,y,e>(
    fax: (a)->x,
    fby: (b)->y,
    action: () -> <logger,step-ctl<a,b>,step-ctl<x,y>|e> c
  ): <logger,step-ctl<x,y>|e> c
  with handler 
    ctl fail(a)
      fail(a.fax)
      resume(()) // never happens
    ctl pause(b) 
      pause(b.fby) 
      resume(()) // happens when pause returns during resumption
  action()

// merging step + bind into a single function
// for a more compact DSL syntax... the trailing lambda and 
// the with lambda are desugared into the desired positions
pub fun sbind<a,b,c,d,e>(
    name: string, 
    thunk: step-thunk<a,b,c,e>, 
    f: stepmfn<a,b,c,d,e>, 
    ?a/json-fns: json-fns<a>,
    ?b/json-fns: json-fns<b>,
    ?c/json-fns: json-fns<c>,
    ?d/json-fns: json-fns<d>
  ): total step<a,b,d,e>
  Step(
      name, 
      ?a/json-fns, 
      ?b/json-fns, 
      ?c/json-fns, 
      thunk
    ).bind(f, ?d/json-fns=?d/json-fns)

// give the output of one stepmfn to another stepmfn
pub fun pipe-stepmfn<a,b,w,x,y,e>(
    fw: stepmfn<a,b,w,x,e>, 
    fx: stepmfn<a,b,x,y,e>, 
    ?a/json-fns: json-fns<a>,
    ?b/json-fns: json-fns<b>,
    ?y/json-fns: json-fns<y>
  ): total stepmfn<a,b,w,y,e>
  fn (w)
    Step(
      "compose-stepmfn",
      ?a/json-fns,
      ?b/json-fns,
      ?y/json-fns,
      fn () -> w.fw.run'.fx.run'
    )

pub fun pipe-nofx-stepmfn<a,b,w,x,y,e>(
    fw: stepmfn<a,b,w,x,e>, 
    fx: nofx-stepmfn<a,b,x,y>, 
    ?a/json-fns: json-fns<a>,
    ?b/json-fns: json-fns<b>,
    ?y/json-fns: json-fns<y>
  ): total stepmfn<a,b,w,y,e>
  fn (w)
    Step(
      "compose-nofx-stepmfn",
      ?a/json-fns,
      ?b/json-fns,
      ?y/json-fns,
      fn () -> w.fw.run'.fx.run-nofx'
    )

pub fun pipe-stepmfn-json<a,b,w,x,y,e>(
    fw: stepmfn-json<a,b,w,x,e>, 
    fx: stepmfn-json<a,b,x,y,e> 
  ): total stepmfn-json<a,b,w,y,e>
  val mfn = pipe-stepmfn(
    fw.mfn,
    fx.mfn,
    ?a/json-fns=fw.mfn-a-json-fns,
    ?b/json-fns=fw.mfn-b-json-fns,
    ?y/json-fns=fx.mfn-d-json-fns)
  stepmfn-json(
    mfn,
    fw.mfn-a-json-fns,
    fw.mfn-b-json-fns,
    fw.mfn-c-json-fns,
    fx.mfn-d-json-fns)

pub fun pipe-nofx-stepmfn-json<a,b,w,x,y,e>(
    fw: stepmfn-json<a,b,w,x,e>, 
    fx: nofx-stepmfn-json<a,b,x,y> 
  ): total stepmfn-json<a,b,w,y,e>
  val mfn = pipe-nofx-stepmfn(
    fw.mfn,
    fx.mfn,
    ?a/json-fns=fw.mfn-a-json-fns,
    ?b/json-fns=fw.mfn-b-json-fns,
    ?y/json-fns=fx.mfn-d-json-fns)
  stepmfn-json(
    mfn,
    fw.mfn-a-json-fns,
    fw.mfn-b-json-fns,
    fw.mfn-c-json-fns,
    fx.mfn-d-json-fns)


///////////////////////////////////////////////////////////////////////////////
//                             internal run'
//
// - implements logging and flow-control
// - to be called during step composition
///////////////////////////////////////////////////////////////////////////////


// run a step without dismissing any effects
pub fun run'<a,e>(st: step<a,b,c,e>): <logger,step-ctl<a,b>|e> c
  match logger/log-enter(st.name)
    Just(step-log) ->
      resume/run'(st, step-log)
    _ -> st.thunk/run-core'

// run a nofx-step without dismissing any effects
pub fun run-nofx'<a>(st: nofx-step<a,b,c>): <logger,step-ctl<a,b>> c
  st.run'

fun resume/run'<a,b,c,e>(
    st: step<a,b,c,e>, 
    step-log: step-log
  ): <logger,step-ctl<a,b>|e> c
  match step-log
    Step-log(_,_,ReasonExec(c-jsrc)) -> resume/resume-core'(st, c-jsrc())
    Step-log(_,_,ReasonResume(c-jsrc)) -> resume/resume-core'(st, c-jsrc())
    _ -> st.thunk/run-core'

fun resume/resume-core'<a,b,c,e>(  
    st: step<a,b,c,e>, 
    c-json: json
  ): <logger,step-ctl<a,b>|e> c
  match parse-json(c-json, "", st.c-json-fns.from-json)
    Right(rr) -> 
      // we are resuming a successful step, short-circuit straight to the result
      logger/log-leave(ReasonResume({c-json}))
      rr
    Left(_err) -> st.thunk/run-core'

fun thunk/run-core'<a,b,c,e>(
    st: step<a,b,c,e>
  ): <logger,step-ctl<a,b>|e> c
  val r = st.thunk/run''
  logger/log-leave(ReasonExec({st.c-json-fns.to-json()(r)}))
  r

// call a thunk
pub fun thunk/run''<a,b,c,e>(st: step<a,b,c,e>): <logger,step-ctl<a,b>|e> c
  st.thunk()()


///////////////////////////////////////////////////////////////////////////////
//                               run
//
// - public run interface
// - external - handles all step effects
///////////////////////////////////////////////////////////////////////////////

// run a step, returning an either result and the computation-log.
// h must handle any non-logger effects from the step - the only 
// effects allowed out are div,logger,step-ctl
pub fun run<a>(
    st: step<a,b,c,e>,
    run-name: string,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (result<a,b,c>, logger/computation-log)
  run-resume(st, run-name, Nothing, handle-fatal-error, h)

// convenience function for running a div step
pub fun run-nofx(
    st: nofx-step<a,b,c>,
    run-name: string,
    handle-fatal-error: (string)->a
  ): total (result<a,b,c>, logger/computation-log)
  with pretend-no-div
  run(st, run-name, handle-fatal-error, nofx-handler)

pub fun resume<a>(
    st: step<a,b,c,e>,
    run-name: string,
    log: logger/computation-log,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (result<a,b,c>, logger/computation-log)
  run-resume(st, run-name, Just(log), handle-fatal-error, h)

pub fun resume-nofx<a>(
    st: nofx-step<a,b,c>,
    run-name: string,
    log: logger/computation-log,
    handle-fatal-error: (string)->a
  ): total (result<a,b,c>, logger/computation-log)
  with pretend-no-div
  resume(st, run-name, log, handle-fatal-error, nofx-handler)

pub fun run-resume<a>(
    st: step<a,b,c,e>,
    run-name: string,
    maybe-log: maybe<logger/computation-log>,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (result<a,b,c>, logger/computation-log)
  match handle-effects(st, run-name, maybe-log, handle-fatal-error, h)
    (outcome, log) -> 
      // convert the more detailed log-outcome to a Fail/Pause/Success result
      match outcome
        ReasonFail(a) -> (Fail(a), log)
        ReasonPause(b) -> (Pause(b), log)
        ReasonExec(c) -> (Success(c), log)
        ReasonResume(c) -> (Success(c), log)

// a handler which does nothing but call its thunk
pub fun nofx-handler<a,b,c>(
    th: thunk<c,<logger,step-ctl<a,b>>>
  ): <logger,step-ctl<a,b>> c
  th()

// handle all the effects! (apart from div)
fun handle-effects<a,b,c,e>(
    st: step<a,b,c,e>,
    run-name: string,
    maybe-log: maybe<logger/computation-log>,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (outcome<a,b,c>, logger/computation-log)
  with logger/log-collector(maybe-log, run-name, handle-fatal-error, st.a-json-fns, st.b-json-fns)
  with h
  st.run'


///////////////////////////////////////////////////////////////////////////////
//                               examples
///////////////////////////////////////////////////////////////////////////////


pub fun make-test-dsl-trivial(i: int): nofx-step<string,string,string>
  with x <- sbind("x") {i}
  step("result") {x.show}

pub fun test-dsl-trivial()
  val (_, log) = make-test-dsl-trivial(100).run-nofx("trivial", id)
  log.pretty.show.println
  val logjson = log.to-json
  logjson.pretty.show.println
  val log' = logjson.computation-log/from-json("")
  log'.pretty.show.println

pub fun make-test-dsl-simple(i: int): nofx-step<string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- sbind("z") {x + y}
  step("result") {(2 * z).show}

pub fun make-test-dsl-simple-left(i: int): nofx-step<string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- step("z") {fail("foo") ; 1}.bind
  step("result") {(2 * z).show}

pub fun test-dsl-simple()
  val (_, log) = make-test-dsl-simple(100).run-nofx("simple",id)
  log.pretty.show.println
  val logjson = log.to-json
  logjson.println
  val log' = logjson.computation-log/from-json("")
  log'.pretty.show.println

// resuming a computation after fixing an error
pub fun test-dsl-simple-error-fix-rerun()
  val (_, log) = make-test-dsl-simple-left(100).run-nofx("will-error",id)
  val logj = log.to-json
  val (_, log2) = make-test-dsl-simple(100).resume("fixed",log, id, nofx-handler)
  val log2j = log2.to-json

  val log' = logj.computation-log/from-json("")
  log'.pretty.show.println

  val log2' = log2j.computation-log/from-json("")
  log2'.pretty.show.println

pub fun make-test-dsl-simple-pause(i: int): nofx-step<string,string,string>
  with x <- sbind("x") {i}
  pause("pause")
  with y <- sbind("y") {10}
  with z <- sbind("z")  {x + y}
  step("result") {pause("again") ; z.show ++ ":stuff"}

val rule = string/repeat("-", 20)

// resuming a normal computation after it was paused
pub fun test-dsl-simple-pause-resume()
  val computation = make-test-dsl-simple-pause(100)
  val (paused-v, paused-log) = computation.run-nofx("run-1",id)
  val paused-logj = paused-log.to-json

  paused-log.pretty.show.println
  rule.println
  paused-logj.show.println
  rule.println
  paused-v.show.println 
  rule.println

  val paused-log' = paused-logj.computation-log/from-json("")
  val (intermed-v, intermed-log) = computation.resume-nofx("run-2", paused-log',id)
  val intermed-logj = intermed-log.to-json

  intermed-log.pretty.show.println
  rule.println
  intermed-logj.show.println
  rule.println
  intermed-v.show.println
  rule.println

  val intermed-log' = intermed-logj.computation-log/from-json("")
  val (final-v, final-log) = computation.resume-nofx("run-3", intermed-log',id)
  val final-logj = final-log.to-json

  final-log.pretty.show.println
  rule.println
  final-logj.show.println
  rule.println
  final-v.show.println
  rule.println

pub fun make-test-dsl-simple-fail(i: int): nofx-step<string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- sbind("z")  {fail("boom") ; x.show ++ "/" ++ y.show}
  step("result") {z ++ ":" ++ z}

pub fun make-test-dsl-simple-fix(i: int): nofx-step<string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- sbind("z")  {x.show ++ "/" ++ y.show}
  step("result") {z ++ ":" ++ z}

pub fun test-dsl-simple-fail()
  val (_, log) = make-test-dsl-simple-fail(100).run-nofx("will-fail", id)
  log.pretty.show.println

pub fun test-dsl-simple-resume()
  val (_, log) = make-test-dsl-simple-fail(100).run-nofx("simple", id)
  val (_, log2) = make-test-dsl-simple-fix(100).resume("rerun", log, id, nofx-handler)
  log.pretty.show.println
  log2.pretty.show.println

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
  log.pretty.show.println  

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
      // TODO inference doesn't do a great job here...
      step("aa'-pair", ?a/json-fns=string/json-fns, ?b/json-fns=string/json-fns) {fail("blagh") ; 10}
    _ -> step("aa'-default") {0}
  with aa: int <- aa'.bind
  with bb: int <- testdsl/stuff(i).bind
  step("result") {aa.show ++ "/" ++ bb.show}

pub fun test-dsl-more-complext-compact()
  val (_, log) = make-test-dsl-more-complex-compact(10).run("more-complex", id, fn(th) -> pseudo-random(100, th))
  log.pretty.show.println
  log.to-json.show.println



pub fun make-test-dsl-ints(i: int): step<string,int,int,e>
  with x <- sbind("x") {i}
  pause(x)
  with y <- sbind("y") {10}
  with z <- sbind("z") {x + y}
  pause(z)
  // fail("foo")
  step("result") {(2 * z)}

pub fun test-trifmap()
  val ints-step = make-test-dsl-ints(100)

  // the int pause is converted to a string... resuming the computation shows that
  // the pause() effect resumption works through the trifmap so trifmap can be used
  // to compose steps with different error and pause types
  val strings-step = trimap(string/to-upper, int/show, int/show, ints-step)

  val (strings-val1, strings-log1) = strings-step.run-nofx("strings1", string/to-upper)
  strings-log1.pretty.show.println
  rule.println
  strings-val1.show.println
  rule.println

  val (strings-val2, strings-log2) = strings-step.resume-nofx("strings2", strings-log1, string/to-lower)
  strings-log2.pretty.show.println
  rule.println
  strings-val2.show.println
  rule.println

  val (strings-val3, strings-log3) = strings-step.resume-nofx("strings3", strings-log2, string/to-upper)
  strings-log3.pretty.show.println
  rule.println
  strings-val3.show.println
  rule.println


///////////////////////////////////////////////////////////////////////////////
//                                    show
///////////////////////////////////////////////////////////////////////////////


pub fun result/show<a,b,c>(
    r: result<a,b,c>, 
    ?a/show: (a)->string, 
    ?b/show: (b)->string, 
    ?c/show: (c)->string
  ): string
  match r
    Fail(a) -> "Fail: " ++ a.?a/show
    Pause(b) -> "Pause: " ++ b.?b/show
    Success(c) -> "Success: " ++ c.?c/show

pub fun step/show(a: step<a,b,c,e>): string
  a.name ++ ": <unevaluated>"


///////////////////////////////////////////////////////////////////////////////
//                                  serialize
///////////////////////////////////////////////////////////////////////////////


val listint/json-fns = Json-fns(
  fn(l: list<int>) -> JSList(l.map(int/to-json)),
  fn(j: json, path: string) -> match j
    JSList(l) -> l.map(fn (jj: json)->int/from-json(jj, "[]." ++ path))
    _ -> throw("listint/from-json: expected list")
)
