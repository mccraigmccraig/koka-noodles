module steps

import std/core/string
import std/core/undiv
import std/data/json
import std/num/random
import std/pretty/pprint
import steps/logger


///////////////////////////////////////////////////////////////////////////////
//                                    types
//
// - steps are thunks, with typed short-circuit error and pause channels 
// - success, error and pause values are serializable
// - running a step returns the result and a structured, serializable,
//   computation-log which can be used to:
//   - examine the values produced by steps
//   - resume the computation after a pause or failure
///////////////////////////////////////////////////////////////////////////////


// like an either, but with a typed pause channel
pub value type result<a,b,x>
  Fail(a: a)
  Pause(b: b)
  Success(x: x)

alias thunk<a,e> = () -> e a

// the logger and step-ctl<a,b> effects will be handled by run, while 
// any polymorphic effect must be handled by the handler given to run
// 
// NB: step-thunk doesn't strictly need the logger effect, but steps 
// compose and we end up with <logger|e> everywhere that calls run'
// which is very messy - it seems generally neater and easier to give
// the step-thunk a logger effect
pub alias step-thunk<a,b,c,x,e> = thunk<x, <logger,step-ctl<a,b,c>|e>>

// step values are a name, a thunk and json de/serializers for all the values.
pub struct step<a,b,c,x,e>
  name: string
  a-json-fns: json-fns<a>
  b-json-fns: json-fns<b>
  c-json-fns: json-fns<c>
  x-json-fns: json-fns<x>
  thunk: step-thunk<a,b,c,x,e>

// allows the logger & step-ctl effects and nothing else. the run-nofx result
// is total
pub alias nofx-step<a,b,c,x> = step<a,b,c,x,<>>

// monadic fns of step values - stepmfns are allowed the step-ctl effect,
// so can pause or fail based on previous step results, but no other effects are
// allowed. this guarantees that resumption plays nicely
pub alias stepmfn<a,b,c,x,y,e> = (x: x) -> step-ctl<a,b,c> step<a,b,c,y,e>
pub alias nofx-stepmfn<a,b,c,x,y> = stepmfn<a,b,c,x,y,<>>

// a stepmfn and all its json de/serializers
pub struct stepmfn-json<a,b,c,x,y,e>
  mfn-a-json-fns: json-fns<a>
  mfn-b-json-fns: json-fns<b>
  mfn-c-json-fns: json-fns<c>
  mfn-x-json-fns: json-fns<x>
  mfn-y-json-fns: json-fns<y>
  mfn: stepmfn<a,b,c,x,y,e>

pub alias nofx-stepmfn-json<a,b,c,x,y> = stepmfn-json<a,b,c,x,y,<>>


// ///////////////////////////////////////////////////////////////////////////////
// //                               constructors
// ///////////////////////////////////////////////////////////////////////////////


// allowing the thunk to be given as a trailing lambda - the
// desugaring puts the trailing lambda before the implicit args!
pub fun step(
  name: string, 
  thunk: step-thunk<a,b,c,x,e>, 
  ?a/json-fns: json-fns<a>,
  ?b/json-fns: json-fns<b>,
  ?c/json-fns: json-fns<c>,
  ?x/json-fns: json-fns<x>): total step<a,b,c,x,e>
  Step(name, ?a/json-fns, ?b/json-fns, ?c/json-fns, ?x/json-fns, thunk)

pub fun stepmfn-json(
    mfn: stepmfn<a,b,c,x,y,e>, 
    ?a/json-fns: json-fns<a>,
    ?b/json-fns: json-fns<b>,
    ?c/json-fns: json-fns<c>,
    ?x/json-fns: json-fns<x>,
    ?y/json-fns: json-fns<y>
  ) : total stepmfn-json<a,b,c,x,y,e>
  Stepmfn-json(
    ?a/json-fns,
    ?b/json-fns,
    ?c/json-fns,
    ?x/json-fns,
    ?y/json-fns,
    mfn
  )


// ///////////////////////////////////////////////////////////////////////////////
// //                               compose
// //
// // - create new steps from old
// // - compose fns are total
// ///////////////////////////////////////////////////////////////////////////////


// bind a stepmfn to a step
pub fun bind(
    st: step<a,b,c,x,e>, 
    mf: stepmfn<a,b,c,x,y,e>,
    ?y/json-fns: json-fns<y>
  ): total step<a,b,c,y,e>

  Step(
    "bind--" ++ st.name,
    st.a-json-fns,
    st.b-json-fns,
    st.c-json-fns,
    ?y/json-fns,
    fn () ->
      st.run'.mf.run'
  )

// bind a stepmfn-json ... would be nice to use a qualifier like stepmfn-json/bind
// but haven't yet made that work without breaking inference
pub fun bind-mfn-json(
    st: step<a,b,c,x,e>, 
    mf-json: stepmfn-json<a,b,c,x,y,e>
  ): total step<a,b,c,y,e>
  bind(st, mf-json.mfn, ?y/json-fns=mf-json.mfn-y-json-fns)

// converts an
//   step<a,b,c,d,e>
// into an
//   step<w,x,y,z,e>
// so steps can be composed with steps with 
// different fail, pause and log channels
pub fun quadmap(
    faw: (a)->w,
    fbx: (b)->x,
    fcy: (c)->y,
    fdz: (d)->z,
    s: step<a,b,c,d,<step-ctl<w,x,y>|e>>,
    ?w/json-fns: json-fns<w>,
    ?x/json-fns: json-fns<x>,
    ?y/json-fns: json-fns<y>,
    ?z/json-fns: json-fns<z>
  ): total step<w,x,y,z,e>
  Step(
    "quadmap--" ++ s.name,
    ?w/json-fns,
    ?x/json-fns,
    ?y/json-fns,
    ?z/json-fns,
    fn () ->
      with handle-step-ctl-fmap(faw, fbx, fcy)
      s.run'.fdz
  )

// map the fail and pause effects for trimap
fun handle-step-ctl-fmap(
    faw: (a)->w,
    fbx: (b)->x,
    fcy: (c)->y,
    action: () -> <logger,step-ctl<a,b,c>,step-ctl<w,x,y>|e> d
  ): <logger,step-ctl<w,x,y>|e> d
  with handler 
    ctl fail(a)
      fail(a.faw)
      resume(()) // never happens
    ctl pause(b) 
      pause(b.fbx) 
      resume(()) // happens when pause returns during resumption
    fun log(c)
      log(c.fcy)
  action()

// merging step + bind into a single function
// for a more compact DSL syntax... the trailing lambda and 
// the with lambda are desugared into the desired positions
pub fun sbind(
    name: string, 
    thunk: step-thunk<a,b,c,x,e>, 
    f: stepmfn<a,b,c,x,y,e>, 
    ?a/json-fns: json-fns<a>,
    ?b/json-fns: json-fns<b>,
    ?c/json-fns: json-fns<c>,
    ?x/json-fns: json-fns<x>,
    ?y/json-fns: json-fns<y>
  ): total step<a,b,c,y,e>
  Step(
      name, 
      ?a/json-fns, 
      ?b/json-fns, 
      ?c/json-fns, 
      ?x/json-fns,
      thunk
    ).bind(f, ?y/json-fns=?y/json-fns)

// give the output of one stepmfn to another stepmfn
pub fun pipe-stepmfn(
    fw: stepmfn<a,b,c,w,x,e>, 
    fx: stepmfn<a,b,c,x,y,e>, 
    ?a/json-fns: json-fns<a>,
    ?b/json-fns: json-fns<b>,
    ?c/json-fns: json-fns<c>,
    ?y/json-fns: json-fns<y>
  ): total stepmfn<a,b,c,w,y,e>
  fn (w)
    Step(
      "compose-stepmfn",
      ?a/json-fns,
      ?b/json-fns,
      ?c/json-fns,
      ?y/json-fns,
      fn () -> w.fw.run'.fx.run'
    )

pub fun pipe-nofx-stepmfn(
    fw: stepmfn<a,b,c,w,x,e>, 
    fx: nofx-stepmfn<a,b,c,x,y>, 
    ?a/json-fns: json-fns<a>,
    ?b/json-fns: json-fns<b>,
    ?c/json-fns: json-fns<c>,
    ?y/json-fns: json-fns<y>
  ): total stepmfn<a,b,c,w,y,e>
  fn (w)
    Step(
      "compose-nofx-stepmfn",
      ?a/json-fns,
      ?b/json-fns,
      ?c/json-fns,
      ?y/json-fns,
      fn () -> w.fw.run'.fx.run-nofx'
    )

pub fun pipe-stepmfn-json(
    fw: stepmfn-json<a,b,c,w,x,e>, 
    fx: stepmfn-json<a,b,c,x,y,e> 
  ): total stepmfn-json<a,b,c,w,y,e>
  val mfn = pipe-stepmfn(
    fw.mfn,
    fx.mfn,
    ?a/json-fns=fw.mfn-a-json-fns,
    ?b/json-fns=fw.mfn-b-json-fns,
    ?c/json-fns=fw.mfn-c-json-fns,
    ?y/json-fns=fx.mfn-y-json-fns)
  stepmfn-json(
    mfn,
    fw.mfn-a-json-fns,
    fw.mfn-b-json-fns,
    fw.mfn-c-json-fns,
    fw.mfn-x-json-fns,
    fx.mfn-y-json-fns)

pub fun pipe-nofx-stepmfn-json(
    fw: stepmfn-json<a,b,c,w,x,e>, 
    fx: nofx-stepmfn-json<a,b,c,x,y> 
  ): total stepmfn-json<a,b,c,w,y,e>
  val mfn = pipe-nofx-stepmfn(
    fw.mfn,
    fx.mfn,
    ?a/json-fns=fw.mfn-a-json-fns,
    ?b/json-fns=fw.mfn-b-json-fns,
    ?c/json-fns=fw.mfn-c-json-fns,
    ?y/json-fns=fx.mfn-y-json-fns)
  stepmfn-json(
    mfn,
    fw.mfn-a-json-fns,
    fw.mfn-b-json-fns,
    fw.mfn-c-json-fns,
    fw.mfn-x-json-fns,
    fx.mfn-y-json-fns)


// ///////////////////////////////////////////////////////////////////////////////
// //                             internal run'
// //
// // - implements logging and flow-control
// // - to be called during step composition
// ///////////////////////////////////////////////////////////////////////////////


// run a step without dismissing any effects
pub fun run'(st: step<a,b,c,x,e>): <logger,step-ctl<a,b,c>|e> x
  match logger/log-enter(st.name)
    Just(step-log) ->
      resume/run'(st, step-log)
    _ -> st.thunk/run-core'

// run a nofx-step without dismissing any effects
pub fun run-nofx'(st: nofx-step<a,b,c,x>): <logger,step-ctl<a,b,c>> x
  st.run'

fun resume/run'(
    st: step<a,b,c,x,e>, 
    step-log: step-log
  ): <logger,step-ctl<a,b,c>|e> x
  match step-log
    Step-log(_,_,ReasonExec(c-jsrc)) -> resume/resume-core'(st, c-jsrc())
    Step-log(_,_,ReasonResume(c-jsrc)) -> resume/resume-core'(st, c-jsrc())
    _ -> st.thunk/run-core'

fun resume/resume-core'(  
    st: step<a,b,c,x,e>, 
    d-json: json
  ): <logger,step-ctl<a,b,c>|e> x
  match parse-json(d-json, "", st.x-json-fns.from-json)
    Right(rr) -> 
      // we are resuming a successful step, short-circuit straight to the result
      logger/log-leave(ReasonResume({d-json}))
      rr
    Left(_err) -> st.thunk/run-core'

fun thunk/run-core'(
    st: step<a,b,c,x,e>
  ): <logger,step-ctl<a,b,c>|e> x
  val r = st.thunk/run''
  logger/log-leave(ReasonExec({st.x-json-fns.to-json()(r)}))
  r

// call a thunk
pub fun thunk/run''(st: step<a,b,c,x,e>): <logger,step-ctl<a,b,c>|e> x
  st.thunk()()


// ///////////////////////////////////////////////////////////////////////////////
// //                               run
// //
// // - public run interface
// // - external - handles all step effects
// ///////////////////////////////////////////////////////////////////////////////

// run a step, returning an either result and the computation-log.
// h must handle any non-logger effects from the step - the only 
// effects allowed out are div,logger,step-ctl
pub fun run<a>(
    st: step<a,b,c,x,e>,
    run-name: string,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,x,e>) -> <div,logger,step-ctl<a,b,c>> x
  ): div (result<a,b,x>, logger/computation-log)
  run-resume(st, run-name, Nothing, handle-fatal-error, h)

// run a nofx-step - total
pub fun run-nofx(
    st: nofx-step<a,b,c,x>,
    run-name: string,
    handle-fatal-error: (string)->a
  ): total (result<a,b,x>, logger/computation-log)
  with pretend-no-div
  run(st, run-name, handle-fatal-error, nofx-handler)

// resume a step
//
// similar to run, but the log will be followed and, for so long as it 
// matches the computation structure, steps will be "resumed" rather than executed - 
// i.e. the logged serialied result value will be returned instead of calling the step thunk. 
// as soon as the log deviates from the computation, or a failure or pause was logged,
// then step execution will pick up. even during resumption, bind stepmfns are always
// executed - but they are only allowed step-ctl effects
pub fun resume(
    st: step<a,b,c,x,e>,
    run-name: string,
    log: logger/computation-log,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,x,e>) -> <div,logger,step-ctl<a,b,c>> x
  ): div (result<a,b,x>, logger/computation-log)
  run-resume(st, run-name, Just(log), handle-fatal-error, h)

// resume a nofx-step - total
pub fun resume-nofx(
    st: nofx-step<a,b,c,x>,
    run-name: string,
    log: logger/computation-log,
    handle-fatal-error: (string)->a
  ): total (result<a,b,x>, logger/computation-log)
  with pretend-no-div
  resume(st, run-name, log, handle-fatal-error, nofx-handler)

// run or resume, depending on whether a log is supplied
pub fun run-resume(
    st: step<a,b,c,x,e>,
    run-name: string,
    maybe-log: maybe<logger/computation-log>,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,x,e>) -> <div,logger,step-ctl<a,b,c>> x
  ): div (result<a,b,x>, logger/computation-log)
  match handle-effects(st, run-name, maybe-log, handle-fatal-error, h)
    (outcome, log) -> 
      // convert the more detailed log-outcome to a Fail/Pause/Success result
      match outcome
        ReasonFail(a) -> (Fail(a), log)
        ReasonPause(b) -> (Pause(b), log)
        ReasonExec(x) -> (Success(x), log)
        ReasonResume(x) -> (Success(x), log)

// a handler which does nothing but call its thunk
pub fun nofx-handler(
    th: thunk<x,<logger,step-ctl<a,b,c>>>
  ): <logger,step-ctl<a,b,c>> x
  th()

// handle all the effects! (apart from div)
fun handle-effects(
    st: step<a,b,c,x,e>,
    run-name: string,
    maybe-log: maybe<logger/computation-log>,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,x,e>) -> <div,logger,step-ctl<a,b,c>> x
  ): div (outcome<a,b,x>, logger/computation-log)
  with logger/log-collector(maybe-log, run-name, handle-fatal-error, st.a-json-fns, st.b-json-fns, st.c-json-fns)
  with h
  st.run'


// ///////////////////////////////////////////////////////////////////////////////
// //                               examples
// ///////////////////////////////////////////////////////////////////////////////


pub fun make-example-dsl-trivial(i: int): nofx-step<string,string,string,string>
  with x <- sbind("x") {i}
  step("result") {x.show}

pub fun example-dsl-trivial()
  val (_, log) = make-example-dsl-trivial(100).run-nofx("trivial", id)
  log.pretty.show.println
  val logjson = log.to-json
  logjson.pretty.show.println
  val log' = logjson.computation-log/from-json("")
  log'.pretty.show.println

pub fun make-example-dsl-simple(i: int): nofx-step<string,string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- sbind("z") {x + y}
  step("result") {(2 * z).show}

pub fun make-example-dsl-simple-left(i: int): nofx-step<string,string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- step("z") {fail("foo") ; 1}.bind
  step("result") {(2 * z).show}

pub fun example-dsl-simple()
  val (_, log) = make-example-dsl-simple(100).run-nofx("simple",id)
  log.pretty.show.println
  val logjson = log.to-json
  logjson.println
  val log' = logjson.computation-log/from-json("")
  log'.pretty.show.println

// resuming a computation after fixing an error
pub fun example-dsl-simple-error-fix-rerun()
  val (_, log) = make-example-dsl-simple-left(100).run-nofx("will-error",id)
  val logj = log.to-json
  val (_, log2) = make-example-dsl-simple(100).resume("fixed",log, id, nofx-handler)
  val log2j = log2.to-json

  val log' = logj.computation-log/from-json("")
  log'.pretty.show.println

  val log2' = log2j.computation-log/from-json("")
  log2'.pretty.show.println

pub fun make-example-dsl-simple-pause(i: int): nofx-step<string,string,string,string>
  with x <- sbind("x") {i}
  pause("pause")
  with y <- sbind("y") {10}
  with z <- sbind("z")  {x + y}
  step("result") {pause("again") ; z.show ++ ":stuff"}

val rule = string/repeat("-", 20)

// resuming a normal computation after it was paused
// pub fun example-dsl-simple-pause-resume()
//   val computation = make-example-dsl-simple-pause(100)
//   val (paused-v, paused-log) = computation.run-nofx("run-1",id)
//   val paused-logj = paused-log.to-json

//   paused-log.pretty.show.println
//   rule.println
//   paused-logj.show.println
//   rule.println
//   paused-v.show.println 
//   rule.println

//   val paused-log' = paused-logj.computation-log/from-json("")
//   val (intermed-v, intermed-log) = computation.resume-nofx("run-2", paused-log',id)
//   val intermed-logj = intermed-log.to-json

//   intermed-log.pretty.show.println
//   rule.println
//   intermed-logj.show.println
//   rule.println
//   intermed-v.show.println
//   rule.println

//   val intermed-log' = intermed-logj.computation-log/from-json("")
//   val (final-v, final-log) = computation.resume-nofx("run-3", intermed-log',id)
//   val final-logj = final-log.to-json

//   final-log.pretty.show.println
//   rule.println
//   final-logj.show.println
//   rule.println
//   final-v.show.println
//   rule.println

pub fun make-example-dsl-simple-fail(i: int): nofx-step<string,string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- sbind("z")  {fail("boom") ; x.show ++ "/" ++ y.show}
  step("result") {z ++ ":" ++ z}

pub fun make-example-dsl-simple-fix(i: int): nofx-step<string,string,string,string>
  with x <- sbind("x") {i}
  with y <- sbind("y") {10}
  with z <- sbind("z")  {x.show ++ "/" ++ y.show}
  step("result") {z ++ ":" ++ z}

pub fun example-dsl-simple-fail()
  val (_, log) = make-example-dsl-simple-fail(100).run-nofx("will-fail", id)
  log.pretty.show.println

pub fun example-dsl-simple-resume()
  val (_, log) = make-example-dsl-simple-fail(100).run-nofx("simple", id)
  val (_, log2) = make-example-dsl-simple-fix(100).resume("rerun", log, id, nofx-handler)
  log.pretty.show.println
  log2.pretty.show.println

pub fun make-example-dsl-random-effect(i: int): step<string,string,string,int,<random>>
  with x <- sbind("x") {i}
  with y <- sbind("y") {random-int()}
  with z <- sbind("z") {x + y}
  step("result") {2 * z}

fun handle-random<a>(th: thunk<a,<random|e>>): e a
  with pseudo-random(100)
  th()

pub fun example-dsl-random-effect()
  val (_, log) = make-example-dsl-random-effect(100).run("random-effect", id, handle-random)
  log.pretty.show.println  

fun testdsl/stuff(i: int) : step<string,string,string,int,<random>>
  with x <- sbind("i"){i}
  with y <- sbind("rand"){random-int()}
  step("stuff") {x + y}

// pub fun make-example-dsl-more-complex-compact(i: int): step<string,string,string,string,<random>>
//   with x <- sbind("x") {i}
//   with y <- sbind("y") { if i>5 then [10,11] else [] }
//   val aa' = match y
//     [a, b] -> 
//       with z <- sbind("z") {x + a + b}
//       // TODO inference doesn't do a great job here...
//       step("aa'-pair", ?a/json-fns=string/json-fns, ?b/json-fns=string/json-fns, ?c/json-fns=string/json-fns) {fail("blagh") ; 10}
//     _ -> step("aa'-default") {0}
//   with aa: int <- aa'.bind
//   with bb: int <- testdsl/stuff(i).bind
//   step("result") {aa.show ++ "/" ++ bb.show}

// pub fun example-dsl-more-complext-compact()
//   val (_, log) = make-example-dsl-more-complex-compact(10).run("more-complex", id, fn(th) -> pseudo-random(100, th))
//   log.pretty.show.println
//   log.to-json.show.println



pub fun make-example-dsl-ints(i: int): step<string,int,string,int,e>
  with x <- sbind("x") {i}
  pause(x)
  with y <- sbind("y") {10}
  with z <- sbind("z") {x + y}
  pause(z)
  // fail("foo")
  step("result") {(2 * z)}

pub fun example-quadmap()
  val ints-step = make-example-dsl-ints(100)

  // the int pause is converted to a string... resuming the computation shows that
  // the pause() effect resumption works through the trifmap so trifmap can be used
  // to compose steps with different error and pause types
  val strings-step = quadmap(string/to-upper, int/show, id, int/show, ints-step)

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


// ///////////////////////////////////////////////////////////////////////////////
// //                                    show
// ///////////////////////////////////////////////////////////////////////////////


pub fun result/show(
    r: result<a,b,x>, 
    ?a/show: (a)->string, 
    ?b/show: (b)->string, 
    ?x/show: (x)->string
  ): string
  match r
    Fail(a) -> "Fail: " ++ a.?a/show
    Pause(b) -> "Pause: " ++ b.?b/show
    Success(x) -> "Success: " ++ x.?x/show

pub fun step/show(a: step<a,b,c,x,e>): string
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
