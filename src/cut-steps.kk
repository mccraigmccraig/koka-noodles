module cut-steps

import std/core/string
import std/data/json
import std/num/random
import std/pretty/pprint
import steps/logger


// like an either, but with a typed pause channel
pub value type result<a,b,c>
  Fail(a: a)
  Pause(b: b)
  Success(c: c)

pub fun result/show<a,b,c>(r: result<a,b,c>, ?a/show: (a)->string, ?b/show: (b)->string, ?c/show: (c)->string): string
  match r
    Fail(a) -> "Fail: " ++ a.?a/show
    Pause(b) -> "Pause: " ++ b.?b/show
    Success(c) -> "Success: " ++ c.?c/show

// consumer control effect - immediately pause or fail a computation.
// 
// There's no short-circuit returning of computation values - because 
// that would mean parameterising step-ctl on the value type, and 
// so would require rebinding handlers at every bind, and need
// (c)->d functions we don't have 
pub effect step-ctl<a,b>
  // Immediately fail the computation with a value - equivalent
  // to returning a Left(a) from a thunk
  ctl fail(a: a): ()
  // Immediately pause the computation with a value
  ctl pause(b: b): ()

effect tmp-ctl<a,b>
  ctl tmp-fail(a: a): ()
  ctl tmp-pause(b: b): ()

pub alias thunk<a,e> = () -> e a

// the logger and step-ctl<a,b> effects are handled by run, while 
// any polymorphic effect is exepcted to be handled by the
// handler given to run
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

fun handle-step-ctl-fmap<a,b,c,x,y,e>(
    fax: (a)->x,
    fby: (b)->y,
    action: () -> <div,logger,step-ctl<a,b>> c
  ): <div,logger,step-ctl<x,y>> c
  with handler 
    ctl fail(a)
      fail(a.fax)
      resume(()) // never happens
    ctl pause(b) 
      pause(b.fby) 
      resume(()) 
  action()

// converts an
//   step<a,b,c,e>
// into an
//   step<x,y,z,e>
// so steps can be composed into steps with 
// different fail and pause channels
//
// TODO can only get this to check with div-steps - 
// the extension effect seems to break things. next up - 
// strip stuff out of steps.kk and move it towards the 
// simple example in rebind-effect.kk - 'cos that seems
// to check just fine
pub fun trifmap<a,b,c,x,y,z>(
    fax: (a)->x,
    fby: (b)->y,
    fcz: (c)->z,
    s: div-step<a,b,c>,
    ?x/json-fns: json-fns<x>,
    ?y/json-fns: json-fns<y>,
    ?z/json-fns: json-fns<z>
  ): total div-step<x,y,z>
  Step(
    "trifmap--" ++ s.name,
    ?x/json-fns,
    ?y/json-fns,
    ?z/json-fns,
    fn () ->
      with handle-step-ctl-fmap(fax, fby)
      s.run'.fcz
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

// convert the step-ctl effects to log-ctl
fun step-ctl-handler<a,b,c>(
    action: thunk<c, <div,logger,step-ctl<a,b>>>
  ): <div,logger,log-ctl<a,b>> c
  with handler
    ctl fail(a)
      log-fail(a)
      resume(()) // never happens
    ctl pause(b)
      log-pause(b)
      resume(())
  action()

// handle all the effects! (apart from div)
fun handle-effects<a>(
    st: step<a,b,c,e>,
    run-name: string,
    maybe-log: maybe<logger/computation-log>,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (outcome<a,b,c>, logger/computation-log)
  with logger/log-collector(maybe-log, run-name, handle-fatal-error, st.a-json-fns, st.b-json-fns)
  with step-ctl-handler
  with h 
  st.run'

pub fun run-resume<a>(
    st: step<a,b,c,e>,
    run-name: string,
    maybe-log: maybe<logger/computation-log>,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (result<a,b,c>, logger/computation-log)
  match handle-effects(st, run-name, maybe-log, handle-fatal-error, h)
    (outcome, log) -> 
      // convert the more detailed log-outcome to a Fail/Pause/Ok result
      match outcome
        ReasonFail(a) -> (Fail(a), log)
        ReasonPause(b) -> (Pause(b), log)
        ReasonExec(c) -> (Success(c), log)
        ReasonResume(c) -> (Success(c), log)

// run a step, returning an either result and the computation-log
// h must handle any non-logger effects from the step - the only 
// effects allowed out are div,exn
pub fun run<a>(
    st: step<a,b,c,e>,
    run-name: string,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (result<a,b,c>, logger/computation-log)
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
  ): div (result<a,b,c>, logger/computation-log)
  run(st, run-name, handle-fatal-error, div-handler)

pub fun resume<a>(
    st: step<a,b,c,e>,
    run-name: string,
    log: logger/computation-log,
    handle-fatal-error: (string)->a,
    h: (step-thunk<a,b,c,e>) -> <div,logger,step-ctl<a,b>> c
  ): div (result<a,b,c>, logger/computation-log)
  run-resume(st, run-name, Just(log), handle-fatal-error, h)

pub fun resume-div<a>(
    st: div-step<a,b,c>,
    run-name: string,
    log: logger/computation-log,
    handle-fatal-error: (string)->a
  ): div (result<a,b,c>, logger/computation-log)
  resume(st, run-name, log, handle-fatal-error, div-handler)

// some examples

val rule = "------"

pub fun make-test-dsl-ints(i: int): step<string,int,int,e>
  with x <- sbind("x") {i}
  pause(x)
  with y <- sbind("y") {10}
  with z <- sbind("z") {x + y}
  pause(z)
  step("result") {(2 * z)}

pub fun test-trifmap()
  val ints-step = make-test-dsl-ints(100)
  val (int-val, log) = ints-step.run-div("ints", id)
  log.pretty.show.println
  rule.println
  int-val.show.println
  rule.println

  // the int pause is converted to a string... resuming the computation shows that
  // the pause() effect resumption works through the trifmap so trifmap can be used
  // to compose steps with different error and pause types
  val strings-step = trifmap(id, int/show, int/show, ints-step)

  val (strings-val1, strings-log1) = strings-step.run-div("strings1", string/to-upper)
  strings-log1.pretty.show.println
  rule.println
  strings-val1.show.println
  rule.println

  val (strings-val2, strings-log2) = strings-step.resume-div("strings2", strings-log1, string/to-lower)
  strings-log2.pretty.show.println
  rule.println
  strings-val2.show.println
  rule.println

  val (strings-val3, strings-log3) = strings-step.resume-div("strings3", strings-log2, string/to-upper)
  strings-log3.pretty.show.println
  rule.println
  strings-val3.show.println
  rule.println
