module fcore

import std/core/list
import logger
import chain
import chain_dsl

// computations have a potentially effectful input-chain, a pure step, and
// a potentially effectful output-chain. the input-chain gathers the
// input to the pure step, and the output-chain processes the output
// from the pure step
pub struct computation<i,d,o,v,e>
  name: string // the name of the computation, and also the name of the pure step
  input-chain: chain<i,d,e>
  pure: pure-step<d,o>
  output-chain: chain<o,v,e>

// controls the input-gathering and output-performing phases - 
// perhaps data is read directly from the serialized log of 
// a previously failed computation, or maybe serialized output 
// is written to a kafka topic rather than actioned immediately
pub effect computation-io<i,d,o,v,e>
  ctl inputs-fx(log-steps: list<step-log>, c: computation<i,d,o,v,e>, i: i) : d
  ctl outputs-fx(log-steps: list<step-log>, c: computation<i,d,o,v,e>, o: o) : v

// a handler for the computation io operations
pub alias computation-io-handler<i,d,o,v,e> = (() -> <computation-io<i,d,o,v,e>,pure,logger|e> v) -> <pure,logger> v

// handle computation-specific effects
pub alias fx-handler<v,e> = (() -> <pure,logger|e> v) -> <pure,logger> v

// the core business of running a computation - using an effect to gather inputs
// and perform outputs
fun resume-computation-steps<i,d,o,v,e>(step-logs: list<step-log>, c: computation<i,d,o,v,e>, i: i) : <computation-io<i,d,o,v,e>,pure,logger|e> v
  // log is backwards!
  val fwd-step-logs = step-logs.reverse
  val input-step-logs = fwd-step-logs.take(c.input-chain.length).reverse
  val pure-step-logs = fwd-step-logs.take(c.input-chain.length + 1).drop(c.input-chain.length).reverse
  val output-step-logs = fwd-step-logs.drop(c.input-chain.length + 1).reverse

  val d = inputs-fx(input-step-logs, c, i)
  val o = match pure-step-logs
    Nil -> resume-pure-step(None, c.pure, d) 
    Cons(step-log, _) -> resume-pure-step(StepLog(step-log), c.pure, d)
  val v = outputs-fx(output-step-logs, c, o)
  v

// basic case - handle computation io by running effectful input/output chains
pub fun resume-chain-io-handler<i,d,o,v,e>(hdl: fx-handler<v,e>): computation-io-handler<i,d,o,v,e>
  fn (action)
    with hdl
    with handler 
      ctl inputs-fx(step-logs, c, i)
        resume(resume-chain(step-logs, c.input-chain, i))
      ctl outputs-fx(step-logs, c, o)
        resume(resume-chain(step-logs, c.output-chain, o))
    action()

// run a computation, handling input and output effects, and returning final
// output, and a log of the computation. the log can be used for 
// debugging and introspection, and in the case of failure can be used 
// with an io-handler to retry the computation
// TODO - can we add a timer to convert the div effect to <ndet,exn> ?
//      how would that work with truly non-terminating computations ?
//      can they be interrupted ?
pub fun run-computation<i,d,o,v,e>(
    io-handler: computation-io-handler<i,d,o,v,e>, 
    c: computation<i,d,o,v,e>, 
    i: i) : div (either<string,v>, computation-log)
    
  with log-collector(c.name)
  with io-handler
  resume-computation-steps([], c, i)

pub fun resume-computation<i,d,o,v,e>(
    log: computation-log,
    io-handler: computation-io-handler<i,d,o,v,e>, 
    c: computation<i,d,o,v,e>,
    i: i) : div (either<string,v>, computation-log)
    
  with log-collector(c.name)
  with io-handler
  resume-computation-steps(log.step-logs, c, i)
