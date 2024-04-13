module fcore

import logger
import chain

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
  ctl inputs-fx(c: computation<i,d,o,v,e>, i: i) : d
  ctl outputs-fx(c: computation<i,d,o,v,e>, o: o) : v

// the core business of running a computation - using effects to gather inputs
// and perform outputs
pub fun run-computation-steps<i,d,o,v,e>(c: computation<i,d,o,v,e>, i: i) : <computation-io<i,d,o,v,e>,pure,logger|e> v
  val d = c.inputs-fx(i)
  val o = c.pure.run-pure-step(d)
  val v = c.outputs-fx(o)
  v

// a handler for the computation io operations
pub alias computation-io-handler<i,d,o,v,e> = (() -> <computation-io<i,d,o,v,e>,pure,logger|e> v) -> <pure,logger> v

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
  run-computation-steps(c, i)

// handle computation-specific effects
pub alias fx-handler<v,e> = (() -> <pure,logger|e> v) -> <pure,logger> v

// basic case - handle computation io by running effectful input/output chains
pub fun run-chain-io-handler<i,d,o,v,e>(hdl: fx-handler<v,e>): computation-io-handler<i,d,o,v,e>
  fn (action)
    with hdl
    with handler 
      ctl inputs-fx(c, i)
        resume(run-chain(c.input-chain, i))
      ctl outputs-fx(c, o)
        resume(run-chain(c.output-chain, o))
    action()

// next up - a resume handler which will take a computation-log, presumably 
// from a failed computation, and resume it from the point of failure
// need proper deserialization to make this work... and probably also want 
// to tie the computation-log to the computation itself so that you can't 
// resume a computation with a log from a completely different computation

// pub fun resume-io-handler<i,d,o,v,e>(log: computation-log, hdl: fx-handler<v,e>): computation-io-handler<i,d,o,v,e>

