module nofx-core

import std/core/list
import steps/logger
import steps

// computations have a potentially effectful input step, which gathers
// an input for a pure core step, which returns a value to a potentially 
// effectful output step. 
// the div-core operation is the composed pipeline of
// input->pure->output steps
//
// TODO is this the best structure ? after the thunk-based bind syntax
// for the steps, it seems awfully constricting - but - it's just about
// getting the input and processing the output for the core, so maybe
// it's good
pub struct nofx-core<a,b,c,w,x,y,z,e>
  input: stepmfn-json<a,b,c,w,x,e>
  core: nofx-stepmfn-json<a,b,c,x,y>
  output: stepmfn-json<a,b,c,y,z,e>

// pipe the input, pure, output computations from a div-core into 
// a composite stepmfn-json
pub fun pipe(fc: nofx-core<a,b,c,w,x,y,z,e>): total stepmfn-json<a,b,c,w,z,e>
  fc.input.pipe-nofx-stepmfn-json(fc.core).pipe-stepmfn-json(fc.output)

// TODO - how to control the pipe... could add pauses in-between the stages
// but sometimes will want to run with no pauses, sometimes might want 
// all the pauses
// is there even any point to this pipe - what else does it 
// do apart from the pipes ?


// really don't like the above formulation
// how about a stepwise super-composition approach ?
// but how to distinguish the pure steps from the effectful steps ?
// can there be more than one pure step ? 
// is there just a single input and output step ?
// is there a more general structure ?

// what do i want to do with a nofx-core
// - generative testing of the core
// - run each of the stages standalone and do some marshalling on the intermediate data 

// but i can do all of the above with just steps with pauses
// but the stepmfns are a nice interface - because they take a data parameter
// so maybe i just want a way of composing stepmfns with pauses inserted ?


