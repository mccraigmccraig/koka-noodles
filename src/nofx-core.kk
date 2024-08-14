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

