module steps/step-ctl

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
