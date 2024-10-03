module types

// consumer control effect - immediately fail or pause a computation,
// or log a value
// 
// There's no short-circuit returning of computation values - because 
// that would mean parameterising step-ctl on the value type, and 
// so would require rebinding handlers at every bind, and need
// (x)->y functions we don't have 
pub effect step-ctl<a,b,c,d>
  // Immediately fail the computation with a value - similar
  // to returning a Left(a) from a thunk
  ctl fail(a: a): ()
  // Immediately pause the computation with a value - resuming with 
  // a value from outside
  ctl pause(b: b): c
  // log some data in the structured computation log
  fun log(data: d): ()
