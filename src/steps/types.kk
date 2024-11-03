module types

import std/data/json

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

pub value struct json-fns<a>
  to-json: (a) -> total json
  from-json: (json: json, path: string) -> pure a

pub value struct json-container-fns<c>
  to-json: forall<a> (c<a>, ?a/json-fns: json-fns<a>) -> total json 
  from-json: forall<a> (json: json, path: string, ?a/json-fns: json-fns<a>) -> pure c<a>

pub value struct monad<m>
  m-pure : forall<a> a -> m<a>
  m-bind : forall<a,b> (m<a>, a -> m<b>) -> m<b>

pub fun pure(a: a, .?monad: monad<m>) : m<a>
  m-pure(a)

pub fun bind(m, f, .?monad)
  m-bind(m, f)

// we want JSON serializable monad types... local effects like 
// maybe, either, error etc...  
// distance effect types (with an in or out side-channel) won't serialize,
// but that's ok, 
// they can use effects... we just want these types to encode how thunk 
// result values get composed - short-circuiting etc
pub struct json-monad<m,a>
  m: monad<m>
  a-json-fns: json-fns<a>
  m-json-container-fns: json-container-fns<m>

  
