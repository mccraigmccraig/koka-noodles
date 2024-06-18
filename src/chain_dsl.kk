module chain_dsl

import std/core/console
import std/core/exn

// using an identity monad to build a simple DSL... don't need monads
// for effects like state, 'cos we have effects, but the binding aspect
// is useful... can also do without flow-control effects like either/maybe
// because we have the exn effect
//
// step is bind is just function application here, but there's no reason it can't 
// be extended to log, resume, etc - the point is that we can bind each 
// step's value... can also forget .pure because it's an identity monad

// step is identity bind, with a name provided. all effects from f are propagated
pub fun step<t, u, e>(m: t, name: string, f: (t) -> e u): e u
  f(m)

pub fun test-dsl2()
  with x <- 1.step("x")
  with y <- 10.step("y")
  with z <- (x + y).step("z")
  2 * z

// can match bound variables and continue binding
pub fun make-test-dsl3(i)
  with x <- i.step("x")
  with y <- [10,11].step("y")
  val aa = match y
    [a, b] -> 
      with z <- (x + a + b).step("z")
      z
    _ -> 0
  with bb <- 12.step("bb")
  aa.show ++ "/" ++ bb.show

pub fun test-dsl3()
  println(make-test-dsl3(1))
  println(make-test-dsl3(10))
