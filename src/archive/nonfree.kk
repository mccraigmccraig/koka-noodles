import std/core/console
import std/core/list
import std/core/maybe
import std/core/either
import std/core/tuple

// playing around with monads... 

ref struct functor<f>
  pure : forall<a> a -> div f<a>
  fmap : forall<a,b> (a -> <div> b, f<a>) -> <div> f<b>

ref struct monad<m>
  pure : forall<a> a -> div m<a> 
  bind : forall<a,b> (m<a>, a -> <div> m<b>) -> <div> m<b>

type simple<t> 
  Simple(t: t)

fun simple/pure<t>(x: t): simple<t>
  Simple(x)

fun simple/fmap<t,u>(f: t -> div u, m: simple<t>): div simple<u>
  match m
    Simple(x) -> Simple(f(x))

fun simple/bind<t,u>(m: simple<t>, f: t -> div simple<u>): div simple<u>
  match m
    Simple(x) -> f(x)

val simple/functor = Functor(simple/pure, simple/fmap)
val simple/monad = Monad(simple/pure, simple/bind)
val maybe/functor = Functor(Just, fn(f, m) -> match m
  Just(x) -> Just(f(x))
  Nothing -> Nothing
)
val maybe/monad = Monad(Just, fn(m, f) -> match m
  Just(x) -> f(x)
  Nothing -> Nothing
)

fun either/pure<a,b>(x: b): either<a,b>
  Right(x)
fun either/fmap<a,b,c>(f: b -> div c, m: either<a,b>): div either<a,c>
  match m
    Left(x) -> Left(x)
    Right(x) -> Right(f(x))
fun either/bind<a,b,c>(m: either<a,b>, f: b -> div either<a,c>): div either<a,c>
  match m
    Left(x) -> Left(x)
    Right(x) -> f(x)

val either/functor = Functor(either/pure, either/fmap)
val either/monad = Monad(either/pure, either/bind)

pub fun (/)(a: int, b: int): either<string,int>
  if b != 0 then
    pure(a / b)
  else
    Left("division by zero: " ++ a.show)

pub fun make-either-op(a: int, b: int): div either<string,int>
  with u <- (a / b).bind
  with v <- 10.pure.bind
  (u * v).pure

pub fun test-simple()
  val f = make-either-op(10, 2)
  val g = make-either-op(10, 0)
  (f, g)
