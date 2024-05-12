import std/core/console
import std/core/list
import std/core/maybe
import std/core/tuple

// playing around with free monads... maybe useful for an effectful
// computation DSL - with autologging, resumption, etc...
// in particular, the `with x <- mval.bind` syntax allowing 
// could be great for composing effectful steps

alias fpure<f,a> = a -> f<a>
alias ffmap<f,a,b> = (a -> div b, f<a>) -> div f<b>
ref struct functor<f>
  pure : forall<a> fpure<f,a>
  fmap : forall<a,b> ffmap<f,a,b>

alias mpure<m,a> = a -> m<a>
alias mbind<m,a,b> = (m<a>, a -> div m<b>) -> div m<b>
ref struct monad<m>
  pure : forall<a> mpure<m,a> 
  bind : forall<a,b> mbind<m,a,b>

type free<f,a>
  Pure(a: a)
  Free(f: f<free<f,a>>)


fun free/pure<a>( a: a, .?functor : functor<f>) : free<f,a>
  Pure(a)

fun free/fmap( f : (a) -> div b, m: free<f,a>, .?functor : functor<f>) : div free<f,b>
  match m
    Pure(x) -> Pure(f(x))
    Free(g) -> Free(fmap(fn(x){free/fmap(f,x,?functor=?functor)},g))

fun free/bind( m: free<f,a>, f : (a) -> div free<f,b>, .?functor : functor<f>) : div free<f,b>
  match m
    Pure(x) -> f(x)
    Free(g) -> Free(fmap(fn(x){free/bind(x,f,?functor=?functor)},g))

val list/functor = Functor( fn(x) [x], fn(f,m) m.map(f))

val maybe/functor = Functor( fn(x) Just(x), fn(f,m) m.map(f))

// failing to type this properly... maybe i need to go back and figure
// out how to convert a free monad to a corresponding vanilla monad
// fun free/show<f,a>( m: free<f, a>, ?a/show : a -> div string, .?functor : functor<f>, ?f/show : f -> div string) : div string
//   match m
//     Pure(x) -> "Pure<" ++ x.a/show ++ ">"
//     Free(g) -> "Free<" ++ fmap(fn(x){free/show(x)}, g).f/show ++ ">"

fun freelist/show<a>( m: free<list, a>, ?a/show : a -> div string) : div string
  match m
    Pure(x) -> "Pure<" ++ x.a/show ++ ">"
    Free(g) -> "Free<" ++ g.map(fn(x){free/show(x)}).show ++ ">"

pub fun mzip<f,a,b>(aas : free<f,a>, bbs : free<f,b>, .?functor: functor<f>) : div free<f,(a,b)>
  with a <- aas.bind
  with b <- bbs.bind
  free/pure((a,b))

pub fun test-list-free()
  val aas = Free([Pure(1), Pure(2)])
  val bbs = Free([Pure(True), Pure(False)])
  val ccs = mzip(aas, bbs)
  "running...".println
  ccs.freelist/show.println

fun freemaybe/show<a>( m: free<maybe, a>, ?a/show : a -> div string) : div string
  match m
    Pure(x) -> "Pure<" ++ x.a/show ++ ">"
    Free(g) -> "Free<" ++ g.map(fn(x){free/show(x)}).show ++ ">"

pub fun test-maybe-free()
  val aas = Free(Just(Pure(1)))
  val bbs = Free(Just(Pure(True)))
  val ccs = Free(Nothing)
  val dds = mzip(aas, bbs)
  val ees : free<maybe,(int, bool)> = mzip(aas, ccs)
  "running...".println
  dds.freemaybe/show.println
  ees.freemaybe/show.println

// val free/functor = Functor(Pure, free/fmap)

// val free/functor = Functor(
//   Pure, 
//   fn(f, m) 
//     match m 
//       Pure(x) -> Pure(f(x)) 
//       Free(g) -> Free(ffmap(fn(x){ffmap(f,x,?functor=free/functor)},g))
// )


// fun fpure( x: a, .?functor: functor<f>) : f<a>
//   pure(x)
  
// fun ffmap( f: (a) -> div b , m: f<a>, .?functor: functor<f>) : div f<b>
//   fmap(f,m)

// val list/functor = Functor( fn(x) [x], fn(f,m) m.map(f))

// pub fun example-list-functor()
//   ffmap(inc, [1,2,3]).show



// fun mbind( m, f, .?monad )
//   bind(m,f)

// fun mpure( x : a, .?monad : monad<m> ) : m<a>
//   pure(x)

// looking at https://serokell.io/blog/introduction-to-free-monads#free-monads



// val free/monad = Monad(Pure, fn (m, f) match m {Pure(x) -> })



// recursive values seem forbidden e.g. 
// val x = fn(i: int) { if i <= 0 then 1 else i + x(i - 1) }

fun x(i: int) : div int
  if i <= 0 then 1 else i + x(i - 1)

pub fun test-x()
  x(5).show