import std/core/console
import std/core/list
import std/core/maybe
import std/core/tuple

// playing around with monads... 

ref struct functor<f>
  pure : forall<a> a -> div f<a>
  fmap : forall<a,b> (a -> <div> b, f<a>) -> <div> f<b>

ref struct monad<m>
  pure : forall<a> a -> div m<a> 
  bind : forall<a,b> (m<a>, a -> <div> m<b>) -> <div> m<b>

val list/functor = Functor( fn(x) [x], fn(f,m) m.map(f))
val list/monad = Monad( fn(x) [x], list/flatmap)

val maybe/functor = Functor( fn(x) Just(x), fn(f,m) m.map(f))
val maybe/monad = Monad( 
  fn(x) Just(x), 
  fn(m,f) match m 
    Nothing -> Nothing
    Just(x) -> f(x)
)

pub fun mzip<a>(ma : m<a>, mb : m<b>, .?monad: monad<m>) : <div> m<(a,b)>
  with a <- ma.bind
  with b <- mb.bind
  pure((a,b))

pub fun example-zip-list()
  val a = [1,2,3]
  val b = ["a","b","c"]
  mzip(a, b)

pub fun example-zip-maybe()
  val a = Just(5)
  val b = Just("foo")
  mzip(a,b)

type state<s, a>
  RunState(sf: s -> <div> (a,s)) 

fun state/get(): state<s,s>
  RunState(fn (s) -> (s,s))

fun state/put(s: s): state<s,()>
  RunState(fn (_) -> ((), s))

fun run-state(st : state<s,a>, s: s): <div> (a,s)
  match st
    RunState(sf) -> sf(s)

fun state/pure<a>(a: a): state<s,a>
  RunState(fn (s) (a,s))

fun state/fmap<a,b>(f: (a) -> <div> b, st: state<s,a>): <div> state<s,b>
  RunState(fn (s)
    match st 
      RunState(sf) -> 
        val (a,s') = sf(s)
        (f(a),s')
  )

val state/functor = Functor(state/pure, state/fmap)

fun state/bind<a,b>(st: state<s,a>, f: (a) -> <div> state<s,b>): <div> state<s,b>
  RunState(fn (s)
    match st 
      RunState(sf) -> 
        val (a,s') = sf(s)
        match f(a)
          RunState(sf') -> sf'(s')
  )

val state/monad = Monad(state/pure, state/bind)

fun build-state()
  with a <- state/get().bind
  with _ <- state/put(a + 5).bind
  state/pure(a + 1)

fun test-state()
  "building...".println
  val st = build-state()
  "running...".println
  val r = run-state(st, 5)
  r

// free monad

type free<f,a>
  Pure(a: a)
  Free(f: f<free<f,a>>)

fun free/pure<f,a>( a: a, .?functor : functor<f>) : free<f,a>
  Pure(a)

fun free/fmap( f : (a) -> <div> b, m: free<f,a>, .?functor : functor<f>) : <div> free<f,b>
  match m
    Pure(x) -> Pure(f(x))
    Free(g) -> Free(fmap(fn(x){free/fmap(f,x,?functor=?functor)},g))

fun free/bind( m: free<f,a>, f : (a) -> <div> free<f,b>, .?functor : functor<f>) : <div> free<f,b>
  match m
    Pure(x) -> f(x)
    Free(g) -> Free(fmap(fn(x){free/bind(x,f,?functor=?functor)},g))

fun foldFree<f,m,a>(morph: (f<free<f,a>>) -> m<free<f,a>>, ff: free<f,a>, .?monad : monad<m>) : <div> m<a>
  match ff
    Pure(a) -> a.pure
    Free(x) -> 
      with a <- x.morph.bind
      foldFree(morph,a)

fun liftFree<f,m,a>(fa: f<a>, .?functor: functor<f>) : <div> free<f,a>
  Free(fmap(fn(x){Pure(x)}, fa)) 

// now some free examples

// fun freelist/pure<a>( a: a) : free<list,a>
//   free/pure( a, ?functor=list/functor)

// fun freelist/bind<a,b>( ma: free<list,a>, f: (a) -> <div> free<list,b> ): <div> free<list,b>
//   free/bind( ma, f, ?functor=list/functor)

// val freelist/monad = Monad(freelist/pure, freelist/bind)

// fun freelist/show<a>( m: free<list, a>, ?a/show : a -> div string) : div string
//   match m
//     Pure(x) -> "Pure<" ++ x.a/show ++ ">"
//     Free(g) -> "Free<" ++ g.map(fn(x){free/show(x)}).show ++ ">"

// pub fun freezip<f,a,b>(aas : free<f,a>, bbs : free<f,b>, .?functor: functor<f>) : <div|e> free<f,(a,b)>
//   with a <- aas.bind
//   with b <- bbs.bind
//   free/pure((a,b))

// pub fun make-list-free()
//   val aas = Free([Pure(1), Pure(2)])
//   val bbs = Free([Pure(True), Pure(False)])
//   val ccs = freezip(aas, bbs)
//   ccs

// pub fun test-list-free()
//   "building...".println
//   val ccs = make-list-free()
//   "running...".println
//   ccs.freelist/show.println

// fun freemaybe/pure<a>(a: a) : free<maybe,a>
//   free/pure(a, ?functor=maybe/functor)

// fun freemaybe/bind<a,b>(ma: free<maybe,a>, f: (a) -> <div> free<maybe,b>): <div> free<maybe,b>
//   free/bind(ma, f, ?functor=maybe/functor)

// val freemaybe/monad = Monad(freemaybe/pure, freemaybe/bind)

// fun freemaybe/show<a>( m: free<maybe, a>, ?a/show : a -> div string) : div string
//   match m
//     Pure(x) -> "Pure<" ++ x.a/show ++ ">"
//     Free(g) -> "Free<" ++ g.map(fn(x){free/show(x)}).show ++ ">"

// pub fun make-maybe-free()
//   val aas = Free(Just(Pure(1)))
//   val bbs = Free(Just(Pure(True)))
//   val ccs = Free(Nothing)
//   val dds = freezip(aas, bbs)
//   val ees : free<maybe,(int, bool)> = freezip(aas, ccs)
//   (dds,ees)

// pub fun test-maybe-free()
//   "building...".println
//   val (dds,ees) = make-maybe-free()
//   "running...".println
//   dds.freemaybe/show.println
//   ees.freemaybe/show.println


// OK - how about a freestate...



// fun freestate/pure<a>(a: a) : div free<state<s>, a>
//   free/pure(a)

// fun freestate/bind<a,b>(ma: free<state<s>, a>, f: (a) -> <div> free<state<s>, b>): <div> free<state<s>, b>
//   free/bind(ma, f, ?functor=state/functor)

// val freestate/monad = Monad(freestate/pure, freestate/bind)  

// fun freestate/get()
//   liftFree(state/get())

// fun freestate/put(s)
//   liftFree(state/put(s))

// fun build-freestate()
//   with a <- freestate/get().bind
//   with _ <- freestate/put(5).bind
//   with b <- freestate/get().bind
//   freestate/pure(a * b)
  
// fun test-freestate()
//   "building...".println
//   val ff = build-freestate()
//   "running...".println
//   val morph = fn(x)
//     match x
//       RunState(sf) -> 
//         match sf
//           Pure
//   val (a,s) = foldFree(morph, ff, ?monad=state/monad)
//   (a,s)
// // https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html

