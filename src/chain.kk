module chain

import std/core/either
import std/core/exn
import std/core/int
import std/core/list
import std/core/undiv
import std/num/random
import logger

pub value struct stringalize<a>
  ser: (a) -> total string
  deser: (string) -> total either<string,a>

pub alias pure-step-fn<i,o> = (i) -> <pure,logger> o
pub alias effectful-step-fn<i,o,e> = (i) -> <pure,logger|e> o

pub struct pure-step<i,o>
  name: string
  step: pure-step-fn<i,o>
  i-stringalize: stringalize<i>
  o-stringalize: stringalize<o>

pub struct effectful-step<i,o,e>
  name: string
  step: effectful-step-fn<i,o,e>
  i-stringalize: stringalize<i>
  o-stringalize: stringalize<o>

// A computation step
// TODO extend with choice steps ? which can choose from 
// a set of continuation chains ?
pub value type step<i,o,e>
  Pure(step: pure-step<i,o>)
  Effectful(step: effectful-step<i,o,e>)

pub value type resume-step-log
  StepLog(step-log: step-log)
  None

// is there a better way to formulate this, maybe a DSL
// which spits out a chain or something ?
// v1 = inc(input)
// v2 = fetch(v1)
// v3 = tostr(v1)
// v4 = concat(v2, v3)
// or:
// chain = inc -> fetch -> tostr -> concat

// reminds me of the free monad stuff ... although we don't 
// really need the container types monads use to encode the 
// effectful behaviours we get from effects - all we need is 
// plain function composition

// could maybe use a chain-builder effect to build up the chain...
// ideal would be each step returning a value to the closure
// and the final closure result is the output of the chain... 
// so an effect operation would add a step to the chain? and 
// how would we capture the final output ?
// 
// maybe a chain is the wrong abstraction - could be that what
// is needed is an input-type, a bunch of steps and an 
// output calculation, just like a monadic do ... as long as the 
// output from each step can be captured and logged then we are 
// golden... sounding very like a free-monad now
// 
// how about something like:
//
// pub effect chain-builder
//   fun step<o>(() -> o): o 
//
// pub fun check-auth(user-id: string, password: string): bool
//   val m-crypted = step({encrypt(password)})
//   val m-user = step({fetch(user-id)})
//   step({check-auth(unwrap(m-user), unwrap(m-crypted))}, [m-crypted, m-user])
//   
// does this work with a monad or applicative and lifted step functions ?


// pub fun check-auth(m: monad<m>, user-id: string, password: string): bool
//   with x <- bind(m, fetch(user-id))
//   with y <- bind(m, encrypt(password))
//   pure(check-auth(x,y))

// it's a backwards cons of steps - last step first... 
// as steps are added to the chain, the interior step
// output types represented by the existential j
// become invisible
pub type chain<i,o,e>
  First(first: step<i,o,e>)
  Link(last: some <j> step<j,o,e>, front: some <j> chain<i,j,e>)

pub fun pure-step<i,o>(name: string, step: pure-step-fn<i,o>, ?i/stringalize: stringalize<i>, ?o/stringalize: stringalize<o>): pure-step<i,o>
  Pure-step(name, step, ?i/stringalize, ?o/stringalize)

pub fun effectful-step<i,o,e>(name: string, step: effectful-step-fn<i,o,e>, ?i/stringalize: stringalize<i>, ?o/stringalize: stringalize<o>): effectful-step<i,o,e>
  Effectful-step(name, step, ?i/stringalize, ?o/stringalize)

pub fun length(c: chain<i,o,e>): total int
  match c
    First(_) -> 1
    Link(_, fr) -> 1 + length(pretend-decreasing(fr))

pub fun add-step(c: chain<i,o,e>, s: step<o,p,e>): chain<i,p,e>
  match c
    First(fst) -> Link(s, First(fst))
    Link(lst, fr) -> Link(s, Link(lst, fr))

// append another chain to the end of c
// TODO can't make the types line up
// pub fun append-chain(c: chain<i,o,e>, d: chain<o,p,e>): exn chain<i,p,e>
//   match d
//     First(fst) -> Link(fst, c)
//     Link(lst, fr) -> Link(lst, append-chain(fr, c))

pub fun resume-pure-step<i,o>(resume-step-log: resume-step-log, s: pure-step<i,o>, i: i): <pure,logger> o
  log-enter(s.name, {s.i-stringalize.ser()(i)})
  val (outcome, o) = match resume-step-log
    StepLog(step-log) -> 
      match step-log.output
        Right(result-src) -> 
          match s.o-stringalize.stringalize/deser()(result-src())
            Right(result) -> (Resume, result)
            Left(err) -> throw("Result desrialization failed: " ++ err ++ " -- " ++ result-src())
        Left(_) -> (Run, s.step()(i))
    None -> (Run, s.step()(i))
  log-complete(outcome, {s.o-stringalize.ser()(o)})
  o

pub fun resume-effectful-step<i,o>(resume-step-log: resume-step-log, s: effectful-step<i,o,e>, i: i): <pure,logger|e> o
  log-enter(s.name, {s.i-stringalize.ser()(i)})
  val (outcome, o) = match resume-step-log 
    StepLog(step-log) ->
      match step-log.output
        Right(result-src) -> 
          match s.o-stringalize.stringalize/deser()(result-src())
            Right(result) -> (Resume, result)
            Left(err) -> throw("Result desrialization failed: "  ++ err ++ " -- "++ result-src())
        Left(_) -> (Run, s.step()(i))
    None -> (Run, s.step()(i))
  log-complete(outcome, {s.o-stringalize.ser()(o)})
  o

pub fun resume-step<i,o,e>(resume-step-log: resume-step-log, s: step<i,o,e>, i: i): <pure,logger|e> o
  match s
    Pure(step) -> resume-pure-step(resume-step-log, step, i)
    Effectful(step) -> resume-effectful-step(resume-step-log, step, i)

fun int/ser(i: int): string
 i.show

fun int/deser(s: string): either<string,int>
  match s.parse-int()
    Just(i) -> Right(i)
    Nothing -> Left("failed to parse int: " ++ s)

val int/stringalize = Stringalize(int/ser, int/deser)

fun string/ser(s: string): string
  s
fun string/deser(s: string): either<string,string>
  Right(s)
val string/stringalize = Stringalize(string/ser, string/deser)

fun resume-chain'(resume-step-logs: list<resume-step-log>, c: chain<i,o,e>, i: i): <pure,logger |e> o
  match c
    First(fst) -> 
      match resume-step-logs
        Cons(sl,_) -> resume-step(sl, fst, i)
        Nil -> resume-step(None, fst, i)
    Link(lst, fr) -> 
      match resume-step-logs
        Cons(sl,rsl) -> 
          val r = resume-chain'(rsl, fr, i)
          resume-step(sl, lst, r)
        Nil -> 
          val r = resume-chain'([], fr, i)
          resume-step(None, lst, r)

// given a log of a previous computation run, and a chain of steps,
// resume the computation
// - steps with a successful output are not re-run - but the result will 
//     be deserialized and reserialized into the new log
// - once failure is encountered, steps start to be re-run
// TODO foldr ?
pub fun resume-chain(step-logs: list<step-log>, c: chain<i,o,e>, i: i): <pure,logger |e> o
  // log is backwards!
  val fwd-step-logs = step-logs.reverse
  val step-count = c.length  
  val padding = replicate(None, step-count - fwd-step-logs.length)
  val padded-step-logs = (fwd-step-logs.map(StepLog) ++ padding).reverse

  resume-chain'(padded-step-logs, c, i)

// test/example

pub fun simple-chain()
  val s1 = Pure(pure-step("inc", fn (x: int) {x + 1}))
  val s2 = Effectful(effectful-step("tostr", fn (x: int) x.show))
  val s3 = Effectful(effectful-step("concat", fn (x: string) 
    x ++ x}))

  First(s1).add-step(s2).add-step(s3)

pub fun broken-simple-chain()
  val s1 = Pure(pure-step("inc", fn (x: int) {x + 1}))
  val s2 = Effectful(effectful-step("tostr", fn (x: int) {
    throw("error")
    x.show}))
  val s3 = Effectful(effectful-step("concat", fn (x: string) x ++ x))

  First(s1).add-step(s2).add-step(s3)

pub fun collect-simple-chain(step-logs, ch, i)
  with log-collector("run-simple-chain")
  resume-chain(step-logs, ch, i)

pub fun test-simple-chain()
  val ch = simple-chain()
  match collect-simple-chain([], ch, 1)
    (v, log) ->
      print-computation-log(log)
      println("result: " ++ v.show())
  
pub fun test-resume()
  val ch = broken-simple-chain()
  match collect-simple-chain([], ch, 1)
    (_v, log) ->
      println("broken-simple-chain")
      print-computation-log(log)
      val ch2 = simple-chain()
      match collect-simple-chain(log.step-logs, ch2, 1)
        (v2, log2) ->
          println("fixed-simple-chain")
          print-computation-log(log2)
          println("result2: " ++ v2.show())
