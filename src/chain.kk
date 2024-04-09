module chain

import std/core/exn
import std/core/int
import std/core/list
import std/num/random

pub alias stringsrc = () -> string

pub effect logger
  // open a step-log
  fun log-enter(name: string, input-ser: stringsrc) : ()
  // close a step log successfully
  fun log-complete(output-ser: stringsrc) : ()
  // close a step log with an error
  fun log-error(error-ser: stringsrc) : ()

pub value struct serde<a>
  ser: (a) -> string
  deser: (string) -> either<string,a>

pub alias pure-step-fn<i,o> = (i) -> <pure,logger> o
pub alias effectful-step-fn<i,o,e> = (i) -> <pure,logger|e> o

pub struct pure-step<i,o>
  name: string
  step: pure-step-fn<i,o>
  i-serde: serde<i>
  o-serde: serde<o>

pub fun pure-step<i,o>(name: string, step: pure-step-fn<i,o>, ?i/serde: serde<i>, ?o/serde: serde<o>): pure-step<i,o>
  Pure-step(name, step, ?i/serde, ?o/serde)

pub struct effectful-step<i,o,e>
  name: string
  step: effectful-step-fn<i,o,e>
  i-serde: serde<i>
  o-serde: serde<o>

pub fun effectful-step<i,o,e>(name: string, step: effectful-step-fn<i,o,e>, ?i/serde: serde<i>, ?o/serde: serde<o>): effectful-step<i,o,e>
  Effectful-step(name, step, ?i/serde, ?o/serde)

pub value type step<i,o,e>
  Pure(step: pure-step<i,o>)
  Effectful(step: effectful-step<i,o,e>)

pub fun run-pure-step<i,o>(s: pure-step<i,o>, i: i): <pure,logger> o
  log-enter(s.name, {s.i-serde.ser()(i)})
  val o = s.step()(i)
  log-complete({s.o-serde.ser()(o)})
  o

pub fun run-effectful-step<i,o,e>(s: effectful-step<i,o,e>, i: i): <pure,logger|e> o
  log-enter(s.name, {s.i-serde.ser()(i)})
  val o = s.step()(i)
  log-complete({s.o-serde.ser()(o)})
  o

pub fun run-step<i,o,e>(s: step<i,o,e>, i: i): <pure,logger|e> o
  match s
    Pure(step) -> run-pure-step(step, i)
    Effectful(step) -> run-effectful-step(step, i)

fun int/ser(i: int): string
 i.show

fun int/deser(s: string): either<string,int>
  match s.parse-int()
    Just(i) -> Right(i)
    Nothing -> Left("failed to parse int: " ++ s)

val int/serde = Serde(int/ser, int/deser)

fun string/ser(s: string): string
  s
fun string/deser(s: string): either<string,string>
  Right(s)
val string/serde = Serde(string/ser, string/deser)

pub fun run-some-steps()
  val s1 = Pure(pure-step("inc", fn (x: int) {x + 1}))
  val s2 = Effectful(effectful-step("double", fn (x: int) {println("double:" ++ x.show) ; x + x}))
  val s3 = Effectful(effectful-step("throws", fn (x: int) {println("throws:" ++ x.show) ; throw("error") ; x + x}))
  val s1r = run-step(s1, 1)
  println("s1r: " ++ s1r.show())
  val s2r = run-step(s2, 2)
  println("s2r: " ++ s2r.show())
  val s3r = run-step(s3, 3)
  println("s3r: " ++ s3r.show())

value type frame 
  NoFrame
  Name(n: string)

// handle the logger effect by printing to console
pub fun print-logger(action)
  var current := NoFrame
  with finally
    match current
      Name(name) -> 
        println("unhandled effect: <" ++ name ++ ">")
        current := NoFrame
      NoFrame -> ()
  with handler
    fun log-enter(name, input-ser)
      current := Name(name)
      println("enter: <" ++ name ++ "> " ++ input-ser())
    fun log-complete(output-ser)
      match current
        Name(name) -> 
          println("complete: <" ++ name ++ "> " ++ output-ser())
          current := NoFrame
        NoFrame -> println("complete: <?>" ++ " " ++ output-ser())
    fun log-error(error-ser)
      match current
        Name(name) -> 
          println("error: <" ++ name ++ "> " ++ error-ser())
          current := NoFrame
        NoFrame -> println("error: <?>" ++ " " ++ error-ser())
  action()

pub fun test-some-steps()
  with print-logger
  run-some-steps()    

// it's a backwards cons of steps - last step first... 
pub type chain<i,o,e>
  First(first: step<i,o,e>)
  Link(last: some <j> step<j,o,e>, front: some <j> chain<i,j,e>)

pub fun add-step(c: chain<i,o,e>, s: step<o,p,e>): chain<i,p,e>
  match c
    First(fst) -> Link(s, First(fst))
    Link(lst, fr) -> Link(s, Link(lst, fr))

pub fun first-i-serde<i,o,e>(c: chain<i,o,e>): serde<i>
  match c
    First(fst) -> 
      match fst
        Pure(Pure-step(_, _, iserde, _)) -> iserde
        Effectful(Effectful-step(_, _, iserde, _)) -> iserde
    Link(_lst, fr) -> 
      first-i-serde(fr)

pub fun last-o-serde<i,o,e>(c: chain<i,o,e>): serde<o>
  match c
    First(fst) -> 
      match fst
        Pure(Pure-step(_, _, _, oserde)) -> oserde
        Effectful(Effectful-step(_, _, _, oserde)) -> oserde
    Link(lst, _fr) -> 
      match lst
        Pure(Pure-step(_, _, _, oserde)) -> oserde
        Effectful(Effectful-step(_, _, _, oserde)) -> oserde

// run a chain of steps. blows up stack if chain too long
pub fun run-chain(c: chain<i,o,e>, i: i): <pure,logger|e> o
  match c
    First(fst) -> run-step(fst, i)
    Link(lst, fr) -> 
      val r = run-chain(fr, i)
      run-step(lst,r)

pub fun simple-chain()
  val s1 = Pure(pure-step("inc", fn (x: int) {x + 1}))
  val s2 = Effectful(effectful-step("tostr", fn (x: int) {println("tostr:" ++ x.show) ; x.show}))
  val s3 = Effectful(effectful-step("concat", fn (x: string) {println("throws:" ++ x) ; throw("error") ; x ++ x}))
  val c = First(s1).add-step(s2).add-step(s3)
  run-chain(c, 1)

pub fun test-simple-chain()
  with print-logger
  simple-chain()
  
pub struct computation<i,d,o,v,e>
  name: string // the name of the computation, and also the name of the pure step
  input-chain: chain<i,d,e>
  pure: pure-step<d,o>
  output-chain: chain<o,v,e>

// allows control of input-gathering and output-performing actions - 
// perhaps data is read directly from a serialized message, or written 
// to a kafka topic rather than performed immediately
pub effect computation-io<i,d,o,v,e>
  ctl inputs-fx(c: computation<i,d,o,v,e>, i: i) : d
  ctl outputs-fx(c: computation<i,d,o,v,e>, o: o) : v

pub alias computation-io-handler<i,d,o,v,e> = (() -> <computation-io<i,d,o,v,e>,pure,logger|e> v) -> <pure,logger|e> v

// a de/serializable log of a single step
// names should be unique, but it won't matter if not,
// since chain position is the true unique identifier
pub value struct open-step
  name: string
  input-str: stringsrc 

pub struct step-log
  name: string
  input-str: stringsrc
  output-str: either<stringsrc,stringsrc>

pub value struct computation-log
  name: string
  step-logs: list<step-log>

pub fun print-log(log: computation-log)
  println("computation: " ++ log.name)
  log.step-logs.foreach( fn (step-log)
    println("  step: " ++ step-log.name)
    println("  input: " ++ step-log.input-str()())
    match step-log.output-str
      Left(err) -> println("  error: " ++ err())
      Right(out) -> println("  output: " ++ out())
  )  

// collect and return a list of step-logs from a computation
// this handler should also handle any exn effect, so the 
// return is div (would be nice for it to be total, but there's
// no stdlib for timeouts yet afaics)
pub fun log-collector<v>(computation-name, action: () -> <div,exn,logger> v) : div (either<string,v>, computation-log)
  var steps: list<step-log> := []
  var next-step: maybe<open-step> := Nothing
  with return(v) (v, Computation-Log(computation-name, steps))
  with handler
    final ctl throw-exn(ex)
      match next-step
        Just(Open-step(name, input-str)) ->
          steps := Cons(Step-Log(name, input-str, Left({"Unhandled Exception: " ++ ex.show})), steps)
          Left("Unhandled Exception: " ++ ex.show)
        Nothing -> Left("Unhandled Exception outside step:" ++ ex.show)
  with handler
    fun log-enter(name, input-ser)
      next-step := Just(Open-Step(name, input-ser))
    fun log-complete(output-ser)
      match next-step
        Just(Open-step(name, input-ser)) -> 
          steps := Cons(Step-Log(name, input-ser, Right(output-ser)), steps)
        Nothing ->
          throw("Unexpected log-complete")
    fun log-error(error-ser)
      match next-step
        Just(Open-step(name, input-ser)) ->
          steps := Cons(Step-Log(name, input-ser, Left(error-ser)), steps)
        Nothing ->
          throw("Unexpected log-error")
  Right(action())

pub fun collect-test-logs()
  with log-collector("boo")
  log-enter("a", {"a"})
  log-complete({"b"})
  1

pub fun test-collect-test-logs()
  val r = collect-test-logs()
  r

// run a computation - using effects to gather inputs and perform outputs
// now need to deal with de/serialization of input/output values, gathering logs
// resuming from errors and giving control over the execution process (for e.g. 
// remote logging)
pub fun run-computation-steps<i,d,o,v,e>(c: computation<i,d,o,v,e>, i: i) : <computation-io<i,d,o,v,e>,pure,logger|e> v
  val d = c.inputs-fx(i)
  log-enter(c.name, {c.pure.i-serde.ser()(d)})
  val o = c.pure.step()(d)
  log-complete({c.pure.o-serde.ser()(o)})
  val v = c.outputs-fx(o)
  v

// a handler for the computation specific effects
pub alias computation-fx-handler<v,e> = (() -> <pure,logger|e> v) -> <pure,logger> v

// run a computation, handling input and output effects, and returning final output
// could add something at this point to deal with e.g. remote logging successes and 
// failures, or retry policies
// need some standard handlers for simple inputs/outputs, e.g. literal data
// (can use the return override of effects to provide literal outputs alongside the log and value)
pub fun run-computation<i,d,o,v,e>(
    io-handler: computation-io-handler<i,d,o,v,e>, 
    fx-handler: computation-fx-handler<v,e>, 
    c: computation<i,d,o,v,e>, 
    i: i) : div (either<string,v>, computation-log)
  with log-collector(c.name)
  with fx-handler
  with io-handler
  run-computation-steps(c, i)

// handle computation io by running chains
pub fun run-chain-io-handler<i,d,o,v,e>(action: () -> <computation-io<i,d,o,v,e>,pure,logger|e> v): <pure,logger|e> v
  with handler 
    ctl inputs-fx(c, i)
      resume(run-chain(c.input-chain, i))
    ctl outputs-fx(c, o)
      resume(run-chain(c.output-chain, o))
  action()
  
// now to build a computation and run it, outputting the result and the log... 
// then test under various failure conditions and 
// create a resume-log-chain-io-handler which resumes the computation from a failure point

struct update-user-command {
  user-id: int
  name: string
  password: string
}

fun update-user-command/ser(cmd: update-user-command): string
  "{id: " ++ cmd.user-id.show ++ ", name: " ++ cmd.name ++ ", password: " ++ cmd.password ++ "}"

fun update-user-command/deser(s: string): either<string,update-user-command>
  Left("not implemented")

val update-user-command/serde = Serde(update-user-command/ser, update-user-command/deser)

struct user {
  user-id: int
  name: string
  encryptedPassword: string
}

fun user/ser(u: user): string
  "{id: " ++ u.user-id.show ++ ", name: " ++ u.name ++ ", password: " ++ u.encryptedPassword ++ "}"

fun user/deser(s: string): either<string,user>
  Left("not implemented")

val user/serde = Serde(user/ser, user/deser)

struct user-updated-event {
  user-id: int
  name: string
  encryptedPassword: string
}

fun user-updated-event/ser(e: user-updated-event): string
  "{id: " ++ e.user-id.show ++ ", name: " ++ e.name ++ ", password: " ++ e.encryptedPassword ++ "}"

fun user-updated-event/deser(s: string): either<string,user-updated-event>
  Left("not implemented")

val user-updated-event/serde = Serde(user-updated-event/ser, user-updated-event/deser)

value struct command-with-user {
  command: update-user-command
  user: user
}

fun command-with-user/ser(cu: command-with-user): string
  "{command: " ++ update-user-command/ser(cu.command) ++ ", user: " ++ user/ser(cu.user) ++ "}"

fun command-with-user/deser(s: string): either<string,command-with-user>
  Left("not implemented")

val command-with-user/serde = Serde(command-with-user/ser, command-with-user/deser)

pub fun fetch-user(cmd: update-user-command): <pure,logger> command-with-user
  match cmd 
    Update-user-command(user-id, _name, _password) -> 
      Command-with-user(cmd, User(user-id, "name", "password"))

value struct command-with-user-and-output {
  command: update-user-command
  user: user
  event: user-updated-event
}

fun command-with-user-and-output/ser(cu: command-with-user-and-output): string
  "{command: " ++ update-user-command/ser(cu.command) ++ ", user: " ++ user/ser(cu.user) ++ ", event: " ++ user-updated-event/ser(cu.event) ++ "}"

fun command-with-user-and-output/deser(s: string): either<string,command-with-user-and-output>
  Left("not implemented")

val command-with-user-and-output/serde = Serde(command-with-user-and-output/ser, command-with-user-and-output/deser)

pub fun update-user(cu: command-with-user): <pure,logger> command-with-user-and-output
  match cu
    Command-with-user(cmd, user) -> 
      Command-with-user-and-output(cmd, user, User-updated-event(user.user-id, cmd.name, cmd.password))

pub fun process-user-updated-event(cuo: command-with-user-and-output): <pure,logger> command-with-user-and-output
  cuo

pub fun test-computation()
  val s1 = Effectful(effectful-step("fetch-user", fetch-user))
  val input-chain = First(s1)
  val s2 = pure-step("update-user", update-user)
  val s3 = Effectful(effectful-step("process-user-updated-event", process-user-updated-event))
  val output-chain = First(s3)
  val computation = Computation("update-user", input-chain, s2, output-chain)

  val r = run-computation(
    run-chain-io-handler, 
    fn (action) {action()}, 
    computation, 
    Update-user-command(1, "new-name", "new-password"))

  match r 
    (_,log) -> print-log(log)
  
