module update_user

import std/text/parse
import logger
import chain
import fcore
import parsing/serjson/serjson

// basic objects

struct user {
  user-id: int
  name: string
  encryptedPassword: string
}

fun user/pfn(v: serjson/value): <div,parse> user 
  match v
    Object([Object-entry("id", Int(id)), 
            Object-entry("name", String(name)), 
            Object-entry("password", String(password))]) -> 
      User(id, name, password)
    _ -> fail("invalid json: user/pfn")

fun user/valuefn(u: user): total serjson/value
  Object([Object-entry("id", Int(u.user-id)), 
          Object-entry("name", String(u.name)), 
          Object-entry("password", String(u.encryptedPassword))])

val user/serde = Serde(fn (u: user) -> json(u), fn (s: string) -> dejson(s))

struct update-user-command {
  user-id: int
  name: string
  password: string
}
fun update-user-command/pfn(v: serjson/value): <div,parse> update-user-command
  match v
    Object([Object-entry("id", Int(id)), 
            Object-entry("name", String(name)), 
            Object-entry("password", String(password))]) -> 
        Update-user-command(id, name, password)
    _ -> fail("invalid json: update-user-command/pfn")
fun update-user-command/valuefn(cmd: update-user-command): total serjson/value
  Object([Object-entry("id", Int(cmd.user-id)), 
          Object-entry("name", String(cmd.name)), 
          Object-entry("password", String(cmd.password))])
val update-user-command/serde = Serde(fn (cmd: update-user-command) -> json(cmd), fn (s: string) -> dejson(s, ?pfn=update-user-command/pfn))

struct user-updated-event {
  user-id: int
  name: string
  encryptedPassword: string
}
fun user-updated-event/pfn(v: serjson/value): <div,parse> user-updated-event
  match v
    Object([Object-entry("id", Int(id)), 
            Object-entry("name", String(name)), 
            Object-entry("encryptedPassword", String(password))]) -> 
        User-updated-event(id, name, password)
    _ -> fail("invalid json: user-updated-event/pfn")
fun user-updated-event/valuefn(e: user-updated-event): total serjson/value
  Object([Object-entry("id", Int(e.user-id)), 
          Object-entry("name", String(e.name)), 
          Object-entry("encryptedPassword", String(e.encryptedPassword))])
val user-updated-event/serde = Serde(fn (e: user-updated-event) -> json(e), fn (s: string) -> dejson(s))

// chain objects

value struct command-with-user {
  command: update-user-command
  user: user
}
fun command-with-user/pfn(v: serjson/value): <div,parse> command-with-user
  match v
    Object([Object-entry("command", cmd-object),
            Object-entry("user", user-object)]) ->
      Command-with-user(update-user-command/pfn(cmd-object), user/pfn(user-object))
    _ -> fail("invalid json: command-with-user/pfn")
fun command-with-user/valuefn(cu: command-with-user): total serjson/value
  Object([Object-entry("command", update-user-command/valuefn(cu.command)),
          Object-entry("user", user/valuefn(cu.user))])
val command-with-user/serde = Serde(fn (cu: command-with-user) -> json(cu, ?valuefn=command-with-user/valuefn), fn (s: string) -> dejson(s, ?pfn=command-with-user/pfn))

value struct command-with-user-and-output {
  command: update-user-command
  user: user
  event: user-updated-event
}
fun command-with-user-and-output/pfn(v: serjson/value): <div,parse> command-with-user-and-output
  match v
    Object([Object-entry("command", cmd-object),
            Object-entry("user", user-object),
            Object-entry("event", event-object)]) ->
      Command-with-user-and-output(update-user-command/pfn(cmd-object), 
        user/pfn(user-object), 
        user-updated-event/pfn(event-object))
    _ -> fail("invalid json: command-with-user-and-output/pfn")    
fun command-with-user-and-output/valuefn(cu: command-with-user-and-output): total serjson/value
  Object([Object-entry("command", update-user-command/valuefn(cu.command)),
          Object-entry("user", user/valuefn(cu.user)),
          Object-entry("event", user-updated-event/valuefn(cu.event))])
val command-with-user-and-output/serde = Serde(fn (cu: command-with-user-and-output) -> json(cu, ?valuefn=command-with-user-and-output/valuefn), fn (s: string) -> dejson(s))

// chain functions

pub fun fetch-user(cmd: update-user-command): <pure,logger> command-with-user
  match cmd 
    Update-user-command(user-id, _name, _password) -> 
      Command-with-user(cmd, User(user-id, "name", "password"))

pub fun update-user(cu: command-with-user): <pure,logger> command-with-user-and-output
  match cu
    Command-with-user(cmd, user) -> 
      Command-with-user-and-output(
        cmd, 
        user, 
        User-updated-event(
          user.user-id, 
          cmd.name, 
          cmd.password.map(fn (p) -> (p.int + 1).char)))

pub fun broken-update-user(cu: command-with-user): <pure,logger> command-with-user-and-output
  throw("boo")

pub fun process-user-updated-event(cuo: command-with-user-and-output): <pure,logger> command-with-user-and-output
  cuo

pub fun make-update-user-computation()
  val in1 = Effectful(effectful-step("fetch-user", fetch-user))
  val input-chain = First(in1)

  val core = pure-step("update-user", update-user)

  val out1 = Effectful(effectful-step("process-user-updated-event", process-user-updated-event))
  val output-chain = First(out1)

  Computation("update-user", input-chain, core, output-chain)

pub fun make-broken-update-user-computation()
  val in1 = Effectful(effectful-step("fetch-user", fetch-user, ?i/serde=update-user-command/serde))
  val input-chain = First(in1)

  val core = pure-step("update-user", broken-update-user)

  val out1 = Effectful(effectful-step("process-user-updated-event", process-user-updated-event))
  val output-chain = First(out1)

  Computation("update-user", input-chain, core, output-chain)


pub fun test-computation()
  val computation = make-update-user-computation()
  
  val r = run-computation(
    resume-chain-io-handler(fn (action) {action()}), 
    computation, 
    Update-user-command(1, "new-name", "new-password"))

  match r 
    (_,log) -> print-computation-log(log)
  
pub fun test-resume-computation()
  val computation = make-broken-update-user-computation()
  
  val r = run-computation(
    resume-chain-io-handler(fn (action) {action()}), 
    computation, 
    Update-user-command(1, "new-name", "new-password"))

  match r 
    (_,log) -> 
      println("broken-update-user-computation")
      print-computation-log(log)
      val computation2 = make-update-user-computation()
      val r2 = resume-computation(
        log,
        resume-chain-io-handler(fn (action) {action()}),
        computation2,
        Update-user-command(1, "new-name", "new-password"))
      
      match r2
        (_,log2) -> 
          println("fixed-update-user-computation")
          print-computation-log(log2)

pub fun test-deserialize()
  val str = "{\"command\": {\"id\": 1, \"name\": \"new-name\", \"password\": \"new-password\"}, \"user\": {\"id\": 1, \"name\": \"name\", \"password\": \"password\"}}"
  val json-value = parse-value(str)
  match command-with-user/serde.deser()(str)
    Left(err) -> println("parse error: " ++ err)
    Right(cu) -> println("parsed ok: " ++ cu.json())
