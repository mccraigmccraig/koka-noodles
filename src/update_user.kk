module update_user

import std/text/parse
import logger
import chain
import fcore
import json

// basic objects

struct user {
  user-id: int
  name: string
  encryptedPassword: string
}

fun user/pvalue(v: json/value): <div,parse> user 
  match v
    Object([Object-entry("id", Int(id)), 
            Object-entry("name", String(name)), 
            Object-entry("password", String(password))]) -> 
      User(id, name, password)
    _ -> fail("invalid json")

fun user/value(u: user): div json/value
  Object([Object-entry("id", Int(u.user-id)), 
          Object-entry("name", String(u.name)), 
          Object-entry("password", String(u.encryptedPassword))])

val user/serde = Serde(fn (u: user) -> json(u), fn (s: string) -> dejson(s))

struct update-user-command {
  user-id: int
  name: string
  password: string
}
fun update-user-command/pvalue(v: json/value): <div,parse> update-user-command
  match v
    Object([Object-entry("id", Int(id)), 
            Object-entry("name", String(name)), 
            Object-entry("password", String(password))]) -> 
        Update-user-command(id, name, password)
    _ -> fail("invalid json")
fun update-user-command/value(cmd: update-user-command): div json/value
  Object([Object-entry("id", Int(cmd.user-id)), 
          Object-entry("name", String(cmd.name)), 
          Object-entry("password", String(cmd.password))])
val update-user-command/serde = Serde(fn (cmd: update-user-command) -> json(cmd), fn (s: string) -> dejson(s, ?pvalue = update-user-command/pvalue))

struct user-updated-event {
  user-id: int
  name: string
  encryptedPassword: string
}
fun user-updated-event/pvalue(v: json/value): <div,parse> user-updated-event
  match v
    Object([Object-entry("id", Int(id)), 
            Object-entry("name", String(name)), 
            Object-entry("encryptedPassword", String(password))]) -> 
        User-updated-event(id, name, password)
    _ -> fail("invalid json")
fun user-updated-event/value(e: user-updated-event): div json/value
  Object([Object-entry("id", Int(e.user-id)), 
          Object-entry("name", String(e.name)), 
          Object-entry("encryptedPassword", String(e.encryptedPassword))])
val user-updated-event/serde = Serde(fn (e: user-updated-event) -> json(e), fn (s: string) -> dejson(s))

// chain objects

value struct command-with-user {
  command: update-user-command
  user: user
}
fun command-with-user/pvalue(v: json/value): <div,parse> command-with-user
  match v
    Object([Object-entry("command", cmd-object),
            Object-entry("user", user-object)]) ->
      val cmd = update-user-command/pvalue(cmd-object)
      val user = user/pvalue(user-object)
      Command-with-user(cmd, user)
    _ -> fail("invalid json")
fun command-with-user/value(cu: command-with-user): div json/value
  Object([Object-entry("command", update-user-command/value(cu.command)),
          Object-entry("user", user/value(cu.user))])
val command-with-user/serde = Serde(fn (cu: command-with-user) -> json(cu, ?value = command-with-user/value), fn (s: string) -> dejson(s, ?pvalue = command-with-user/pvalue))

value struct command-with-user-and-output {
  command: update-user-command
  user: user
  event: user-updated-event
}
fun command-with-user-and-output/pvalue(v: json/value): <div,parse> command-with-user-and-output
  match v
    Object([Object-entry("command", cmd-object),
            Object-entry("user", user-object),
            Object-entry("event", event-object)]) ->
      val cmd = update-user-command/pvalue(cmd-object)
      val user = user/pvalue(user-object)
      val event = user-updated-event/pvalue(event-object)
      Command-with-user-and-output(cmd, user, event)
    _ -> fail("invalid json")    
fun command-with-user-and-output/value(cu: command-with-user-and-output): div json/value
  Object([Object-entry("command", update-user-command/value(cu.command)),
          Object-entry("user", user/value(cu.user)),
          Object-entry("event", user-updated-event/value(cu.event))])
val command-with-user-and-output/serde = Serde(fn (cu: command-with-user-and-output) -> json(cu, ?value=command-with-user-and-output/value), fn (s: string) -> dejson(s))

// chain functions

pub fun fetch-user(cmd: update-user-command): <pure,logger> command-with-user
  match cmd 
    Update-user-command(user-id, _name, _password) -> 
      Command-with-user(cmd, User(user-id, "name", "password"))

pub fun update-user(cu: command-with-user): <pure,logger> command-with-user-and-output
  match cu
    Command-with-user(cmd, user) -> 
      Command-with-user-and-output(cmd, user, User-updated-event(user.user-id, cmd.name, cmd.password))

pub fun process-user-updated-event(cuo: command-with-user-and-output): <pure,logger> command-with-user-and-output
  cuo

pub fun test-computation()
  val in1 = Effectful(effectful-step("fetch-user", fetch-user, ?i/serde = update-user-command/serde, ?o/serde = command-with-user/serde))
  val input-chain = First(in1)

  val core = pure-step("update-user", update-user)

  val out1 = Effectful(effectful-step("process-user-updated-event", process-user-updated-event))
  val output-chain = First(out1)

  val computation = Computation("update-user", input-chain, core, output-chain)

  val r = run-computation(
    run-chain-io-handler(fn (action) {action()}), 
    computation, 
    Update-user-command(1, "new-name", "new-password"))

  match r 
    (_,log) -> print-computation-log(log)
  
