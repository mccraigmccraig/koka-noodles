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
fun user/pobject(v: json/value): <div,parse> user 
  match v
    Object([Object-entry("id", Int(id)), 
            Object-entry("name", String(name)), 
            Object-entry("password", String(password))]) -> 
      User(id, name, password)
    _ -> fail("invalid json")
fun user/puser(): <div,parse> user
  json/pvalue().user/pobject
fun user/deser(s: string): div either<string,user>
  s.slice.parse(user/puser).either
fun user/ser(u: user): div string
  Object([Object-entry("id", Int(u.user-id)), 
          Object-entry("name", String(u.name)), 
          Object-entry("password", String(u.encryptedPassword))]).show
val user/serde = Serde(user/ser, user/deser)

struct update-user-command {
  user-id: int
  name: string
  password: string
}
fun update-user-command/pobject(v: json/value): <div,parse> update-user-command
  match v
    Object([Object-entry("id", Int(id)), 
            Object-entry("name", String(name)), 
            Object-entry("password", String(password))]) -> 
        Update-user-command(id, name, password)
    _ -> fail("invalid json")
fun update-user-command/p(): <div,parse> update-user-command
  json/pvalue().update-user-command/pobject
fun update-user-command/deser(s: string): div either<string,update-user-command>
  s.slice.parse(update-user-command/p).either
fun update-user-command/ser(cmd: update-user-command): div string
  Object([Object-entry("id", Int(cmd.user-id)), 
          Object-entry("name", String(cmd.name)), 
          Object-entry("password", String(cmd.password))]).show
val update-user-command/serde = Serde(update-user-command/ser, update-user-command/deser)

struct user-updated-event {
  user-id: int
  name: string
  encryptedPassword: string
}
fun user-updated-event/pobject(v: json/value): <div,parse> user-updated-event
  match v
    Object([Object-entry("id", Int(id)), 
            Object-entry("name", String(name)), 
            Object-entry("encryptedPassword", String(password))]) -> 
        User-updated-event(id, name, password)
    _ -> fail("invalid json")
fun puser-updated-event(): <div,parse> user-updated-event
  json/pvalue().user-updated-event/pobject
fun user-updated-event/deser(s: string): div either<string,user-updated-event>
  s.slice.parse(puser-updated-event).either
fun user-updated-event/ser(e: user-updated-event): div string
  Object([Object-entry("id", Int(e.user-id)), 
          Object-entry("name", String(e.name)), 
          Object-entry("encryptedPassword", String(e.encryptedPassword))]).show
val user-updated-event/serde = Serde(user-updated-event/ser, user-updated-event/deser)

// chain objects

value struct command-with-user {
  command: update-user-command
  user: user
}
fun pcommand-with-user(): <div,parse> command-with-user
  match json/pvalue()
    Object([Object-entry("command", cmd-object),
            Object-entry("user", user-object)]) ->
      val cmd = update-user-command/pobject(cmd-object)
      val user = user/pobject(user-object)
      Command-with-user(cmd, user)
    _ -> fail("invalid json")
fun command-with-user/deser(s: string): div either<string,command-with-user>
  s.slice.parse(pcommand-with-user).either
fun command-with-user/ser(cu: command-with-user): div string
  "{command: " ++ update-user-command/ser(cu.command) ++ ", user: " ++ user/ser(cu.user) ++ "}"
val command-with-user/serde = Serde(command-with-user/ser, command-with-user/deser)

value struct command-with-user-and-output {
  command: update-user-command
  user: user
  event: user-updated-event
}
fun pcommand-with-user-and-output(): <div,parse> command-with-user-and-output
  match json/pvalue()
    Object([Object-entry("command", cmd-object),
            Object-entry("user", user-object),
            Object-entry("event", event-object)]) ->
      val cmd = update-user-command/pobject(cmd-object)
      val user = user/pobject(user-object)
      val event = user-updated-event/pobject(event-object)
      Command-with-user-and-output(cmd, user, event)
    _ -> fail("invalid json")    
fun command-with-user-and-output/deser(s: string): div either<string,command-with-user-and-output>
  s.slice.parse(pcommand-with-user-and-output).either
fun command-with-user-and-output/ser(cu: command-with-user-and-output): div string
  "{command: " ++ update-user-command/ser(cu.command) ++ ", user: " ++ user/ser(cu.user) ++ ", event: " ++ user-updated-event/ser(cu.event) ++ "}"
val command-with-user-and-output/serde = Serde(command-with-user-and-output/ser, command-with-user-and-output/deser)

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
  
