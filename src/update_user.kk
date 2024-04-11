module update_user

import chain
import logger

// basic objects

struct user {
  user-id: int
  name: string
  encryptedPassword: string
}
fun user/ser(u: user): string
  "{id: \"" ++ u.user-id.show ++ "\", name: \"" ++ u.name ++ "\", password: \"" ++ u.encryptedPassword ++ "\"}"

fun user/deser(s: string): either<string,user>
  Left("not implemented")

val user/serde = Serde(user/ser, user/deser)

struct update-user-command {
  user-id: int
  name: string
  password: string
}

fun update-user-command/ser(cmd: update-user-command): string
  "{id: \"" ++ cmd.user-id.show ++ "\", name: \"" ++ cmd.name ++ "\", password: \"" ++ cmd.password ++ "\"}"

fun update-user-command/deser(s: string): either<string,update-user-command>
  Left("not implemented")

val update-user-command/serde = Serde(update-user-command/ser, update-user-command/deser)

struct user-updated-event {
  user-id: int
  name: string
  encryptedPassword: string
}

fun user-updated-event/ser(e: user-updated-event): string
  "{id: \"" ++ e.user-id.show ++ "\", name: \"" ++ e.name ++ "\", password: \"" ++ e.encryptedPassword ++ "\"}"

fun user-updated-event/deser(s: string): either<string,user-updated-event>
  Left("not implemented")

val user-updated-event/serde = Serde(user-updated-event/ser, user-updated-event/deser)

// chain objects

value struct command-with-user {
  command: update-user-command
  user: user
}

fun command-with-user/ser(cu: command-with-user): string
  "{command: " ++ update-user-command/ser(cu.command) ++ ", user: " ++ user/ser(cu.user) ++ "}"

fun command-with-user/deser(s: string): either<string,command-with-user>
  Left("not implemented")

val command-with-user/serde = Serde(command-with-user/ser, command-with-user/deser)

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
  
