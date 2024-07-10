module logger-test

import logger
import parsers/serjson/serjson

pub fun test-stringsrc-json()
  val src = fn () -> "foo"
  val j = serjson/json(src)
  val src' = serjson/dejson(j, ?pfn=logger/stringsrc/pfn)
  src'.show

pub fun test-step-type-json()
  val bst = BindStep
  val bstj = serjson/json(bst)
  val either-bst' = serjson/dejson(bstj, ?pfn=logger/step-type/pfn)
  either-bst'.show

pub fun test-step-outcome-json()
  val so = Exec
  val soj = serjson/json(so)
  val either-so' = serjson/dejson(soj, ?pfn=logger/step-outcome/pfn)
  either-so'.show

pub fun test-step-log-json()
  val sl = Step-log("fooobar", BindStep, Just(fn () -> "foo"), [], Exec, Right(fn () -> "bar"))
  val slj = serjson/json(sl)
  val either-sl' = serjson/dejson(slj, ?pfn=logger/step-log/pfn)
  either-sl'.show

pub fun test-computation-log-json()
  val cl = Computation-log("fooobar", Log-options(False), [])
  val clj = serjson/json(cl)
  val either-cl' = serjson/dejson(clj, ?pfn=logger/computation-log/pfn)
  either-cl'.show