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

pub fun test-nested-step-log-json()
  val slc = Step-log("baz", RunStep, Nothing, [], Exec, Right(fn () -> "qux"))
  val sl = Step-log("fooobar", BindStep, Just(fn () -> "foo"), [slc], Exec, Right(fn () -> "bar"))
  val slj = serjson/json(sl)
  val either-sl' = serjson/dejson(slj, ?pfn=logger/step-log/pfn)
  with apply-indent(0)
  match either-sl'
    Right(sl') -> sl'.pp

pub fun test-computation-log-json()
  val slc = Step-log("baz", RunStep, Nothing, [], Exec, Right(fn () -> "qux"))
  val sl = Step-log("fooobar", BindStep, Just(fn () -> "foo"), [slc], Exec, Right(fn () -> "bar"))
  val cl = Computation-log("fooobar", Log-options(False), 0, [sl])
  val clj = serjson/json(cl)
  val either-cl' = serjson/dejson(clj, ?pfn=logger/computation-log/pfn)
  with apply-indent(0)
  match either-cl'
    Right(cl') -> cl'.pp

pub fun test-double-json()
  val src = fn () -> "foo"
  val srcj = serjson/json(src)
  val srcsrc = fn () -> srcj
  val srcsrcj = serjson/json(srcsrc)
  srcsrcj.println
  val either-srcsrc' = serjson/dejson(srcsrcj, ?pfn=logger/stringsrc/pfn)
  match either-srcsrc'
    Right(srcsrc') -> "." ++ srcsrc'.show ++ "."
