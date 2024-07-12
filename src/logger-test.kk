module logger-test

import logger
import parsers/serjson/serjson

pub fun test-stringsrc-json()
  val src = fn () -> "foo"
  val j = serjson/json(src)
  val src' = serjson/dejson(j, ?pfn=logger/stringsrc/pfn)
  src'.show

pub fun test-log-outcome-json()
  val ofailpause = Left(ReasonPaused)
  val ofailpausej = serjson/json(ofailpause, ?vfn=logger/log-outcome/vfn)
  val either-ofailpause' = serjson/dejson(ofailpausej, ?pfn=logger/log-outcome/pfn)
  ofailpausej.println
  either-ofailpause'.show.println
  val ofailerr = Left(ReasonError({"foo"}))
  val ofailerrj = serjson/json(ofailerr, ?vfn=logger/log-outcome/vfn)
  val either-ofailerr' = serjson/dejson(ofailerrj, ?pfn=logger/log-outcome/pfn)
  ofailerrj.println
  either-ofailerr'.show.println
  val osuccexec = Right(ReasonExec({"foo"}))
  val osuccexecj = serjson/json(osuccexec, ?vfn=logger/log-outcome/vfn)
  val either-osuccexec' = serjson/dejson(osuccexecj, ?pfn=logger/log-outcome/pfn)
  osuccexecj.println
  either-osuccexec'.show.println
  val osuccresume = Right(ReasonResume({"foo"}))
  val osuccresumej = serjson/json(osuccresume, ?vfn=logger/log-outcome/vfn)
  val either-osuccresume' = serjson/dejson(osuccresumej, ?pfn=logger/log-outcome/pfn)
  osuccresumej.println
  either-osuccresume'.show.println

pub fun test-step-log-json()
  val sl = Step-log("fooobar", [], Right(ReasonExec({"bar"})))
  val slj = serjson/json(sl)
  val either-sl' = serjson/dejson(slj, ?pfn=logger/step-log/pfn)
  slj.println
  match either-sl'
    Right(sl') -> sl'.show.println
    Left(err) -> err.show.println
  

pub fun test-nested-step-log-json()
  val slc = Step-log("baz", [], Right(ReasonExec({"qux"})))
  val sl = Step-log("fooobar", [slc], Right(ReasonExec({"bar"})))
  val slj = serjson/json(sl)
  val either-sl' = serjson/dejson(slj, ?pfn=logger/step-log/pfn)
  slj.println
  match either-sl'
    Right(sl') -> sl'.show.println

pub fun test-computation-log-json()
  val slc = Step-log("baz", [], Right(ReasonExec({"qux"})))
  val sl = Step-log("fooobar", [slc], Right(ReasonExec({"bar"})))
  val cl = Computation-log("fooobar", 0, [sl])
  val clj = serjson/json(cl)
  val either-cl' = serjson/dejson(clj, ?pfn=logger/computation-log/pfn)
  with apply-indent(0)
  match either-cl'
    Right(cl') -> cl'.show.println
    Left(err) -> err.show.println

// pub fun test-double-json()
//   val src = fn () -> "foo"
//   val srcj = serjson/json(src)
//   val srcsrc = fn () -> srcj
//   val srcsrcj = serjson/json(srcsrc)
//   srcsrcj.println
//   val either-srcsrc' = serjson/dejson(srcsrcj, ?pfn=logger/stringsrc/pfn)
//   match either-srcsrc'
//     Right(srcsrc') -> "." ++ srcsrc'.show ++ "."
