module logger-test

import logger
import parsers/regex/json-fast
import std/data/json

pub fun test-stringsrc-json()
  val src = fn () -> "foo"
  val j = src.to-json
  val src' = j.string-src/from-json("")
  src'.show

pub fun test-stringsrc-datajson()
  val src = fn () -> "f\"o\no"
  val j = src.to-json.show
  j.list.println
  match j.str/parse 
    JSString(s) -> s.list.println
    _ -> "error".println

pub fun test-unquote()
  val s = "\\\"f\\\"o\\\no\\\""
  s.list.println
  s.unquote.list.println

pub fun test-string()
  r#""[^\\\"]*(?:\\.[^"\\]*)*""#.list


pub fun test-log-outcome-json()
  val ofailpause = Left(ReasonPaused)
  val ofailpausej = ofailpause.to-json
  val either-ofailpause' = ofailpausej.log-outcome/from-json("")
  ofailpausej.println
  either-ofailpause'.show.println
  val ofailerr = Left(ReasonError({"foo"}))
  val ofailerrj = ofailerr.to-json
  val either-ofailerr' = ofailerrj.log-outcome/from-json("")
  ofailerrj.println
  either-ofailerr'.show.println
  val osuccexec = Right(ReasonExec({JSString("foo")}))
  val osuccexecj = osuccexec.to-json
  val either-osuccexec' = osuccexecj.log-outcome/from-json("")  
  osuccexecj.println
  either-osuccexec'.show.println
  val osuccresume = Right(ReasonResume({JSString("foo")}))
  val osuccresumej = osuccresume.to-json
  val either-osuccresume' = osuccresumej.log-outcome/from-json("")
  osuccresumej.println
  either-osuccresume'.show.println

// pub fun test-step-log-json()
//   val sl = Step-log("fooobar", [], Right(ReasonExec({"bar"})))
//   val slj = serjson/json(sl)
//   val either-sl' = serjson/dejson(slj, ?pfn=logger/step-log/pfn)
//   slj.println
//   match either-sl'
//     Right(sl') -> sl'.show.println
//     Left(err) -> err.show.println
  

// pub fun test-nested-step-log-json()
//   val slc = Step-log("baz", [], Right(ReasonExec({"qux"})))
//   val sl = Step-log("fooobar", [slc], Right(ReasonExec({"bar"})))
//   val slj = serjson/json(sl)
//   val either-sl' = serjson/dejson(slj, ?pfn=logger/step-log/pfn)
//   slj.println
//   match either-sl'
//     Right(sl') -> sl'.show.println

// pub fun test-computation-log-json()
//   val slc = Step-log("baz", [], Right(ReasonExec({"qux"})))
//   val sl = Step-log("fooobar", [slc], Right(ReasonExec({"bar"})))
//   val cl = Computation-log("fooobar", 0, [sl])
//   val clj = serjson/json(cl)
//   val either-cl' = serjson/dejson(clj, ?pfn=logger/computation-log/pfn)
//   with apply-indent(0)
//   match either-cl'
//     Right(cl') -> cl'.show.println
//     Left(err) -> err.show.println

// pub fun test-double-json()
//   val src = fn () -> "foo"
//   val srcj = serjson/json(src)
//   val srcsrc = fn () -> srcj
//   val srcsrcj = serjson/json(srcsrc)
//   srcsrcj.println
//   val either-srcsrc' = serjson/dejson(srcsrcj, ?pfn=logger/stringsrc/pfn)
//   match either-srcsrc'
//     Right(srcsrc') -> "." ++ srcsrc'.show ++ "."
