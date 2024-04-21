module serjson_test

import std/test/test
import std/text/parse 
import serjson

// some structs to test parsing nested structures

struct thing {
  name: string
}

struct stuff {
  foo: int
  bar: string
  thing: thing
}

fun thing/(==)(a: thing, b: thing): bool
  a.name == b.name

fun stuff/(==)(a: stuff, b: stuff): bool
  a.foo == b.foo
  && a.bar == b.bar
  && a.thing == b.thing

fun eitherstuff/(==)(a: either<string,stuff>, b: either<string,stuff>): bool
  match (a,b)
    (Left(_aa),Left(_bb)) -> False
    (Right(aa),Right(bb)) -> aa == bb
    _ -> False

// valuefn and pfn for thing and stuff

fun thing/valuefn(t: thing)
  Object([
    Object-entry("name", String(t.name))
  ])

fun thing/pfn(v: value)
  match v
    Object([
      Object-entry("name", String(name))]) ->
        Thing(name)
    _ -> fail("thing: bad json")

fun stuff/valuefn(s: stuff)
  Object([
    Object-entry("foo", Int(s.foo)),
    Object-entry("bar", String(s.bar)),
    Object-entry("thing", thing/valuefn(s.thing))
  ])

pub fun stuff/pfn(v: value)
  match v
    Object([
      Object-entry("foo", Int(foo)),
      Object-entry("bar", String(bar)),
      Object-entry("thing", tv)]) ->
        val t = thing/pfn(tv)
        Stuff(foo, bar, t)
    _ -> fail("stuff: bad json")

fun stuff/show(s: stuff)
  s.json(?valuefn=stuff/valuefn).show


// tests

pub fun test-serjson()
  basic/scope("value/(==)")
    scoped/test("string-equals")
      expect-fail(String("foo"), fn () String("foo"))
    scoped/test("int-equals")
      expect-fail(Int(1), fn () Int(1))
    scoped/test("bool-equals")
      expect-fail(Bool(True), fn () Bool(True))
    scoped/test("null-equals")
      expect-fail(Null, fn () Null)
    scoped/test("array-test")
      expect-fail(Array([Int(1), Int(2)]), fn () Array([Int(1), Int(2)]))
    scoped/test("object-test")
      expect-fail(Object([Object-entry("foo", Int(1))]), fn () Object([Object-entry("foo", Int(1))]))

  basic/scope("simple parse")
    scoped/test("simple parse")
      val res = "{\"foo\": [1,\"two\",true,{\"bar\": 200}]}"
      val expectation = Right(Object([
        Object-entry("foo", Array([
          Int(1),
          String("two"),
          Bool(True),
          Object([
            Object-entry("bar", Int(200))])]))]))
      expect-fail(expectation, fn () res.parse-value, ?(==) = eithervalue/(==))

  basic/scope("simple roundtrip")
    scoped/test("simple roundtrip")
      val obj = Object([
        Object-entry("foo", Array([
          Int(1),
          String("two"),
          Bool(True),
          Object([
            Object-entry("bar", Int(200))])]))])
      val json = obj.show
      val reparsed = json.parse-value
      val expectation = Right(obj)
      expect-fail(expectation, fn () reparsed, ?(==)=eithervalue/(==))

  basic/scope("nested parse")
    scoped/test("nested parse")
      val json = "{\"foo\": 10, \"bar\": \"blah\", \"thing\": {\"name\": \"graagh\"}}"
      val parsed: either<string,stuff> = json.dejson(?pfn=stuff/pfn)
      val expectation = Right(Stuff(10, "blah", Thing("graagh")))
      expect-fail(expectation, fn () parsed)

  basic/scope("nested roundtrip")
    scoped/test("stuff-roundtrip")
      val s = Stuff(10, "blah", Thing("graagh"))
      val json = s.json(?valuefn=stuff/valuefn)

      val reparsed: either<string,stuff> = json.dejson

      val expectation = Right(s)
      expect-fail(expectation, fn () reparsed)

// examples

pub fun example-parse()
  "{\"foo\": [1,\"two\",true,{\"bar\": 200}]}".parse-value.show

pub fun example-roundtrip()
  val obj = Object([
    Object-entry("foo", Array([
      Int(1),
      String("two"),
      Bool(True),
      Object([
        Object-entry("bar", Int(200))
      ])
    ]))
  ])
  val json = obj.show
  val reparsed = json.parse-value

  reparsed.show

pub fun example-nested-parse()
  val json = "{\"foo\": 10, \"bar\": \"blah\", \"thing\": {\"name\": \"graagh\"}}"
  val parsed: either<string,stuff> = json.dejson(?pfn=stuff/pfn)
  parsed.show

pub fun example-nested-roundtrip()
  val s = Stuff(10, "blah", Thing("graagh"))
  val json = s.json

  val reparsed: either<string,stuff> = json.dejson

  reparsed.show