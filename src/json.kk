module json

import std/core/either
import std/core/undiv
import std/core/list
import std/text/parse 
import std/test/test

pub struct object-entry {
  key: string
  value: value
}

pub type value
  String(s: string)
  Int(i: int)
  Bool(b: bool)
  Null
  Array(a: list<value>)
  Object(o: list<object-entry>)

pub fun object-entry/show(e: object-entry)
  "\"" ++ e.key ++ "\": " ++ e.value.show

pub fun value/show(v: value)
  match v
    String(s) -> "\"" ++ s ++ "\""
    Int(i) -> i.show
    Bool(b) -> if b then "true" else "false"
    Null -> "null"
    Array(a) -> "[" ++ a.map(show).join(", ") ++ "]"
    Object(o) -> "{" ++ o.map(show).join(", ") ++ "}"

pub fun value/(==)(a: value, b: value): bool
  match (a,b) 
    (String(aa),String(bb)) -> aa == bb
    (Int(aa),Int(bb)) -> aa == bb
    (Bool(aa),Bool(bb)) -> aa == bb
    (Null,Null) -> True
    (Array(aa),Array(bb)) -> 
      match (aa,bb)
        ([],[]) -> True
        ([],_) -> False
        (_,[]) -> False
        (Cons(x,xs),Cons(y,ys)) -> 
          value/(==)(pretend-decreasing(x), y) && 
          value/(==)(pretend-decreasing(Array(xs)),Array(ys))
    (Object(aa),Object(bb)) -> 
      match (aa,bb)
        ([],[]) -> True
        ([],_) -> False
        (_,[]) -> False
        (Cons(x,xs),Cons(y,ys)) -> 
          x.key == y.key &&
          value/(==)(pretend-decreasing(x.value), y.value) &&
          value/(==)(pretend-decreasing(Object(xs)),Object(ys))
    _ -> False

pub fun eithervalue/(==)(a: either<string,value>, b: either<string,value>): bool
  match (a,b)
    (Left(_aa),Left(_bb)) -> False
    (Right(aa),Right(bb)) -> value/(==)(aa,bb)
    _ -> False

pub alias pfn<t> = (json/value) -> <div,parse> t
pub alias valuefn<t> = (t) -> div json/value

fun hex-digit()
  char-is("hex-digit", is-hex-digit)

fun four-hex-digits()
  count(4, hex-digit).string

fun quoted-char()
  char('\\')
  choose([
    {char('"')},
    {char('\\')},
    {char('/')},
    // {char('b')},
    // {char('f')},
    {char('n') ; '\n'},
    {char('r') ; '\r'},
    {char('t') ; '\t'},
    // {char('u')}
  ])

fun is-json-string-char(c)
  c != '"'
  && c != '\\'
  && !is-control(c)

fun json-string-chars()
  parse/(||)(
    {chars-are("json-string-chars", is-json-string-char).string}, 
    {quoted-char().string}
  )

fun double-quote()
  char('"')

fun json-string()
  double-quote()
  val str = many(json-string-chars).join
  double-quote()
  str

fun is-json-whitespace-char(c)
  c == ' '
  || c == '\n'
  || c == '\r'
  || c == '\t'

fun json-whitespace()
  optional([], {chars-are("json-whitespace-chars", is-json-whitespace-char)}).string

fun bool-true()
  pstring("true")
  True

fun bool-false()
  pstring("false")
  False

fun bool()
  choose([
    {bool-true()},
    {bool-false()}
  ])

fun null()
  pstring("null")

fun is-whitespace-comma-char(c)
  c == ','
  || c.is-json-whitespace-char

fun whitespace-comma()
  optional([], {chars-are("whitespace-comma-chars", is-whitespace-comma-char)}).string

fun array-values()
  char('[')
  json-whitespace()
  val values = many({
    whitespace-comma()
    val v = pvalue()
    whitespace-comma()
    v})
  json-whitespace()
  char(']')
  values

fun object-entries()
  char('{')
  json-whitespace()
  val entries = many({
    whitespace-comma() ;
    val k = json-string()
    json-whitespace()
    char(':')
    json-whitespace()
    val v = pvalue()
    whitespace-comma()
    Object-entry(k, v)
  })
  json-whitespace()
  char('}')
  entries

pub fun json/pvalue()
  choose([
    {json-string().String},
    {pint().Int},  
    {bool().Bool},
    {null() ; Null},
    {array-values().Array},
    {object-entries().Object}
  ])

pub fun parse-value(s: string)
  s.slice.parse(json/pvalue).either

pub fun dejson<t>(s: string, ?pfn: pfn<t>): div either<string,t>
  s.slice.parse({json/pvalue().?pfn}).either

pub fun json<t>(t: t, ?valuefn: valuefn<t>): div string
  ?valuefn(t).show

pub fun test-equals()
  basic/test("string-equals")
    expect-fail(String("foo"), fn () String("foo"), ?(==)=value/(==))
  basic/test("int-equals")
    expect-fail(Int(1), fn () Int(1), ?(==)=value/(==))
  basic/test("array-test")
    expect-fail(Array([Int(1), Int(2)]), fn () Array([Int(1), Int(2)]), ?(==)=value/(==))
  basic/test("object-test")
    expect-fail(Object([Object-entry("foo", Int(1))]), fn () Object([Object-entry("foo", Int(1))]), ?(==)=value/(==))

pub fun test-parse()
  basic/test("simple-parse")
    val res = "{\"foo\": [1,\"two\",true,{\"bar\": 200}]}"
    val expectation = Right(Object([
      Object-entry("foo", Array([
        Int(1),
        String("two"),
        Bool(True),
        Object([
          Object-entry("bar", Int(200))])]))]))
    expect-fail(expectation, fn () res.parse-value, ?(==)=eithervalue/(==))

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

struct thing {
  name: string
}

struct stuff {
  foo: int
  bar: string
  thing: thing
}

// a stuff/value and a stuff/pvalue are required for JSON
// generation and parsing respectively

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
  json(s).show

pub fun example-stuff-parse()
  val json = "{\"foo\": 10, \"bar\": \"blah\", \"thing\": {\"name\": \"graagh\"}}"
  val parsed: either<string,stuff> = json.dejson()
  parsed.show

pub fun example-stuff-roundtrip()
  val s = Stuff(10, "blah", Thing("graagh"))
  val json = s.json

  val reparsed: either<string,stuff> = json.dejson()

  reparsed.show