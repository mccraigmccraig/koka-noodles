module json

import std/text/parse 
import std/num/decimal

pub struct object-entry {
  key: string
  value: value
}

fun object-entry/show(e: object-entry)
  "\"" ++ e.key ++ "\": " ++ e.value.show

pub rec type value
  String(s: string)
  Int(i: int)
  Bool(b: bool)
  Null
  Array(a: list<value>)
  Object(o: list<object-entry>)

fun value/show(v: value)
  match v
    String(s) -> "\"" ++ s ++ "\""
    Int(i) -> i.show
    Bool(b) -> b.show
    Null -> "null"
    Array(a) -> "[" ++ a.map(show).join(", ") ++ "]"
    Object(o) -> "{" ++ o.map(show).join(", ") ++ "}"

fun hex-digit()
  char-is("hex-digit", is-hex-digit)

fun four-hex-digits()
  count(4, hex-digit).string

pub fun json-quoted-char()
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

pub fun is-json-string-char(c)
  c != '"'
  && c != '\\'
  && !is-control(c)

pub fun json-string-chars()
  parse/(||)(
    {chars-are("json-sring-chars", is-json-string-char).string}, 
    {json-quoted-char().string}
  )

pub fun double-quote()
  char('"')

pub fun json-string()
  double-quote()
  val str = many(json-string-chars).join
  double-quote()
  str

pub fun is-json-whitespace-char(c)
  c == ' '
  || c == '\n'
  || c == '\r'
  || c == '\t'

pub fun json-whitespace()
  optional([], {chars-are("whitespace-chars", is-json-whitespace-char)}).string

pub fun json-int()
  pint()

pub fun json-true()
  pstring("true")
  True

pub fun json-false()
  pstring("false")
  False

pub fun json-bool()
  choose([
    {json-true()},
    {json-false()}
  ])

pub fun json-null()
  pstring("null")

pub fun is-json-whitespace-comma-char(c)
  c == ','
  || c == ' '
  || c == '\n'
  || c == '\r'
  || c == '\t'

pub fun json-whitespace-comma()
  optional([], {chars-are("whitespace-comma-chars", is-json-whitespace-comma-char)}).string

pub fun json-array()
  char('[')
  json-whitespace()
  val values = many({
    json-whitespace-comma() ;
    val v = json-value()
    json-whitespace-comma()
    v})
  json-whitespace()
  char(']')
  values

pub fun json-object()
  char('{')
  json-whitespace()
  val entries = many({
    json-whitespace-comma() ;
    val key = json-string()
    json-whitespace()
    char(':')
    json-whitespace()
    val value = json-value()
    json-whitespace-comma()
    Object-entry(key, value)
  })
  json-whitespace()
  char('}')
  entries

pub fun json-value()
  choose([
    {json-string().String},
    {json-int().Int},  
    {json-bool().Bool},
    {json-null() ; Null},
    {json-array().Array},
    {json-object().Object}
  ])

pub fun test-parse()
  "{\"foo\": 123}".slice.parse(json-value).maybe.show
