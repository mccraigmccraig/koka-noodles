module json

import std/text/parse 
import std/num/decimal

pub struct object-entry {
  key: string
  value: value
}

pub rec type value
  String(s: string)
  Int(i: int)
  Bool(b: bool)
  Null
  Array(a: list<value>)
  Object(o: list<object-entry>)

fun object-entry/show(e: object-entry)
  "\"" ++ e.key ++ "\": " ++ e.value.show

fun value/show(v: value)
  match v
    String(s) -> "\"" ++ s ++ "\""
    Int(i) -> i.show
    Bool(b) -> if b then "true" else "false"
    Null -> "null"
    Array(a) -> "[" ++ a.map(show).join(", ") ++ "]"
    Object(o) -> "{" ++ o.map(show).join(", ") ++ "}"

fun hex-digit()
  char-is("hex-digit", is-hex-digit)

fun four-hex-digits()
  count(4, hex-digit).string

fun json-quoted-char()
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

pub fun bool-true()
  pstring("true")
  True

pub fun bool-false()
  pstring("false")
  False

pub fun bool()
  choose([
    {bool-true()},
    {bool-false()}
  ])

pub fun null()
  pstring("null")

pub fun is-whitespace-comma-char(c)
  c == ','
  || c == ' '
  || c == '\n'
  || c == '\r'
  || c == '\t'

pub fun whitespace-comma()
  optional([], {chars-are("whitespace-comma-chars", is-whitespace-comma-char)}).string

pub fun array-values()
  char('[')
  json-whitespace()
  val values = many({
    whitespace-comma() ;
    val v = value()
    whitespace-comma()
    v})
  json-whitespace()
  char(']')
  values

pub fun object-entries()
  char('{')
  json-whitespace()
  val entries = many({
    whitespace-comma() ;
    val k = json-string()
    json-whitespace()
    char(':')
    json-whitespace()
    val v = value()
    whitespace-comma()
    Object-entry(k, v)
  })
  json-whitespace()
  char('}')
  entries

pub fun value()
  choose([
    {json-string().String},
    {pint().Int},  
    {bool().Bool},
    {null() ; Null},
    {array-values().Array},
    {object-entries().Object}
  ])

pub fun test-parse()
  "{\"foo\": [1,2,true]}".slice.parse(value).maybe.show
