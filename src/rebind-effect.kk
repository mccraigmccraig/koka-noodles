module rebind-effect

pub effect foo<a>
  ctl foo(a: a): a

pub alias foo-thunk-nox<a,b> = () -> <console,foo<a>> b
pub alias foo-thunk-x<a,b,e> = () -> <console,foo<a>|e> b

pub fun handle-foo<a,b>(
  action: () -> <console,foo<a>> b
  ): console b
  with handler 
    ctl foo(a)
      "handle-foo".println      
      resume(a)
  action()

pub fun handle-map-foo-nox<a,b,c>(
    fab: (a)->b,
    action: () -> <console,foo<a>> c
  ): <console,foo<b>> c
  with handler 
    ctl foo(a)
      "map-foo-nox".println
      foo(a.fab)
      resume(a)
  action()

pub fun handle-map-foo-x<a,b,c,e>(
    fab: (a)->b,
    action: () -> <console,foo<a>,foo<b>|e> c
  ): <console,foo<b>|e> c
  with handler 
    ctl foo(a)
      "map-foo-x".println
      foo(a.fab)
      resume(a)
  action()

////////////////////////////////
// map effect no extension

pub fun example-handle-foo-nox()
  with handle-foo
  with handle-map-foo-nox(int/show)
  foo(100)
  "blah"
  
////////////////////////////////
// map effect with extension

pub fun example-handle-foo-x()
  with handle-foo
  with handle-map-foo-x(int/show)
  foo(100)
  "blah"

////////////////////////////////
// map effect via struct with no extension

pub value struct foo-struct-nox<a,b>
  name: string
  thunk: foo-thunk-nox<a,b>

pub fun map-foo-struct-nox<a,b,c,d>(
  fac: (a)->c,
  fbd: (b)->d,
  fs: foo-struct-nox<a,b>
): total foo-struct-nox<c,d>
  Foo-struct-nox(
    "map-foo-struct-nox--"++fs.name,
    fn () ->
      with handle-map-foo-nox(fac)
      fs.thunk()().fbd
  )

pub fun example-handle-foo-struct-nox()
  with handle-foo
  val fsn = Foo-struct-nox("foo", fn()-> {foo(100) ; "blah"})
  val fsn2 = map-foo-struct-nox(int/show, string/to-upper, fsn)
  fsn2.thunk()()


////////////////////////////////
// map effect via struct with extension

pub value struct foo-struct-x<a,b,e>
  name: string
  thunk: foo-thunk-x<a,b,e>

pub fun map-foo-struct-x<a,b,c,d,e>(
  fac: (a)->c,
  fbd: (b)->d,
  fs: foo-struct-x<a,b,<foo<c>|e>>
): total foo-struct-x<c,d,e>
  Foo-struct-x(
    "map-foo-struct-x--"++fs.name,
    fn () ->
      with handle-map-foo-x(fac)
      fs.thunk()().fbd
  )
  
pub fun example-handle-foo-struct-x()
  with handle-foo
  val fsn = Foo-struct-x("foo", fn()-> {foo(100) ; "blah"})
  val fsn2 = map-foo-struct-x(int/show, string/to-upper, fsn)
  fsn2.thunk()()
