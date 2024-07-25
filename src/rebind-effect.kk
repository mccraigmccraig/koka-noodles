module rebind-effect

pub effect foo<a>
  ctl foo(a: a): a

pub fun handle-foo<a,b>(
  action: () -> <console,foo<a>> b
  ): console b
  with handler 
    ctl foo(a)
      "handle-foo".println      
      resume(a)
  action()

pub fun map-foo-nox<a,b,c>(
    fab: (a)->b,
    action: () -> <console,foo<a>> c
  ): <console,foo<b>> c
  with handler 
    ctl foo(a)
      "map-foo-nox".println
      foo(a.fab)
      resume(a)
  action()

pub fun do-foo(a: a): foo<a> a
  foo(a)

pub fun example-handle-foo-nox()
  with handle-foo
  with map-foo-nox(int/show)
  foo(100)
  "blah"
  
pub fun map-foo-x<a,b,c,e>(
    fab: (a)->b,
    action: () -> <console,foo<a>,foo<b>|e> c
  ): <console,foo<b>|e> c
  with handler 
    ctl foo(a)
      "map-foo-x".println
      foo(a.fab)
      resume(a)
  action()

pub fun example-handle-foo-x()
  with handle-foo
  with map-foo-x(int/show)
  foo(100)
  "blah"

alias foo-thunk<a,b,e> = () -> <console,foo<a>|e> b

pub fun map-foo-x2<a,b,c,e>(
    fab: (a)->b,
    action: foo-thunk<a,c,<foo<b>|e>>
  ): <console,foo<b>|e> c
  with handler 
    ctl foo(a)
      "map-foo-x2".println
      foo(a.fab)
      resume(a)
  action()

fun make-foo-thunk(): foo-thunk<int,string,<>>
  fn()
    foo(100)
    "blah"
  
pub fun example-handle-foo-x2()
  with handle-foo
  with map-foo-x2(int/show)
  make-foo-thunk()()
