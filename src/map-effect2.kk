module map-effect2

pub effect foo<a>
  ctl foo(a: a): a 

pub alias foo-thunk-nox<a,b> = () -> <console,foo<a>> b

pub fun handle-foo<a,b>(action: foo-thunk-nox<a,b>): console b
  with handler 
    ctl foo(a)
      "handle-foo".println      
      resume(a)
  action()

// map foo(a: a) to foo(b: b)
pub fun handle-map-foo-nox<a,b,c>(
    fab: (a)->b,
    action: foo-thunk-nox<a,c>
  ): <console,foo<b>> c
  with handler 
    ctl foo(a)
      "map-foo-nox".println
      foo(a.fab)
      resume(a)
  action()

pub fun map-foo-nox(
  fac: (a)->c,
  fbd: (b)->d,
  bth: foo-thunk-nox<a,b>): total foo-thunk-nox<c,d>
  fn ()
    with handle-map-foo-nox(fac)
    bth().fbd

pub fun thunk-with-int-effect()
  fn() {foo(100).println ; "blah"}

pub fun convert-to-string-effect()
  val th = thunk-with-int-effect()
  map-foo-nox(int/show, string/count, th)

pub fun example-map-foo-nox()
  with handle-foo
  val th = fn() {foo(100).println ; "blah"}
  val th' = map-foo-nox(int/show, string/count, th)
  val th2 = convert-to-string-effect()
  th'()
  th2()  