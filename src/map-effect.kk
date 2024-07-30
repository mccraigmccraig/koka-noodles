module map-effect

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

// alias, struct and handle-map with no extension

pub alias foo-thunk-nox<a,b> = () -> <console,foo<a>> b
pub value struct foo-struct-nox<a,b>
  name: string
  thunk: foo-thunk-nox<a,b>

// map foo(a: a) to foo(b: b)
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

// alias, struct and handle-map with extension

pub alias foo-thunk-x<a,b,e> = () -> <console,foo<a>|e> b
pub value struct foo-struct-x<a,b,e>
  name: string
  thunk: foo-thunk-x<a,b,e>

// map foo(a: a) to foo(b: b)
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

pub fun map-foo-nox(
  fac: (a)->c,
  fbd: (b)->d,
  bth: foo-thunk-nox<a,b>): foo-thunk-nox<c,d>
  fn ()
    with handle-map-foo-nox(fac)
    bth().fbd

pub fun example-map-foo-nox()
  with handle-foo
  val th: foo-thunk-nox<int,string> = fn() {"blah"}
  val th2: foo-thunk-nox<string,string> = map-foo-nox(int/show, string/to-upper, th)
  th2()
  // checks and runs until a second map is introduced
  // val th3 = map-foo-nox(string/count, string/count, th2)
  // th3()

////////////////////////////////
// map effect with extension

pub fun map-foo-x(
  fac: (a)->c,
  fbd: (b)->d,
  bth: foo-thunk-x<a,b,<foo<c>|e>>): foo-thunk-x<c,d,e>
  fn ()
    with handle-map-foo-x(fac)
    bth().fbd

pub fun example-map-foo-x()
  with handle-foo
  val th: foo-thunk-x<int,string,total> = fn() {"blah"}
  val th2: foo-thunk-x<string,string,total> = map-foo-x(int/show, string/to-upper, th)
  th2()
  // checks and runs until a second map is introduced
  // val th3 = map-foo-nox(string/count, string/count, th2)
  // th3()

// ////////////////////////////////
// // map effect via struct with no extension

pub fun map-foo-struct-nox<a,b,c,d>(
  fac: (a)->c,
  fbd: (b)->d,
  fs: foo-struct-nox<a,b>
): total foo-struct-nox<c,d>
  Foo-struct-nox(
    "map-foo-struct-nox--"++fs.name,
    fn () 
      with handle-map-foo-nox(fac)
      fs.thunk()().fbd
  )

pub fun example-map-foo-struct-nox()
  with handle-foo
  val fsn = Foo-struct-nox("foo", fn()-> {foo(100) ; "blah"})
  val fsn2 = map-foo-struct-nox(int/show, string/to-upper, fsn)
  fsn2.thunk()()
  // val fsn3 = map-foo-struct-nox(string/count, string/to-lower, fsn2)
  // fsn3.thunk()()

////////////////////////////////
// map effect via struct with extension

pub fun map-foo-struct-x<a,b,c,d,e>(
  fac: (a)->c,
  fbd: (b)->d,
  fs: foo-struct-x<a,b,<foo<c>|e>>
): total foo-struct-x<c,d,e>
  Foo-struct-x(
    "map-foo-struct-x--"++fs.name,
    fn ()
      with handle-map-foo-x(fac)
      fs.thunk()().fbd
  )
  
// can't get this to check at all!
// pub fun example-map-foo-struct-x()
//   with handle-foo
//   val fsn: foo-struct-x<int,string,total> = Foo-struct-x("foo", fn()-> {foo(100) ; "blah"})
//   val fsn2: foo-struct-x<string,string,total> = map-foo-struct-x(int/show, string/to-upper, fsn)
//   fsn2.thunk()()
//   // val fsn3 = map-foo-struct-x(string/count, string/count, fsn2)
//   // fsn3.thunk()()
