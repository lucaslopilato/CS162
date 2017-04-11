-- Result: 126
type Foo = Bar num | Baz bool

let a = [ x = Bar(3), y = true ] in
let b = ((n: num) =>
  let b = 42 in
  if (a.y) then n * b else 0) in
case a.x of
| Bar(i) => b(i)
| Baz(j) => 42
