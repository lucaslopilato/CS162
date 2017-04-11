-- Result: 2
type Foo = Bar num | Baz bool

let a = [ x = Bar(3), y = true ] in
rec fib: ((num) => num) =
  ((n: num) => if (n = 0)
               then 0
               else if (n = 1)
               then 1 
               else fib(n - 1) + fib(n - 2)) in
case a.x of
| Bar(i) => fib(i)
| Baz(j) => 42
