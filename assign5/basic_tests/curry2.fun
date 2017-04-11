-- Result: 252
let foo = ((f: (num) => num) => (n: num) => (m: num) =>  f(n) * m) in
let add1 = (i: num) => i + 1 in
let doStuffWith5 = foo(add1)(5) in
doStuffWith5(42)
