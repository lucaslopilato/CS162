-- Result: Closure: ((n) ⇒ ((m: num) ⇒ *(f(n), m))), ρ = { f ↦ Closure: ((i) ⇒ +(i, 1)), ρ = { foo ↦ Closure: ((f) ⇒ ((n: num) ⇒ ((m: num) ⇒ *(f(n), m)))), ρ = {  } } }
let foo = ((f: (num) => num) => (n: num) => (m: num) =>  f(n) * m) in
let add1 = (i: num) => i + 1 in
foo(add1)
