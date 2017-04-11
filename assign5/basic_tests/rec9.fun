-- Result: [ testList1Result = true, testList2Result = false ]
type List = Nil unit | Cons [hd: num, tl: List]

rec foldLeft: ((bool, num) => bool) => (bool) => (List) => bool =
  (f: (bool, num) => bool) => (accum: bool) => (l: List) =>
  case l of
  | Nil => accum
  | Cons(m) => foldLeft(f)(f(accum, m.hd))(m.tl) in
let even = (n: num) => ((2 * (n / 2)) = n) in
let allEven = foldLeft((b: bool, n: num) => b && even(n))(true) in
let testList1 = Cons([hd = 2, tl = Cons([hd = 8, tl = Cons([hd = 24, tl = Nil])])]) in
let testList2 = Cons([hd = 2, tl = Cons([hd = 8, tl = Cons([hd = 23, tl = Nil])])]) in
let result = [testList1Result = allEven(testList1), testList2Result = allEven(testList2)] in
result
