-- Result: 3
type List = Nil unit | Cons [hd: num, tl: List]

rec length: ((List) => num) = ((l: List) =>
  case l of
  | Nil => 0
  | Cons(m) => 1 + length(m.tl)) in
let testList = Cons([hd = 10, tl = Cons([hd = 11, tl = Cons([hd = 12, tl = Nil])])]) in
length(testList)
