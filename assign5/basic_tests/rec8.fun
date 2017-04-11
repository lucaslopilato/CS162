-- Result: Cons [ hd = 8, tl = Cons [ hd = 16, tl = Cons [ hd = 24, tl = Nil () ] ] ]
type List = Nil unit | Cons [hd: num, tl: List]

rec mapIntList: ((((num) => num), List) => List) =
  (f: ((num) => num), l: List) =>
    case l of
    | Nil => Nil
    | Cons(m) => Cons([hd = f(m.hd), tl = mapIntList(f, m.tl)]) in
let testList = Cons([hd = 1, tl = Cons([hd = 2, tl = Cons([hd = 3, tl = Nil])])]) in
let mult8 = ((i: num) => i * 8) in
mapIntList(mult8, testList)
