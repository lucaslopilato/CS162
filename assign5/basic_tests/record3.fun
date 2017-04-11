-- Result: Cons [ hd = 1, tl = Cons [ hd = 2, tl = Nil () ] ]
type List = Nil unit | Cons [hd: num, tl: List]

Cons([hd = 1, tl = Cons([hd = 2, tl = Nil])])
