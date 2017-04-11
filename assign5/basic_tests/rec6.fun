-- Result: 480
rec fact: ((num) => num) =
  ((n: num) => if (n = 0)
               then 1
               else fact(n-1) * n) in
let a = [x = 4, y = true] in
fact(5) * a.x
