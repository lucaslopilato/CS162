-- Result: 92
rec x: ((num) => num) => (num) => (num) => num =
  (f: (num) => num) => (i: num) => (j: num) =>
  f(i) + j
in let mult10 = ((i: num) => i * 10)
in x(mult10)(5)(42)
