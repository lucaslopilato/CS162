-- Program is stuck
((x: [a: int, b: int], y: bool) => if (y) then x.a * x.b else 42)([a=4, b=10])
