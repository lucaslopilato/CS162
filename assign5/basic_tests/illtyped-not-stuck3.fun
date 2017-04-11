-- Result: 112
type Foo = Bar int | Baz bool

case Bar(56) of
| Bar(x) => x * 2
| Zulu(y) => y
