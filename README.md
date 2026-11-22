- Nepura funkcia programlingvo simila al OCaml
- Uzas ŝlosilvortojn prenitajn de la Esperanta vortostoko

```
// 1
1

// 2
1 + 1

// 2
(1 + 1)

// 2
komenc 1 + 1 fin

// vera
1 < 2

// falsa
1 < 1

// 1
est x = 1 en x

// 2
est x = 1 en x + 1

// 100
est x = 1 en est x = 100 en x

// 10
est x = 4 en est y = 6 en x + y

// 1
se vera tiam 1 alie 0

// 0
se falsa tiam 1 alie 0

// 1
se 1 < 2 tiam 1 alie 0

// 0
se 1 < 1 tiam 1 alie 0

// 2
(fn x -> x + 1) 1

// 3
est f = fn x -> fn y -> x + y en f 1 2

// 55
est rek f = fn x ->
  se 10 < x
  tiam 0
  alie x + f (x + 1)
en
f 1
```