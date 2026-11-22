- Nepura funkcia programlingvo simila al OCaml
- Uzas ŝlosilvortojn prenitajn de la Esperanta vortostoko

エスペラントの語彙をキーワードとして使うプログラミング言語を作ろうとしています。OCamlとF#冗長構文を基にした構文を持ちます。

作者はエスペラントとコンパイラ開発の両方について初学者です。レビューと修正を頂けると幸いです。

TODO:
1. ソースファイルから実行
2. タプル Tuple
3. 判別共用体 Discriminated Union
4. パターンマッチ Pattern Matching

Sintakso:

```
e ::= <id> 
    | <digit> 
    | b 
    | e op e 
    | se e tiam e alie e
    | est <id> = e en e
    | fn <id> -> e
    | e e
    | est rek <id> = fn <id> -> e en e
    | ( e ) 
    | komenc e fin

b ::= vera | falsa

op ::= + | <
```

現時点で書ける式の例です。

- `komenc E fin`はOCamlの`begin E end`，
- `vera` `falsa`は`true` `false`
- `est E1 en E1`は`let E1 in E2`
- `se E1 tiam E2 alie E3`は`if E1 then E2 else E3`
- `rek`は`rec`

にあたります。


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

// <funkcio>
fn x -> x

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