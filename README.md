# Lingveto: Esperantobazita Programlingvo

- Nepura funkcia programlingvo simila al OCaml
- Uzas ŝlosilvortojn prenitajn de la Esperanta vortostoko

エスペラントの語彙をキーワードとして使うプログラミング言語の開発を試みています。OCamlとF#冗長構文を基にした構文を持ちます。

作者はエスペラントとコンパイラ開発の両方について初学者です。レビューと修正を求めています。

簡易なインタプリタを書きました。duneプロジェクトをビルドして動かせます。
```
~/lingveto$ dune exec lingveto ~/examples/factorial.txt
720 : ent
```

### TODO
1. ~~ソースファイルから実行~~
2. **esperantigi error-messages**
3. タプル Tuple (paro?)
4. 判別共用体 Discriminated Unions (エスペラント表現わからない)
5. パターンマッチ Pattern Matching (同上)
6. 連結リスト Linked List (ĉeno? listo?)

### エラー一覧 Errors (Eraroj?)
(lexer)
- 不正な文字 unexpected_char
- 範囲外の整数リテラル int_lit_out_of_range
- 予約語 reserved_word

(parser)
- 構文エラー syntax_error

(typing)
- 型が合わない type_mismatch
- occurs check
- 未束縛変数の参照 unbound_variable

### Sintakso

```
e ::= <id> 
    | <digit> 
    | b 
    | -e
    | e op e
    | se e tiam e alie e
    | estu <id> = e en e
    | estu <id> : <ty> = e en e
    | fn <id> -> e
    | fn (<id> : <ty>) -> e
    | e e
    | estu rek <id> = fn <id> -> e en e
    | ( e ) 
    | (e : <ty>)
    | komenc e fin

b ::= vera | falsa

op ::= + | - | * | = | <
```

現時点で書ける式の例です。

- `komenc E fin`はOCamlの`begin E end`，
- `vera` `falsa`は`true` `false`
- `estu E1 en E2`は`let E1 in E2`
- `se E1 tiam E2 alie E3`は`if E1 then E2 else E3`
- `rek`は`rec`

にあたります。

※ 単項マイナスの優先度：乗算`*`より強く，関数適用より弱いです。`-f x`は`-(f x)`，`-2 * 3`は`(-2) * 3`です。


```
// -1
-1

// 2
1 + 1

// 4
(3 - 1) * 2

// 4
komenc (3 - 1) * 2 fin

// vera
1 < 2

// falsa
1 = 2

// 2
estu x = 1 en x + 1

// 100
estu x = 1 en estu x = 100 en x

// 10
estu x = 4 en estu y = 6 en x + y

// 1
se vera tiam 1 alie 0

// 0
se 1 < 1 tiam 1 alie 0

// 0
(fn x -> x + 1) (-1)

// 3
estu f = fn x -> fn y -> x + y en f 1 2

// 55
estu rek f = fn x ->
  se 10 < x
  tiam 0
  alie x + f (x + 1)
en
f 1
```