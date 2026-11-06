# robjc

[English](README.md)

`robjc` は R6 を基盤とする、Objective-C に着想を得た R のオブジェクトフレームワークです。
クラスファクトリ、プロトコル、スカラーのラッパー、変更可能・変更不可のコレクション、
オブジェクトグラフの保存と復元を提供します。
複数の場所から参照されるドメインオブジェクトを扱い、同一性・等価性・ハッシュの規則を
明示したいアプリケーションを対象としています。

## インストール

開発版は GitHub からインストールできます。CRAN 公開後は
`install.packages("robjc")` でもインストールできます。

```r
install.packages("remotes")  # 未導入の場合
remotes::install_github("YanoSatoshi/RObjC")
```

## 最初の例

コレクションには `robjc` オブジェクトを格納します。通常の R の値はスカラー
オブジェクトで包んでから追加します。Dictionary のキー検索には `hash()` と
`isEqual()` を使います。

```r
library(robjc)

items <- RSMutableArray$new()
items$addObject(RSString$new("Ada"))
items$addObject(RSNumber$new(42L))
items$count()
#> [1] 2

lookup <- RSMutableDictionary$new()
lookup$setObjectForKey(RSNumber$new(42L), RSString$new("answer"))
lookup$objectForKey(RSString$new("answer"))$value()
#> [1] 42

saved_items <- items$copy()  # 変更不可の RSArray
```

Dictionary のキーや Set の要素に独自クラスを使う場合は、整合する `isEqual()` と
`hash()` を実装してください。格納中はハッシュの基になる状態を変更しないでください。
`NULL` は自動変換されないため、null オブジェクトには `RSNull$null` を使います。

## ドキュメント

- [クイックスタート](vignettes_jp/guide_01_QuickStart.Rmd)
- [コレクション](vignettes_jp/guide_07_Collections.Rmd)
- [アーカイブ](vignettes_jp/guide_10_Archiving.Rmd)

不具合や機能の提案は [GitHub Issues](https://github.com/YanoSatoshi/RObjC/issues) にお寄せください。
