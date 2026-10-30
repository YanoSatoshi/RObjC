# robjc

**English** · [日本語](#日本語)

## English

`robjc` is an Objective-C-inspired object system for R, built on R6. It provides class and protocol factories, scalar objects with typed missing values, mutable and immutable collections, and keyed archives for restoring object state.

The current package version is **0.1.0 beta**. The archive format and public APIs may change during beta development.

### Quick start

```r
library(robjc)

number <- RSNumber$new(42L)
number$value()                         # 42L
identical(RSNumber$new(NA_real_), RSNumber$na)  # TRUE

# Archives are ordinary R lists and can be saved with saveRDS().
archive <- RSKeyedArchiver$new()$archive(list(number, number))
decoder <- RSKeyedUnarchiver$new(archive)
decoder$showAllClasses()               # Built-in classes resolve automatically
restored <- decoder$decode()
identical(restored[[1L]], restored[[2L]])       # TRUE: shared reference
```

Create an object class with `RSObjectClass()`, a protocol with `RSProtocolClass()`, a singleton with `RSSingletonClass()`, or a scalar class and its NA companion with `RSScalarClass()`. Scalar `$new(NA)` returns the class's shared NA object. `RSFactor$new()` instead creates an `RSNAFactor` that preserves levels and ordering.

Collections include arrays, vectors, sets, dictionaries, and data frames. Dictionary keys must have stable `hash()` and `isEqual()` behavior while stored; Set members also use these methods for lookup. Collection enumeration order is unspecified where the underlying set or dictionary is unordered.

### Keyed archives and extension packages

Objects encode logical fields through `encodeWithCoder()` and restore them through `initWithCoder()`. `RSKeyedUnarchiver$new(archive)` loads the data without decoding it. Inspect classes with `$showAllClasses()` or `$dictionaryWithAllClasses()`, supply any missing mappings with `$setClasses(mapping)`, then call `$decode()`.

Built-in classes are registered automatically. An extension package can register its own class in `.onLoad()` with `robjc_register_class(package, id, binding, generator)`. Classes defined in scripts can be mapped manually before decoding. See [Keyed archives](vignettes/robjc_keyed_coding.Rmd) for both examples.

### Documentation and checks

The [class factory guide](vignettes/robjc_class_factories.Rmd), [scalar guide](vignettes/robjc_scalar_values.Rmd), and [other vignettes](vignettes/) cover the APIs in more detail. Japanese vignettes are in [vignettes_jp/](vignettes_jp/). Local HTML reference pages are available at [html/00Index.html](html/00Index.html).

The latest recorded `R CMD check --as-cran robjc_0.1.0.tar.gz` on R.app R 4.6.1 completed with **0 errors, 0 warnings, and 1 NOTE** (`New submission` in CRAN incoming feasibility). See [beta status](zzz_memo/beta_status.md) for the current scope and open work.

## 日本語

`robjc` は Objective-C に着想を得た、R6 ベースの R 用オブジェクトシステムです。クラスとプロトコルの定義ファクトリー、型付き欠損値を扱う scalar、可変・不変のコレクション、オブジェクトの状態を復元する keyed archive を提供します。

現在のバージョンは **0.1.0 beta** です。ベータ期間中はアーカイブ形式や公開 API が変更される可能性があります。

### 基本的な使い方

```r
library(robjc)

number <- RSNumber$new(42L)
number$value()                         # 42L
identical(RSNumber$new(NA_real_), RSNumber$na)  # TRUE

# アーカイブは通常の R の list で、saveRDS() に保存できます。
archive <- RSKeyedArchiver$new()$archive(list(number, number))
decoder <- RSKeyedUnarchiver$new(archive)
decoder$showAllClasses()               # 組み込みクラスは自動で解決
restored <- decoder$decode()
identical(restored[[1L]], restored[[2L]])       # TRUE: 共有参照を維持
```

通常のオブジェクトクラスは `RSObjectClass()`、プロトコルは `RSProtocolClass()`、singleton は `RSSingletonClass()`、scalar とその NA クラスは `RSScalarClass()` で定義します。scalar の `$new(NA)` は共有 NA インスタンスを返します。`RSFactor$new()` は levels と ordered 属性を保つ `RSNAFactor` を新しく生成します。

コレクションには Array、Vector、Set、Dictionary、DataFrame があります。Dictionary のキーを格納中は、`hash()` と `isEqual()` に影響する状態を変更しないでください。Set のメンバーも検索にこれらのメソッドを使います。Set と Dictionary の列挙順は規定されていません。

### Keyed archive と拡張パッケージ

オブジェクトは `encodeWithCoder()` で論理的な状態を保存し、`initWithCoder()` で復元します。`RSKeyedUnarchiver$new(archive)` はデータの読み込みのみを行います。`$showAllClasses()` または `$dictionaryWithAllClasses()` でクラスを確認し、未解決の対応を `$setClasses(mapping)` で設定してから `$decode()` を呼びます。

組み込みクラスは自動登録されます。拡張パッケージは `.onLoad()` から `robjc_register_class(package, id, binding, generator)` を呼んで独自クラスを登録できます。スクリプト内で定義したクラスは、復元前に手動で対応付けます。両方の例は [Keyed archives](vignettes/robjc_keyed_coding.Rmd) を参照してください。

### 文書と確認結果

[クラスファクトリー](vignettes_jp/robjc_class_factories.Rmd)、[scalar と欠損値](vignettes_jp/robjc_scalar_values.Rmd)、[その他の日本語 vignette](vignettes_jp/) に詳しい説明があります。ローカルの HTML リファレンスは [html/00Index.html](html/00Index.html) から開けます。

記録が残る最新の `R CMD check --as-cran robjc_0.1.0.tar.gz` は、R.app の R 4.6.1 で **ERROR 0、WARNING 0、NOTE 1** でした。NOTE は CRAN incoming feasibility の `New submission` です。現状と残課題は [beta status](zzz_memo/beta_status.md) にまとめています。
