# robjc Alpha Status Report

更新日: 2026-10-09

パッケージバージョン: `0.0.2`（alpha）

クラス定義のバージョン: `0.0.2 alpha`

対象ブランチ: `Factories--RSClass(),-RSProtocol(),---`

## 現在の位置づけ

`robjc`は、Objective-C/Foundationのオブジェクトモデルに着想を得た、R6ベースのRパッケージである。`RSObject`を基底とするクラス階層、protocol、singleton、scalar、collection、クラス情報を扱うencoder/decoderを実装している。DictionaryとSetの格納部分にはRcpp経由のC++実装を使う。

`DESCRIPTION`のパッケージバージョンは`0.0.2`、起動メッセージは`0.0.2 alpha`である。組み込みのクラスとprotocolのメタデータには`0.0.2 alpha`を記録している。

## 0.0.2 alphaの主な変更

- Dictionary/Setの格納部分をR実装のhash tableからC++の`std::unordered_map`/`std::unordered_set`へ移行した。Rcpp bridgeを介して`RSConcreteDictionary`/`RSConcreteSet`がnative storageを操作する。hashと同値判定には従来のRオブジェクトの`hash()`/`isEqual()`を使う。
- クラス定義は原則として`RSObjectClass()`、`RSSingletonClass()`、`RSScalarClass()`、`RSProtocolClass()`を利用する。各ファクトリーがR6 generatorを生成し、クラス情報とバージョンを設定する。
- generatorの`robjc_isa`と`private_fields$.__isa`に同じクラス情報を設定し、定義後にメタデータを固定する。`RSObject$initialize()`はこの情報からインスタンスのRクラス属性を設定する。基底の`RSObject`、`RSProtocol`、`RSCopying`は`primary_class_setup.r`で直接設定する。
- singletonファクトリーは定義時に`Class$new()`を呼び、生成したオブジェクトを`Class$instance`とクラス情報の`slot`へ保存する。`$instance`のbindingは固定されるが、`$new()`自体は引き続き呼べる。`RSModule`もこの方式に従う。
- scalarファクトリーは`isNAClass`を設定して固定する。singleton scalarでは、最初のインスタンスを生成する前にこの値を設定する。`RSNull$null`はsingletonインスタンスへの別名である。
- `RSConcreteDictionary`と`RSConcreteSet`は、現在は`class = TRUE`の内部R6クラスである。RObjCの公開クラス階層には属さず、native storageのadapterを務める。
- roxygenの`@include`で読み込み順を指定し、パッケージヘルプからファクトリーAPI、クラス、runtimeの索引へ辿れるようにした。索引のソースは`R/robjc-factories.r`、`R/robjc-classes.r`、`R/robjc-runtime.r`、`R/robjc-package.R`である。

クラス定義が明示的に指定するバージョンは`0.0.2 alpha`へ更新済みである。ファクトリーAPIの`version`引数の既定値は`1.0.0`のため、新しいユーザー定義クラスにこのalphaバージョンが自動適用されるわけではない。

## 実装の構成

```text
R/api_*Class.r              クラス・protocolの定義ファクトリー
R/primary_class_setup.r     基底クラス・protocolの初期設定
R/RS*.r                     クラス・protocol・内部adapter
R/robjc-*.r                 ヘルプの索引
R/util_io.r                 保存・読み込み用wrapper
R/RcppExports.R             Rcpp bridge
src/RSNativeDictionary.cpp  native Dictionary
src/RSNativeSet.cpp         native Set
src/RSObjectHash.h/.cpp     Rオブジェクトのhash/equality
src/RcppExports.cpp         native registration
tests/testthat/             testthatテスト
```

### オブジェクトモデル

`RSObject`は`class()`、`superClass()`、`isMemberOf()`、`isKindOf()`、`respondsTo()`、`conformsTo()`などを提供する。クラスの同一性、親クラス、採用protocol、バージョンはgeneratorのクラス情報に保持される。`RSObject`の既定の`isEqual()`と`hash()`はidentityを基準とする。

`RSProtocolClass()`は継承元protocolと必要メソッド名を記録する。protocol適合性はrobjcのruntime情報に基づく。静的なinterface検査ではない。

公開のRObjCクラスを作るファクトリーはR6 generatorを`class = FALSE`、`cloneable = FALSE`で生成する。`RSObject$initialize()`がインスタンスのRクラス属性を設定するため、通常の`inherits()`による型判定も利用できる。内部adapterの`RSConcrete*`は別途`class = TRUE`を使う。

### 値とcollection

`RSScalar`とその具象クラスは、Rの長さ1の値、型別の`NA`、比較、値に基づくhash/equalityを扱う。`RSNull`はRの`NULL`と区別できる実在のオブジェクトであり、Dictionary/Setの公開wrapperは境界で`NULL`を`RSNull$null`へ変換する。

`RSArray`/`RSMutableArray`、`RSDataFrame`/`RSMutableDataFrame`はR側の実装である。`RSDictionary`/`RSMutableDictionary`、`RSSet`/`RSMutableSet`は内部adapterを介してnative tableを使う。外部pointerはadapterのprivate fieldに保持する。

Dictionaryはkeyを挿入時に`copy()`し、valueは参照を保持する。Setはmemberをcopyせず保持する。mutable collectionの`copy()`は独立したnative tableを持つimmutable objectを返し、`mutableCopy()`は独立したmutable objectを返す。格納されたobject自体は原則としてshallowに共有される。immutable collectionの`copy()`は`self`を返す。

native tableは`hash()`と`isEqual()`をlookupに使う。同値のobjectは同じhashを返し、格納中はhash/equalityに影響する状態を変更しない、という契約が必要である。Dictionaryのkey/valueとSetのmemberの列挙順は未規定である。

### 保存・復元

`RSClassEncoder`は`objectsForEncoding()`を走査し、クラスIDとインスタンスの対応を記録する。再訪したobjectは識別して重複走査を避ける。`RSClassDecoder`はクラスIDを解決し、各インスタンスの`resolveClassWith()`を呼ぶ。保存APIは`robjc_save()`、`robjc_save_image()`、`robjc_save_rds()`などである。

native adapterは保存前にRオブジェクトのsnapshotを用意し、読み込み後に無効になったexternal pointerからnative tableを再構築する仕組みを持つ。ただし、**保存・復元全体の仕様と完全なround tripは、このalphaで確定・検証した機能として扱わない**。現在の`resolveClassWith()`は主に`private$.__isa`を差し替える。保存済みR6オブジェクトのメソッドやfieldを現在のgeneratorから作り直す処理ではない。クラス定義の変更後に保存済みインスタンスを読み込む場合の意味、共有参照・循環参照を含む復元は再設計・追加検証が必要である。

## 公開APIと文書

現行`NAMESPACE`には52個の`export()`がある。主な公開面は、定義ファクトリー、`RSObject`と各種protocol、scalar・collectionクラス、保存・読み込みwrapper、および一部のruntime関数である。一般利用者向けの入口はパッケージヘルプと階層化した索引にまとめ、低レベルのruntime補助関数は内部文書として扱う。

`NAMESPACE`、`man/*.Rd`、`R/RcppExports.R`、`src/RcppExports.cpp`は生成物である。Rの公開APIを変更したら`devtools::document()`、Rcppのexportを変更したら`Rcpp::compileAttributes()`で同期する。

## 確認済みの状態

2026-10-09時点で、`tests/testthat/`には154個の`test_that()`ブロックがある。`devtools::test(reporter = "silent")`は失敗0件、skip 0件、warning 0件で完了した。対象にはオブジェクトモデル、値、collection、native adapter、copy semantics、encoder/decoderの基本操作、保存APIのテストが含まれる。

この更新では`R CMD INSTALL`と`R CMD check`を再実行していない。テスト成功は、クリーンインストールやパッケージチェックの成功を意味しない。

## 未確定事項

1. **保存・復元の設計:** R6の保存済みインスタンスを現在のクラス定義へどう移行するか、循環参照と共有参照をどう保持するかを決め、round tripを追加検証する。過去の保存データとの互換性は保証しない。
2. **パッケージチェック:** クリーンな環境でinstall/checkを再実行し、NOTEとWARNINGを現在のコードで評価する。
3. **native collectionの契約:** mutable key/member、列挙順、R methodを呼ぶhash/equalityの性能を利用者向けに明示し、必要に応じてbenchmarkを設ける。
4. **APIと説明文書:** alpha期間中はconstructorやprotocolのinterfaceが変更され得る。encoding strategy vignetteも復元方式が固まった時点で同期する。

## 開発時の確認手順

```r
devtools::load_all()
devtools::test()
```

roxygenを変更した場合は`devtools::document()`、Rcpp exportを変更した場合は`Rcpp::compileAttributes()`を先に実行する。リリース候補ではクリーンインストールと`R CMD check`も確認する。
