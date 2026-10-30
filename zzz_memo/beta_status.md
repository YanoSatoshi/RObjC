# robjc Alpha Status Report

更新日: 2026-10-28

パッケージバージョン: `0.0.4`（alpha）

組み込みクラス定義のバージョン: 原則 `0.0.4 alpha`

対象ブランチ: `main`

## 現在の位置づけ

`robjc`は、Objective-C/Foundationのオブジェクトモデルに着想を得た、R6ベースのRパッケージである。`RSObject`を基底とするクラス階層、protocol、singleton、scalar、collection、クラス情報を扱うencoder/decoderを実装している。DictionaryとSetの格納部分にはRcpp経由のC++実装を使う。

`DESCRIPTION`のパッケージバージョンは`0.0.4`、起動メッセージは`0.0.4 alpha`である。組み込みのクラスとprotocolのメタデータには原則として`0.0.4 alpha`を記録している。例外として、`RSMutableDataFrame`の明示的なバージョン指定は現在も`0.0.1 alpha`である。

## 0.0.4 alphaまでの主な変更

- Dictionary/Setの格納部分をR実装のhash tableからC++の`std::unordered_map`/`std::unordered_set`へ移行した。Rcpp bridgeを介して`RSConcreteDictionary`/`RSConcreteSet`がnative storageを操作する。hashと同値判定には従来のRオブジェクトの`hash()`/`isEqual()`を使う。
- クラス定義は原則として`RSObjectClass()`、`RSSingletonClass()`、`RSScalarClass()`、`RSProtocolClass()`を利用する。各ファクトリーがR6 generatorを生成し、クラス情報とバージョンを設定する。
- generatorの`robjc_isa`と`private_fields$.__isa`に同じクラス情報を設定し、定義後にメタデータを固定する。`RSObject$initialize()`はこの情報からインスタンスのRクラス属性を設定する。基底の`RSObject`、`RSProtocol`、`RSCopying`は`primary_class_setup.r`で直接設定する。
- singletonファクトリーは定義時に`Class$new()`を呼び、生成したオブジェクトを`Class$instance`とクラス情報の`slot`へ保存する。`$instance`のbindingは固定されるが、`$new()`自体は引き続き呼べる。`RSModule`もこの方式に従う。
- scalarファクトリーは`isNAClass`を設定して固定する。singleton scalarでは、最初のインスタンスを生成する前にこの値を設定する。`RSNull$null`はsingletonインスタンスへの別名である。
- `RSConcreteDictionary`と`RSConcreteSet`は、現在は`class = TRUE`の内部R6クラスである。RObjCの公開クラス階層には属さず、native storageのadapterを務める。
- roxygenの`@include`で読み込み順を指定し、パッケージヘルプからファクトリーAPI、クラス、runtimeの索引へ辿れるようにした。索引のソースは`R/robjc-factories.r`、`R/robjc-classes.r`、`R/robjc-runtime.r`、`R/robjc-package.R`である。
- 独自の`hash()`を持つクラスは、最終的な整数を`robjc_normalize_hash()`へ渡す。Rの整数演算で`NA_integer_`になった場合は`0L`を返す。カスタム`hash()`向けの説明もヘルプとvignetteに追加した。
- インストール後のlazy-loadでgeneratorと`robjc_isa`の個別bindingのロックが失われるため、`.onLoad()`でパッケージ定義済みgeneratorのロックを復元する。元の`lockBinding()`と`lockEnvironment()`も、ソース読み込み時と動的なクラス生成時のために残している。

ファクトリーAPIの`version`引数の既定値は`1.0.0`のため、新しいユーザー定義クラスにパッケージのalphaバージョンが自動適用されるわけではない。

## 実装の構成

```text
R/api_*Class.r              クラス・protocolの定義ファクトリー
R/primary_class_setup.r     基底クラス・protocolの初期設定
R/zzz.r                     パッケージ読み込み時のbindingロック復元
R/RS*.r                     クラス・protocol・内部adapter
R/robjc-*.r                 ヘルプの索引
R/util_io.r                 保存・読み込み用wrapper
R/RcppExports.R             Rcpp bridge
src/RSNativeDictionary.cpp  native Dictionary
src/RSNativeSet.cpp         native Set
src/RSObjectHash.h/.cpp     Rオブジェクトのhash/equality
src/RcppExports.cpp         native registration
inst/testdata/              インストール後も参照するテスト用ファイル
tests/testthat/             testthatテスト
tools/build_html.R          静的HTMLヘルプの再生成
html/                       プロジェクトから開ける静的HTMLヘルプ
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

現行`NAMESPACE`には103個の`export()`がある。主な公開面は、定義ファクトリー、`RSObject`と各種protocol、scalar・collectionクラス、保存・読み込みwrapper、および一部のruntime関数である。一般利用者向けの入口はパッケージヘルプと階層化した索引にまとめ、低レベルのruntime補助関数は内部文書として扱う。

`NAMESPACE`、`man/*.Rd`、`R/RcppExports.R`、`src/RcppExports.cpp`は生成物である。Rの公開APIを変更したら`devtools::document()`、Rcppのexportを変更したら`Rcpp::compileAttributes()`で同期する。

プロジェクト直下の`html/00Index.html`から、静的HTMLヘルプを開ける。R.appの`R CMD INSTALL --html`で作ったページを`tools/build_html.R`で配置し、同一パッケージ内のリンクとaliasをローカル閲覧用に調整している。`html/`は`.Rbuildignore`でパッケージのビルド対象から除外する。roxygenの説明や`man/*.Rd`を変更したら再生成が必要である。

## 確認済みの状態

2026-10-22にR.appのR 4.6.1で`R CMD build`と`R CMD check --no-manual`を実行し、`Status: OK`を確認した。インストール済みパッケージでのbindingロック復元を確認する回帰テストも含む。

このcheckでは、`.Rbuildignore`によりencoder/decoder、instance map、保存・復元に関する6つのテストファイルをビルド対象から外している。したがって、`Status: OK`は保存・復元のテストを含めた全件成功を意味しない。2026-10-28には、静的HTML生成のためR.appから`R CMD INSTALL --html`を実行し、109ページの生成とローカルリンク切れ0件を確認した。HTML追加後の`R CMD check`は未実施である。

## 未確定事項

1. **保存・復元の設計:** R6の保存済みインスタンスを現在のクラス定義へどう移行するか、循環参照と共有参照をどう保持するかを決め、round tripを追加検証する。過去の保存データとの互換性は保証しない。
2. **除外中のテスト:** `.Rbuildignore`から6つのテストファイルの除外を戻す前に、encoder/decoder、instance map、保存・復元の失敗を個別に調査する。除外を戻した状態でbuild/checkを再評価する。
3. **native collectionの契約:** mutable key/member、列挙順、R methodを呼ぶhash/equalityの性能を利用者向けに明示し、必要に応じてbenchmarkを設ける。
4. **APIと説明文書:** alpha期間中はconstructorやprotocolのinterfaceが変更され得る。encoding strategy vignetteも復元方式が固まった時点で同期する。

## 開発時の確認手順

```r
devtools::load_all()
devtools::test()
```

`devtools::test()`はビルドから除外中の6ファイルも実行するため、現時点では上記の`R CMD check`と同じ成功条件にはならない。roxygenを変更した場合は`devtools::document()`、Rcpp exportを変更した場合は`Rcpp::compileAttributes()`を先に実行する。リリース候補ではクリーンインストールと`R CMD check`も確認する。

静的HTMLヘルプを更新する場合は、パッケージルートから次を実行する。このスクリプトは一時ライブラリへの`R CMD INSTALL --html`を使い、プロジェクトの`html/`を生成し直す。

```sh
/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript --vanilla tools/build_html.R
```
