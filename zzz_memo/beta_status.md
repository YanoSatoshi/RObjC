# robjc beta status

更新日: 2026-10-30

対象ブランチ: `0.0.5_alpha`

パッケージバージョン: `0.1.0`（起動メッセージと組み込みクラスの表記は `0.1.0 beta`）

## 現在の位置づけ

`robjc` は Objective-C/Foundation に着想を得た R6 ベースのオブジェクトシステム。クラスとプロトコル、singleton、型付き scalar、可変・不変 collection、keyed archive を提供する。数値計算の高速化ではなく、参照を共有するドメインオブジェクトの管理を主な用途とする。

旧 alpha の `RSClassEncoder`、`RSClassDecoder`、`RSClassReference`、`RSInstanceMap`、`RSWrappedClass`、`RSClassRegistry` と `robjc_save*()` / `robjc_read_rds()` 系 API は現行設計から削除した。参照用の旧実装は `prototypes/` に置いている。旧形式との互換性は提供していない。

## 0.1.0 beta の実装

- `RSObjectClass()`、`RSProtocolClass()`、`RSSingletonClass()`、`RSScalarClass()` がクラスとプロトコルの定義入口。generator のクラス情報と必要な binding は固定し、インストール後に失われる binding lock を `.onLoad()` で復元する。
- `$new()` は `robjc_allocate()` で確保したインスタンスの `initialize()` の結果を返す。`robjc_allocate()` は `initialize()` を呼ばず、復元処理で使用する。singleton の `$new()` は既存インスタンスを返す。
- `RSScalar` 系列の通常クラスに `NA` を渡すと対応する共有 NA インスタンスを返す。`RSFactor$new(NA_factor)` は levels と ordered 属性を保持する新しい `RSNAFactor` を返す。`NaN` は通常の scalar として扱う。
- Array、Vector、Set、Dictionary、DataFrame の可変・不変クラスを提供する。Dictionary と Set は Rcpp 経由で native table を使う。`hash()` と `isEqual()` に依存するため、格納中のキー・メンバーの関連状態は安定させる必要がある。
- `RSCoding`、`RSEncoderProtocol`、`RSDecoderProtocol` に基づき、各対応クラスが `encodeWithCoder()` と `initWithCoder()` を実装する。`RSKeyedArchiver$new()$archive(root)` は通常の R list を返し、`saveRDS()` で保存できる。参照 ID はエンコード元オブジェクトのポインターを文字列として記録し、処理中は元オブジェクトを保持する。復元時にその ID をアドレスとして使用しない。list 内の RObjC 参照と共有参照を扱う。
- `RSKeyedUnarchiver$new(archive)` はデータを読み込み、復元は `$decode()` まで行わない。`$showAllClasses()` は自動解決済みのクラスを既定で除いた tibble を返す。`$showAllClasses(remove_default = FALSE)` と `$dictionaryWithAllClasses()` で全クラスを見られる。必要な対応を `$setClasses()` に渡し、`$decode()` で復元する。`$unarchive()` は互換用の別名。
- 組み込みクラスは `.onLoad()` で登録する。拡張パッケージも `.onLoad()` から `robjc_register_class()` / `robjc_register_protocol()` を呼べる。登録表には generator 自体を保持せず、package・固定 ID・namespace binding を記録する。スクリプト内のクラスは手動対応または resolver を使う。拡張パッケージの `.onLoad()` 登録から resolver なしで復元する統合テストがある。

## 文書

プロジェクト直下の `README.md` は英語・日本語を併記する。英語 vignette の `introduction.Rmd`、`robjc_encoding_strategy.Rmd`、`robjc_encoding_use_case.Rmd` は現行 API に合わせた。`robjc_keyed_coding.Rmd` はクラス登録と手動対応の例を含む。日本語版は `vignettes_jp/` にある。

`man/*.Rd` と `NAMESPACE` は roxygen2 の生成物。`html/00Index.html` 以下のローカル HTML ヘルプは `tools/build_html.R` で再生成する。`html/` と `vignettes_jp/`、`zzz_memo/` は `.Rbuildignore` でパッケージのソース配布から除外している。

## 検証結果

- 2026-10-30: R.app の R 4.6.1 で `devtools::test()` が全件通過。`devtools::check(document = FALSE, args = c("--no-manual"))` は ERROR 0、WARNING 0、NOTE 2。NOTE は時刻検証不可と一時ディレクトリの `xcrun_db`。
- 2026-10-30: プロジェクト直下の `robjc_0.1.0.tar.gz` に対する `R CMD check --as-cran` の記録は ERROR 0、WARNING 0、NOTE 1。NOTE は CRAN incoming feasibility の `New submission`。`robjc.Rcheck/00check.log` に記録がある。PDF・HTML マニュアル、testthat、vignette の確認は OK。

## 残る確認事項

1. archive 形式 `robjc-keyed-1` と class ID の長期互換性、異なるクラス版での移行手順を確定する。現時点で旧 alpha 形式との互換性はない。
2. 循環参照、複雑な list、singleton、外部パッケージの更新やアンロードを含む復元経路を継続して検証する。
3. Dictionary/Set の mutable key・member の契約、列挙順、hash/equality の性能を利用者向けに整理する。
4. 公開前に README、英語・日本語 vignette、生成済み HTML ヘルプの整合性を再確認する。

## 開発時の確認

R.app の R を使用する。ソースを変更した場合の基本的な確認は次の通り。

```sh
/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript --vanilla -e 'roxygen2::roxygenise(); devtools::test()'
/Library/Frameworks/R.framework/Versions/Current/Resources/bin/R CMD build .
/Library/Frameworks/R.framework/Versions/Current/Resources/bin/R CMD check --as-cran robjc_0.1.0.tar.gz
```

HTML ヘルプの再生成は次の通り。

```sh
/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript --vanilla tools/build_html.R
```
