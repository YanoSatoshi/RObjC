# Validation audit

開始日: 2026-10-14

## 進め方

- 依存関係の下から確認する。各段階で、入力検証、異種型の扱い、`isEqual()`・`hash()`・`compare()` の整合性、コピーと保持期間を必要に応じて調べる。
- 要判断箇所にはソース中へ `#! validation check`、`#todo 確認内容`、`#? 提案コード` のコメントを置く。提案コードは実行せず、判断後に修正する。
- `確認完了` は、契約を確認し、必要な修正と関連テストを終えた状態を指す。既存テストが通るだけでは完了にしない。
- 発見ごとに再現条件と判断結果を記録する。実装後は対応するソースの確認コメントを削除し、記録を `完了` に更新する。
- この記録は現行の作業ツリーを対象とする。既存の未コミット変更は保持する。

## 確認順序と進捗

1. [~] **契約と現状** — 公開 API とテストの範囲を列挙し、異種型の `isEqual()`、比較不能な `compare()`、等価なオブジェクトの `hash()` の期待を明記する。Scalar はキー等価性・三値順序・base R 値比較を分離した。残りの契約は確認中。
2. [~] **ランタイムと基底クラス** — `runtime_*.r`、クラス・protocol ファクトリー、`RSObject` の型判定、継承、メソッド照会、基底の等価性を確認する。V-001〜009 は完了。残りのランタイムは確認中。
3. [~] **スカラー系** — `RSScalar`、`RSNull`、具象クラスと NA クラス。型をまたぐ等価性、比較、hash の対応を確認する。V-010〜018 は完了。残りの型と hash の契約を確認中。
4. [~] **ネイティブ層** — `RSObjectHash`、`RSNativeDictionary`、`RSNativeSet`、Rcpp bridge。hash/equality の契約、参照保持、列挙と複製を確認する。V-019 は見送り、V-020 は完了。残りを確認中。
5. [~] **コレクション** — Array、Vector、Set、Dictionary と各 mutable/immutable クラス。検索、変更、コピー、不変性、`NULL` の扱いを確認する。V-021〜029 は完了、DataFrame の V-030 は棚上げ。他のコレクションを確認中。
6. [~] **上位機能** — class registry は確認対象。Encoder／Decoder、`R/util_io.r` の encode／decode 関連、Archiving strategy はユーザー指示により棚上げ。
7. [~] **公開入口と文書** — `api_*.r` の入力検証とクラス側の契約を照合し、テスト・Rd・使用例を確認する。

進捗記号: `[ ]` 未着手、`[~]` 確認中または判断待ち、`[x]` 確認完了。

## 発見と判断

| ID | 状態 | 場所 | 再現・確認事項 | 提案 |
| --- | --- | --- | --- | --- |
| V-001 | 完了（削除） | `R/runtime_robjc.r` の `robjc_isMetaClasses()` | 混在入力でエラーになり、有効な generator だけでも誤判定していた。内部呼び出し元なし。 | 単要素用 `robjc_isMetaClass()` だけを残し、複数要素用関数を削除。 |
| V-002 | 完了（削除） | 同ファイルの `robjc_isClasses()` | 混在入力でエラー。内部呼び出し元なし。 | 単要素用 `robjc_isClass()` だけを残し、複数要素用関数を削除。 |
| V-003 | 完了（削除） | 同ファイルの `robjc_isProtocols()` | 混在入力でエラー。内部呼び出し元なし。 | 単要素用 `robjc_isProtocol()` だけを残し、複数要素用関数を削除。 |
| V-004 | 完了 | `R/runtime_robjc.r` の `robjc_make_singleton_isa()` | 戻り値の `isa` は関数内で定義されていなかった。試験用クラスで、戻り値と `r6generator$robjc_isa` は `identical()` で `FALSE`。 | `invisible(r6generator$robjc_isa)` に修正し、戻り値のテストを追加。 |
| V-005 | 完了 | `R/RSObject.r` の `respondsTo()` | `NULL` や複数のメソッド名で、引数エラーではなく `exists()` 由来のエラーになっていた。 | `method` を長さ 1 の非空・非 NA 文字列として確認し、テストと Rd を更新。 |
| V-006 | 完了 | `R/RSObject.r` の `respondsTo()` | スカラーインスタンスの `respondsTo(".comparable")` は `TRUE` だが、外から `$.comparable()` は呼べずエラーになっていた。 | 公開メソッドだけを照会するよう修正。テストと Rd を更新。 |
| V-007 | 完了 | `R/runtime_robjc.r` の `robjc_make_isa()` と `RSProtocolClass()` | `RSObjectClass(..., protocols = list(1L))` は作成できるが、インスタンスの `conformsTo()` で `$ operator is invalid for atomic vectors` になっていた。 | `robjc_make_isa()` で protocol list を検証。`RSProtocolClass()` では親 protocol の読取前にも検証する。 |
| V-008 | 完了 | `R/runtime_robjc.r` の `robjc_make_isa()` | `RSObjectClass(..., version = NA_character_)` と `version = ""` が受理されていた。 | `robjc_make_isa()` で version の型・長さ・欠損・空文字・`_` を検証。4 種のファクトリーでテスト。 |
| V-009 | 完了 | `R/runtime_robjc.r` の `robjc_isRobjcObject()` | 通常の環境でも `.__enclos_env__$private <- 1L` を持つと、`FALSE` ではなく `$ operator is invalid for atomic vectors` になっていた。 | `.__enclos_env__` と `private` が環境か確かめ、`isa` も環境であることを確認してから判定する。 |
| V-010 | 完了 | `R/RSFactor.r` の `compare()` | 同じ文字列値で `RSString$isEqual(RSFactor)` は `TRUE`、逆向きは `attempt to apply non-function`。さらに `factor("a", levels=c("a","b"))` と `factor("x", levels=c("x","y"))` は `isEqual() == TRUE` だが hash は異なる。同じ文字列でも levels が違うと非等価になりうる。 | 異種比較と levels の異なる factor 同士は文字列値で比較し、levels と ordered 属性が同じ factor 同士に限り因子順序で比較する。全体の順序の推移律は別途検討が必要。 |
| V-011 | 完了 | `R/RSFactor.r` の `compare()` | levels が同じ factor 同士では `na.last = 1L` を受理し、`NA` や複数値は `if()` 由来のエラーになる。親の `RSScalar$compare()` はすべて引数エラーにしていた。 | `as.logical()` で正規化し、長さ1の非欠損 logical だけ許す。`RSScalar`・`RSFactor` で統一する。後に `RSRaw$compare()` は base R のソート不能に合わせてエラー専用へ変更。 |
| V-012 | 完了 | `R/RSDate.r` の `hash()` とスカラー系の hash | `as.Date(2147483648, origin="1970-01-01")` は作れるが、hash は警告付きの `NA_integer_`。`RSDate$na$hash()` も `NA_integer_`。 | 非欠損の日数を `RSHashOperator$instance$hashForScalar()` に渡す。スカラー系の NA 値は `0L` とし、Date と DateTime の独自 hash も揃える。通常の日付の hash 値は変わる。 |
| V-013 | 完了（方針変更） | `R/RSScalar.r` の `initialize()`・`.comparable()` | 直接生成した `RSScalar` と具象クラスを比較すると型変換で異種値が等価になり、hash が異なる場合もあった。 | `RSScalar` は抽象クラスとし、`initialize()` の先頭で直接生成を拒否する。直接生成向けに追加した値の種類チェックと Date/POSIXt 専用拒否は撤去。具象クラス間は従来のクラス関係で比較する。 |
| V-014 | 完了 | `R/RSNull.r` の継承 `compare()` | `RSNull$null$isEqual(RSNull$null)` は `TRUE`、hash は `0L` だが、`compare()` は `subscript out of bounds` になる。 | `RSNull` で `compare()` をオーバーライドし、singleton 自身は `0L`、それ以外は比較不能エラーにした。`na.last` の検証は基底クラスに合わせた。 |
| V-015 | 完了 | `R/RSNull.r` の継承 `description()` | `RSNull$null$description()` は `sprintf()` に `NULL` を渡すため `character(0)` になる。 | `RSNull` 固有の `description()` を定義し、クラス名・ポインタ・`NULL` を含む長さ 1 の文字列を返す。 |
| V-016 | 完了 | `R/RSFactor.r` の `compare()` | 同じ levels の factor 同士の比較は `sign()` の結果で `double` を返す。他の `compare()` と Rd は `-1L`・`0L`・`1L` を想定。 | `sign()` の結果を `as.integer()` で包み、戻り値の型を統一した。 |
| V-017 | 完了（契約分離） | `R/RSFactor.r` の level 順序と `RSString` との比較 | levels が `c("z", "a")` の `Fz < Fa`、一方 `Fa < Sz` だが `Sz == Fz`。等価な値を置換すると比較結果が変わり、推移律も崩れる。 | `isEqual()` はラベルによるキー等価性とし、`compare()` は factor/string 間に三値順序を与えない。base R の演算結果は `nativeCompare()` で得る。 |
| V-018 | 完了（V-017 と統合） | `R/RSFactor.r` の異なる levels 間の比較 | 同じクラスでも levels が `c("z", "a")` の `Fz < Fa`、levels が `c("a", "z")` の `Gz` に対して `Fa < Gz` かつ `Gz == Fz`。 | `compare()` は同じ levels の factor 同士で level 順序を返す。ordered と unordered の両方を対象とし、異なる levels は順序不能。キー等価性と base R 演算はそれぞれ別 API で扱う。 |
| V-019 | 見送り | `R/RSConcreteSet.r`、`R/RSConcreteDictionary.r` と両 Native bridge | `RSConcreteSet$new(.native_ptr = dictionary_create())` がエラーなく成立する。adapter は `externalptr` だけを検査する。`typeof()` は R の内部型を返し、個々の external pointer に独自の型名は設定できない。 | `.native_ptr` は文書上 internal 引数であり、誤った種類の pointer を直接渡す使い方は対象外とする。実装は変更しない。 |
| V-020 | 完了（責務変更） | `R/RSMutableDictionary.r` と `R/RSMutableSet.r` の挿入 | インストール済み旧版では 2 回目の `hash()` 失敗で `set()` が失敗し `count()==0`。その後、検証場所を見直した。 | 公開クラスの挿入経路で、コピー後のキー／正規化後のメンバーの `hash()` を native adapter 呼出し直前に実行し、長さ 1 の非欠損 integer を要求する。Concrete adapter は bridge に専念し、C++ は正常な hash の継続を前提として例外処理を追加しない。事前検証後に状態依存で `hash()` が失敗するケースは対象外。 |
| V-021 | 完了 | `R/RSMutableArray.r` の追加・挿入・置換 | `initialize()` は RObjC object 以外を拒否するが、`addObject(1L)`、`insertObjectAtIndex(1L, 1L)`、`replaceObjectAtIndex(1L, 1L)` は通る。格納後の `hash()` は `$ operator is invalid for atomic vectors` で失敗する。 | 3 つの変更経路で `robjc_isRobjcObject(obj)` を確認し、初期化時と同じ要素契約を保つ。テストと文書を更新。 |
| V-022 | 完了 | `R/RSMutableArray.r` の `compare()` | `list("a", "b")` と `list("b", "a")` は `isEqual() == FALSE` でも集約 hash が共に `3L` のため `compare() == 0`。単一要素の `"z"` と `"aa"` は要素の比較が `1L` なのに配列の比較は `-1`。 | 要素の `compare()` を先頭から呼ぶ辞書式順序に変更。同順なら長さで決め、`RSArray` と `RSMutableArray` の相互比較、異種入力、`na.last` を確認。文書とテストを更新。 |
| V-023 | 完了 | `R/RSMutableArray.r` の `isEqual()` | 同内容の `RSArray` と `RSMutableArray` は両方向の `compare() == 0L`、hash も同じだが `isEqual()` は `FALSE`。`isEqual(1L)` は `$ operator is invalid for atomic vectors` で失敗する。 | 配列型を `isKindOf(RSMutableArray)` で判定し、同じ長さ・対応する要素の等価性で評価する。異種入力と不定の要素等価性は `FALSE` にする。 |
| V-024 | 完了 | `R/RSMutableArray.r` の index 操作 | `index = 1.9` は読取・置換・削除で1番目を対象にする。挿入では `[a,b]` が `[a,x,a]` となり、`b` が消えて `a` が重複した。 | 4操作で共通の private 検証を使い、長さ1・有限・整数値・許容範囲の numeric index のみ受理する。テストと文書を更新。 |
| V-025 | 完了 | `R/RSMutableArray.r` と `man/RSMutableArray.Rd` の冒頭説明 | 文書は挿入時に要素を deep-copy し構造が分離されると記す。一方、初期化した親配列と `copy()` 後の配列は、元の可変要素と同じオブジェクトを参照し、その変更が両方に反映された。後半の copy 文書も shallow copy と記している。 | 冒頭説明を実装に合わせ、要素参照を保持し、配列のコピーは新しいコンテナと共有要素を持つことを明記した。実装は変更しない。 |
| V-026 | 完了 | `R/RSArray.r` の変更禁止 override | 親の `insertObjectAtIndex(obj, index)` と `replaceObjectAtIndex(obj, index)` に対し子は `idx` を使い、`removeObjectAtIndex(index)` に対し子は `(obj, idx)` を使う。3 メソッドとも `index=` で呼ぶと変更禁止エラーでなく `unused argument` になる。 | override の仮引数名と数を親に揃え、名前付き呼出しでも変更禁止エラーを返す。対応する Rd とテストも更新。 |
| V-027 | 完了 | `R/RSKeyValuePair.r` の `setKey()` と `isEqual()` | `initialize()` は key に RObjC object を要求するが、`setKey(1L)` が通り、以後の `hash()` は `$ operator is invalid for atomic vectors`。`isEqual(1L)` も同じ `$` エラー。 | `setKey()` は constructor と同じ key 型を要求し、`isEqual()` は非 RObjC 入力に `FALSE` を返す。 |
| V-028 | 完了 | `R/RSMutableDataFrame.r` と継承する `RSDataFrame` の `isEqual()` | 型判定が `robjc_isRobjcObject(other) || ...` と逆で、DataFrame の自分自身との比較も `FALSE`。非 RObjC 入力では `$` エラーになる。 | 条件を反転して正しい DataFrame 同士の比較を有効にし、非 RObjC 入力には `FALSE` を返す。 |
| V-029 | 完了 | `R/RSMutableDataFrame.r` の `hash()` | `RSMutableDataFrame$new(data.frame(x=1L))$hash()` が `attempt to apply non-function`。`RSHashOperator$computeHashCodeWithString` は generator に存在せず、メソッドは singleton instance にある。 | `RSHashOperator$instance$computeHashCodeWithString(hash_basis)` を呼ぶ。hash と等価性の整合は別途確認する。 |
| V-030 | 棚上げ | `R/RSMutableDataFrame.r` の `isEqual()` と `hash()` | 同じ表で `key_columns="x"` と `"y"` は `isEqual() == TRUE` でも hash が異なる。さらに `x=1` と `x=1+1e-8` は `all.equal()` により等価・`identical()` では非等価だが hash は異なる。`x=1`, `1+1e-8`, `1+2e-8` では現 `isEqual()` が A=B、B=C、A≠C と非推移的。 | `all.equal()` をキー等価性に使わず、`identical(df0, df1)` を軸にする。`key_columns` をオブジェクトの同一性に含めるかを決め、同じ正規化契約に沿って hash を再設計する。ユーザー指示により現状の課題コメントを残して実装は保留。 |
| V-031 | 完了 | `R/RSMutableVector.r` の `isEqual()` と `compare()` | `x <- RSMutableVector$new(c(1L, 2L)); y <- x$copy()` で両方向の `isEqual()` は `FALSE`、`x$compare(y)` はクラス相違エラー。一方、両者の hash は同じ。 | `other$isMemberOf(self$class())` を両メソッドで `other$isKindOf(RSMutableVector)` に変更し、mutable/immutable 間も内容で等価・順序比較する。 |
| V-032 | 完了 | `R/RSMutableVector.r` の `hashesOfValues()` と `R/RSHashOperator.r` の `hashesOfEachObject()` | 空 Vector と空 Array の要素数は 0 だが、各要素の hash を返すメソッドはいずれも `0L`（長さ 1）を返す。集約 hash の値は変わらないが、戻り値の長さが要素数と食い違う。 | 空入力では `integer()` を返す。集約 `hash()` は `Reduce(..., init = 0L)` により引き続き `0L` を返す。 |
| V-033 | 見送り（前提を確認） | `R/RSHashOperator.r` の `areEqualSets()` | 重複を含むリストを直接渡すと `list(Aa,Aa)` と `list(Aa,BB)` が `TRUE` になる。`"Aa"` と `"BB"` は hash がともに `2112` だが非等価。 | 重複のない集合を表すリスト同士なら、要素数が等しく、左の各要素が右に一致するだけで十分。`RSMutableSet` は重複を格納せず、この関数の内部呼び出し元もないため、実装変更を見送る。直接渡すリストに重複がある場合の結果は集合比較として保証しない。 |
| V-034 | 完了 | `R/util_io.r` の `get_standard_robjc_classes()` | 文書は `RSObject` 継承クラスを列挙し、`RSArray` を例示するが、実装は直接の親だけを調べる。実測で `RSArray`、`RSSet`、`RSVector` が一覧から漏れ、`.standard_robjc_class_dictionary()` にも入らない。 | 継承鎖に `RSObject` を含む exported RObjC class を選ぶ。間接継承クラスも自動解決 map に含める。 |
| V-035 | 完了 | `R/util_io.r` の `get_standard_robjc_classes(FALSE)` と `.standard_robjc_class_dictionary()` | 文書はクラス名をキーとする generator の名前付きリストを約束するが、実際は名前のない二重リスト。`get_standard_robjc_classes(FALSE)[["RSArray"]]` は `NULL`。内部辞書の生成側も二重リストを前提にしている。 | 公開関数を名前付き generator list にし、内部辞書の生成側では余分な `[[1L]]` の取り出しをやめる。 |
| V-036 | 完了 | `R/RSClassDecoder.r` の `resolveClassesWithDictionary()` | `class_map = NULL` や `list()` は `attempt to apply non-function`、`1L` は `$ operator is invalid for atomic vectors`。`class_map$isKindOf()` を入力検証前に呼ぶため、意図した辞書型エラーにならない。 | `robjc_isRobjcObject(class_map)` を先に確かめ、`RSMutableDictionary` 系だけを受理する。誤入力は明示的な引数エラーにする。 |
| V-037 | 完了 | `R/RSClassDecoder.r` の `loadEncodedData()` | `structure(list(), class="RSMutableDictionary")` を受理して `class_store` を置換するが、続く `encodedClasses()` は `attempt to apply non-function`。`inherits()` だけでは RObjC の辞書を確認できない。 | `robjc_isRobjcObject(encoded_data)` と辞書クラス属性を読み込み前に確認し、偽装 class 属性の入力を拒否する。RDS 復元後は generator 参照が異なるため `isKindOf()` に依存しない。 |
| V-038 | 完了 | `R/RSClassDecoder.r` の `resolveClassesWithDictionary()` | 2 クラスの encoded data と、片方のみの class_map を渡すと `Unresolved classIDs` で停止する前に、解決できた方のインスタンスの class が置換される。失敗後にオブジェクトが一部だけ更新される。 | 全 class ID の対応を先に取得し、未解決 ID をまとめてエラーにする。すべて解決できた場合にのみ `resolveClassWith()` を実行する。 |
| V-039 | 完了 | `R/RSClassDecoder.r` の `resolveClassesWithDictionary()` | 2 クラスの class_map で前の対応を正しい `RSWrappedClass`、後の対応を `RSString` にすると、`unresolved` は空のまま `attempt to apply non-function`。先のインスタンスの class は更新済み。 | ユーザー方針に従い、`class_map` を受け取った直後に全項目のキー型・値型・class ID の一致を検証する。各インスタンスとの互換性検査は `resolveClassWith()` に委ねる。関連文書の値型も訂正する。 |
| V-040 | 完了 | `R/RSClassEncoder.r` の `encodeObject()` | 親が不正な子を返すと再帰処理でエラーになるが、親の object ID は `visited_env` に残る。子を修正して同じ親を再実行しても訪問済み判定で即時終了し、子クラスは `class_store` に追加されない。 | 失敗した親だけを戻すと成功済みの子・クラスが途中状態で残る。再帰をローカル関数にまとめ、外側の `tryCatch()` で失敗時に `refresh()` して encoder 全体を空にする。再試行では全ルートを再投入する。 |
| V-041 | 完了 | `R/RSClassEncoder.r` の `encodingContainer()` と `lockInstance()` | 凍結後の `encodingContainer()` が新しいクラスを登録し、`lockedData()$count()` が 1 から 2 に増える。`class_store` は mutable のままで、`lockInstance()` 再呼出しは `RSInstanceMap` の警告を出す。 | 凍結中の `encodingContainer()` を拒否し、`lockInstance()` で class_store 自体を immutable コピーに置き換えた。再呼出しは既存の frozen store を返す。公開フィールド `class_store` 自体への再代入はこの修正の対象外。 |
| V-042 | 完了（暫定処置） | `R/util_io.r` の `robjc_load()` | `base::load()` の仮引数は `file, envir, verbose, ...` で `hookFunction` はない。旧実装の `robjc_load(file)` は既定の `hookFunction = NULL` を渡して `unused argument` で停止した。 | ユーザーが `hookFunction` 仮引数をコメントアウトし、`base::load()` への転送を停止。用途が不明なためコメントは残す。公開文書から無効な引数を外し、`...` は現状どおり未使用と明記。 |
| V-043 | 完了 | `R/util_io.r` の `robjc_load()` | `robjc_save()` が保存するメタデータ名は `.__rs_encoded_data__`。実測の `base::load()` 戻り値も同じだが、旧条件は先頭の点がない `__rs_encoded_data__` を調べていた。 | ユーザーが条件を `".__rs_encoded_data__" %in% loaded_names` に修正。保存・復元の往復で Decoder の実行とメタデータ除去を確認。 |
| V-044 | 完了 | `R/util_io.r` の `robjc_encode_and_save()` | `robjc_save_image()` に `.__rs_encoded_data__ = "user value"` を含む環境を渡すと、保存後は同名の内部 `RSDictionary` に置き換わる。`robjc_save()` でも同じ内部ヘルパーを通る。利用者の変数が無警告で失われる。 | encode より前に `names(objects)` の予約名を確認し、明示的なエラーで保存を拒否する。両公開入口の回帰テストと文書を追加。 |
| V-045 | 棚上げ | `R/util_io.r` の `robjc_save()` | `eval.promises = FALSE` を指定しても `mget()` が promise を強制評価する。副作用を持つ delayed binding で実測すると `robjc_save()` は副作用あり、`base::save(..., eval.promises = FALSE)` は副作用なし。`precheck = FALSE` も内部の `mget()` により効果がない。 | エンコード前に対象値を取得する設計ではこれらの base::save オプションをそのまま再現できない。ユーザー指示により util_io 全体の検証を棚上げし、既存の意図コメントを残す。 |
| V-046 | 保留（意図を保持） | `R/util_io.r` の `robjc_read_rds()` と `robjc_load()` | 文書は未解決の独自 class ID に警告を出すと記すが、両入口で `resolveClassesWithDictionary()` が先に `Unresolved classIDs` エラーを出す。後続の警告コードには到達しない。独自クラスを保存して `class_map` なしで読むと両方で再現。 | ユーザーが `robjc_read_rds()` の提案コメントを手動で整理。save/load の更新計画があるため、警告処理とその意図を両入口で保持し、今は動作を変更しない。 |
| V-047 | 完了 | `R/util_io.r` の `robjc_read_rds()` と `robjc_load()` の `class_map` 検証 | 空の `RSMutableDictionary` を `class_map` に渡すと両入口が `inherits(class_map, "RSDictionary") is not TRUE` で停止する。一方、Decoder と辞書結合処理は `RSMutableDictionary` を受理する。`RSDictionary` はそのサブクラス。逆に class 属性だけを偽装したリストは入口の判定を通過する。 | 両入口を `robjc_isRobjcObject(class_map) && class_map$isKindOf(RSMutableDictionary)` に揃え、可変・不変の両辞書を受理。関連文書も両型を記載。ユーザー指示によりテスト追加なし。 |
| V-048 | 棚上げ | `R/util_io.r` の `robjc_encode_and_save()` | `list(inner = RSString_withValue("x"))` を名前付き root として `robjc_save()` → `robjc_load()` すると、内側の値は読めるが `inner$isKindOf(RSString)` は `FALSE`。トップレベルの list は RObjC ではないため encoder が中身を探索せず、class metadata が保存されない。 | ユーザーは利用者の公開入口で止める方針を示した。その後、`util_io.r` 全体を棚上げしたため、現状のコードと提案コメントを保持して実装を保留する。 |
| V-049 | 完了 | `R/RSDate.r` の `initialize()` と `RSDate_withDate()` | `structure("x", class = "Date")` は長さ 1 で `inherits(..., "Date")` を満たし、直接生成と便利コンストラクタの両方で受理される。結果の `nativeValue()` は character、日数への数値変換は `NA`。 | `RSDate$initialize()` で class 属性に加えて内部型が integer/double であることを検証し、直接生成と便利コンストラクタの両方の不変条件を守る。文書と回帰テストを更新。 |
| V-050 | 完了 | `R/RSDateTime.r` の `initialize()` と `RSDateTime_withDateTime()` | `structure("x", class = c("POSIXct", "POSIXt"))` が直接生成と便利コンストラクタの両方を通り、`nativeValue()` は character のままになる。`inherits(val, "POSIXt")` と `as.POSIXct(val)` だけでは数値時刻を保証しない。 | `as.POSIXct(val)` 後に `POSIXct` class と integer/double の内部型を確認してから格納。正常な `POSIXlt` 変換と NA companion の経路も維持する。 |
| V-051 | 完了 | `R/api_RSObjectClass.r`、`R/api_RSSingletonClass.r`、`R/api_RSProtocolClass.r` | 3 つとも名前を長さ 1 の character としか検証せず、空文字または `NA_character_` の generator を正常に生成する。`RSScalarClass` は非空・非欠損を要求している。 | 3 入口の名前条件に `!is.na(name) && nzchar(name)` を加え、ファクトリー間の契約を統一。文書と回帰テストを更新。 |
| V-052 | 完了 | `R/api_RSProtocolClass.r` の `contract_methods` | `contract_methods = NA_character_`、`""`、`c("ok", NA_character_)` のいずれも protocol 生成が成功し、`protocol_getAllContracts()` にその無効な名前が残る。`character(0)` は要求なしとして有効。 | 各メソッド名が非欠損・非空であることを入口で確認。`character(0)` は許可し、文書と回帰テストを更新。 |
| V-053 | 完了 | `R/RSClassRegistry.r` の初期化と `isStandardClass()` | singleton 生成時は標準クラスの読み込みが終わっておらず、直後の収集結果が空になる。 | `initialize()` は収集せず、最初の `isStandardClass()` で内部辞書を作る。公開 `std_classes` フィールドはユーザー指示で廃止。保存・復元経路はこの registry を現在使わず、別に標準クラスを収集する。文書と回帰テストを更新。 |
| V-054 | 完了 | `R/RSClassReference.r` の `initialize()` | `class_ref = NULL` または `list()` は `attempt to apply non-function`、`1L` は `$ operator is invalid for atomic vectors`。`RSString` は意図した型エラーになる。 | `robjc_isRobjcObject(class_ref)` を先に確認し、`RSWrappedClass` 以外を明示的な引数エラーにした。 |
| V-055 | 完了 | `R/RSWrappedClass.r` の `initialize()` | `object = NULL` や `list()` は `attempt to apply non-function`、`1L` は `$ operator is invalid for atomic vectors`。通常の `R6::R6Class("Plain")` も generator 判定を通るが `robjc_isa` がなく、後続の値検証エラーになる。 | RObjC の object class generator またはそのインスタンスだけを入口で受け付け、異種入力は明示的な引数エラーにした。文書とテストを更新。 |
| V-056 | 完了 | `R/RSInstanceMap.r` の `initialize()` | `class_ref = RSString_withValue("x")` や `RSObject$new()` は生成に成功するが、直後の `classID()` で `attempt to apply non-function`。親の `RSKeyValuePair` は RObjC object かだけを確認する。 | `RSInstanceMap` 入口で `RSWrappedClass` を要求し、構築時に不正なキーを拒否した。 |
| V-057 | 完了 | `R/RSInstanceMap.r` の `addObject()` と `lockInstance()` | 文書はいずれも `self` を不可視で返すと記す。実測では `addObject()` は `RSMutableSet`、初回の `lockInstance()` は `NULL` を不可視で返し、再呼出しだけ `self` を返す。 | 両メソッドの正常終了時に `invisible(self)` を返し、文書の契約と揃えた。再呼出し時の警告の説明も修正。 |
| V-058 | 完了 | `R/RSWrappedClass.r` の `isEqual()` と `hash()` | `RSWrappedClass$new(RSString)` を二つ作ると class ID は同じだが、継承した `isEqual()` は `FALSE`、hash は異なる。文書はラッパーをクラス名と版で識別すると説明している。 | ユーザー方針に従い `isEqual()` を class ID の比較、`hash()` を class ID の hash に変更。同名・同版の別 generator も等価になることをテストし、文書に明記。 |
| V-059 | 完了 | `R/RSInstanceMap.r` の `initialize()`、`addObject()`、`objects()` | `RSString` の `RSWrappedClass` と `RSNumber` インスタンスを組み合わせても、生成・追加・`objects()` 経由の直接追加がすべて成功する。class ID ごとの instance map という説明と食い違う。 | 初期化と追加時に generator 参照で実クラスを確認。`objects()` と継承した `value()`・`objectsForEncoding()` は変更不能な `RSSet` の snapshot を返し、encoder は `map$addObject()` を使う。関連文書とテストを更新。 |
| V-060 | 完了 | `R/RSInstanceMap.r` の所属クラス検証と `R/RSClassEncoder.r` の class ID lookup | 同名・同版の別 generator は V-058 で同じ class ID として等価。一方、encoder は最初の generator の map を class ID で再利用し、V-059 の参照一致検証が二つ目の instance を拒否していた。 | 所属判定を generator 参照ではなく class ID（名前・版）の一致に揃えた。異なる class ID は引き続き拒否。同名・同版の別 generator を一つの map に入れる構築・追加・encode 経路をテスト。 |
| V-061 | 完了 | `R/RSClassEncoder.r` の `encodingContainer()` と `R/RSObject.r` の `classVersion()` | `classVersion()` を `"2.0"` に override し generator 版を `"1.0"` にすると、検索キーは `Class_2.0`、登録キーは `Class_1.0` になり、2 個目の encode で既存 map を置換する。本体内の `classVersion()` 呼出しは encoder の一箇所のみ。 | `RSObject$classVersion()` を廃止。encoder は generator の `robjc_isa$version` から検索キーを組み立て、登録キーと揃えた。テスト、英日 vignette、関連 Rd を更新。 |
| V-062 | 完了（strategy 再設計は保留） | `R/RSKeyValuePair.r` の `objectsForEncoding()` | Class／Protocol generator を値にした pair は構築できたが、encoder が値の `objectID()` を呼んで失敗していた。 | generator 値を子オブジェクト一覧から除外し、キーと RObjC インスタンス値だけを返すようにした。Class／Protocol 値を持つ Dictionary の encode と lock、通常の RObjC 値の登録を確認。保存後の generator 参照は元と異なり、安定 ID や `RSWrappedClass` を含む strategy の再設計はユーザー指示で保留。 |
| V-063 | 完了 | `R/RSMutableArray.r` と継承する `RSArray` の `objectsForEncoding()` | 配列に `RSString` インスタンスを格納しても、継承した `RSObject$objectsForEncoding()` は空リストを返す。配列を encode しても `RSString` の class ID は登録されなかった。 | 配列が保持する RObjC インスタンスだけを子として返す override を追加。Class／Protocol generator 要素は V-062 と同様に除外。両配列の encode と文書を確認。 |
| V-064 | 完了 | `R/RSMutableDictionary.r` の `addEntry()` | `RSMutableArray` をキーにした pair を `addEntry()` すると、キーをコピーせず保持していた。元の配列を変更すると、変更前の内容で `containsKey()` は `FALSE`。同じキーを `setObjectForKey()` で挿入した場合は immutable なコピーを保持していた。 | `addEntry()` でも key をコピーするよう変更し、不要になった `copy_key` 分岐を削除。変更前の内容で検索できることを回帰テストで確認し、文書を更新。 |
| V-065 | 完了 | `R/RSClassEncoder.r` の `lockInstance()` と `R/RSClassDecoder.r` の空 map 解決 | generator に `rs_inherit_chain` は定義されておらず、子クラスを encode して lock しても親 `RSObject` の class ID が登録されなかった。親 map を新規作成する場合、初期 snapshot だけを lock すると新規 map が可変のまま残る。 | `robjc_inheritChain(klass$robjc_isa)` から親を取得し、既存 class ID の map を置換せず追加。追加後の全 map を lock する。Decoder は instance を持たない祖先 map に現行クラスの対応を要求しない。`RSObject` は現行の標準クラス辞書に含まれないため、これで既存の save/load 往復を維持。関連テストと decoder の文書を更新。 |
| V-066 | 完了 | `R/api_RSSet.r` の `.rs_api_set_objects()` | `RSSet$new(list(NULL))` は 1 要素の `RSNull` set を生成するが、`RSSet_withObject(NULL)` と `RSMutableSet_withObjects(list(NULL))` は API 側の型検査で停止していた。 | 便利コンストラクタでも `NULL` 要素を許し、Set 本体と同じく `RSNull` に正規化する。4 種の便利コンストラクタをテストし、文書を更新。 |
| V-067 | 完了 | `R/api_RSDictionary.r` の key/value 検証 | `RSDictionary$new(objects_and_keys=list(NULL, NULL))` は `RSNull` の key/value を持つ辞書を生成する。一方、`RSDictionary_withObjectForKey(NULL, key)` は値エラー、`RSDictionary_withObjectForKey(value, NULL)` はキーエラーで停止していた。 | 便利コンストラクタでも key/value の `NULL` を許し、Dictionary 本体の `RSNull` 正規化に委ねた。6 種の便利コンストラクタをテストし、文書を更新。 |
| V-068 | 完了 | `R/api_RSArray.r` の `.rs_api_array_objects()` | `RSArray$new(list(RSString, protocol))` は成功するが、`RSArray_withObject(RSString)`、`RSMutableArray_withObject(RSString)`、`RSArray_withObjects(list(RSString, protocol))` は API 側で拒否されていた。 | 便利コンストラクタでも RObjC インスタンス、Class、Protocol を許し、Array 本体の契約と揃えた。4 種の入口をテストし、文書を更新。 |
| V-069 | 完了 | `R/runtime_class.r` の `class_getInstanceMethod()` | `method_name = 1L` はメソッド一覧の位置を参照して関数を返す。`NULL` は `[[` 由来のエラー、長さ 2 の文字列は `subscript out of bounds`。文書はメソッド名の文字列を要求する。 | 長さ 1 の非空・非欠損 character だけを受理するように変更。自クラス・親クラスの探索と未定義名、無効名を確認。 |
| V-070 | 完了 | `R/runtime_robjc.r` の `robjc_inheritChain()` | 公開関数に `NULL` と空リストを渡すと `list(NULL)`、generator や整数を渡すと内部の `$` エラーになる。文書は identity record を引数とする。 | `RSIsA` を持つ環境だけを受理し、異なる入力を明示的な引数エラーに変更。class・protocol の継承探索をテスト。 |
| V-071 | 完了 | `R/api_RSDate.r`・`R/api_RSDateTime.r` の便利コンストラクタと `R/RSScalar.r` の NA companion 直接生成 | `as.Date(NaN, origin="1970-01-01")` と `as.POSIXct(NaN, origin="1970-01-01")` は `is.na()`・`is.nan()` ともに `TRUE`。直接の通常クラスでは `NaN` を保持するが、便利コンストラクタは共有 NA に変換していた。逆に NA companion の `$new(NaN)` や `$new(1)` は受理されていた。 | Scalar 系の直接生成・型別 API・汎用 API を点検。Number・Complex は既に NaN を保持、Boolean・String・Factor・Raw は NaN 非該当。Date・DateTime の便利コンストラクタは真の NA のみ共有インスタンスへ写像。NA companion 直接生成は真の NA のみ許す。POSIXlt も含めテストと文書を更新。 |
| V-072 | 完了 | `R/RSMutableArray.r` の `initialize(hash_operator=)` | `NULL`、`list()`、`RSObject$new()` を hash operator として渡しても Array を生成できるが、後で `hash()` を呼ぶと `attempt to apply non-function` になる。`RSMutableVector` は `hashForScalar()` の有無を構築時に確認している。 | RObjC インスタンスであり、Array が使う `hashFromObjects()` と `hashesOfEachObject()` の両方に応答することを構築時に確認。`RSArray` も継承して同じ契約。Dictionary と Set は演算器を引数で受け取らず、key/member 自身の `hash()` を挿入前に検証済みのため変更不要。文書とテストを更新。 |
| V-073 | 完了 | `R/RSClassEncoder.r` の `encodeObject()` と `encodingContainer()` | Encoder に正常なオブジェクトを登録後、`encodeObject(1L)` を呼ぶと `$ operator is invalid for atomic vectors` が出て、失敗時の `refresh()` により既存の class_store も空になる。直接 `encodingContainer(NULL)` は `attempt to apply non-function`。 | 両公開入口で RObjC インスタンスを明示的に検証。`encodeObject()` は再帰・`tryCatch()` より前に検証し、不正なルート入力では既存状態を保持。子オブジェクトの処理失敗時は既存の全体 refresh 方針を維持することをテスト。 |
| V-074 | 完了 | `R/RSClassDecoder.r` の `unpackTo(envir=)` | `envir = 1L` を渡すと、空の class_store では成功して `self` を返すが、instance がある場合は `assign()` 由来の `invalid 'envir' argument` になる。文書は環境を要求する。 | class_store の内容に関係なく、`envir` が環境であることを処理前に明示的に検証。空・非空双方のデータと正常な環境への展開をテスト。 |
| V-075 | 完了 | `R/RSMutableVector.r` の `compare(na.last=)` | `RSScalar` と `RSMutableArray` の `compare()` は `na.last = 1L`・`0L` を logical に強制変換する。`RSMutableVector` と継承する `RSVector` は同じ入力を型エラーで拒否していた。 | Vector も `as.logical()` で正規化したうえで長さ1・非欠損の logical だけを受理。Mutable／Immutable 双方の順序と無効入力をテストし、文書を更新。 |
| V-076 | 完了 | `R/utilities.r` の `cast_collections_to_list()` | `RSString_withValue("x")` は RObjC instance だが `asList()` を持たず、helper は `NULL` を返す。その値を `RSMutableSet$new()` や `RSMutableDictionary$new(key_value_pairs=)` に渡すと、入力が消えて空の collection が構築される。 | `asList()` を持たない RObjC instance を明示的に拒否。空の Array・Set・Dictionary の `asList()` と helper の返り値はいずれも `list()` と確認。文書とテストを更新。 |
| V-077 | 完了 | `R/RSMutableArray.r` の `indexOfObject()`／`containsObject()` | `isEqual()` が `NA` を返す要素を格納すると、検索時の `any(idxes)` が `missing value where TRUE/FALSE needed` で停止する。 | `vapply()` 内で `isTRUE(x$isEqual(obj))` を使い、`TRUE` だけを一致とする。Array 自身の `isEqual()` と同じ扱いに揃え、後続要素の検索と非一致をテスト。 |
| V-078 | 完了 | `R/RSHashOperator.r` の `objectsNotIn()` と `unionOfSets()` | hash が同じ 2 要素で `isEqual()` が `NA` を返すと、`areEqualSets()` は `FALSE`、`objectsAlsoIn()` は空だが、`objectsNotIn()` も空になり、union から左の要素が消える。 | `objectsNotIn()` でも `isTRUE(o1$isEqual(o2))` を使い、`TRUE` のみ一致とした。全クラスの `isEqual()` 定義も点検し、結果を `isTRUE()` で確定させた。回帰テストを追加。 |
| V-079 | 完了 | `R/RSMutableDictionary.r` の `isEqual()` | `vapply()` の結果を直接返す状態になっており、2 キーでは長さ 2 の logical、空辞書では `logical(0)` を返した。 | 各キーの条件を `isTRUE()` で確定し、`all(vapply(...))` で単一の真偽値に集約する。外側の `isTRUE()` は不要として除去。キーの順序、値の差、空辞書をテスト。 |
| V-080 | 対象外 | `R/RSHashOperator.r` の `areEqualSets()` | 重複したリストを入力すると集合判定と異なる結果になり得る。 | Set の要素には重複が生じないことを既に確認済み。重複リストはこの検証の対象外とし、実装は変更しない。 |
| V-081 | 完了 | `R/RSString.r` の factor 入力と `compare()` | `RSString_withValue(factor("z", levels=c("a","z")))` は `RSString_withValue("z")` と等価で hash も同じだが、前者と文字列 `"a"` の `compare()` は `-1L`、後者は `1L`。factor の整数コードが `c()` で文字列化されるため。 | `RSString` は factor のラベルを character として保持。`RSFactor` の factor metadata と `nativeValue()` は従来どおり。文書と直接生成・便利コンストラクタ双方のテストを更新。 |
| V-082 | 完了 | `R/RSBoolean.r` の `asNumber()` | `RSBoolean$na$asNumber()` は `RSNumber$new(NA_real_)` を呼び、「共有 NA を使う」エラーで停止する。`TRUE`・`FALSE` の変換は成功する。 | `RSNumber_withValue(as.numeric(private$.val))` に変更し、通常値は `RSNumber`、欠損値は `RSNumber$na` に変換。文書とテストを更新。 |
| V-083 | 却下 | `R/RSKeyValuePair.r` の `copy()` と `setKey()`、Dictionary のキー挿入 | `RSKeyValuePair$new(old, RSNull$null)` を Dictionary のキーにし、挿入後に元の pair を `setKey(new)` で変更すると検索不能になる。 | `RSKeyValuePair` は文書どおり変更リスクを伴うコンテナとして扱う。可変キーの変更は利用側の責任とし、`copy()` や `setKey()` は変更しない。提案コメントを撤去。 |
| V-084 | 完了 | `R/RSMutableVector.r` の `compare()` と factor levels | levels の順序が逆の factor ベクトル `a = factor("a", levels=c("a","b"), ordered=TRUE)` と `b = factor("b", levels=c("b","a"), ordered=TRUE)` で、`a$compare(b)` と `b$compare(a)` がともに `-1L`。`c(left[i], right[i])` が左側の levels を採用するため、大小比較の反対称性が崩れる。 | factor 同士で levels が一致しない場合は比較不能エラーにした。`RSFactor$compare()` の既存方針と揃え、双方向の拒否と同一 levels での順序をテスト。 |
| V-085 | 完了 | `R/RSMutableVector.r` の factor と character の `compare()` | `factor("z", levels=c("z","a"))` と `"a"` を包んだベクトルを比較すると `-1L`。`c(factor, character)` が factor の内部コード `1` を文字列化するため、ラベル `"z"` に基づく順序と逆になる。 | factor と character のベクトル間は比較不能エラーにした。既存の `RSFactor` と `RSString` の `compare()` と揃え、両方向をテスト。併せて base R の原子値演算で辞書式比較する `nativeCompare()` を両 Vector クラスへ追加。 |
| V-086 | 完了 | `R/RSMutableVector.r` の factor と非 factor の `compare()` | `factor("z", levels=c("z","a"))` と `1L` を包んだベクトルで `compare()` が `0L` を返す。`c()` が factor のラベルでなく内部コード `1` を使うため、無関係な値が同順位になる。`nativeCompare()` は別途 base R の原子値演算に従う。 | V-085 の拒否条件を factor/非 factor 全般へ広げた。factor/character と factor/integer の両方向をテスト。 |
| V-087 | 完了 | `R/RSMutableVector.r` の異種型 `compare()` | `RSVector$new(2L)$compare(RSVector$new("10"))` は `1L`。整数が文字列に暗黙変換され、`nativeCompare()` と同じ結果になる。 | ユーザー方針に従い、Vector の `compare()` は格納値の `typeof()` と `class()` がともに一致する場合だけ許す。integer/double 間も拒否し、base R の型変換は `nativeCompare()` に任せる。Scalar 側は RSNumber が両型を一クラスへ統合しているため比較可能な既存仕様を維持。 |
| V-088 | 完了（API 変更） | `R/api_RSObjectClass.r`・`R/api_RSSingletonClass.r`・`R/api_RSScalarClass.r` の `active` 引数 | active binding を生成すると、`RSObject$respondsTo()` の照会で getter が実行され得る。 | ユーザー方針に従い 3 つの class factory から `active` 仮引数と R6 への転送を削除。既存の `respondsTo()` は変更せず、元の提案コメントを撤去。`RSProtocolClass()` は元々 `active` 引数を持たない。 |
| V-089 | 完了（文書修正） | `R/RSMutableArray.r` の `allObjects()`／`allObjectsAsList()` の文書 | `allObjects()` は「list を返す」と記すが実際は `RSArray`。`allObjectsAsList()` は「base R 値」と記すが、`asList()` と同じ RObjC オブジェクト参照を返す。`RSString` 要素との `identical()` を確認。 | 実装を正として roxygen と Rd の返り値説明を修正。実装は変更しない。 |
| V-090 | 完了（方針変更） | `R/RSMutableDictionary.r`・`R/RSKeyValuePair.r`・`R/api_RSDictionary.r` の generator キー | 以前は Class／Protocol generator を値にできたが、キーでは `.wrapNull()`、挿入時の `copy()`、pair の検証、便利コンストラクタで拒否した。 | ユーザー方針に従い generator をキーと値の双方に許可。RObjC インスタンスのキーはコピーし、generator キーは同一性で保持する。列挙・逆引き・等価性・便利コンストラクタ・encoding 子オブジェクト抽出まで対応。文書とテストを更新。 |
| V-091 | 完了 | `R/RSMutableSet.r`・`R/api_RSSet.r` の generator 要素 | `RSString$hash()` は整数を返し、generator には `isEqual()` もあるが、`RSMutableSet$new(list(RSString))` は「All elements must inherit from RSObject.」、`RSSet_withObject(RSString)` は入力エラーだった。 | Dictionary と同じく Class／Protocol generator を直接生成・変更・便利コンストラクタで許可。集合演算・等価性・encoding 子オブジェクト抽出・文書とテストを更新。 |
| V-092 | 完了（方針変更） | `R/RSMutableArray.r` の `compare()` と要素の比較結果 | `RSComparable` 準拠クラスの `compare()` が `2L` を返すと、`RSArray$compare()` も `2L` を返した。配列の公開契約は `-1L`・`0L`・`1L`。 | 厳格な戻り値検証案は却下。要素比較の結果を `as.integer(sign(...))` で三値に正規化する。 |
| V-093 | 完了（文書修正） | `R/RSClassReference.r` の継承説明と Rd | 文書は `RSKeyValuePair` がキーの `RSCopying` 準拠を強制すると記していたが、現在の `RSKeyValuePair$initialize()` は RObjC object または Class／Protocol generator だけを検証する。 | `RSCopying` 強制の記述を削除し、`RSClassReference` のキーが `RSString` であることを明記。実装は変更しない。 |
| V-094 | 却下 | `R/RSClassDecoder.r` の `loadEncodedData()` | 辞書型の確認だけで `RSString` キーと `RSInstanceMap` 値の構造は確認しない。誤った項目を含む辞書も受理する。 | 一般ユーザーが触れる入口ではないため、入力内容は呼出し側の責任とする。追加検証は行わず、提案コメントを撤去。 |
| V-095 | 完了（文書修正） | `R/RSMutableArray.r` の `mutableCopy()` 文書 | 文書は「元が mutable なら self を返すこともある」と記していたが、実装は常に `RSMutableArray$new(...)` を呼び、`identical(x, x$mutableCopy())` は `FALSE`。要素参照は共有される。 | 実装に合わせ、別の mutable コンテナを返し要素参照を共有することを明記。 |
| V-096 | 完了（文書修正） | `R/RSArray.r` 冒頭の `copy()` 説明 | 冒頭は `copy()` で配列を duplicate すると記していたが、実装とメソッド文書は immutable な `self` を返す。`identical(x, x$copy())` は `TRUE`。 | 冒頭を「`copy()` は同じ immutable 配列を返し、`mutableCopy()` は要素参照を共有する新しい mutable コンテナを返す」に訂正。 |
| V-097 | 完了（文書修正） | `R/RSKeyValuePair.r` の `setKey()` 文書 | 文書は Dictionary が `setKey()` で一時 lookup 用の pair を再利用すると記していたが、パッケージ内の呼び出しはテスト以外になく、Dictionary は native storage にキーを直接渡す。 | `setKey()` は残し、現行実装にない Dictionary 内部利用・一時 lookup の説明を削除。 |
| V-098 | 完了（文書修正） | `R/RSArray.r` 冒頭の不変性説明 | 「without risk of modification」は要素まで不変と読めたが、RSArray は要素参照を保持する。可変な要素を挿入した後に外部参照から変更すると、配列から見える要素の状態も変わる。 | コンテナの要素数・順序・参照は固定されるが、保持した可変要素の内容は変わり得ると明記。 |
| V-099 | 完了（文書修正） | `R/RSHashOperator.r` の `hashesOfEachObject()` 引数説明 | 文書は「RSScalar-compatible objects」と限定していたが、Array の通常オブジェクトや Class／Protocol generator も `hash()` で処理できる。generator 2 件の hash と集合等価判定が成功することを確認。 | 実装は維持し、引数を hashable RObjC object または Class／Protocol generator のリストと説明。 |
| V-100 | 完了（文書修正） | `R/RSEncoding.r` の `RSDecoderProtocol` の `encodedClasses()` 説明 | 文書は「classID → instance set の Dictionary を返す」と記すが、`RSClassDecoder$encodedClasses()` は `class_store$allEntriesAsList()` を返す。各要素は `RSKeyValuePair`、値は `RSInstanceMap`。 | プロトコルの返り値を class ID の `RSString` キーと `RSInstanceMap` 値を持つ `RSKeyValuePair` の list と明記。内部辞書 `class_store` の型説明も訂正。 |
| V-101 | 完了（実装修正） | `R/RSFactor.r` の `asFactor()`・`nativeValue()` と NA factor コンストラクター | 以前は NA 入力を共有 `RSFactor$na` に写像して levels と ordered 属性を失っていた。 | `RSFactor_withFactor()` は NA ごとに `RSNAFactor$new(val)` を返し、`initialize()` で元の属性を保持する。通常の `RSFactor` は NA を拒否し、`RSScalar` の例外条件も修正した。`RSFactor$na` は factory の既定インスタンスとして残る。 |
| V-102 | 完了（文書修正） | `R/RSString.r` 冒頭の factor 説明 | 「factor levels を character strings と interchangeably 扱う」は levels も保持されるように読める。後段と実装では factor 入力をラベル文字列に変換して levels を破棄する。 | 冒頭にラベル文字列への変換と levels／ordering が保持されないことを明記し、`RSFactor` の案内を維持した。 |
| V-103 | 棚上げ | `R/RSClassDecoder.r` の `encodedClassIDs()` 説明 | `@return` は list と記すが、実装は `vapply(..., character(1))` により class ID の character vector を返す。空入力でも `character(0)`。`RSClassReference` entries という説明も、取得しているのは `class_store` の一般的な `RSKeyValuePair` である。 | Encoder／Decoder と Archiving strategy の見直しまで修正を保留。提案コメントは保持する。 |
| V-104 | 完了（文書修正） | `R/RSMutableVector.r` の `hashOperator()` 説明 | 文書は常に `RSHashOperator` の singleton を返すと記すが、`initialize()` は `hashForScalar()` を持つ任意の RObjC object を受け付け、その参照を保持する。独自 operator を渡すと `hashOperator()` は同一オブジェクトを返した。 | `@return` を生成時に渡された operator（省略時は `RSHashOperator$instance`）と訂正。実装は変更しない。 |
| V-105 | 完了（文書修正） | `R/RSHashOperator.r` の `hashFromObjects()` 引数・返り値説明 | `@param objects` は「RS objects」と限定するが、メソッドは各要素の `hash()` を呼ぶだけで Class／Protocol generator にも対応する。両 generator を渡して整数 hash を返すことを確認。`@return A combined integer` も型と用途が曖昧。 | 引数を hashable RObjC object または Class／Protocol generator のリスト、返り値を集約した integer hash code と訂正。実装は変更しない。 |
| V-106 | 完了（文書修正） | `R/RSMutableArray.r` の `hashOperator()` 説明 | Vector の V-104 と同様、`initialize()` は `hashFromObjects()` と `hashesOfEachObject()` を持つ任意の RObjC object を受け付ける。独自 operator を渡すと `hashOperator()` は同じ参照を返し、`hash()` もそれを使用した。現文書の「RSHashOperator singleton」「set operations」は実装と用途に合わない。 | 渡された hash operator を返すこと、既定値が `RSHashOperator$instance` であることを記し、set operations への言及を削除。実装は変更しない。 |
| V-107 | 完了（文書修正） | `R/RSMutableArray.r` の変更メソッド5つの `@return` | `addObject()`、`insertObjectAtIndex()`、`replaceObjectAtIndex()`、`removeObjectAtIndex()`、`removeAllObjects()` は「Invisible `NULL`」と記すが、全て `invisible(self)` を返す。5 経路の `withVisible()` で返り値が同じ array 参照・不可視であることを確認。 | 5 メソッドの `@return` を「`self`, invisibly」に訂正。実装は変更しない。 |
| V-108 | 完了（文書修正） | `R/RSMutableArray.r` の `containsObject()`・`indexOfObject()` の引数説明 | 両メソッドの `@param obj` は `RSObject` と記すが、Array は Class／Protocol generator も保持し、両メソッドで検索できる。generator を格納して index と membership の結果を実測した。 | 両メソッドの引数説明を RObjC object、Class generator、Protocol generator を含む表現へ訂正。実装は変更しない。 |
| V-109 | 完了（文書修正） | `R/RSMutableVector.r` の `compare()` 説明 | Vector の比較なのに prefix が一致したときの説明が「shorter array」で、`@return` も整数三値であることを明記していない。短い Vector と長い Vector の双方向比較で `-1L`・`1L` を確認。 | 「shorter vector」に直し、返り値を `-1L`・`0L`・`1L` と明記。実装は変更しない。 |
| V-110 | 完了（実装・文書修正） | `R/api_RSScalarClass.r` の NA companion 説明と singleton マーカー | 通常の NA class は singleton マーカーを持ち、型別コンストラクターは共有インスタンスを返す。以前は `$new()` で追加インスタンスも作れた。前回の factor 修正後も `RSNAFactor` に singleton マーカーが残っていた。 | `RSScalarClass(na_singleton = TRUE)` を既定とし、`RSFactor` のみ `FALSE` を指定。さらに singleton NA class は共有 instance 生成後に `$new()` を拒否して binding を固定。`RSNAFactor` の直接生成だけは維持する。 |
| V-111 | 完了（ビルド対象から除外） | `RSSorter` と公開文書 | パッケージ内の呼び出し元はなく、旧 Dictionary のハッシュテーブル構築・検索を支えるためのコードが残っていた。 | ソースと専用テストを build 対象外の `prototypes/` に保存。`DESCRIPTION` の Collate、`NAMESPACE` の export、クラス一覧と生成済み Rd から除外。旧ソースの冒頭に用途と撤去予定を通常コメントで記録。 |
| V-112 | 完了（文書修正） | `R/RSHashOperator.r` の集合演算の引数説明 | `areEqualSets()` などは引数を「RS objects」と記すが、実装は要素の `hash()` と `isEqual()` を呼び、対応する Class／Protocol generator も受け付ける。 | 集合演算各メソッドとクラス概要の説明を hashable RObjC object または Class／Protocol generator に統一。実装は変更しない。 |
| V-113 | 完了（文書修正） | `R/RSHashOperatorProtocol.r` の `areEqualSets()` 用途説明 | 文書は Set／Dictionary の等価性判定に使われると記すが、両クラスは native storage を使い、このメソッドのパッケージ内部呼び出し元はない。 | ハッシュ可能なオブジェクトのリストを比較する公開ユーティリティと記し、native-backed Set／Dictionary とは独立した機能であることを明記。API は維持。 |
| V-114 | 完了（文書修正） | `R/RSHashOperatorProtocol.r` の hash 返り値形式 | Protocol の説明は hash 値を任意の比較可能な形式としている。一方、`RSMutableVector$hashesOfValues()` は `hashForScalar()` を `vapply(..., integer(1))` で受け、Array を Dictionary／Set のキーに使う場合も長さ 1 の非欠損 integer `hash()` が必要。 | Protocol はメソッド名のみの契約であると明記し、Vector／Array で使う operator の返り値には追加の整数型制約があることを文書に補足。実装は変更しない。 |
| V-115 | 完了（文書修正） | `R/RSHashOperatorProtocol.r` の準拠要件説明 | `conformsTo()` は準拠宣言を確認し、全メソッドの実装を検証しない。`RSMutableVector` は `hashForScalar()`、`RSMutableArray` は `hashFromObjects()` と `hashesOfEachObject()` の存在だけを検査する。 | ユーザー方針に従い、Protocol への準拠は宣言のみでよく、利用する処理に必要なメソッドを実装することを説明。見出しも「Declared Methods」に変更。実装は変更しない。 |
| V-116 | 完了（文書修正） | `R/api_RSProtocolClass.r` などの一般 Protocol 説明 | ファクトリー文書は `contract_methods` を「required methods」と記す。一方、`conformsTo()` は準拠宣言のみを確認し、全メソッドの実装は検査しない。 | `contract_methods` を宣言されたメソッド名と説明し、各処理に必要なメソッドを利用時に備える旨を Protocol・ファクトリー一覧・`RSObject$conformsTo()` に反映。実装は変更しない。 |
| V-117 | 完了（文書修正） | `R/RSCopying.r` と `R/RSMutableCopying.r` の準拠説明 | 両 Protocol は「準拠クラスは `copy()`／`mutableCopy()` を必ず実装する」と記すが、準拠宣言時には実装を検査しない。 | 宣言時の検査がないこと、各 Protocol のメソッドは一つだけでコピー機能を使う際に実質必須であることを明記。`RSObject$copy()` の既定動作も補足し、見出しを変更。実装は変更しない。 |
| V-118 | 完了（文書修正） | `R/RSMutableDictionary.r` のキー挿入時コピー説明 | キー挿入では `key$copy()` を呼ぶが、`RSObject$copy()` の既定実装は `self` を返す。可変サブクラスがこれを継承すると、辞書のキーと呼び出し元が同じ参照を保持する。 | `RSCopying` に沿って `key$copy()` を呼ぶこと、戻り値が元の参照でもよいこと、格納中は hash／等価性に関わる状態を変えないことを明記。実装は変更しない。 |
| V-119 | 完了（文書修正） | `R/utilities.r` の `msg_immutable_try_mutate()` 例 | 文書例の `RSNumber` からは存在しない `RSMutableNumber` という案内が生成される。実際の呼び出し元は Array／Vector／Set／Dictionary の不変クラス。 | 例を `RSArray` に改め、可変 counterpart があるクラス向けの helper であることを明記。実装は変更しない。 |
| V-120 | 完了（実装・文書修正） | `R/utilities.r` の `cast_collections_to_list()` 入出力 | RObjC collection の `asList()` が `NULL` を返すと、`RSMutableSet$new()` などで空入力に化ける。入力自体が `NULL` や非 RObjC でも専用の検証がなかった。 | `list` はそのまま返し、それ以外は RObjC instance であることと `respondsTo("asList")` を順に確認。戻り値が list でなければエラーにした。`conformsTo()` は Protocol generator 用なのでメソッド照会には使わない。 |
| V-121 | 完了（文書修正） | `R/runtime_protocol.r` と `R/robjc-runtime.r` の Protocol contract 説明 | `protocol_getContract()` 系は宣言されたメソッド名を取得するだけで、実装要件の検査は行わないが、文書では「requirements」と表現する。 | 取得対象を「declared method names」と記し、V-116 の Protocol 方針に合わせた。実装は変更していない。 |
| V-122 | 完了（文書修正） | `R/RSObject.r` の `hash()` 説明と Notes | 実装はアドレスの一部から 32-bit 整数を作るため、異なるオブジェクト間の hash 衝突はありうる。一方、Notes は「unique key」、公開メソッド一覧は「numeric hash」と書いていた。 | 一意性の断定を外して integer hash と説明し、hash 衝突がありうることを明記した。実装は変更していない。 |
| V-123 | 見送り（仮説撤回） | `R/runtime_util.r` の `get_pointer_hex()` | `sub()` が不一致の文字列をそのまま返すことだけを模擬文字列で確認した。通常の環境や R6 メソッド環境の表示が実際に変わる根拠はなく、環境差による問題として提示したのは過剰だった。 | 防御的な検証の提案を撤回し、確認コメントを削除した。実際の非標準出力が観測された場合に再検討する。 |
| V-124 | 完了（文書修正） | `R/RSObject.r` の `objectID()` 説明 | `objectID()` は `get_pointer_hex(self)` に委譲し、この関数は `environment(self$print)` のアドレスを返す。文書の「instance の pointer」は対象環境を正確に表していなかった。 | `print` メソッドの環境アドレスを 16 進文字列として返し、現在の R セッションでは C のポインタに似た識別子として使えると説明した。API と返り値は変更していない。 |
| V-125 | 完了（実装・文書修正） | `R/RSObject.r` と `R/runtime_robjc.r` のアドレス由来 hash | 32-bit hash の上位 16 bit が `0x8000` のとき、`bitwShiftL(32768L, 16L)` は `NA_integer_`。両実装とも `bitwOr()` 後も `NA` を返し、Dictionary／Set の挿入時 hash 検証に失敗しうる。算術演算で再現した。 | 両実装で組み立てた整数が `NA_integer_` の場合だけ `0L` へ写像した。hash 衝突は許容し、通常の hash 値は維持する。ソース・Rd の説明も更新。 |
| V-126 | 完了（実装・文書・テスト） | `R/RSHashOperator.r` の `hashFromObjects()` と `R/RSMutableVector.r` の `hash()` | 非欠損整数 `-2147418112L` と `65536L` の XOR は `NA_integer_`。前者は上位 16 bit が `0x8001`、後者は `0x0001` の hash に相当する。個々の hash が正常でも Array／Set／Dictionary の集約 hash、または独自演算器を使う Vector の hash が欠損しうる。 | 両集約関数で最終 XOR が `NA_integer_` の場合だけ `0L` へ写像した。通常の集約結果は維持し、文書に非欠損の返り値を明記した。対象テストは通過。 |
| V-127 | 完了（実装修正） | `R/RSHashOperator.r` の `computeHashCodeWithString()` | 文書は `string` を character string とするが、長さ 1 の integer や factor が従前の入口を通り、`enc2utf8()` の「argument is not a character vector」で失敗していた。 | `is.character(string)` を既存の長さ・欠損確認に加え、メソッド自身のエラーで型契約を示した。正常な文字列の hash は変更していない。 |
| V-128 | 完了（実装・文書・テスト） | `R/RSMutableVector.r` の `hashesOfValues()` と `R/RSHashOperator.r` の `hashFromObjects()` | 独自 `hashForScalar()` や要素の `hash()` が `NA_integer_` を返しても `vapply(..., integer(1))` は通過する。両集約関数は V-126 の XOR 補正で `0L` を返し、入力 hash の契約違反が隠れていた。Vector と HashOperator の両方で再現した。 | 両方で `vapply()` の結果に `anyNA()` を適用し、欠損した構成要素 hash を明示的に拒否した。V-126 の `0L` 写像は正常な構成要素から XOR 結果が NA になった場合だけに使う。対象テストは通過。 |
| V-129 | 完了（実装・文書・テスト） | `R/RSHashOperator.r` の `hashesOfEachObject()` と集合演算 | `hash()` が `NA_integer_` を返すオブジェクト 1 件で、`hashesOfEachObject()` は `NA_integer_`、同じリスト同士の `areEqualSets()` も `NA` を返していた。 | `hashesOfEachObject()` が欠損 hash を拒否し、`hashFromObjects()` もこのメソッドを使って V-128 の検証を共用した。集合演算は両入力の hash を一度検証した結果を再利用し、NA 混入時に一貫してエラーにした。対象テストは通過。 |
| V-130 | 完了（実装・文書・テスト） | `R/RSHashOperator.r` の `areEqualSets()` | 同じ `RSString` オブジェクトを順序違いで並べると、名前なしリスト同士では `TRUE`、異なる名前を持つリスト同士では `FALSE`。`vapply()` がリストの名前を hash ベクトルに引き継ぎ、`identical(sort(...), sort(...))` が名前も比較する。 | ソート前に `unname()` して hash ベクトルの値だけを比較する。リスト名を集合の等価性に含めないことを文書とテストに記録した。 |
| V-131 | 完了（実装・文書・テスト） | `R/RSFactor.r` の `compare()` | levels の異なる factor は通常値同士だとエラーになるが、片方が NA だと早期に `super$compare()` へ進む。levels `c("a", "b")` の NA と levels `c("x", "y")` の NA で `0L`、通常値と異なる levels の NA で `-1L` を実測した。 | levels の一致確認を NA 分岐より前へ移し、欠損を含めて「levels が異なる factor に共通の順序はない」という契約に揃えた。既定の `RSFactor$na` は levels が空なので、通常値との比較テストは同じ levels を持つ `RSNAFactor` に変更した。 |
| V-132 | 完了（実装・テスト） | `R/RSFactor.r` の `initialize()` と他の組込 `RSScalar` 継承クラス | 直接生成時に `factor(character())` は「missing value where TRUE/FALSE needed」、長さ 2 の factor は「'length = 2' in coercion to 'logical(1)'」で失敗した。他の組込派生クラスは `RSScalar$initialize()` の長さ確認を先に通るか、独自に先行検証する。`RSNull` は無引数である。 | `RSFactor$initialize()` の最初で factor 型と長さ 1 を検証した。長さ 0・複数要素・非 factor の直接生成を回帰テストで確認し、NA subclass の生成を維持した。 |
| V-133 | 完了（実装・文書・テスト） | `R/RSScalar.r`、`R/RSString.r`、`R/RSFactor.r`、型別・汎用 scalar API、`RSScalarClass()` | `RSScalar_withValue(matrix(1, 1, 1))` は `RSNumber` を作り、`value()` に `dim = c(1, 1)` を残していた。complex/raw の行列も同様。一方 logical/character 行列は下位の型検証で拒否されていた。`dim` 付き factor は `as.character()` で属性を落として受理され、型別 API の NA 早期返却も `dim` 検証を迂回していた。 | スカラーを「長さ 1 かつ `dim` なし」とし、基底初期化、正規化前の String/Factor/DateTime、型別・汎用 API、NA companion ファクトリーで確認した。長さ 1 の行列を型・NA 状態にかかわらず拒否するテストを追加した。 |
| V-134 | 完了（実装・テスト） | `RSBoolean`、`RSNumber`、`RSComplex`、`RSRaw` の `initialize()` | 直接 `$new()` で型違いの NA を渡すと、具象クラスの型検証より先に `RSScalar$initialize()` が欠損判定を行う。`RSNumber$new(NA_character_)` は「Use RSNANumber」、`RSRaw$new(NA_integer_)` は「This scalar class has no NA companion」と返していた。他の型でも同様。 | 具象クラスの型判定を `super$initialize(val)` より前へ移した。型違いは型エラー、正しい型の NA は従来どおり NA companion 案内になることをテストした。`super$initialize(val)` は残した。 |
| V-135 | 完了（実装・テスト） | `R/api_RSDate.r` と `R/api_RSDateTime.r` の欠損値入口 | `structure(NA_character_, class = "Date")` は `RSDate$new()` なら numeric Date 型エラーだが、`RSDate_withDate()` は `RSNADate` を返していた。`structure(NA_character_, class = c("POSIXct", "POSIXt"))` も直接生成ではエラー、API では `RSNADateTime` を返していた。クラス属性だけを確認して NA companion に早期返却するため、実体の型検証を迂回していた。 | Date はクラス本体と同じ `typeof(val)`、DateTime は `as.POSIXct(val)` 後のクラスと `typeof()` を欠損判定前に確認する。正常な `POSIXlt` の NA は共有 instance を返す。 |
| V-136 | 完了（実装・文書・テスト） | `R/api_RSScalarClass.r` の `na_value` 検証と型別 Scalar API | `RSScalar$initialize()` は scalar `POSIXlt` を許容するが、`RSScalarClass(..., na_value = as.POSIXlt(NA))` は `is.atomic()` が `FALSE` のため拒否されていた。また、class だけ付けた logical/character の NA は直接クラスでは拒否されても型別 API では NA companion に写像された。 | factory でも `POSIXlt` 例外を許容し、Boolean/String API は実体クラスと同じ `inherits()` 判定へ変更した。Factor API は `dim` を事前拒否し、DateTime API は正規化後の長さ・`dim` も確認する。型別 API 全体のチェック順を照合した。 |
| V-137 | 完了（実装・文書・テスト） | `R/RSHashOperator.r` の `hashForScalar()` 入力 | `matrix(1, 1, 1)` と `list(1)` はともに受理され、数値スカラー `1` と同じ hash `49L` を返していた。V-133 で採用したスカラー条件（atomic または `POSIXlt`、長さ 1、`dim` なし）と異なる。 | 冒頭に同じ条件の検証を追加し、非スカラー値を拒否する。通常のスカラーの hash は維持し、回帰テストを追加した。 |
| V-138 | 完了（文書修正） | `R/robjc-classes.r` の class index | `RSComplex`／`RSNAComplex`、`RSRaw`、`RSVector`／`RSMutableVector` は export されているが、クラス索引の Scalar values／Collections に載っていなかった。 | 索引にこれらのクラスへのリンクと簡潔な説明を追加し、Rd を同期した。 |
| V-139 | 完了（実装・文書・テスト） | `RSNumber`／`RSBoolean`／`RSComplex`／`RSRaw` の class 属性付き atomic 入力 | `as.character.audit_hash_atom()` が別の表示を返す class 属性付き値を包むと、通常値との `isEqual()` は `TRUE` なのに hash は異なった。4 型で再現し、`RSMutableSet` に挿入した値を等価な通常値で検索すると `FALSE`。`RSString` は入力を文字列へ正規化するため同じ問題にはならない。 | 4 具象クラスと対応する型別 API で `is.object(val)` を拒否し、素の atomic 値だけを受け付ける。NA を型別 API が早期返却する経路も含めて回帰テストを追加した。 |
| V-140 | 完了（実装・文書・テスト） | `RSDate`／`RSDateTime`／`RSString`／`RSFactor` と型別 API の S3 サブクラス入力 | `Ops.audit_date()` または `Ops.audit_dt()` が常に `TRUE` を返す Date／POSIXct サブクラスでは、異なる日数・時刻の `isEqual()` が `TRUE`、数値時刻由来の hash は異なった。Set に一方を挿入して他方を検索すると `FALSE`。`as.POSIXct()` は POSIXct サブクラス属性を保持する。 | 組込 Scalar は R 標準クラスだけを受け入れる方針とした。Date、POSIXct／POSIXt、POSIXlt／POSIXt、factor、ordered／factor の標準 class 構成と class 属性なし character を直接生成・型別 API で確認する。NA 早期返却前に検証し、標準値の受け入れとサブクラス拒否の回帰テストを追加した。 |
| V-141 | 完了（実装・文書・テスト） | `R/RSMutableArray.r` の初期化と `indexOfObject()` 返り値 | 名前付き list `list(first = x)` で初期化すると、`indexOfObject(x)` は `c(first = 1L)` を返していた。名前なしの `1L` と `identical()` では異なり、位置だけを返す API に元 list の名前が漏れた。 | ユーザー方針に従い、返り値だけでなく初期化時に入力 list の名前を除去する。`RSMutableArray` と継承する `RSArray` の格納・検索・コピーで位置だけを保持する。 |
| V-142 | 完了（実装・文書・テスト） | `R/RSMutableVector.r` の raw 型 `compare()` | `RSVector$new(raw())$compare(RSVector$new(as.raw(1)))` は `-1L`、空同士は `0L`、非空同士は比較不能エラー。base R の `sort(raw())` もエラーで、`RSRaw$compare()` は常にエラー。 | 型一致確認の後に raw 型を一律拒否し、空/非空でも同じ比較不能エラーとした。`nativeCompare()` は base R の `<`/`>` に従う既存仕様を維持。 |
| V-143 | 完了（実装・文書・テスト） | `R/RSMutableVector.r`・`R/RSMutableArray.r` の `isEqual()` と独自 `hash_operator` | 同じ `1L` の Vector に既定演算器と常に `1L` を返す独自演算器を使うと `isEqual() == TRUE`、hash は `49L` と `1L`、Set の `containsObject()` は `FALSE`。Array でも同じ要素で hash `97L` と `1L` を再現。 | 演算器の参照同一性を等価性の条件に加えた。コピーは同じ演算器を保持するため従来どおり等価。異なる演算器間は内容が同じでも非等価と文書化した。演算器自体の決定性は利用側の契約とする。 |
| V-144 | 見送り（仮説撤回） | `R/RSRaw.r` の `compare()` 説明 | `order(raw())` と `order(as.raw(1))` は位置を返すが、複数値の `order(as.raw(c(2,1)))` は `unimplemented type 'raw'` でエラー。`sort()` は空でもエラー。 | `order()` の特例を一般化した誤った提案を撤回。raw 型に順序を与えない現行方針と説明を維持する。 |

### `isEqual()` の非 RObjC 入力（2026-10-14、`FALSE` に統一）

- `RSObject`・`RSNull`: `identical()` により `FALSE`。
- `RSScalar` と具象スカラー、Array、Vector、Set、Dictionary: 明示的な型判定により `FALSE`。`RSArray` と `RSVector`、`RSSet`、`RSDictionary` は親の実装を継承する。
- `RSKeyValuePair`: 非 RObjC 入力で `$` 由来のエラーだったため、`FALSE` に修正（V-027）。
- `RSMutableDataFrame` と `RSDataFrame`: 非 RObjC 入力で `$` 由来のエラー、正しい DataFrame 同士でも `FALSE` だったため、条件を修正（V-028）。
- 実測入力は `NULL`、`1L`、`list()`、`"x"`。型違いは `FALSE`、変更メソッドへの不正な入力は `stop` とする。

## Scalar と base R の比較差分（R 4.5.1）

以下は変更前の `==` と `<` を、それぞれ `isEqual()` と `compare()` に対応させて実測した履歴であり、現在の実装を示す表ではない。現在は `nativeCompare(other, na.last = TRUE)` が base R の値比較を三値結果に変換する。`isEqual()` はキー用の真偽値、`compare()` は base R のソート順による三値結果に分離した。NA と NULL はキーの同一性を維持する。

- logical/numeric/character/raw/complex の異種型: base R は型変換のうえ等価性を判定する場合がある（例: `TRUE == 1`, `1 == "1"`, `as.raw(1) == 1L`, `1 == 1+0i` は `TRUE`）。RObjC は異なるクラスを非等価・比較不能とする。
- 通常の factor: base R の `==` はラベル比較だが、`<` は警告と `NA`。RObjC は同じ levels なら level 順序、異なる levels や string 相手なら文字列順で大小を決める。
- ordered factor と character: base R は ordered factor の level 順序で比較できる。RObjC は異種比較で文字列順になる。例: levels `c("z", "a")` の `ordered("z") < "a"` は base R で `TRUE`、RObjC の対応比較は `1L`。
- factor の推移律: level 順序と文字列順序を混ぜると、等価な値で置換しても大小関係が保たれない。V-017/V-018 で扱う。
- complex: base R は `<` をエラーにする。RObjC の `RSComplex$compare()` は継承した `xtfrm()` により順序を返す。
- Date/POSIXct と数値: base R は内部の数値と比較できる場合がある。RObjC は異なるクラスなので非等価・比較不能。Date と POSIXct の base R 比較は「Incompatible methods」警告も出るため、単純な互換対象とはしない。
- 欠損値と NULL: base R の `NA == NA`、`NaN == NaN` は `NA`、`NULL == NULL` は長さ 0。RObjC の共有 NA/NULL インスタンスは自身と等価で `compare()` は `0L`。`RSNumber_withValue(NaN)` は `RSNumber$na` へ写像され、NaN と NA の区別も失われる。

## 確認履歴

- 2026-10-17: V-142 は raw 型 Vector の空/非空比較も一律エラーに修正。V-143 は hash operator の参照同一性を Vector／Array の `isEqual()` に含めた。両クラスの対象テスト、Rd 構文解析、diff check が成功。V-144 は `order(raw)` の空・単一要素だけの特例を誤って一般化したため撤回した。
- 2026-10-16: singleton NA class の `$new()` から追加 instance を作れる経路を、ユーザー指示に従い遮断。共有 instance の初期生成後に `$new` を拒否関数へ置換して binding を固定した。`RSNAFactor` は引き続き生成可能。
- 2026-10-16: ユーザーの指摘を受け V-110 を再確認。`RSNANumber$new(NA_real_)` は別個の instance を作れる一方、`RSNumber_withDouble(NA_real_)` は共有 `RSNumber$na` を返す。`RSNAFactor` に残っていた singleton マーカーを外し、その他の NA class は従来のマーカーを維持。Factor と class factory の対象テストが成功。
- 2026-10-16: V-109 を承認に従って文書修正。次の V-110 は `RSNAFactor$new()` が個別の NA インスタンスを生成し、属性を保つことを実測。RSScalarClass の「singleton」説明の修正案のみ追加。
- 2026-10-16: V-108 を承認に従って文書修正。通常／NA／既定の Factor の `levels()` を確認し、通常の有効な factor では character vector、NA でも元の levels を保持することを確認。次の V-109 は Vector の prefix 比較を実測し、文書上の型名と返り値の修正案のみ追加。
- 2026-10-16: V-107 を承認に従って文書修正。次の V-108 は Class／Protocol generator を Array に格納し、`containsObject()` と `indexOfObject()` で検索できることを確認。引数説明の修正案のみ追加。
- 2026-10-16: V-106 を承認に従って文書修正。次の V-107 は Array の変更メソッド5つの返り値を `withVisible()` で検証し、文書修正案のみ追加。
- 2026-10-16: V-105 を承認に従って文書修正。次の V-106 は Array に独自 hash operator を渡して getter の同一性と `hash()` の使用を確認し、文書修正案のみ追加。
- 2026-10-16: V-104 を承認に従って文書修正し、任意の hash operator を返す実装と説明を一致させた。次の V-105 は `hashFromObjects()` に Class／Protocol generator を渡して整数 hash を実測し、文書修正案のみ記録した。
- 2026-10-16: ユーザー指示で Encoder／Decoder、`R/util_io.r` の encode／decode 関連、Archiving strategy を棚上げ。V-103 を保留し、別領域へ移動。V-104 は独自 `hashForScalar()` 実装を持つ RObjC object を Vector に渡して再現し、提案コメントのみ追加。
- 2026-10-16: V-100 を承認に従って文書修正。`encodedClasses()` の list 要素が `RSKeyValuePair<RSString, RSInstanceMap>` であることと、`class_store` が辞書であることを分けて説明した。V-101 は前回の実装修正で完了、V-102 は判断待ちのまま。次の V-103 は `encodedClassIDs()` の返り値型を実測して提案コメントを追加。R 4.6.1 で古い `robjc.so` を読むと segfault したため `pkgload::load_all(recompile=TRUE)` で再ビルドし、空・非空の両ケースを確認した。
- 2026-10-15: V-053 の遅延収集を実測。`isStandardClass("RSString")` は `TRUE` になる一方、`std_classes` は空のままだったため、初回照会時に辞書と名前一覧を同時に設定するよう修正。registry の現用途に合わせて文書を訂正し、対象テストが成功。
- 2026-10-15: V-054 の不正な `class_ref` 入力を再現。`RSClassReference$initialize()` の型判定順序について提案コメントのみ追加。実装は未変更。
- 2026-10-15: ユーザー指示に従い `RSClassRegistry$std_classes` を廃止。V-054 の入口で RObjC 実体と `RSWrappedClass` 型を検証し、提案コメントを削除。文書とテストを更新。
- 2026-10-15: V-055 の不正な `object` 入力を再現。`RSWrappedClass$initialize()` に型確認の提案コメントのみ追加。実装は未変更。
- 2026-10-15: V-055 を承認に従って実装。RObjC class generator／instance の正常経路を維持し、異種入力を明示的に拒否。文書とテストを更新。続いて V-056 の誤った `class_ref` 受理を確認し、提案コメントのみ追加。
- 2026-10-15: V-056 を承認に従って実装し、不正なキーを構築時に拒否する対象テストが成功。続いて V-057 の戻り値を実測し、提案コメントのみ追加。
- 2026-10-15: V-057 を承認に従って実装し、`addObject()` と `lockInstance()` の不可視返却を確認。続いて V-058 の同一 class ID で異なるラッパーの等価性・hash を実測し、設計判断用コメントのみ追加。
- 2026-10-15: V-058 を承認に従って実装。同じ class ID のラッパーは等価・同 hash とし、同名・同版の別 generator の挙動も回帰テストと文書で明示。
- 2026-10-15: V-059 の異なるクラスの instance を保持できる経路を `RSInstanceMap$new()`、`addObject()`、`objects()` で実測。完全な検証には可変 set を返す API の判断が必要なため、提案コメントのみ追加。
- 2026-10-15: V-059 を承認に従って実装。初期化・追加の所属確認、可変 set への外部経路遮断、encoder の追加経路変更を行い、全テスト成功。続いて V-060 の同名・同版の別 generator による encode 失敗と状態クリアを再現し、提案コメントのみ追加。
- 2026-10-15: V-060 を承認に従って実装。`RSInstanceMap` の所属判定を class ID に統一し、同名・同版の別 generator の instance も encoder で同じ map に格納できることを確認。文書と回帰テストを更新。
- 2026-10-15: V-061 の classVersion override と generator 版の不一致を再現。2 個目の encode で既存 map が置換され、格納 instance 数が 1 のままになることを確認。検索キーを `RSWrappedClass$classID()` に統一する提案コメントのみ追加。
- 2026-10-15: `classVersion()` の全参照を確認。本体の呼出しは encoder の一箇所だけで、残りは RSObject の定義、テスト 3 箇所、英日 vignette と継承メソッド一覧の Rd。ユーザー提案に合わせ、V-061 に全廃案を追記。version metadata 自体は他の用途で必要。
- 2026-10-15: V-061 を承認に従って実装。`RSObject$classVersion()` を削除し、encoder の版数を generator metadata から取得。テストと英日 vignette、Rd の全参照を更新し、全テストと Rd の構文検証が成功。
- 2026-10-15: V-062 として、Class／Protocol generator 値の pair を encoder に渡すと `objectID()` 呼出しで失敗することを再現。serialize/unserialize 後の generator 参照が元と異なることも確認。提案コメントのみ追加し、復元後の同一性は設計判断に残した。
- 2026-10-15: V-062 の encode strategy を再検討。generator の hash はポインタ由来で serialize/unserialize 後に変わり、`isEqual()` も元との比較で `FALSE`。`RSMutableDictionary$setObjectForKey()` と `RSKeyValuePair$new()` は generator キーを現状拒否する。実行中の識別と保存後の class ID 照合を分ける方針を設計候補として記録。
- 2026-10-15: ユーザー方針に従い V-062 の encode エラーのみ修正。`RSKeyValuePair$objectsForEncoding()` から generator 値を除外し、Class／Protocol 値を含む Dictionary の encode と lock が成功。通常の object 値は従来どおり登録。strategy 再設計は保留。
- 2026-10-15: V-063 として、Array 内の RObjC インスタンスが encoder に渡らず、その class ID が未登録になることを再現。RObjC インスタンスだけを返す提案コメントを追加し、実装は判断待ち。
- 2026-10-15: V-063 を承認に従って実装。両 Array 型で RObjC 子インスタンスが encoder に登録され、generator 要素は除外されることをテスト。Rd を更新。
- 2026-10-15: V-064 として、`addEntry()` が可変キーを直接保持し、後から元のキーを変更すると旧内容で検索できなくなることを再現。`setObjectForKey()` はコピーを保持する。提案コメントのみ追加。
- 2026-10-15: V-064 を承認に従って実装。`addEntry()` を他の挿入経路と同じ key copy に統一し、可変キーの変更後も保存時の内容で検索できることを確認。Rd とテストを更新。
- 2026-10-15: V-065 として、`lockInstance()` の `rs_inherit_chain` が実在せず親クラスが登録されないことを再現。親を登録する際に新規 map の lock と class ID 重複も扱う提案コメントを追加。実装は判断待ち。
- 2026-10-15: V-065 を承認に従って実装。実際の継承チェーンから親クラスを登録し、同じ class ID の既存 map を保持。追加後の全 instance map を lock。親 `RSObject` の空 map が標準クラス辞書にないため save/load で未解決になったので、decoder は instance のない祖先 map を解決不要として扱う。全テスト成功。
- 2026-10-15: V-066 として、Set の直接構築は `NULL` 要素を受け付けるのに便利コンストラクタは拒否することを再現。API と本体を揃える提案コメントのみ追加。
- 2026-10-15: V-066 を承認に従って実装。Set の便利コンストラクタで `NULL` を受理し、`RSNull` としての重複除去・検索を確認。テストと Rd を更新。
- 2026-10-15: V-067 として、Dictionary 直接構築は key/value の `NULL` を受け付けるが便利コンストラクタは拒否することを再現。両入口の正規化を揃える提案コメントのみ追加。
- 2026-10-15: V-067 を承認に従って実装。Dictionary の便利コンストラクタで `NULL` key/value を受理し、`RSNull` として検索できることを確認。テストと Rd を更新。
- 2026-10-15: V-068 として、Array 本体は Class/Protocol 要素を受け付けるが便利コンストラクタは拒否することを再現。入口を本体と揃える提案コメントのみ追加。
- 2026-10-15: V-068 を承認に従って実装。Array の便利コンストラクタで Class/Protocol 要素を受理し、型と順序を確認。テストと Rd を更新。
- 2026-10-15: V-069 として、`class_getInstanceMethod()` の整数 index 受理と不正な名前での不統一なエラーを再現。引数検証の提案コメントのみ追加。
- 2026-10-15: V-069 を承認に従って実装。`method_name` を非空・非欠損の単一文字列に限定し、自クラスと親クラスの探索、未定義名、無効入力をテスト。
- 2026-10-15: V-070 として、公開関数 `robjc_inheritChain()` に identity record 以外を渡した際の誤返却・内部エラーを再現。引数検証の提案コメントのみ追加。
- 2026-10-15: V-070 を承認に従って実装。identity record 以外を明示的に拒否し、class・protocol の継承チェーンと不正入力をテスト。
- 2026-10-15: V-071 として、Date／POSIXct の NaN が便利コンストラクタで NA companion に変わることを直接構築と比較して再現。判定を NA のみに絞る提案コメントを追加し、実装は判断待ち。
- 2026-10-15: ユーザー指示に従い Scalar 系の直接生成と型別・汎用 API の NaN 処理を確認。V-071 を実装し、Date／POSIXct／POSIXlt の NaN が全経路で保持され、NA は共有インスタンスへ写像されることをテスト。
- 2026-10-15: NA companion の `$new()` が NaN・通常値を保持できることを追加確認。V-071 の範囲として `RSScalar$initialize()` に真の NA 限定の検証を追加。
- 2026-10-15: V-072 として、Array の無効な hash operator が生成時に通り、`hash()` 時に内部エラーになることを再現。Vector と揃えた能力検証の提案コメントのみ追加。
- 2026-10-15: V-072 を承認に従って実装。Array の演算器を構築時に検証。Dictionary／Set は外部演算器を受け取らず、既存の公開層で挿入対象の hash を検証するため追加変更なし。
- 2026-10-15: V-073 として、Encoder の不正な root 入力で既存登録が消えることと、`encodingContainer(NULL)` の内部エラーを再現。入口検証の提案コメントのみ追加。
- 2026-10-15: V-073 を承認に従って実装。両入口で不正入力を明示的に拒否し、正常な既存登録が保持されることを確認。既存の不正子オブジェクト処理テストは全体 refresh を確認する。
- 2026-10-15: V-074 として、Decoder の `unpackTo(envir=1L)` が空の class_store では成功し、instance がある場合だけ `assign()` で失敗することを再現。環境型を入口で検証する提案コメントのみ追加。
- 2026-10-15: V-074 を承認に従って実装。環境以外を常に明示的に拒否し、空・非空の class_store と正常な展開をテスト。
- 2026-10-16: V-074 の実装と監査記録を再確認し、全テスト成功。
- 2026-10-16: V-075 として、Vector の `compare(na.last=1L/0L)` だけが Scalar・Array と異なり拒否することを再現。論理値への正規化案をコメントで記録し、実装は判断待ち。
- 2026-10-16: V-075 を承認に従って実装。Vector の `na.last` を Scalar・Array と同じく正規化し、Mutable／Immutable 双方を確認。
- 2026-10-16: V-076 として、`cast_collections_to_list()` が RObjC 非 collection 入力を `NULL` に変換し、Set と Dictionary の一部で空 collection に化けることを再現。明示エラーの提案コメントのみ追加。
- 2026-10-16: V-076 を承認に従って実装。非 collection の RObjC object は明示エラーとし、空 collection は `list()` を返すことを確認。
- 2026-10-16: V-077 として、Array 要素の `isEqual()` が `NA` を返す場合に検索が停止することを再現。`isTRUE()` で一致を判定する提案コメントのみ追加。
- 2026-10-16: V-077 を承認に従って実装。`NA` は非一致とし、後続の一致要素を検索できることをテスト。
- 2026-10-16: V-078 として、`objectsNotIn()` が不定の `isEqual()` 結果を非一致ではなく要素の脱落として扱うことを再現。`isTRUE()` 使用の提案コメントのみ追加。
- 2026-10-16: V-078 を承認に従って実装。全クラスの `isEqual()` 返り値を `isTRUE()` で確定させ、非一致の要素が差集合と和集合から消えないことを確認。
- 2026-10-16: V-079 として Dictionary の `isEqual()` が logical ベクトルを返す状態を再現。前回の「単一の真偽値」方針に従い `all()` と `isTRUE()` で修正した。
- 2026-10-16: V-080 として、`areEqualSets()` の重複と hash 衝突による誤判定を実際の `RSString` 値で再現。双方向の差集合で判定する提案コメントのみ追加。
- 2026-10-16: ユーザー指摘により V-080 を対象外に変更。Set は重複を保持しない既確認の前提を適用し、提案コメントを撤去。
- 2026-10-16: V-081 として、RSString が factor を保持した場合に同じラベルの String と順序比較が逆転することを再現。factor ラベルを character として保存する提案コメントのみ追加。
- 2026-10-16: V-081 を承認に従って実装。RSString の factor 入力を character ラベルに正規化し、等価性・hash・順序、RSFactor の nativeValue をテスト。
- 2026-10-16: V-082 として、RSNABoolean の `asNumber()` が欠損数値を直接生成しようとして停止することを再現。便利コンストラクタ経由の提案コメントのみ追加。
- 2026-10-16: V-082 を承認に従って実装。RSNABoolean の数値変換が共有 RSNANumber を返すことを確認。
- 2026-10-16: V-083 として、可変な RSKeyValuePair を Dictionary のキーにした場合、挿入時の `copy()` が同一参照を返して元の変更で検索不能になることを再現。コピーを独立・固定化する提案コメントのみ追加。
- 2026-10-15: 保存・復元の入口を再開。`robjc_load(file)` が `hookFunction` の転送で即時停止することを再現（V-042）。`robjc_save()` のファイルを `base::load()` で開き、実際のメタデータ名と `robjc_load()` の判定名の不一致を確認（V-043）。修正案コメントのみ追加。
- 2026-10-15: ユーザーの手動変更を確認。`hookFunction` は仮引数からコメントアウトし転送停止、メタデータ名も修正済み。`robjc_save()` → `robjc_load()` で返却名 `item`、内部メタデータ非公開、`RSString` のクラス再結合を実測。V-042/043 を完了に更新し、公開文書と回帰テストを追加。
- 2026-10-15: `robjc_save_image()` の入力環境に内部予約名と同名の利用者変数を置き、保存ファイル内で無警告の上書きを再現（V-044）。共通ヘルパーへの事前検証案だけをコメントで追加。
- 2026-10-15: V-044 を承認に従って実装。共通ヘルパーで保存前に予約名を拒否し、`robjc_save()` と `robjc_save_image()` の両経路で利用者の値とファイル未作成を確認するテストを追加。関連文書を更新。
- 2026-10-15: `robjc_save(eval.promises = FALSE)` が delayed binding を強制評価することを `base::save()` と比較して再現。`precheck` も mget() の先行実行と整合しないため、V-045 の判断用コメントを追加。
- 2026-10-15: 独自クラスを `class_map` なしで `robjc_read_rds()` と `robjc_load()` に渡すと、文書の警告ではなく両方とも `Unresolved classIDs` で停止することを再現（V-046）。修正案コメントのみ追加。
- 2026-10-15: V-046 はユーザーの手動整理を確認。save/load 更新計画に向け、到達不能な警告処理とその意図を残す判断を記録。空の `RSMutableDictionary` を `class_map` に渡すと両復元入口が拒否することを再現し、V-047 の修正案コメントを追加。
- 2026-10-15: V-047 を承認に従って実装。両復元入口の `class_map` 判定を Decoder/merge と同じ RObjC 辞書判定に揃え、文書を更新。テストは追加しない。
- 2026-10-15: RObjC object を通常の list に入れて `robjc_save()` → `robjc_load()` すると class 参照が復元されないことを再現（V-048）。対応するコンテナ範囲は判断待ちとし、提案コメントのみ追加。
- 2026-10-15: ユーザー指示で `R/util_io.r` 全体を棚上げし、現状のコードを保持。公開 API と具象クラスの契約確認へ移る。V-049 の非数値 Date 受理を直接生成・便利コンストラクタで再現し、提案コメントのみ追加。
- 2026-10-15: V-049 を承認に従って実装。Date の内部型を生成前に検証し、文書と回帰テストを更新。
- 2026-10-15: V-049 の対象テスト `devtools::test(filter="RSDate")` が成功。V-050 の文字列実体を持つ POSIXct 受理を直接生成・便利コンストラクタで再現し、提案コメントのみ追加。
- 2026-10-15: V-050 を承認に従って実装。POSIXct 正規化後に数値内部型を確認し、文書と回帰テストを更新。
- 2026-10-15: V-050 の対象テスト `devtools::test(filter="RSDatetime")` が成功。3 つのクラス／protocol ファクトリーが空文字と NA の名前を生成することを再現（V-051）。修正案コメントのみ追加。
- 2026-10-15: V-051 を承認に従って実装。3 ファクトリーの名前を非空・非欠損に限定し、文書と回帰テストを更新。
- 2026-10-15: V-051 の対象テスト `devtools::test(filter="factories")` が成功。V-052 の空文字・NA 要求メソッドが protocol metadata に残ることを再現し、提案コメントのみ追加。
- 2026-10-15: V-052 を承認に従って実装。要求メソッド名を非空・非欠損に限定し、要求なしは引き続き受理。文書と回帰テストを更新。
- 2026-10-15: V-052 の対象テスト `devtools::test(filter="factories")` が成功。V-053 の registry がロード後も空のままなのを確認し、同時点の再収集なら 33 件得られることを実測。提案コメントのみ追加。

- 2026-10-14: コレクションの確認を開始。V-021 を `pkgload::load_all()` で再現し、追加・挿入・置換の各経路に提案コメントのみ追加。実装は未変更。
- 2026-10-14: V-021 を承認に従って実装。変更メソッドに型検証を加え、確認コメントを削除。異常入力後の内容維持をテストし、文書を更新。
- 2026-10-14: V-021 実装後に `pkgload::load_all()` による全テストが成功。V-022 の順序逆転と非等価な配列の比較結果 `0` を再現し、提案コメントのみ追加。
- 2026-10-14: V-022 を承認に従って辞書式比較へ変更。要素の `compare()` と配列長で順序を決定し、集約 hash を順序判定に使わない。
- 2026-10-14: V-022 の交換要素、要素順序、共通接頭辞、空配列、mutable/immutable 相互比較、NA と不正入力をテスト。`pkgload::load_all()` による全テスト、Rd 構文解析、`git diff --check` が成功。
- 2026-10-14: V-023 を実測で再現。同内容の immutable/mutable 配列で `compare() == 0L` と `isEqual() == FALSE`、非オブジェクト入力で `$` エラーを確認。提案コメントのみ追加。
- 2026-10-14: V-023 を承認に従って修正。mutable/immutable 間の内容比較を認め、異種入力を安全に `FALSE` とする。テストと文書を更新。
- 2026-10-14: V-023 の全テストが成功。V-024 の小数 index による暗黙切り捨てと挿入時の内容変化を再現し、共通検証案のコメントのみ追加。
- 2026-10-14: V-024 を承認に従って実装。4 メソッドの index 検証を private helper に統合し、暗黙の小数切り捨てを拒否する。
- 2026-10-14: V-024 実装後の全テストが成功。V-025 の文書と参照保持の不一致を、可変子配列と親配列のコピーで再現。提案コメントのみ追加。
- 2026-10-14: V-025 を承認に従って文書のみ修正。要素参照を保持する実装と、浅いコピーの説明を一致させた。
- 2026-10-14: V-026 を名前付き `index=` 呼出しで再現。RSArray の変更禁止 override 3 件に提案コメントのみ追加。
- 2026-10-14: V-026 を承認に従って修正。3 つの override の仮引数と Rd を親の API と揃え、名前付き呼出しのテストを追加。
- 2026-10-14: V-026 の全テストが成功。V-027 の `setKey(1L)` 後の hash エラーと `isEqual(1L)` の型エラーを再現し、提案コメントのみ追加。
- 2026-10-14: 非 RObjC 入力に対する全 `isEqual()` 実装を棚卸し。大半は `FALSE`、V-027・028 は `$` エラー。DataFrame の逆条件も再現。方針判断のため、V-027 実装は保留した。
- 2026-10-14: 既存の全体方針である `FALSE` に合わせ V-027・028 を実装。`setKey()` の不正 key は変更操作として `stop` にした。
- 2026-10-14: V-027・028 の非 RObjC 入力、DataFrame 同士の等価性、`setKey()` 失敗後の状態をテスト。全テスト、両 Rd の構文解析、`git diff --check` が成功。
- 2026-10-14: 全 `isEqual()` 実装と代表クラスの非 RObjC 入力を再確認し、例外なく `FALSE` と判定。V-029 の通常 DataFrame の hash エラーを再現し、提案コメントのみ追加。
- 2026-10-14: V-029 を承認に従って修正。ユーザー指定によりテストコードは追加しない。
- 2026-10-14: V-029 の通常 DataFrame の `hash()` が整数を返すことだけ実測。次に V-030 の等価な DataFrame 間の hash 不一致を再現し、提案コメントのみ追加。
- 2026-10-14: V-030 を再検討。base R の `identical()` は列型・factor levels・行名・列順を区別し、`all.equal()` は近似値を等価とする。現 `isEqual()` の非推移律と同じキー列のときの hash 不一致も実測。hash と等価性を一緒に再設計するまで実装を保留。
- 2026-10-14: ユーザー指示により DataFrame 周りを棚上げ。V-030 の `#! validation check` と `#todo` を残し、他のコレクションの確認へ進む。
- 2026-10-14: Vector の immutable コピーが元の mutable インスタンスと等価・比較可能でないことを実測。V-031 の提案コメントのみ追加し、実装は変更していない。
- 2026-10-14: V-031 を承認に従って実装。mutable/immutable 間の等価性と比較をテストし、Rd を更新。
- 2026-10-14: V-031 実装後の全テストが成功。V-032 を空 Vector と空 Array で実測し、提案コメントのみ追加。
- 2026-10-14: V-032 は集約 hash の正しさには影響しないことを確認。ユーザー判断で要素数と戻り値の長さを一致させ、空入力を `integer()` に変更。
- 2026-10-14: V-032 の空戻り値・集約 hash と全テストを確認。V-033 の衝突時の偽陽性と比較の非対称性を実測し、提案コメントのみ追加。
- 2026-10-14: V-033 を再評価。再現例には重複があり、重複のない集合では片方向の包含判定で十分と確認。提案コメントを撤去し、実装は維持。
- 2026-10-14: V-034 の間接継承クラスの一覧漏れを実測。自動解決 map の作成元も同じ関数であることを確認し、提案コメントのみ追加。
- 2026-10-14: V-034 を承認に従って実装。直接の親だけを見る判定を継承鎖の確認に変更。RSArray の保存・復元の実測に成功。
- 2026-10-14: V-035 の `FALSE` 引数の戻り値が文書と異なることを実測。公開関数と内部辞書の利用側へ提案コメントのみ追加。
- 2026-10-14: V-035 を承認に従って実装。公開関数を名前付き generator list に変更し、標準クラス辞書の生成側を追従。
- 2026-10-14: V-035 の全テストが成功。関連する class registry の説明も間接継承に合わせた。V-036 の無効な class_map 入力を実測し、提案コメントのみ追加。
- 2026-10-14: V-036 を承認に従って実装。辞書型の確認を先に行い、無効な入力を明示的な引数エラーにした。
- 2026-10-14: V-037 の S3 class 属性のみの偽辞書を decoder が受理することを実測。入力検証案のコメントのみ追加。
- 2026-10-14: V-037 を承認に従って実装。辞書判定を読み込み前に行い、偽辞書では decoder の状態を変更しない。
- 2026-10-14: V-037 の全テストが成功。V-038 の未解決 class ID による部分更新を 2 クラスの encoded data で再現し、事前確認案のコメントのみ追加。
- 2026-10-14: V-038 を承認に従って実装。全 class ID の対応を確認してからインスタンスの class を更新する。未解決 ID があると両インスタンスの class が不変で、対応を追加した再実行は成功することをテスト。decoder 単体と全テストが成功。
- 2026-10-14: V-039 の不正な対応値による部分更新を 2 クラスで再現。値型・class ID・元クラスとの互換性を事前確認する案のコメントのみ追加。
- 2026-10-14: V-039 はユーザー方針に従い `class_map` 全項目を入口で検証する形で実装。誤った値型と key/value の class ID 不一致を拒否し、失敗時にインスタンスの class が変わらないことをテスト。decoder 単体と全テスト、関連 Rd の構文解析、`git diff --check` が成功。
- 2026-10-14: V-040 の子 encode 失敗後に親が訪問済みのまま残り、再実行で子が欠落することを実測。失敗時の訪問済み印の取り消し案のみ追加。
- 2026-10-14: ユーザー指摘により V-040 の部分巻き戻し案を撤回。失敗時は `class_store` と `visited_env` をまとめて `refresh()` し、全ルートを再投入する方針へ提案コメントを更新。
- 2026-10-14: V-040 を承認に従って実装。encodeObject() の再帰をローカル関数にまとめ、失敗時に encoder 全体を refresh() する。
- 2026-10-14: V-040 の失敗時全クリア、全ルート再投入、循環参照をテスト。全テストが成功。V-041 の凍結後のクラス登録と再凍結時の警告を実測し、提案コメントのみ追加。
- 2026-10-14: V-041 を承認に従って実装。凍結後の登録拒否、辞書の immutable 化、再凍結の冪等性、refresh 後の再利用をテストした。Encoder 単体・全テスト、構文解析、差分検査が成功。Encoder の確認をここで区切る。
- 2026-10-14: 使用例の作成中に RDS 復元後の generator 参照が異なることを確認。V-037 の辞書判定を RObjC 実体検査と S3 継承属性の併用に調整し、Decoder の class ID 検索キーを現行 `RSString` で再生成した。標準・独自クラスの RDS 往復テストを追加。英文・和文の使用例と既存 strategy をレンダリングし、全テストと `git diff --check` が成功。

- 2026-10-14: `R/`、`src/`、`tests/testthat/` の構成と型判定の実装を確認。インストール済みの現行パッケージで V-001〜003 のエラーを再現。V-001 は有効な generator だけの入力でも誤判定を再現。`R/runtime_robjc.r` の構文解析と `git diff --check` は成功。実装は変更せず、確認コメントのみ挿入。
- 2026-10-14: V-001〜003 の提案を承認に従って実装。該当する確認コメントを削除し、3 関数の roxygen 説明と Rd を追加。混在入力と空入力のテストを追加。`R CMD INSTALL` と `testthat::test_dir()` による全テストは成功。
- 2026-10-14: 利用箇所を再確認し、V-001〜003 の関数は単要素用述語に `vapply()` するだけの未使用 wrapper と判断。方針を削除に変更し、3 関数と専用の Rd・テストを削除。再インストール後の全テスト、namespace に関数が残らないこと、`git diff --check` を確認済み。
- 2026-10-14: singleton identity の戻り値と `respondsTo()` の不正入力を確認し、V-004〜005 の提案コメントを追加。実装は未変更。
- 2026-10-14: V-004〜005 を承認に従って修正し、確認コメントを削除。戻り値と入力検証のテストを追加。再インストール後の全テストは成功。続いて private メソッドの照会 V-006 を再現し、提案コメントを追加。実装は未変更。
- 2026-10-14: V-006 を承認に従って修正し、公開メソッドのみ照会するテストと Rd を更新。再インストール後の全テストは成功。続いてファクトリーの入力 V-007〜008 を再現し、共通初期化箇所に提案コメントを追加。実装は未変更。
- 2026-10-14: V-007〜008 を承認に従って実装。`robjc_validate_isa_metadata()` を追加し、`robjc_make_isa()` と Object・Singleton・Scalar・Protocol の各ファクトリーで再利用。重複する version/protocol 検証を整理し、テストと Rd を更新。再インストール後の全テストは成功。
- 2026-10-14: 共通検証関数の外出しを取り消し、検証を `robjc_make_isa()` 内に戻した。`RSProtocolClass()` は親 protocol の読取前に必要な検証のみ保持。
- 2026-10-14: `robjc_isRobjcObject()` の不正な環境入力で V-009 を再現。提案コメントのみ追加し、実装は未変更。
- 2026-10-14: インスタンスの S3 クラス属性を調査。R6 2.6.1 で `class = FALSE` のインスタンスは既定で `class(x) == "environment"`、`class = TRUE` は継承名と `"R6"` を付与する。現行の `RSObject$initialize()` は S3 属性を付与するが、これを呼ばないサブクラスでは付与されない。`resolveClassWith()` で異なる継承構造の同名・同版クラスへ切り替えると、S3 の `inherits()` と ISA に基づく `isKindOf()` が食い違う。`classname` の binding は変更可能で、文字列のキャッシュはその変更を反映しない。6 階層・5,000 回の簡易計測では、継承鎖とクラス名の計算は約 0.047 秒、インスタンス生成全体は約 0.952 秒。S3 方針とキャッシュ方針は未決定。
- 2026-10-14: `super$initialize()` を呼ばないサブクラスは作法違反として検討対象から除外。`resolveClassWith()` で S3 クラス属性を再設定し、V-009 の環境型チェックを実装。継承構造が変わる同名・同版クラスへの解決と不正な環境入力のテストを追加。
- 2026-10-14: V-009 と S3 属性の修正後、再インストールして全テスト成功。次に V-010 の混合比較と、異なる値での誤った等価判定・hash 不一致を再現し、`RSFactor.r` に提案コメントのみ追加。
- 2026-10-14: V-010 を承認に従って修正。同じ因子定義の間では因子順序、それ以外では文字列値で比較し、混合型の等価性と hash のテストを追加。
- 2026-10-14: V-010 の Rd 更新と再インストール後の全テストに成功。続いて V-011 の `na.last` 不正入力を再現し、提案コメントのみ追加。
- 2026-10-14: `na.last` を強制変換する方針を採用し、`RSScalar`・`RSFactor`・`RSRaw` の比較で `as.logical()` と結果の長さ・欠損確認を揃えた。テストと Rd を更新し、再インストール後の全テスト、Rd の構文解析、`git diff --check` は成功。
- 2026-10-14: V-012 の整数範囲外の日付と NA 日付で `NA_integer_` の hash を再現し、提案コメントのみ追加。
- 2026-10-14: NA スカラーの hash を `0L` に統一。`RSScalar`、`RSDate`、`RSDateTime` の実装を修正し、整数範囲外の日付にはスカラー用 hash を使用。NA companion 全種と範囲外 Date のテスト・Rd を追加し、再インストール後の全テスト、Rd 構文解析、`git diff --check` は成功。
- 2026-10-14: V-013 の直接生成した基底スカラーと具象クラスの等価性・hash 不一致を再現。比較時の値の種類と hash 正規化について提案コメントのみ追加。
- 2026-10-14: V-013 の比較側を承認どおり実装。`RSScalar$hash()` は単純なままとし、Date/POSIXt の直接生成を拒否して `RSDate`/`RSDateTime` の hash に任せる。
- 2026-10-14: V-013 のテストと Rd を更新し、再インストール後の全テスト、Rd 構文解析、`git diff --check` は成功。続いて V-014 の RSNull 自身との比較エラーを再現し、提案コメントのみ追加。
- 2026-10-14: `RSScalar` を抽象クラスとする設計に変更。`initialize()` の最初に直接生成を拒否し、前回追加した値の種類判定と Date/POSIXt 専用拒否を撤去。具象クラスの比較テストと文書を更新。再インストール後の全テスト、Rd 構文解析、`git diff --check` は成功。
- 2026-10-14: V-014 を承認に従って実装。`RSNull$compare()` は自身に `0L`、異種オブジェクトに比較不能エラーを返す。テストと Rd を更新し、再インストール後に `pkgload::load_all()` を使った全テストと Rd 構文解析が成功。続いて V-015 の空の `description()` を再現し、提案コメントのみ追加。
- 2026-10-14: V-015 を承認に従って実装。`RSNull$description()` が長さ 1 の表示文字列を返すことをテストし、Rd を更新。`pkgload::load_all()` を使った全テストと Rd 構文解析が成功。続いて V-016 の factor 比較結果が double 型になることを再現し、提案コメントのみ追加。
- 2026-10-14: V-016 を承認に従って実装。`RSFactor$compare()` の level 順序分岐も整数値を返すよう修正し、テストと Rd を更新。`pkgload::load_all()` を使った全テストと Rd 構文解析が成功。続いて V-017 の比較順序と等価性の不整合を再現し、提案コメントのみ追加。
- 2026-10-14: V-017 は factor と string を比較不能とする方針に変更。両方向の `compare()`・`isEqual()` と NA companion を揃え、テスト・Rd を更新。`pkgload::load_all()` を使った全テストと Rd 構文解析が成功。異なる levels の factor 同士でも同様の不整合 V-018 を再現し、提案コメントのみ追加。
- 2026-10-14: factor/string の比較不能化をいったん差し戻し、V-016 の整数型修正は維持。テストと Rd を元の相互比較契約へ戻し、`pkgload::load_all()` による全テスト、Rd 構文解析、`git diff --check` が成功。R 4.5.1 の base 演算と各 Scalar の等価・大小比較を実測し、上の差分表に記録した。
- 2026-10-14: base R の値比較を `RSScalar$nativeCompare()` に分離。factor は当初 `asRValue()`（後に `nativeValue()` へ改名）で levels を復元する。`isEqual()` は `compare()` に依存せずキー等価性を返し、NA/NULL は共有オブジェクトの同一性を維持。`compare()` は complex、通常の factor、異なる levels、factor/string 間で独自順序を返さない。全テスト、対象 Rd 構文解析、`R CMD INSTALL`、`git diff --check` が成功。
- 2026-10-14: `RSNumber`・`RSComplex` のファクトリと `RSScalar$initialize()` で `is.na()` と `is.nan()` を区別し、NaN を通常の値として保持。キーの `isEqual()` は NA と NaN を区別し、NaN 同士は保持値が `identical()` の場合だけ等価とする。NA・NaN の hash はともに `0L` を許容し、`compare()` では両者を欠損位置に置く。数値・複素数と Dictionary キーのテスト、文書を更新。全テスト、対象 Rd の構文解析、`git diff --check` が成功。
- 2026-10-14: `asRValue()` を `nativeValue()` に改名。`value()` はオブジェクト側の値、`nativeValue()` は元の R 型の値という契約にし、Factor のみ factor を復元する。Complex の `value()` と `nativeValue()` はともに複素数スカラー。文書とテストを更新。全テスト、対象 Rd 構文解析、`git diff --check` が成功。
- 2026-10-14: `valueCompare()` を `nativeCompare()` に改名。`compare()` は base R のソート順に合わせ、complex は実部→虚部、unordered factor も levels 順、raw はソート不能とした。NA/NaN は同順位とし、`RSSorter$sortByCompare()` は同順位の入力順を維持する。異なる factor levels や factor/string の組は共通の順序を定義せずエラーを維持。全テスト、対象 Rd 構文解析、`git diff --check` が成功。
- 2026-10-14: Native adapter の pointer 検証を開始。Dictionary 用 pointer が `RSConcreteSet$new(.native_ptr=...)` に受理されることを、危険な bridge 呼出しをせずに再現。V-019 の提案コメントを R と C++ に追加した。
- 2026-10-14: `typeof()` は external pointer の個別型名を設定できないことを確認。`.native_ptr` は internal 引数と文書に記載済みのため、V-019 は見送り。提案コメントを除去し、実装は変更しない。
- 2026-10-14: インストール済み robjc 0.0.3（R 4.5.1）で V-020 を再現。2 回目の `hash()` だけ失敗するキーを使い、呼出し 2 回・`hash failure`・`count()==0` を観測した。作業ツリーの C++ ソースに失敗時の release がない点を確認。GC による残存は実証していないため、提案を「例外経路を確認してから解除」に限定した。
- 2026-10-14: V-020 を一度 Concrete adapter と C++ で修正し、新ビルドとテストを確認した。
- 2026-10-14: ユーザーの責務指定に従い、直前の Concrete adapter の検証と C++ 例外処理を撤回。`RSMutableDictionary`・`RSMutableSet` の挿入直前へ hash 検証を移し、immutable 派生クラスの初期化も同じ経路を通ることを確認。再ビルド、全テスト、対象 Rd 構文解析、`git diff --check` が成功。
- 2026-10-17: V-139・V-140 で組込 Scalar を R 標準クラス入力に限定した。対象の Scalar テスト、Rd 構文解析、`git diff --check` は成功。全テストでは棚上げ中の `RSClassEncoder`・`RSClassDecoder`・`RSInstanceMap` 周辺に失敗が残り、`RSClassEncoder` の単独実行でも失敗を確認した。この監査では Encoder／Decoder の修正を行わない。
