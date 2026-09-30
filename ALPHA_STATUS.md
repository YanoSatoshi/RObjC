# robjc Alpha Status Report

更新日: 2026-09-29  
対象バージョン: 0.0.1 alpha  
対象ブランチ: `Encoding`  
基準コミット: `c0e02d6` (`C++ Dic & Set`)

## 1. 概要

`robjc`は、Objective-C/Foundationのオブジェクトモデルから着想を得た、R6ベースのR向けオブジェクト指向フレームワークである。

現在の実装は、単にObjective-C風の命名やメソッド呼び出しを再現するものではなく、次の機能をひとつのオブジェクトモデルとして提供している。

- `RSObject`を基底とするクラス階層
- `isMemberOf()`、`isKindOf()`、`respondsTo()`によるruntime introspection
- protocol、module、singletonの登録と参照
- `copy()`、`mutableCopy()`によるcopy semantics
- `hash()`と`isEqual()`に基づく値・collection semantics
- `RSNull`による「存在するnull値」の表現
- scalar、array、data frame、dictionary、setのFoundation風wrapper
- class metadataとobject graphを保存するencoder/decoder
- C++ native storageを利用したmutable dictionary/set

alpha段階として、主要な設計要素は相互に接続され、テスト可能な状態に到達している。特にDictionaryとSetについては、Rのcopy-on-writeに依存した旧hash tableを廃止し、C++の`std::unordered_map`および`std::unordered_set`へ移行した。

## 2. 現在の評価

現状は、`0.0.1-alpha`相当の基準点として十分に成立している。

成立している理由は次のとおりである。

1. オブジェクトモデルの中心となる`RSObject`、class metadata、protocol conformanceが動作している。
2. 値オブジェクトのhash/equalityがcollectionから実際に利用されている。
3. mutable/immutable collectionの役割とcopy semanticsが定義されている。
4. native pointerを公開APIへ露出させず、R側adapterへ封じ込めている。
5. native collectionを含むobject graphを専用serializer経由で保存・復元できる。
6. 旧`RSDynamicHash`と、その専用補助だった`RSEntryFinder`を削除し、collection実装が一本化されている。
7. package全体に150個の`test_that()`ブロックがあり、現行テストはすべて成功している。
8. C++を含む`R CMD INSTALL --preclean`が成功している。

一方で、API互換性、過去のserialized dataとの互換性、performance benchmark、encoder/decoderの細部はalpha以降も調整対象である。

## 3. Package構成

主要なdirectoryは次の役割を持つ。

```text
R/
  000_*                         基底runtimeと共通utility
  001_*                         module/protocol/singleton登録処理
  002_*                         module/protocol/singleton基底クラス
  003_*                         protocolとscalar基盤
  010_* - 012_*                 hash、sort、null
  031_*                         R実装のcollection/value補助
  040_RSConcreteDictionary.r    C++ Dictionary adapter
  040_RSConcreteSet.r           C++ Set adapter
  041_RSMutableDictionary.r     公開mutable Dictionary
  041_RSMutableSet.r            公開mutable Set
  RS*.r                         公開・内部クラス
  RcppExports.R                 自動生成されたRcpp bridge

src/
  RSNativeDictionary.cpp        std::unordered_mapベースのnative storage
  RSNativeSet.cpp               std::unordered_setベースのnative storage
  RSObjectHash.h/.cpp           R object用hash/equality functor
  RcppExports.cpp               自動生成されたnative registration

tests/testthat/
  class単位のtestthat tests

vignettes/
  encoding/decoding設計文書

man/
  roxygen2生成文書
```

`RcppExports.R`、`RcppExports.cpp`、`NAMESPACE`、`man/*.Rd`は生成物である。変更は原則として元のR/C++ sourceへ行い、`Rcpp::compileAttributes()`または`roxygen2::roxygenise()`で更新する。

## 4. レイヤ構造

現在の設計は、おおむね次の4層に分かれている。

```text
Public R API
  RSMutableDictionary / RSDictionary
  RSMutableSet        / RSSet
             |
             v
Internal R Adapter
  RSConcreteDictionary
  RSConcreteSet
             |
             v
Rcpp Bridge
  R/RcppExports.R
  src/RcppExports.cpp
             |
             v
C++ Native Storage
  NativeDictionary
  NativeSet
  RSObjectHash / RSObjectEqual
```

この分割により、`.native_ptr`は`RSConcreteDictionary`および`RSConcreteSet`のprivate fieldに限定される。公開クラスのinitializerやconvenience constructorはexternal pointerを受け取らない。

## 5. Core Object Model

### 5.1 RSObject

`RSObject`はすべてのrobjc objectの基底クラスである。主な責務は次のとおり。

- class objectへの参照をprivate `.__isa`に保持する
- class version、inheritance chain、super class、protocol一覧をclass metadataへ設定する
- `class()`、`superClass()`、`inheritClassesAsList()`を提供する
- `isMemberOf()`と`isKindOf()`を提供する
- `respondsTo()`と`conformsTo()`を提供する
- identityベースの既定`isEqual()`と`hash()`を提供する
- 既定の`copy()`を提供する
- encoding traversal用の`objectsForEncoding()`を提供する
- decoding時に`resolveClassWith()`でclass objectを差し替える

class metadataはR6 class generator上の`robjc_isa` environmentに保持される。instanceはそれを参照し、同一classのmetadataを共有する。

### 5.2 Protocol

現在、少なくとも次のprotocolが存在する。

- `RSCopying`
- `RSMutableCopying`
- `RSEncoding`
- `RSEncoderProtocol`
- `RSDecoderProtocol`
- `RSHashOperatorProtocol`

`conform_protocol_to_class()`は、対象classのmetadataへprotocol conformanceを登録する。Objective-Cのcompile-time protocol checkではなく、robjc runtime metadataによる動的な仕組みである。

### 5.3 ModuleとSingleton

`RSModule`と`RSSingleton`は、class generator側のslotへ共有instanceを登録する。

代表的なmoduleは次のとおり。

- `RSHashOperator`
- `RSSorter`
- `RSClassEncoder`
- `RSClassDecoder`

`RSClassRegistry`はsingletonとしてpackage load時に初期化され、standard robjc classの解決に利用される。

登録関数そのものは`lockBinding()`で固定されており、通常のpackage利用中に差し替えられない。

## 6. Scalarと値オブジェクト

`RSScalar`はlength 1のR値を包む基底クラスであり、次を共通化している。

- scalar length validation
- NA wrapperへのrouting
- `compare()`
- valueベースの`isEqual()`
- scalar valueからの`hash()`
- `value()`

主な具象クラスは次のとおり。

- `RSString` / `RSNAString`
- `RSNumber` / `RSNANumber`
- `RSBoolean` / `RSNABoolean`
- `RSDate` / `RSNADate`
- `RSDateTime` / `RSNADateTime`
- `RSFactor` / `RSNAFactor`

scalar hashはclass-levelに保持された`RSHashOperator`へroutingされる。これにより、同値なscalar objectは同じhashを返し、native Dictionary/Setのkey/memberとして利用できる。

## 7. NULLとNA

Rの`NULL`はlistやcollectionへの代入時に「値」ではなく削除や非存在として扱われる場合がある。そのためrobjcでは、Foundationの`NSNull`に相当する`RSNull`を用意している。

Dictionary/Setの公開wrapperは、Rの`NULL`を境界で`RSNull$null()`へ変換する。

これにより、次を区別できる。

- key/memberが存在しない
- key/memberが存在し、その値がnullである

一方、Rの`NA`は型別の`RSNA*`クラスで扱う。`RSNull`と`RSNA*`は別の概念である。

## 8. Dictionary

### 8.1 公開クラス

- `RSMutableDictionary`: mutable API
- `RSDictionary`: immutable API

主な操作は次のとおり。

- `setObjectForKey()`
- `objectForKey()`
- `containsKey()`
- `removeObjectForKey()`
- `removeAllObjects()`
- `allKeysAsList()` / `allValuesAsList()`
- `count()`
- `hash()` / `isEqual()`
- `copy()` / `mutableCopy()`

### 8.2 Storage semantics

Dictionaryへ挿入するkeyは`copy()`される。valueはcopyせず保持される。

native storageは次の型である。

```cpp
std::unordered_map<SEXP, SEXP, RSObjectHash, RSObjectEqual>
```

`RSObjectHash`はkeyのR method `hash()`を呼び、`RSObjectEqual`は`isEqual()`を呼ぶ。このためC++側はrobjc固有classを直接知らず、SEXPのstorage semanticsだけを担当する。

### 8.3 Copy semantics

- `RSMutableDictionary$copy()`は新しいimmutable `RSDictionary`を返す
- `RSMutableDictionary$mutableCopy()`は新しいmutable dictionaryを返す
- `RSDictionary$copy()`は`self`を返す
- `RSDictionary$mutableCopy()`は新しいmutable dictionaryを返す

新しく作られるcopyは独立したnative tableを持つ。ただしtable内のkey/value objectはshallowに共有される。なおkeyは最初の挿入時点でcopy済みである。

## 9. Set

### 9.1 公開クラス

- `RSMutableSet`: mutable API
- `RSSet`: immutable API

主な操作は次のとおり。

- `addObject()`
- `member()`
- `containsObject()`
- `anyObject()`
- `removeObject()`
- `removeAllObjects()`
- `allObjectsAsList()`
- `count()`
- `uniteSet()` / `intersectSet()` / `subtractSet()`
- `hash()` / `isEqual()`
- `copy()` / `mutableCopy()`

集合演算はreceiverと引数を変更せず、新しいimmutable `RSSet`を返す。

### 9.2 Storage semantics

Setへ挿入するmemberはcopyせず、そのobjectを保持する。同値objectを再挿入した場合、最初に格納されたrepresentativeが維持され、`member()`で取得できる。

native storageは次の型である。

```cpp
std::unordered_set<SEXP, RSObjectHash, RSObjectEqual>
```

### 9.3 Copy semantics

- `RSMutableSet$copy()`は独立したnative tableを持つimmutable `RSSet`を返す
- `RSMutableSet$mutableCopy()`は独立したnative tableを持つmutable setを返す
- `RSSet$copy()`は`self`を返す
- `RSSet$mutableCopy()`は独立したnative tableを持つmutable setを返す

member object自体はcopyされず、shallowに共有される。

## 10. Native Storageとmemory management

`NativeDictionary`と`NativeSet`は、格納したSEXPをRのGCから保護する必要がある。そのため各native instanceは`R_NewPreciousMSet()`で作成したprecious multisetを所有する。

storageへの追加時に`R_PreserveInMSet()`を呼び、削除時に`R_ReleaseFromMSet()`を呼ぶ。native objectのdestructorではprecious multiset自体を`R_ReleaseObject()`する。

外部pointerは`Rcpp::XPtr<T>`で所有され、finalizerによってnative objectが破棄される。

現在のC++層の責務は次に限定されている。

- create/clone
- set/add
- get/member/contains
- remove/clear
- keys/values/all members
- count
- set union/intersection/subtraction
- SEXPのlifetime保持

`RSNull`、copy policy、immutable policy、encoding policyはR側wrapperの責務である。

## 11. Hashとequalityの契約

native collectionの正当性は、次の契約に依存する。

1. `a$isEqual(b)`が`TRUE`なら、`a$hash()`と`b$hash()`は同じ値を返すこと。
2. Dictionary keyまたはSet memberとして格納されている間、hash/equalityに影響する状態を変更しないこと。
3. `isEqual()`はcollection lookup中に安定した結果を返すこと。

格納後にhashが変わると、`std::unordered_map`/`std::unordered_set`が期待するbucketと実際のbucketが一致しなくなる。結果として、存在するobjectを検索・削除できない可能性がある。

これはnative collectionの不具合ではなく、hash collection一般の制約である。alpha APIでは明示的な契約として扱う。

## 12. ArrayとDataFrame

`RSArray`/`RSMutableArray`および`RSDataFrame`/`RSMutableDataFrame`は現在もR側実装である。

Dictionary/Setとは異なり、現段階でnative化の対象にはしていない。順序付きcollectionやdata frame固有の操作は、hash collectionとは別のperformance特性とAPI設計を持つためである。

## 13. EncodingとDecoding

### 13.1 目的

通常のR serializationだけでは、load先でR6 class generatorが差し替わった場合や、package/user classを解決し直す必要がある場合に、robjc class metadataの復元が不十分になる。

robjcのencoder/decoderは、object graphとclass-instance対応をmetadataとして保存し、load後にclass objectを現在のruntimeへ解決し直す。

### 13.2 Encoding path

概略は次のとおり。

```text
root object
  -> RSClassEncoder$encodeObject()
  -> objectsForEncoding()を再帰走査
  -> class IDごとにRSInstanceMapへinstanceを登録
  -> RSClassEncoder$lockInstance()
  -> immutable RSDictionary metadata
  -> robjc_save*()で本体とmetadataを保存
```

同一objectの再訪はobject identityで検出され、循環参照や共有参照を無限に再帰しないよう管理される。

### 13.3 Decoding path

概略は次のとおり。

```text
saved object + metadata
  -> RSClassDecoder$loadEncodedData()
  -> class IDをstandard/user class mapから解決
  -> 各instanceのresolveClassWith()
  -> 現在のR6 class generatorへ再接続
```

class versionはclass IDの一部として扱われ、`can_resolve_class()`で互換性を検査する。

### 13.4 Native collectionのserialization

external pointerはそのままR serializationしても、有効なnative storageとして復元できない。そのため`RSConcreteDictionary`と`RSConcreteSet`は、保存前にR objectのsnapshotを一時的に保持する。

復元後、adapterはexternal pointerが無効であることを検出し、snapshotからnative tableをlazy rebuildする。

この準備は`robjc_save()`、`robjc_save_image()`、`robjc_save_rds()`の内部で行われる。native collectionを含むrobjc objectには、base Rの`saveRDS()`ではなくrobjcの保存APIを使用することが前提である。

## 14. Public APIの現状

現在のNAMESPACEには46個のexportがある。主要なpublic surfaceは次のとおり。

### Object model

- `RSObject`
- `RSProtocol`
- `RSModule`
- `RSSingleton`
- `RSCopying`
- `RSMutableCopying`
- `RSEncoding`
- class/protocol utility functions

### Value types

- `RSScalar`
- `RSString`
- `RSNumber`
- `RSBoolean`
- `RSDate`
- `RSDateTime`
- `RSFactor`
- 各`RSNA*` class
- `RSNull`

### Collections

- `RSArray` / `RSMutableArray`
- `RSDataFrame` / `RSMutableDataFrame`
- `RSDictionary` / `RSMutableDictionary`
- `RSSet` / `RSMutableSet`

### Serialization

- `RSClassEncoder`
- `RSClassDecoder`
- `robjc_save()` / `robjc_load()`
- `robjc_save_image()`
- `robjc_save_rds()` / `robjc_read_rds()`

### Utility modules

- `RSHashOperator`
- `RSSorter`

`RSConcreteDictionary`、`RSConcreteSet`、`RSKeyValuePair`、`RSInstanceMap`などはpackage内部の実装要素である。

## 15. 削除済みの旧設計

次のR実装hash infrastructureは削除済みである。

- `RSDynamicHash`
- `RSEntryFinder`

旧Dictionary/SetはRのbucket構造とlist更新を利用していたため、要素数が増えるとcopy-on-writeの負担が大きかった。また、Dictionaryのためだけに`RSKeyValuePair`をhash tableへ格納する構造も必要だった。

現在のDictionary/Setはkey/valueまたはmemberをC++ tableへ直接保持する。`RSKeyValuePair`はserializationやentry表現には残っているが、native Dictionaryの内部storage単位ではない。

## 16. Testとbuild状況

### 16.1 Test inventory

`tests/testthat/`には150個の`test_that()`ブロックがある。対象には次が含まれる。

- scalar value、NA、hash、comparison
- array/data frame
- Dictionary/Setの基本操作
- native adapterのclone、remove、clear、集合演算
- mutable/immutable copy semantics
- `RSNull`
- class encoder/decoder
- instance map
- robjc save/load wrappers

### 16.2 確認済みcommand

```r
Rcpp::compileAttributes()
roxygen2::roxygenise()
devtools::test(reporter = "summary")
```

```sh
R CMD INSTALL --preclean .
R CMD check --no-manual robjc_0.0.1.tar.gz
```

現行テストとclean installは成功している。

直近の`R CMD check --no-manual`では、Set/Dictionary/C++実装に由来するERRORまたはWARNINGはない。残るNOTEはpackage startup時のR6 warning、未使用Import、`unlockBinding()`を含む動的binding操作に関するものだった。

## 17. Alphaとしての既知の制約

### 17.1 API stability

method名、constructor引数、protocol interfaceはalpha期間中に変更される可能性がある。特に集合演算、encoding/decoding、`RSKeyValuePair`周辺は最終APIとして固定していない。

### 17.2 Serialized data compatibility

過去バージョンで保存したobject graphの永続的な後方互換性は保証しない。class削除、class version変更、内部metadata形式変更によって、以前の保存データを解決できなくなる可能性がある。

### 17.3 Mutable hash keys/members

Dictionary keyまたはSet memberのhash/equalityに関わる状態を、格納中に変更してはならない。現在、自動rehashやmutation監視は提供していない。

### 17.4 Ordering

Dictionaryのkey/value列挙順とSetのmember列挙順は未規定である。`std::unordered_*`のiteration orderへ依存したcodeを書いてはならない。

### 17.5 Thread safety

C++ functorからR methodを呼び出し、R APIを利用しているため、native collectionはR main thread上での利用を前提とする。並列threadから直接操作する設計ではない。

### 17.6 Performance

storageとmutationはC++化されたが、hash/equality判定はR method callである。したがって、純粋なC++ scalar containerと同等の性能を目標にはしていない。

現時点では、大量挿入に対する正式なbenchmark suiteや回帰基準はない。`setMany`のようなbatch APIも未導入である。

### 17.7 Check NOTE

次のpackage hygiene項目は未解決である。

- `RSClassRegistry`生成時のR6 `cloneable` warning
- `digest`の未使用Import
- module/protocol/singleton登録処理における`unlockBinding()`のcheck NOTE
- startup messageとsuppressibilityの確認

これらは現行機能を壊す問題ではないが、betaまたは公開release前に整理する価値がある。

### 17.8 文書の同期

roxygen API文書は現行実装へ更新されている。一方、encoding strategy vignetteには、unresolved classの扱いやconstructor例など、実装との細かな差異が残る可能性がある。encoder/decoder再検討時にvignetteも同時更新する。

## 18. 次の推奨作業

### Alpha固定時

1. `DESCRIPTION`のversion表現とtag名を決定する。
2. root `README.md`へ最小限のinstall/exampleを用意する。
3. NEWSまたはCHANGELOGへnative Dictionary/Set移行を記録する。
4. `R CMD check`のNOTEを分類し、意図したものか確認する。
5. alpha tag時点のserialized data互換性方針を明記する。

### 次の機能開発

1. RSSet完成後の前提でencoder/decoder設計を再確認する。
2. object graphの循環参照、共有参照、custom class解決を重点的に試験する。
3. Dictionary/Setのbenchmarkを追加する。
4. `RSKeyValuePair`を公開概念として残すか、encoding内部へ限定するか決定する。
5. Array/DataFrameをnative化する必要が本当にあるか、実測して判断する。

### Release品質

1. clean R sessionでのinstall/test/checkをCIへ移す。
2. sanitizerやvalgrind相当のmemory検査を検討する。
3. generated filesとsource filesの更新手順をcontributor向けに文書化する。
4. public API exampleを実行可能な形で増やす。

## 19. 開発workflow

日常的なR code開発では次の流れを推奨する。

```r
devtools::load_all()
devtools::test()
```

roxygen文書を変更した場合:

```r
devtools::document()
devtools::test()
```

Rcpp exportを追加・変更した場合:

```r
Rcpp::compileAttributes()
devtools::document()
devtools::load_all()
devtools::test()
```

release境界またはC++のclean build確認:

```sh
R CMD INSTALL --preclean .
R CMD build .
R CMD check --no-manual robjc_0.0.1.tar.gz
```

## 20. 結論

`robjc`は、alphaとして必要な「設計の芯」と「実際に動く縦方向の経路」を持つ段階に到達している。

現在の強みは、Objective-C風の表面的なAPIではなく、class metadata、protocol、copy、hash/equality、null semantics、mutable/immutable collection、serializationがひとつのruntime modelとして接続されている点にある。

特にDictionary/Setのnative化によって、Rのcopy-on-writeに起因する実用上の上限を回避しながら、公開APIをR6 objectとして維持できた。`RSConcreteDictionary`と`RSConcreteSet`をadapterとして挟んだことで、C++ integrationの詳細も公開classから隔離されている。

今後は機能を無制限に増やすより、alphaの契約を明文化し、実際の利用例から不自然なAPIを見つけ、encoder/decoderとpackage品質を段階的に固めるのが妥当である。

