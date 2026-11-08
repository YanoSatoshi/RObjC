# robjc 公開 API 最終確認表

`NAMESPACE` の export と現行の `R/` 実装を基準にした一覧。クラスの `$...` は public メソッドを示す。継承した共通メソッドは各行で繰り返さない。

## クラス基盤

| 分類 | 公開名 | 主な API |
| --- | --- | --- |
| 基底クラス | `RSObject` | `$class()`、`$superclass()`、`$isMemberOf()`、`$isKindOf()`、`$conformsTo()`、`$respondsTo()`、`$isEqual()`、`$hash()`、`$objectID()`、`$copy()`、`$description()`、`$debugDescription()` |
| クラス生成 | `RSObjectClass()`、`RSSingletonClass()`、`RSScalarClass()`、`RSProtocolClass()` | 通常クラス、Singleton、NA 伴侶クラスを持つ Scalar、プロトコルを生成 |
| 共有モジュール | `RSModule` | Singleton の基底クラス |
| 同一性の S3 演算子 | `==`、`!=`（`RSObject`） | インスタンスの同一性を比較 |

## プロトコル

| 分類 | 公開名 | 契約・用途 |
| --- | --- | --- |
| 基底 | `RSProtocol` | プロトコル生成の基底 |
| コピー | `RSCopying`、`RSMutableCopying` | `copy()`、`mutableCopy()` |
| 比較・列挙 | `RSComparable`、`RSListing` | `compare()`、`asList()` |
| Coding | `RSEncoderProtocol`、`RSDecoderProtocol`、`RSCoding` | `encodeWithCoder()`、`initWithCoder()` |

契約不足の検査は公開関数 `protocol_getMissingContracts(klass)`。不足がなければ `character(0)` を返す。

## 値クラス

| 分類 | 公開クラス | 主な API・生成関数 |
| --- | --- | --- |
| Scalar 基底 | `RSScalar` | `$value()`、`$nativeValue()`、`$compare()`、`$nativeCompare()`、`$isEqual()`、`$hash()`、`RSScalar_withValue()` |
| 論理・数値・複素数 | `RSBoolean`、`RSNumber`、`RSComplex` | `RSBoolean_withBoolean()`、`RSNumber_withValue()`、`RSNumber_withInteger()`、`RSNumber_withDouble()`、`RSComplex_withValue()` |
| 文字・因子 | `RSString`、`RSFactor` | `RSString_empty()`、`RSString_withValue()`、`RSString_withString()`、`RSFactor_withFactor()` |
| 日付・日時 | `RSDate`、`RSDateTime` | `RSDate_today()`、`RSDate_withDate()`、`RSDateTime_now()`、`RSDateTime_withDateTime()` |
| raw 値 | `RSRaw` | `RSObject` を直接継承。`$value()`、`$nativeValue()`、`$isEqual()`、`$hash()`、`RSRaw_withValue()` |
| 型付き NA | `RSNABoolean`、`RSNANumber`、`RSNAComplex`、`RSNAString`、`RSNAFactor`、`RSNADate`、`RSNADateTime` | 各 Scalar の共有 NA インスタンス |
| null | `RSNull` | `RSNull$null`、`RSNull_null()`。未実装メソッドへの `$` ディスパッチ |

## コレクション

immutable が親、mutable が子。共通して `$copy()` と `$mutableCopy()` を持つ。`NULL` を要素として使う場合は `RSNull$null` または `robjc_wrap_null()` を明示する。

| 系列 | 公開クラス | 主な読取・非破壊メソッド | mutable の変更メソッド |
| --- | --- | --- | --- |
| Array | `RSArray`、`RSMutableArray` | `$count()`、`$objectAtIndex()`、`$objectsAtIndexes()`、`$lastObject()`、`$containsObject()`、`$indexOfObject()`、`$asList()`、`$arrayByAddingObject()`、`$arrayByAddingObjectsFromArray()`、`$filteredArrayByFunction()` | `$addObject()`、`$addObjectsFromArray()`、`$insertObjectAtIndex()`、`$replaceObjectAtIndex()`、`$removeObjectAtIndex()`、`$removeAllObjects()` |
| Vector | `RSVector`、`RSMutableVector` | `$values()`、`$valueAtIndex()`、`$containsValues()`、`$indexesOfValues()`、`$asVector()`、`$asList()`、`$count()` | `$appendValues()`、`$insertValuesAtIndex()`、`$replaceValuesAtIndexes()`、`$removeValuesAtIndexes()`、`$removeAllValues()` |
| Set | `RSSet`、`RSMutableSet` | `$count()`、`$member()`、`$containsObject()`、`$isSubsetOfSet()`、`$asList()`、`$setByAddingObject()`、`$setByAddingObjectsFromSet()`、`$setByAddingObjectsFromArray()`、`$setByUnitingSet()`、`$setByIntersectingSet()`、`$setBySubtractingSet()` | `$addObject()`、`$removeObject()`、`$removeAllObjects()`、`$uniteSet()`、`$unionSet()`、`$intersectSet()`、`$subtractSet()`、`$minusSet()` |
| Dictionary | `RSDictionary`、`RSMutableDictionary` | `$objectForKey()`、`$containsKey()`、`$allKeysAsList()`、`$allValuesAsList()`、`$allEntriesAsList()`、`$asList()`、`$dictionaryByMergingDictionary()`、`$dictionaryByExcludingDictionary()` | `$setObjectForKey()`、`$removeObjectForKey()`、`$removeAllObjects()`、`$addEntry()`、`$removeEntry()`、`$removeAllEntries()` |
| Dictionary の entry | `RSKeyValuePair` | `$key()`、`$value()`、`$isEqual()`、`$hash()` | `$setKey()` は値が `RSNull$null` のときのみ使用可 |
| DataFrame | `RSDataFrame`、`RSMutableDataFrame` | `$getDataFrame()`、`$getColumn()`、`$nrow()`、`$ncol()`、`$colnames()`、`$rownames()` | `$setDataFrame()`、`$setColumn()` |

### コレクション生成関数

| 系列 | 公開関数 |
| --- | --- |
| Array | `RSArray_empty()`、`RSArray_withObject()`、`RSArray_withObjects()`、`RSArray_withArray()`；`RSMutableArray_empty()`、`RSMutableArray_withObject()`、`RSMutableArray_withObjects()`、`RSMutableArray_withArray()` |
| Set | `RSSet_empty()`、`RSSet_withObject()`、`RSSet_withObjects()`、`RSSet_withArray()`、`RSSet_withSet()`；`RSMutableSet_empty()`、`RSMutableSet_withObject()`、`RSMutableSet_withObjects()`、`RSMutableSet_withArray()`、`RSMutableSet_withSet()` |
| Dictionary | `RSDictionary_empty()`、`RSDictionary_withObjectForKey()`、`RSDictionary_withObjectsForKeys()`、`RSDictionary_withObjectsAndKeys()`、`RSDictionary_withDictionary()`；`RSMutableDictionary_empty()`、`RSMutableDictionary_withObjectForKey()`、`RSMutableDictionary_withObjectsForKeys()`、`RSMutableDictionary_withObjectsAndKeys()`、`RSMutableDictionary_withDictionary()` |
| Vector・DataFrame | 独立した生成関数はなく、クラスの `$new()` を使用 |

## アーカイブ

| 公開クラス | 主な API |
| --- | --- |
| `RSCoder` | `$encodeObjectForKey()`、`$decodeObjectForKey()` の基底 |
| `RSKeyedArchiver` | `$encode(object)`。単一ルートまたは複数ルートを含む R list を生成 |
| `RSKeyedUnarchiver` | `$new(archive)`、`$classMappings(include_automatic = FALSE)`、`$classMappingsAsDictionary()`、`$classMappingForKey()`、`$setClassMappings()`、`$decode()` |

## Runtime・登録関数

| 分類 | 公開名 |
| --- | --- |
| 型判定 | `robjc_isMetaClass()`、`robjc_isClass()`、`robjc_isProtocol()`、`robjc_isInstance()`、`robjc_isRObjC()` |
| クラス情報 | `robjc_get_pointer()`、`robjc_inheritChain()` |
| 割当て・補助 | `robjc_allocate()`、`robjc_normalize_hash()`、`robjc_wrap_null()` |
| 登録 | `robjc_register_class()`、`robjc_register_protocol()` |
| Hash operator | `RSHashOperator`：`$hashForScalar()`、`$computeHashCodeWithString()`、`$hashesOfObjects()`、`$combinedHashFromObjects()` |
| Runtime 定数 | `R6GENERATOR` |

## 公開 API ではないクラス

`RSHashOperatorProtocol`、`RSConcreteSet`、`RSConcreteDictionary` は実装に存在するが、現行の `NAMESPACE` では export されていない。

## 最終確認で判断する点

1. `RSMutableSet` の `$unionSet()` と `$minusSet()` は、それぞれ `$uniteSet()` と `$subtractSet()` の別名として残すか。
2. raw 値の生成には `RSRaw$new()` または `RSRaw_withValue()` を使い、`RSScalar_withValue(raw)` はエラーにする。
