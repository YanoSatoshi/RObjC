# RS クラスのコンビニエンスコンストラクタ候補

Objective-C Foundation のクラス生成メソッドを参考に、`api_RSClassName.r` に置く候補を整理する。表の「既存」は現行コードにクラスメソッドがあることを示し、「追加」は提案であり未実装。原則として戻り値は表に記した RS クラスのインスタンスとする。

R では C 配列と要素数、`nil` 終端の可変長引数をそのまま移植しない。複数要素は `list(...)` または `...` として受け、長さや型を明示的に検査する。コンテナの要素は原則 `RSObject`（Set / Dictionary では既存仕様に従い `NULL` も可）とする。

## NSArray

[Apple: NSArray](https://developer.apple.com/documentation/foundation/nsarray?language=objc)

| Objective-C | RS 候補 | 状態・入力 |
| --- | --- | --- |
| `+array` | `RSArray$array()` | 追加。空の配列。 |
| `+arrayWithObject:` | `RSArray$arrayWithObject(object)` | 追加。単一要素。 |
| `+arrayWithObjects:` | `RSArray$arrayWithObjects(...)` | 追加。R では `nil` 終端なし。 |
| `+arrayWithArray:` | `RSArray$arrayWithArray(array)` | 追加。既存配列からの浅い複製。 |

`+arrayWithObjects:count:` は C 配列向けなので候補から外す。現状は `RSArray$new(objects = list(...))` で生成できる。`RSArray` の初期化は `RSMutableArray` から継承している。

## NSMutableArray

[Apple: NSMutableArray](https://developer.apple.com/documentation/foundation/nsmutablearray?language=objc)

| Objective-C | RS 候補 | 状態・入力 |
| --- | --- | --- |
| `+array` | `RSMutableArray$array()` | 追加。空の可変配列。 |
| `+arrayWithObject:` | `RSMutableArray$arrayWithObject(object)` | 追加。 |
| `+arrayWithObjects:` | `RSMutableArray$arrayWithObjects(...)` | 追加。 |
| `+arrayWithArray:` | `RSMutableArray$arrayWithArray(array)` | 追加。浅い複製。 |
| `+arrayWithCapacity:` | `RSMutableArray$arrayWithCapacity(capacity)` | 保留。現行の R リスト実装に容量予約 API はない。 |

現状は `RSMutableArray$new(objects = list(...))` で生成できる。

## NSDictionary

[Apple: NSDictionary](https://developer.apple.com/documentation/foundation/nsdictionary?language=objc)

| Objective-C | RS 候補 | 状態・入力 |
| --- | --- | --- |
| `+dictionary` | `RSDictionary$dictionary()` | 追加。空の辞書。 |
| `+dictionaryWithObject:forKey:` | `RSDictionary$dictionaryWithObjectForKey(object, key)` | 追加。単一の値とキー。 |
| `+dictionaryWithObjects:forKeys:` | `RSDictionary$dictionaryWithObjectsForKeys(objects, keys)` | 追加。等長のリストまたは RS コレクション。 |
| `+dictionaryWithObjectsAndKeys:` | `RSDictionary$dictionaryWithObjectsAndKeys(...)` | 追加。値・キーの交互並び。R では `nil` 終端なし。 |
| `+dictionaryWithDictionary:` | `RSDictionary$dictionaryWithDictionary(dictionary)` | 既存。独立したストレージを持つ浅い複製。 |

現在の `initialize()` は `objects_and_keys = list(value, key, ...)`、`keys` と `values`、`key_value_pairs` を受ける。`R/util_io.r` は **未定義** の `RSDictionary$dictionaryWithKeysAndObjects(keys = ..., objects = ...)` を呼んでいる。この呼び出しには `dictionaryWithObjectsForKeys(objects, keys)` が近いが、引数名と値が `RSObject` かどうかの確認も必要。交互並びの `dictionaryWithObjectsAndKeys(...)` とは別の入力形式である。

`+dictionaryWithObjects:forKeys:count:` は C 配列向けなので候補から外す。ファイル・URL 読み込み系は保存・復元 API の検討と分ける。

## NSMutableDictionary

[Apple: NSMutableDictionary](https://developer.apple.com/documentation/foundation/nsmutabledictionary?language=objc)

| Objective-C | RS 候補 | 状態・入力 |
| --- | --- | --- |
| `+dictionary` | `RSMutableDictionary$dictionary()` | 追加。空の可変辞書。 |
| `+dictionaryWithObject:forKey:` | `RSMutableDictionary$dictionaryWithObjectForKey(object, key)` | 追加。 |
| `+dictionaryWithObjects:forKeys:` | `RSMutableDictionary$dictionaryWithObjectsForKeys(objects, keys)` | 追加。 |
| `+dictionaryWithObjectsAndKeys:` | `RSMutableDictionary$dictionaryWithObjectsAndKeys(...)` | 追加。 |
| `+dictionaryWithDictionary:` | `RSMutableDictionary$dictionaryWithDictionary(dictionary)` | 既存。独立したストレージを持つ浅い複製。 |
| `+dictionaryWithCapacity:` | `RSMutableDictionary$dictionaryWithCapacity(capacity)` | 保留。現行ストレージに容量予約 API はない。 |

`RSMutableDictionary$new()` と、`RSDictionary` と同じ３種類の入力形式は既存。

## NSSet

[Apple: NSSet](https://developer.apple.com/documentation/foundation/nsset?language=objc)

| Objective-C | RS 候補 | 状態・入力 |
| --- | --- | --- |
| `+set` | `RSSet$set()` | 追加。空集合。 |
| `+setWithObject:` | `RSSet$setWithObject(object)` | 追加。単一要素。 |
| `+setWithObjects:` | `RSSet$setWithObjects(...)` | 追加。R では `nil` 終端なし。 |
| `+setWithArray:` | `RSSet$setWithArray(array)` | 追加。配列から集合へ。重複は集合の規則で統合。 |
| `+setWithSet:` | `RSSet$setWithSet(set)` | 既存。独立したストレージを持つ浅い複製。 |

`+setWithObjects:count:` は C 配列向けなので候補から外す。現状は `RSSet$new(objects = list(...))` で生成できる。

## NSMutableSet

[Apple: NSMutableSet](https://developer.apple.com/documentation/foundation/nsmutableset?language=objc)

| Objective-C | RS 候補 | 状態・入力 |
| --- | --- | --- |
| `+set` | `RSMutableSet$set()` | 追加。空の可変集合。 |
| `+setWithObject:` | `RSMutableSet$setWithObject(object)` | 追加。 |
| `+setWithObjects:` | `RSMutableSet$setWithObjects(...)` | 追加。 |
| `+setWithArray:` | `RSMutableSet$setWithArray(array)` | 追加。 |
| `+setWithSet:` | `RSMutableSet$setWithSet(set)` | 既存。独立したストレージを持つ浅い複製。 |
| `+setWithCapacity:` | `RSMutableSet$setWithCapacity(capacity)` | 保留。現行ストレージに容量予約 API はない。 |

現状は `RSMutableSet$new(objects = list(...))` で生成できる。

## NSNumber

[Apple: NSNumber](https://developer.apple.com/documentation/foundation/nsnumber?language=objc)

| Objective-C | RS 候補 | 状態・入力 |
| --- | --- | --- |
| `+numberWithInt:` / `+numberWithInteger:` | `RSNumber$numberWithInteger(value)` | 追加。R の単一 `integer` に対応。 |
| `+numberWithDouble:` | `RSNumber$numberWithDouble(value)` | 追加。R の単一 `double` に対応。 |
| `+numberWithBool:` | `RSNumber$numberWithBool(value)` | 保留。`RSNumber$initialize()` は `is.numeric(val)` を要求し、R の `logical` を直接は受け付けない。`RSNumber` と `RSBoolean` はともに `RSScalar` の子クラスなので、真偽値を数値 `0`/`1` に変換して `RSNumber` を返すか、`RSBoolean` を返すかを決める。 |
| （R 独自） | `RSNumber$numberWith(val)` | 既存。`integer`/`double` を受け、`NA` は `RSNANumber` を返す。 |

C の `char`、`short`、`long`、`long long`、符号なし整数、`float` などは R の基本スカラー型との一対一対応がないため、まずは候補から外す。整数の範囲外や `NA` の扱いは追加 API でも明示する。

## NSString

[Apple: NSString](https://developer.apple.com/documentation/foundation/nsstring?language=objc)

| Objective-C | RS 候補 | 状態・入力 |
| --- | --- | --- |
| `+string` | `RSString$string()` | 追加。空文字 `""`。 |
| `+stringWithString:` | `RSString$stringWithString(string)` | 追加。文字列または `RSString` の受理範囲を決める。 |
| `+stringWithFormat:` | `RSString$stringWithFormat(format, ...)` | 追加候補。R の `sprintf()` を使うなら書式は Objective-C と同一ではないと明記する。 |
| `+stringWithUTF8String:` | `RSString$stringWithUTF8String(bytes)` | 保留。C の NUL 終端文字列と R の `character`/`raw` は異なるため、入力形式と不正 UTF-8 の扱いを先に決める。 |
| （R 独自） | `RSString$stringWith(val)` | 既存。`character`/`factor` を受け、`NA` は `RSNAString` を返す。 |

## NSDate

[Apple: NSDate](https://developer.apple.com/documentation/foundation/nsdate?language=objc)

| Objective-C | RS 候補 | 状態・入力 |
| --- | --- | --- |
| `+date` | `RSDate$date()` | 追加。`Sys.Date()` による今日の日付。 |
| `+dateWithTimeIntervalSince1970:` | `RSDate$dateWithTimeIntervalSince1970(seconds)` | 保留。`RSDate` は日単位の R `Date` で、`NSDate` の時刻精度を保持できない。`RSDateTime` 側の候補にする。 |
| `+dateWithTimeIntervalSinceReferenceDate:` | `RSDate$dateWithTimeIntervalSinceReferenceDate(seconds)` | 同上。基準は 2001-01-01 UTC。 |
| `+dateWithTimeIntervalSinceNow:` | `RSDate$dateWithTimeIntervalSinceNow(seconds)` | 同上。 |
| （R 独自） | `RSDate$dateWith(val)` | 既存。R の `Date` を受け、`NA` は `RSNADate` を返す。 |

`NSDate` は瞬間を表すのに対し、現行 `RSDate` は暦日を包むクラス。時刻付きコンストラクタを `RSDate` に足す場合は、日付への丸めとタイムゾーンを仕様化する必要がある。現時点では `RSDateTime` への割り当てを優先する。

## 実装時の共通確認事項

- `api_RSArray.r`、`api_RSDictionary.r` など、対象の RS クラスごとに API 定義を置き、`@include` でジェネレータ定義より後に読み込ませる。
- `...` を受ける API では、`list(...)` と既存の `objects` / `objects_and_keys` の関係を統一する。空入力、長さ不一致、無効な要素の扱いを決める。
- 不変コンテナの生成経路が、可変クラスの禁止された更新メソッドを呼ばないことを確かめる。
- Dictionary / Set の `NULL` と `RSNull`、スカラーの `NA` の扱いは現行仕様と合わせる。
