

str2fct_item_status <- function(str) {
  str %>% factor(
    levels  = ORG_ITEM_STATUS$levels,
    labels  = ORG_ITEM_STATUS$labels,
    ordered = ORG_ITEM_STATUS$ordered
  )
}

str2fct_org_status <- function(str) {
  str %>% factor(
    levels  = ORG_ORG_STATUS$levels,
    labels  = ORG_ORG_STATUS$labels,
    ordered = ORG_ORG_STATUS$ordered
  )
}


str2fct_item_condition <- function(str) {
  str %>% factor(
    levels  = ORG_ITEM_CONDITION$levels,
    labels  = ORG_ITEM_CONDITION$labels,
    ordered = ORG_ITEM_CONDITION$ordered,
  )
}


ptr2hex <- function(ptr) {
  # ptr: bit64
  print(ptr)
  lo <- bitwAnd(as.integer(ptr), 0xffffffff)
  hi <- bitwAnd(as.integer(bitwShiftR(ptr, 32L)), 0xffffffff)
  sprintf("0x%08x%08x", hi, lo)
}

hexstr_to_integer64 <- function(hex_str) {
  # 1. "0x" を除去して小文字に
  # 2. 16桁に左ゼロパディング
  padded_str <- sprintf("%016s", gsub("^0x", "", tolower(hex_str)))
  # 3. 4文字ずつ分割
  start_pos <- seq(1, 16, by = 4)
  blocks <- substring(padded_str, start_pos, start_pos + 3)
  # 4. 各ブロックに "0x" を付ける
  blocks <- paste0("0x", blocks)
  # 5. 各ブロックを16進数としてstrtoi
  chunk_vals <- sapply(blocks, strtoi, base = 16L)
  # 6. 16⁴単位で重み付けして積み上げ
  shifts <- c(48, 32, 16, 0)
  shifted_vals <- bit64::bitwShiftL(bit64::as.integer64(chunk_vals), shifts)
  val <- Reduce(bit64::bitOr, shifted_vals)
  return(val)
}

msg_not_overridden <- function(obj, method_name) {
  sprintf(
    "Class %s does not satisfy the over-riding requirement for '%s'.",
    class(obj)[1], method_name
  )
}



msg_inv_arg_error <- function(req_class_name, obj) {
  sprintf(
    "Invalid Argument Error. Required %s class, but %s is given.",
    req_class_name, class(obj)[1]
  )
}