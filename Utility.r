

str2fct_item_status <- function(str) {
  str %>% factor(
    levels  = ORG_ITEM_STATUS$levels, # nolint
    labels  = ORG_ITEM_STATUS$labels,
    ordered = ORG_ITEM_STATUS$ordered
  )
}

str2fct_org_status <- function(str) {
  str %>% factor(
    levels  = ORG_ORG_STATUS$levels, # nolint
    labels  = ORG_ORG_STATUS$labels,
    ordered = ORG_ORG_STATUS$ordered
  )
}


str2fct_item_condition <- function(str) {
  str %>% factor(
    levels  = ORG_ITEM_CONDITION$levels, # nolint
    labels  = ORG_ITEM_CONDITION$labels,
    ordered = ORG_ITEM_CONDITION$ordered,
  )
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



