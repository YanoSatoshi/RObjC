#' @include RSObject.r
#' @include RSProtocol.r
#' @include RSCopying.r

# -----------------------------
# RSObject Setup isa
# -----------------------------
isa <- list2env(
  list(
    class     = RSObject,
    super     = NULL,
    protocols = list(),
    slot      = NULL,
    version   = "0.0.2 alpha"
  ),
  envir = new.env(parent = emptyenv(), hash = FALSE)
)
# make isa S3
class(isa) <- c(ISA_OBJECT, ISA_ROOT)
RSObject$robjc_isa              <- isa
RSObject$private_fields$.__isa  <- isa

# -----------------------------
# RSProtocol Setup isa
# -----------------------------
isa <- list2env(
  list(
    class     = RSProtocol,
    super     = NULL,
    protocols = list(),
    slot      = character(0),
    version   = "0.0.2 alpha"
  ),
  envir = new.env(parent = emptyenv(), hash = FALSE)
)
# make isa S3
class(isa) <- c(ISA_PROTOCOL, ISA_ROOT)
RSProtocol$robjc_isa              <- isa
RSProtocol$private_fields$.__isa  <- isa
# make class var
RSProtocol$contract_methods       <- isa$slot

# -----------------------------
# RSCopying Setup isa
# -----------------------------
isa <- list2env(
  list(
    class     = RSCopying,
    super     = RSProtocol,
    protocols = list(),
    slot      = c("copy"),
    version   = "0.0.2 alpha"
  ),
  envir = new.env(parent = emptyenv(), hash = FALSE)
)
# make isa S3
class(isa) <- c(ISA_PROTOCOL, ISA_ROOT)
RSCopying$robjc_isa             <- isa
RSCopying$private_fields$.__isa <- isa
# make class var
RSCopying$contract_methods      <- isa$slot

# -----------------------------
# RSObjcet apply RSCopying, Lock
# -----------------------------
RSObject$robjc_isa$protocols <- list(RSCopying)
lockEnvironment(RSObject$robjc_isa, bindings = TRUE)
lockBinding("robjc_isa", RSObject)
class(RSObject) <- c(ROBJC_CLASS, R6GENERATOR)

# -----------------------------
# RSProtocol Lock
# -----------------------------
lockEnvironment(RSProtocol$robjc_isa, bindings = TRUE)
lockBinding("robjc_isa",        RSProtocol)
lockBinding("contract_methods", RSProtocol)
class(RSProtocol) <- c(ROBJC_CLASS, R6GENERATOR)

# -----------------------------
# RSCopying lock
# -----------------------------
lockEnvironment(RSCopying$robjc_isa, bindings = TRUE)
lockBinding("robjc_isa",        RSCopying)
lockBinding("contract_methods", RSCopying)
class(RSCopying) <- c(ROBJC_CLASS, R6GENERATOR)
