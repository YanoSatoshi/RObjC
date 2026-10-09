#' @include RSObject.r
#' @include RSProtocol.r
#' @include RSCopying.r
##' @include RSSingleton.r

# -----------------------------
# RSObject Setup isa
# -----------------------------
isa <- list2env(
  list(
    class     = RSObject,
    super     = NULL,
    protocols = list(),
    slot      = NULL,
    version   = "0.0.1 alpha"
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
    version   = "0.0.1 alpha"
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
    version   = "0.0.1 alpha"
  ),
  envir = new.env(parent = emptyenv(), hash = FALSE)
)
# make isa S3
class(isa) <- c(ISA_PROTOCOL, ISA_ROOT)
RSCopying$robjc_isa             <- isa
RSCopying$private_fields$.__isa <- isa
# make class var
RSCopying$contract_methods      <- isa$slot

# # -----------------------------
# # RSSingleton Setup isa
# # -----------------------------
# isa <- list2env(
#   list(
#     class     = RSSingleton,
#     super     = RSObject,
#     protocols = list(),
#     slot      = NULL,
#     version   = "0.0.1 alpha"
#   ),
#   envir = new.env(parent = emptyenv(), hash = FALSE)
# )
# # make isa S3
# class(isa) <- c(ISA_OBJECT, ISA_ROOT)
# RSSingleton$robjc_isa             <- isa
# RSSingleton$private_fields$.__isa <- isa
# # make instance
# instance  <- RSSingleton$new()
# isa$slot  <- instance
# RSSingleton$instance  <- instance


# -----------------------------
# RSObjcet apply RSCopying, Lock
# -----------------------------
RSObject$robjc_isa$protocols <- list(RSCopying)
lockEnvironment(RSObject$robjc_isa, bindings = TRUE)
lockBinding("robjc_isa", RSObject)
class(RSObject) <- c(ROBJC_CLASS, R6GENERATOR)

# # -----------------------------
# # RSSingleton apply RSCopying, Lock
# # -----------------------------
# RSSingleton$robjc_isa$protocols <- list(RSCopying)
# lockEnvironment(RSSingleton$robjc_isa, bindings = TRUE)
# RSSingleton$._constructor <- RSSingleton$new
# RSSingleton$new <- function(...) {
#   stop("Cannot instantiate a Singleton directly.", call. = FALSE)
# }
# lockBinding("instance",       RSSingleton)
# lockBinding("._constructor",  RSSingleton)
# lockBinding("new",            RSSingleton)
# lockBinding("robjc_isa",      RSSingleton)
# class(RSSingleton) <- c(ROBJC_SINGLETON, ROBJC_CLASS, R6GENERATOR)

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
