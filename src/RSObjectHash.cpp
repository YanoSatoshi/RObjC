#include "RSObjectHash.h"

std::size_t RSObjectHash::operator()(SEXP key) const {
  Rcpp::Environment env(key);
  Rcpp::Function fn = env["hash"];
  return Rcpp::as<std::size_t>(fn());
}

bool  RSObjectEqual::operator()(SEXP a, SEXP b) const {
  Rcpp::Environment env(a);
  Rcpp::Function fn = env["isEqual"];
  return Rcpp::as<bool>(fn(b));
}
