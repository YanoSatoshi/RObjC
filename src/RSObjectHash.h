// src/RSObjectHash.cpp
#ifndef ROBJC_RSOBJECTHASH_H
#define ROBJC_RSOBJECTHASH_H

#include <Rcpp.h>

struct RSObjectHash {
  std::size_t operator()(SEXP key) const;
};

struct RSObjectEqual {
  bool  operator()(SEXP a, SEXP b) const;
};

#endif