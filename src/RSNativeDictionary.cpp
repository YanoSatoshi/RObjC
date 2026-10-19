#include <Rcpp.h>
#include <unordered_map>

#include "RSObjectHash.h"

class NativeDictionary {
private:
  std::unordered_map<SEXP, SEXP, RSObjectHash, RSObjectEqual> storage_;
  SEXP precious_;

public:
  NativeDictionary() {
    precious_ = PROTECT(R_NewPreciousMSet(128));
    R_PreserveObject(precious_);
    UNPROTECT(1);
  }

  NativeDictionary(const NativeDictionary&) = delete;
  NativeDictionary& operator=(const NativeDictionary&) = delete;

  ~NativeDictionary() {
    R_ReleaseObject(precious_);
  }

  void set(SEXP key, SEXP value) {
    auto it = storage_.find(key);
    if (it == storage_.end()) {
      R_PreserveInMSet(key, precious_);
      R_PreserveInMSet(value, precious_);
      storage_.emplace(key, value);
      return;
    }

    SEXP old_value = it->second;
    R_PreserveInMSet(value, precious_);
    it->second = value;
    R_ReleaseFromMSet(old_value, precious_);
  }

  SEXP get(SEXP key) const {
    auto it = storage_.find(key);
    if (it == storage_.end()) {
      return R_NilValue;
    }
    return it->second;
  }

  bool contains(SEXP key) const {
    return storage_.find(key) != storage_.end();
  }

  std::size_t size() const {
    return storage_.size();
  }

  Rcpp::List keys() const {
    Rcpp::List result(storage_.size());
    std::size_t index = 0;
    for (const auto& entry : storage_) {
      result[index++] = entry.first;
    }
    return result;
  }

  Rcpp::List values() const {
    Rcpp::List result(storage_.size());
    std::size_t index = 0;
    for (const auto& entry : storage_) {
      result[index++] = entry.second;
    }
    return result;
  }

  Rcpp::List entries() const {
    Rcpp::List result(storage_.size());
    std::size_t index = 0;

    for (const auto& entry : storage_) {
      result[index++] = Rcpp::List::create(
        entry.first,
        entry.second
      );
    }

    return result;
  }

  bool remove(SEXP key) {
    auto it = storage_.find(key);
    if (it == storage_.end()) {
      return false;
    }

    SEXP stored_key = it->first;
    SEXP stored_value = it->second;
    storage_.erase(it);
    R_ReleaseFromMSet(stored_key, precious_);
    R_ReleaseFromMSet(stored_value, precious_);
    return true;
  }

  void clear() {
    for (const auto& entry : storage_) {
      R_ReleaseFromMSet(entry.first, precious_);
      R_ReleaseFromMSet(entry.second, precious_);
    }
    storage_.clear();
  }

  NativeDictionary* clone() const {
    NativeDictionary* copy = new NativeDictionary();
    for (const auto& entry : storage_) {
      copy->set(entry.first, entry.second);
    }
    return copy;
  }
};

// [[Rcpp::export]]
SEXP dictionary_create() {
  NativeDictionary* dict = new NativeDictionary();
  Rcpp::XPtr<NativeDictionary> ptr(dict, true);
  return ptr;
}

// [[Rcpp::export]]
SEXP dictionary_clone(SEXP xp) {
  Rcpp::XPtr<NativeDictionary> source(xp);
  Rcpp::XPtr<NativeDictionary> copy(source->clone(), true);
  return copy;
}

// [[Rcpp::export]]
void dictionary_set(SEXP xp, SEXP key, SEXP value) {
  Rcpp::XPtr<NativeDictionary> dict(xp);
  dict->set(key, value);
}

// [[Rcpp::export]]
SEXP dictionary_get(SEXP xp, SEXP key) {
  Rcpp::XPtr<NativeDictionary> dict(xp);
  return dict->get(key);
}

// [[Rcpp::export]]
bool dictionary_contains(SEXP xp, SEXP key) {
  Rcpp::XPtr<NativeDictionary> dict(xp);
  return dict->contains(key);
}

// [[Rcpp::export]]
std::size_t dictionary_size(SEXP xp) {
  Rcpp::XPtr<NativeDictionary> dict(xp);
  return dict->size();
}

// [[Rcpp::export]]
bool dictionary_remove(SEXP xp, SEXP key) {
  Rcpp::XPtr<NativeDictionary> dict(xp);
  return dict->remove(key);
}

// [[Rcpp::export]]
void dictionary_clear(SEXP xp) {
  Rcpp::XPtr<NativeDictionary> dict(xp);
  dict->clear();
}

// [[Rcpp::export]]
Rcpp::List dictionary_keys(SEXP xp) {
  Rcpp::XPtr<NativeDictionary> dict(xp);
  return dict->keys();
}

// [[Rcpp::export]]
Rcpp::List dictionary_values(SEXP xp) {
  Rcpp::XPtr<NativeDictionary> dict(xp);
  return dict->values();
}

// [[Rcpp::export]]
Rcpp::List dictionary_entries(SEXP xp) {
  Rcpp::XPtr<NativeDictionary> dict(xp);
  return dict->entries();
}
