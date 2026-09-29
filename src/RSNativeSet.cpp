#include <Rcpp.h>
#include <unordered_set>

#include "RSObjectHash.h"

class NativeSet {
  private:
    std::unordered_set<SEXP, RSObjectHash, RSObjectEqual> storage_;
    SEXP precious_;
  
  public:
    NativeSet() {
      precious_ = PROTECT(R_NewPreciousMSet(128));
      R_PreserveObject(precious_);
      UNPROTECT(1);
    }

    NativeSet(const NativeSet&) = delete;
    NativeSet& operator=(const NativeSet&) = delete;

    ~NativeSet() {
      R_ReleaseObject(precious_);
    }

    bool add(SEXP item) {
      const auto it = storage_.emplace(item);
      if (!it.second) return false;
      R_PreserveInMSet(item, precious_);
      return true;
    }

    bool contains(SEXP item) const {
      return storage_.find(item) != storage_.end();
    }

    SEXP member(SEXP item) const {
      auto it = storage_.find(item);
      if (it == storage_.end())
        return R_NilValue;
      return *it;
    }

    SEXP anyObject() const {
      if (storage_.empty())
        return R_NilValue;
      return *storage_.begin();
    }

    std::size_t size() const {
      return storage_.size();
    }

    Rcpp::List values() const {
      Rcpp::List result(storage_.size());
      std::size_t index = 0;
      for (SEXP item : storage_)
        result[index++] = item;
      return result;
    }

    bool remove(SEXP item) {
      auto found = storage_.find(item);
      if (found == storage_.end())
        return false;
      SEXP stored_item = *found;
      storage_.erase(found);
      R_ReleaseFromMSet(stored_item, precious_);
      return true;
    }

    void clear() {
      for (const auto& item : storage_)
        R_ReleaseFromMSet(item, precious_);
      storage_.clear();
    }

    NativeSet* clone() const {
      NativeSet* copy = new NativeSet();
      for (SEXP item : storage_)
        copy->add(item);
      return copy;
    }

    // Set Operations
    void uniteWith(const NativeSet& other) {
      if (this == &other) return;
      for (SEXP item : other.storage_)
        add(item);
    }

    void intersectWith(const NativeSet& other) {
      if (this == &other)
        return;
      auto it = storage_.begin();
      while (it != storage_.end()) {
        if (!other.contains(*it)) {
          SEXP item = *it;
          it = storage_.erase(it);
          R_ReleaseFromMSet(item, precious_);
        } else {
          ++it;
        }
      }
    }

    void subtractWith(const NativeSet& other) {
      if (this == &other) {
        clear();
        return;
      }
      auto it = storage_.begin();
      while(it != storage_.end()) {
        if (other.contains(*it)) {
          SEXP item = *it;
          it = storage_.erase(it);
          R_ReleaseFromMSet(item, precious_);
        } else {
          ++it;
        }
      }
    }
};



// [[Rcpp::export]]
SEXP set_create() {
  NativeSet* set = new NativeSet();
  Rcpp::XPtr<NativeSet> ptr(set, true);
  return ptr;
}

// [[Rcpp::export]]
SEXP set_clone(SEXP xp) {
  Rcpp::XPtr<NativeSet> source(xp);
  Rcpp::XPtr<NativeSet> copy(source->clone(), true);
  return copy;
}

// [[Rcpp::export]]
bool set_add(SEXP xp, SEXP item) {
  Rcpp::XPtr<NativeSet> set(xp);
  return set->add(item);
}

// [[Rcpp::export]]
bool set_contains(SEXP xp, SEXP item) {
  Rcpp::XPtr<NativeSet> set(xp);
  return set->contains(item);
}

// [[Rcpp::export]]
SEXP set_member(SEXP xp, SEXP item) {
  Rcpp::XPtr<NativeSet> set(xp);
  return set->member(item);
}

// [[Rcpp::export]]
SEXP set_any_object(SEXP xp) {
  Rcpp::XPtr<NativeSet> set(xp);
  return set->anyObject();
}

// [[Rcpp::export]]
std::size_t set_size(SEXP xp) {
  Rcpp::XPtr<NativeSet> set(xp);
  return set->size();
}

// [[Rcpp::export]]
Rcpp::List set_values(SEXP xp) {
  Rcpp::XPtr<NativeSet> set(xp);
  return set->values();
}

// [[Rcpp::export]]
bool set_remove(SEXP xp, SEXP item) {
  Rcpp::XPtr<NativeSet> set(xp);
  return set->remove(item);
}

// [[Rcpp::export]]
void set_clear(SEXP xp) {
  Rcpp::XPtr<NativeSet> set(xp);
  set->clear();
}


// Set Operations

// [[Rcpp::export]]
void set_unite_with(SEXP target_xp, SEXP other_xp) {
  Rcpp::XPtr<NativeSet> target(target_xp);
  Rcpp::XPtr<NativeSet> other(other_xp);
  target->uniteWith(*other);
}

// [[Rcpp::export]]
void set_intersect_with(SEXP target_xp, SEXP other_xp) {
  Rcpp::XPtr<NativeSet> target(target_xp);
  Rcpp::XPtr<NativeSet> other(other_xp);
  target->intersectWith(*other);
}

// [[Rcpp::export]]
void set_subtract_with(SEXP target_xp, SEXP other_xp) {
  Rcpp::XPtr<NativeSet> target(target_xp);
  Rcpp::XPtr<NativeSet> other(other_xp);
  target->subtractWith(*other);
}
