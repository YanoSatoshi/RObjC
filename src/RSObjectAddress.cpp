#include <Rcpp.h>
#include <cstdint>
#include <sstream>

// [[Rcpp::export]]
std::string robjc_object_address(SEXP object) {
  std::ostringstream address;
  address << "0x" << std::hex
          << reinterpret_cast<std::uintptr_t>(object);
  return address.str();
}
