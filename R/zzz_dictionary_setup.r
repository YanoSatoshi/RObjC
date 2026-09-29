# Native dictionary construction requires the registered Rcpp wrappers to have
# been loaded before protocol conformance initializes the class metadata.
conform_protocol_to_class(RSMutableCopying$protocol(), RSMutableDictionary)
conform_protocol_to_class(RSCopying$protocol(), RSMutableDictionary)
