.onLoad <- function(libname, pkgname) {
  # LazyData does not work with
  # external pointers
 data(default_acker, package = "acker")
 data(drought_index, package = "acker")
}