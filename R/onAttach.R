.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    '*********************\n
    sample.jpg is titled \'A tiger in the water\'
    By Bob Jagendorf
    [CC BY 2.0 (http://creativecommons.org/licenses/by/2.0)], 
    via Wikimedia Commons
    ')
  cat('Warning: This package creates files in your current directory,\n')
  cat('one file for each version of an image that you create.\n')
}
