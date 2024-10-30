#' load_pkgs
#'
#' @export
load_pkgs = function(pkgs_) {

  for (pkg_ in pkgs_) {
    failed_ = suppressMessages(
      tryCatch(
        expr = {
          library(pkg_, character.only = T)
          NULL
        },
        error = function(e) {
          pkg_
        }
      )
    )
  }

  loaded_ = setdiff(pkgs_, failed_)

  if (length(failed_) > 0) {
    cat(c('Loaded: ', loaded_, 'Failed: ', failed_), sep = '\n')
  } else {
    cat(c('Loaded: ', loaded_), sep = '\n')
  }

}
#' pkg_setup
#'
#' @export
pkg_setup = function() {

  text_ = "
  source('~/.Rprofile')
  pkgs = c('devtools', 'usethis', 'testthat')
  paperTools::load_pkgs(pkgs)
  "

  cat(text_, file = '.Rprofile')

}

