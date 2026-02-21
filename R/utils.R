#' Extract file extension
#' @keywords internal
#' @noRd
file_ext <- function(file_path) {
  pos <- regexpr("\\.([[:alnum:]]+)$", file_path)
  ifelse(pos > -1L, substring(file_path, pos + 1L), "")
}


# Call a function from an optional (Suggested) package
call_pkg_fun <- function(pkg, fun, ...) {
  rlang::check_installed(pkg, reason = paste("to write", pkg, "files"))
  getExportedValue(pkg, fun)(...)
}
