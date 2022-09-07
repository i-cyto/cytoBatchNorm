#' @title timetag
#'
#' @description Append a time tag to a file name. The time tag counts every
#'   seconds since 2020-01-01 00:00. If the file has an extension, the suffix is
#'   added before it. This allows to write backup files for example.
#'
#' @param file string
#'
#' @examples
#' timetag()
#' timetag("panel.xlsx")
#'
#' @export

timetag <- function(
  file
) {
  tag <- sprintf("%08X", as.integer(Sys.time()) -
                   as.integer(as.POSIXct("2020-01-01 00:00")))
  if (missing(file)) return(tag)
  if (grepl("\\..{1,5}$", file)) {
    gsub("(.+)(\\..{1,5}$)", paste0("\\1-", tag, "\\2"), file)
  } else {
    paste0(file, "-", tag)
  }
}


#' @title is_package_installed
#'
#' @description Check if the the given package is installed.
#'
#' @param pkg string, the package that should be installed.

is_package_installed <- function(pkg) {
  if (missing(pkg)) stop()
  length(find.package(pkg, quiet = TRUE, verbose = FALSE))
}
