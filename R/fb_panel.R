#' @title fb_read_panel
#'
#' @description Read Excel file and replace in flowBunch.
#'
#' @param fb flowBunch
#' @param file string
#'
#' @importFrom readxl read_excel
#' @importFrom checkmate assertClass assertString assert testClass
#' @export

fb_read_panel <- function(
  fb,
  file
) {
  assertClass(fb, "flowBunch")
  if (missing(file))
    file <- fb_file_name(fb, "%s-panel.xlsx")
  assertString(file)
  assertFileExists(file)
  if (grepl("xlsx$", file)) {
    panel <- readxl::read_excel(file)
    panel <- as.data.frame(panel)
  } else
    stop("Unrecognized file format for panel")
  if (testClass(panel, "data.frame")) {
    # TODO: check that it is a pheno file
  }
  fb@panel <- panel
  fb
}



#' @title fb_write_panel
#'
#' @description Write Excel file to disk.
#'
#' @param fb flowBunch
#'
#' @importFrom writexl write_xlsx
#' @importFrom checkmate assertClass assertPathForOutput
#' @export

fb_write_panel <- function(
  fb
) {
  assertClass(fb, "flowBunch")
  file <- fb_file_name(fb, "%s-panel.xlsx")
  assertPathForOutput(file, overwrite = TRUE)
  if (grepl("xlsx$", file)) {
    if (file.exists(file))
      file.rename(file, timetag(file))
    writexl::write_xlsx(fb@panel, file)
  } else
    stop("Unrecognized file format for panel. No data written to disk.")
}
