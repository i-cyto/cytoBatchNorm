#' @title fb_read_pheno
#'
#' @description Read Excel file and replace in flowBunch.
#'
#' @param fb flowBunch
#' @param file string
#'
#' @importFrom readxl read_xlsx
#' @importFrom checkmate assertClass assertString assertFileExists testClass
#' @export

fb_read_pheno <- function(
  fb,
  file
) {
  assertClass(fb, "flowBunch")
  if (missing(file))
    file <- fb_file_name(fb, "%s-pheno.xlsx")
  assertString(file)
  assertFileExists(file)
  if (grepl("xlsx$", file)) {
    pheno <- readxl::read_excel(file)
    pheno$batch_id <- as.character(pheno$batch_id)
    pheno <- as.data.frame(pheno)
  } else
    stop("Unrecognized file format for pheno.")
  if (testClass(pheno, "data.frame")) {
    # TODO: check that it is a pheno file
  }
  # TODO: check same number of lines
  # TODO: check lines are matching
  fb@pheno <- pheno
  fb
}


#' @title fb_write_pheno
#'
#' @description Write Excel file to disk.
#'
#' @param fb flowBunch
#'
#' @importFrom writexl write_xlsx
#' @importFrom checkmate assertClass assertPathForOutput
#' @export

fb_write_pheno <- function(
  fb,
  mini = FALSE
) {
  assertClass(fb, "flowBunch")
  file <- fb_file_name(fb, "%s-pheno.xlsx")
  assertPathForOutput(file, overwrite = TRUE)
  if (grepl("xlsx$", file)) {
    if (file.exists(file))
      file.rename(file, timetag(file))
    # minimalize pheno
    pheno <- fb@pheno
    if (mini) {
      # simplify file_name
      pheno$file_name <- unname(sapply(pheno$file_name, basename))
    }
    # write pheno
    writexl::write_xlsx(pheno, file)
  } else
    stop("Unrecognized file format for pheno. No data written to disk.")
}
