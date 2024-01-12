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
    if (any(colnames(pheno)=="batch_id"))
      pheno$batch_id <- as.character(pheno$batch_id)
    pheno <- as.data.frame(pheno)
  } else
    stop("Unrecognized file format for pheno.")
  if (testClass(pheno, "data.frame")) {
    # TODO: check that it is a pheno file
  }
  # TODO: check same number of lines
  # TODO: check lines are matching
  # check sample_id is included in file_name
  test = sapply(seq(nrow(pheno)), function(i) {
    res = grep(pheno$sample_id[i], basename(pheno$file_name[i]))
    length(res) == 1
  })
  if (!all(test == TRUE)) {
    stop("sample_id is not of file_name in the pheno.xlsx: ",
         paste(pheno$sample_id[test], collapse = ", "))
  }
  # skip test comparing in-memory to disk
  if (!is.null(fb@pheno)) {
    # check files are the same
    # test_filepath = c(
    #   setdiff(pheno$file_name, fb@pheno$file_name),
    #   setdiff(fb@pheno$file_name, pheno$file_name))
    test_filename = c(
      setdiff(basename(pheno$file_name), basename(fb@pheno$file_name)),
      setdiff(basename(fb@pheno$file_name), basename(pheno$file_name)))
    if (length(test_filename) > 0) {
      stop("file names of pheno.xlsx do not match the fb object: ",
           paste(test_filename, collapse = ", "))
    }
  }
  # ready
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
