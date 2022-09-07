#' @title fb_initiate
#'
#' @description Create a flowBunch at the designed location by reading the
#'   designated FCS. A directory is also created to keep information (panel and
#'   phenotype of FCS files). The data measured on the cells are not loaded.
#'
#' @param project_name string, name of the project which is a short name for the
#'   current normalization process. The function creates a directory with the
#'   project name in the project_dir.
#' @param project_dir string, the directory in which the project resides.
#' @param fcs_dir string, the directory where all FCS files could be found.
#' @param pattern string, the pattern to identify FCS files.
#' @param cytometer string, either "mass", "flow" or "spectral". This defines
#'   the default cofactor for transforming channels.
#'
#' @return a flowBunch object.
#'
#' @importFrom checkmate assertDirectoryExists
#' @export

fb_initiate <- function(
    project_name,
    project_dir = NULL,
    fcs_dir,
    pattern = "\\.fcs$",
    cytometer = c("mass", "flow", "spectral")
) {
  assertDirectoryExists(fcs_dir)
  if (!is.null(project_dir))
    assertDirectoryExists(project_dir)
  # Empty init
  my_fb <- flowBunch()
  # Set storage params
  my_fb@storage$basen <- project_name
  my_fb@storage$dirn <- project_dir
  # Scan FCS files
  my_fb <- fb_init_from_files(
    my_fb, dirname = fcs_dir, pattern = pattern, verbose = 1)
  # info
  fb_print(my_fb)
  # set direct and reverse transformations
  method <- "asinh"
  cofactor <- switch (cytometer,
                      mass = 5,
                      flow = 500,
                      spectral = 6000
  )
  # my_fb <- fb_tr_set_funcs(
  #   my_fb,
  #   transforms = function(x) asinh(x/cofactor),
  #   revtransforms = function(x) sinh(x)*cofactor)
  # names(my_fb@options$transforms)
  # names(my_fb@options$revtransforms)
  # add transform information to panel table, ie define markers transformation
  my_fb <- fb_tr_set_params(
    my_fb, transf_method = method, transf_params = cofactor)

  my_fb
}


#' @title fb_init_from_files
#'
#' @description Initialize a flowBunch from a set of files, checking their
#'   parameters and channels. Based on fb_scan_files function, similar to
#'   read.flowSet function.
#'
#' @param fb flowBunch
#' @param files strings, the vector of file names. NULL by default, which leads
#'   to load all FCS files in the given dirname.
#' @param dirname string, the unique dirname where the files are located.
#' @param pattern string.
#' @param which.lines integer, the events to load for the quick scan.
#' @param channel_alias strings, see read.flowSet
#' @param ... parameters passed to read.flowSet
#' @param verbose integer, level of verbosity
#'
#' @importFrom stats quantile
#' @importFrom checkmate assertClass assertLogical assertIntegerish
#' @export

fb_init_from_files <- function(
  fb,
  files = NULL,
  dirname = ".",
  pattern = NULL,
  which.lines = 1:50,
  channel_alias = NULL,
  ...,
  verbose = 0
) {
  assertClass(fb, "flowBunch")
  assertIntegerish(verbose, lower = 0, len = 1)
  # TODO: manage options overlap with ...
  # clear expressions
  fb@exprs <- NULL
  # read all FCS files through read.flowSet which does consistency checks
  if (verbose) message("Scanning a bunch of FCS files...")
  fscan <- do.call(
    "fb_scan_files",
    c(list(
      files = files, dirname = dirname, pattern = pattern,
      outfile = NULL,
      which.lines = which.lines, channel_alias = channel_alias),
      fb@options$read_fcs,
      ...
    )
  )
  # return
  if (attr(fscan, "is_compliant") == FALSE) {
    message("Initialization aborted: files are not homogeneous.")
    return(NULL)
  }
  if (verbose) message("Preparing panel and pheno")
  # create panel
  panel <- data.frame(
    fcs_colname = as.character(fscan[1,grep("^P\\d+N", colnames(fscan))]),
    antigen = as.character(fscan[1,grep("^P\\d+S", colnames(fscan))]),
    guess_antigen = NA,
    comment = ""
  )
  idx <- is.na(panel$antigen)
  panel[idx, "antigen"] <- panel[idx, "fcs_colname"]
  panel$guess_antigen <- guess_antigen(panel$antigen)
  fb@panel <- panel
  # create pheno/metadata
  pheno <- data.frame(
    file_name = as.character(fscan[, "file_fullname"]),
    sample_id = NA,
    fscan[, c("TOT", "PAR", "DATE", "BTIM", "ETIM")],
    stringsAsFactors = FALSE
  )
  sample_id <- basename(pheno$file_name)
  sample_id <- gsub("\\.fcs$", "", sample_id, ignore.case = TRUE)
  pheno$sample_id <- sample_id
  pheno$TOT <- as.integer(pheno$TOT)
  pheno$PAR <- as.integer(pheno$PAR)
  fb@pheno <- pheno
  # store input
  fb@input$files <- files
  fb@input$dirn <- dirname
  fb@input$pattern <- pattern
  # return
  message("Initialization done.")
  if (verbose) {
    message(sprintf("flowBunch initialized from %d FCS files", nrow(pheno)))
    message(sprintf("FCS files directory: %s", paste(unique(dirname(pheno$file_name)), collapse = ',')))
    message(sprintf("Channels/columns: %d", nrow(panel)))
    message(sprintf("Cells/rows: %d  (%s)", sum(pheno$TOT), paste(quantile(pheno$TOT), collapse = "/")))
  }
  fb
}
