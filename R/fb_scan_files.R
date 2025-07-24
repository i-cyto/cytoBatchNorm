#' @title fb_scan_files
#'
#' @description Scan files and check flowBunch compliance.
#'
#' @param files strings, the vector of file names. NULL by default, which leads
#'   to load all FCS files in the given dirname.
#' @param dirname string, the unique dirname where the files are located.
#' @param pattern string.
#' @param keywords string.
#' @param outfile string.
#' @param transformation FALSE,
#' @param truncate_max_range FALSE,
#' @param which.lines integer, the events to load for the quick scan.
#' @param ... parameters passed to read.FCS
#'
#' @importFrom writexl write_xlsx
#' @importFrom flowCore read.FCS keyword pData parameters
#' @importClassesFrom flowCore flowFrame
#' @export

fb_scan_files <- function(
    files = NULL,
    dirname = ".",
    pattern = "\\.fcs$",
    keywords = c("FILENAME", "$TOT", "$PAR", "$CYT", "$CYTSN",
                 "$DATE", "$BTIM", "$ETIM"),
    outfile = "scan_files.xlsx",
    transformation = FALSE,
    truncate_max_range = FALSE,
    which.lines = 1:50,
    ...
) {
  if (is.null(files)) {
    files <- dir(dirname, pattern, ignore.case = TRUE, full.names = TRUE)
    if (length(files) < 1)
      stop("No file matching '", pattern, "' found in ", dirname)
  } else {
    if (!is.character(files))
      stop("'files' must be a character vector.")
    if (!is.null(dirname) && dirname != ".")
      files <- file.path(dirname, files)
    if (!all(file.exists(files)))
      stop("Not all given files could be found in", dirname)
  }
  # TODO: check duplicated basenames
  # fn <- make.unique(basename(files))

  message("Scanning FCS files...")
  # scan files and report information and parameters
  res <- list()
  for (i in 1:length(files)) {
    fn <- files[i]
    if (!file.exists(fn)) {
      report <- c(fn, "-1")
    } else {
      ff <- read.FCS(fn, transformation = transformation,
                     which.lines = which.lines,
                     truncate_max_range = truncate_max_range, ...)
      # get keywords replacing unfound aka NULL by NA
      report <- keyword(ff, keywords)
      report <- c(fn, sapply(report, function(x) if (is.null(x)) NA else x))
      # append name and description of channels
      report <- c(report, c(t(
        as.matrix(pData(parameters(ff))[, c("name", "desc")]))))
    }
    res[[i]] <- report
  }

  message("Extracting panel and pheno")
  # one string per file, filling up with NA
  nc <- max(sapply(res, length))
  res <- lapply(res, function(x)
    paste0(c(x, rep(NA, nc-length(x))), collapse = "\t"))
  # convert strings as data.frame
  res2 <- read.table(
    text = paste0(res, collapse = "\n"), sep = "\t",
    comment.char = "")
  klen <- length(keywords)
  idx <- seq((ncol(res2)-klen)/2)
  colnames(res2) <- c("file_fullname", gsub("\\$", "", keywords))
  colnames(res2)[klen+idx*2] <- sprintf("P%02dN", idx)
  colnames(res2)[klen+idx*2+1] <- sprintf("P%02dS", idx)

  # compliance test
  msg <- ""
  idx_PxxS <- klen+idx*2+1
  idx_PxxN <- klen+idx*2
  val_PxxS <- sapply(res2[, idx_PxxS], function(x) unique(x))
  non_compliant_PxxS <- non_compliant_PxxN <- integer(0)
  if (is.list(val_PxxS)) {
    non_compliant_PxxS <- which(sapply(val_PxxS, length) > 1)
    msg <- paste0(
      "The description of the following channels are identical: ",
      paste0(names(non_compliant_PxxS), collapse = ","), "\n")
    # sapply(val_PxxS[non_compliant], function(x) paste0(sprintf("\'%s\'", x), collapse = ","))
  }
  val_PxxN <- sapply(res2[, idx_PxxN], function(x) unique(x))
  if (is.list(val_PxxN)) {
    non_compliant_PxxN <- which(sapply(val_PxxN, length) > 1)
    msg <- paste0(
      msg,
      paste0("The name of the following channels are identical: ",
             paste0(names(non_compliant_PxxN), collapse = ","), "\n"))
    # sapply(val_PxxN[non_compliant], function(x) paste0(sprintf("\'%s\'", x), collapse = ","))
  }
  # PxxN must be identical
  is_compliant <- length(non_compliant_PxxN) == 0

  # write report
  if (!is.null(outfile)) {
    outdirname <- dirname(outfile)
    if (!dir.exists(outdirname)) {
      message("Report directory ", outdirname, " does not exist")
    } else {
      if (dir.exists(outfile))  # outfile is a directory
        outfile <- file.path(outfile, "scan_files.xlsx")
      writexl::write_xlsx(res2, path = outfile)
      message("Report written ", outfile)
    }
  }

  # return
  structure(res2, outfile = outfile, is_compliant = is_compliant)
}
