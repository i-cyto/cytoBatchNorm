library(data.table)

setClassUnion("m_df_dt_null", c("matrix", "data.frame", "data.table", "NULL"))

#' 'flowBunch': a class for storing observed quantitative properties for a
#' population of cells from a FACS run
#'
#' This class represents the data contained in a \acronym{FCS} file or similar
#' data structure. There are three parts of the data: \enumerate{ \item a
#' numeric matrix of the raw measurement values with \kbd{rows=events} and
#' \kbd{columns=parameters} \item annotation for the parameters (e.g., the
#' measurement channels, stains, dynamic range) \item additional annotation
#' provided through keywords in the \acronym{FCS} file }
#'
#'
#'
#' Objects of class \code{flowBunch} can be used to hold arbitrary data of cell
#' populations, acquired in flow-cytometry.
#'
#' \acronym{FCS} is the Data File Standard for Flow Cytometry, the current
#' version is FCS 3.0. See the vignette of this package for additional
#' information on using the object system for handling of flow-cytometry data.
#'
#' @name flowBunch-class
#' @aliases flowBunch-class flowBunch [,flowBunch,ANY-method
#'   [,flowBunch,filter-method [,flowBunch,filterResult-method $.flowBunch exprs
#'   exprs<- exprs,flowBunch-method exprs<-,flowBunch,matrix-method
#'   exprs<-,flowBunch,ANY-method initialize,flowBunch-method
#'   head,flowBunch-method tail,flowBunch-method description
#'   description,flowBunch-method description<-,flowBunch,list-method
#'   description<-,flowBunch,ANY-method show,flowBunch-method
#'   plot,flowBunch,ANY-method plot,flowBunch-method summary,flowBunch-method
#'   ncol,flowBunch-method nrow,flowBunch-method dim dim,flowBunch-method
#'   featureNames featureNames,flowBunch-method colnames,flowBunch-method
#'   colnames<- colnames<-,flowBunch-method names names,flowBunch-method range
#'   range,flowBunch-method cbind2,flowBunch,matrix-method
#'   cbind2,flowBunch,numeric-method compensate,flowBunch,matrix-method
#'   compensate,flowBunch,data.frame-method
#'   compensate,flowBunch,compensation-method ==,flowBunch,filterResult-method
#'   ==,flowBunch,flowBunch-method <,flowBunch,ANY-method
#'   <=,flowBunch,ANY-method >,flowBunch,ANY-method >=,flowBunch,ANY-method
#'   spillover,flowBunch-method spillover
#' @docType class
#'
#' @slot exprs {Object of class \code{matrix} containing the measured
#'   intensities. Rows correspond to cells, columns to the different measurement
#'   channels. The \code{colnames} attribute of the matrix is supposed to hold
#'   the names or identifiers for the channels. The \code{rownames} attribute
#'   would usually not be set. }
#' @slot panel {An
#'   \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}}
#'   containing information about each column of the \code{flowBunch}. This will
#'   generally be filled in by \code{read.FCS} or similar functions using data
#'   from the \acronym{FCS} keywords describing the parameters.}
#' @slot pheno .
#' @slot histo .
#' @slot options .
#' @slot procs .
#' @slot input .
#' @slot storage .
#' @slot output .
#'
#' @section Creating Objects: Objects can be created using\cr \code{
#'   new("flowBunch",}\cr \code{ exprs = ...., Object of class matrix}\cr \code{
#'   parameters = ...., Object of class AnnotatedDataFrame}\cr \code{
#'   description = ...., Object of class list}\cr \code{ )}\cr
#'
#'   or the constructor \code{flowBunch}, with mandatory arguments \code{exprs}
#'   and optional arguments \code{parameters} and \code{description}.
#'
#'   \code{flowBunch(exprs, parameters, description=list())}
#'
#'   To create a \code{flowBunch} directly from an \acronym{FCS} file, use
#'   function \code{\link[flowCore]{read.FCS}}. This is the recommended and
#'   safest way of object creation, since \code{read.FCS} will perform basic
#'   data quality checks upon import. Unless you know exactly what you are
#'   doing, creating objects using \code{new} or the constructor is discouraged.
#'
#' @section Methods: There are separate documentation pages for most of the
#'   methods listed here which should be consulted for more details. \describe{
#'   \item{[}{Subsetting. Returns an object of class \code{flowBunch}. The
#'   subsetting is applied to the \code{exprs} slot, while the
#'   \code{description} slot is unchanged. The syntax for subsetting is similar
#'   to that of \code{\link[=data.frame]{data.frames}}. In addition to the usual
#'   index vectors (integer and logical by position, character by parameter
#'   names), \code{flowBunchs} can be subset via \code{\link{filterResult}} and
#'   \code{\linkS4class{filter}} objects.
#'
#'   \emph{Usage:}
#'
#'   \code{   flowBunch[i,j]}
#'
#'   \code{   flowBunch[filter,]}
#'
#'   \code{   flowBunch[filterResult,]}
#'
#'   Note that the value of argument \code{drop} is ignored when subsetting
#'   \code{flowBunchs}.
#'
#'   } }
#'
#' @export

setClass(
  # set the name for the class
  "flowBunch",
  # define the slots
  slots = list(
    exprs = "m_df_dt_null",
    pheno = "data.frame",
    panel = "data.frame",
    histo = "list",
    options = "list",
    procs = "list",
    input = "list",
    storage = "list",
    output = "list"
  ),
  # set the default values for the slots.
  prototype=list(
    exprs = NULL,
    pheno = NULL,
    panel = NULL,
    histo = list(
      paste0(Sys.time(), " Initialized")
    ),
    options = list(
      read_fcs = list(
        transformation = FALSE,
        emptyValue = TRUE,
        min.limit = NULL,
        truncate_max_range = FALSE
      ),
      do_compensate = TRUE,
      compensated = FALSE,
      do_transform = FALSE,
      transformed = FALSE,
      transforms = NULL,
      revtransforms = NULL
    ),
    procs = NULL,
    input = list(
      files = NULL,
      dirn = NULL,
      pattern = NULL
    ),
    storage = list(
      basen = NULL,
      dirn = NULL
    ),
    output = list(
      fcs = list(
        basen = "fcs",
        prefix = "",
        suffix = ""
      )
    )
  ),

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    return(TRUE)
  }
)


#' @export
flowBunch <- function()
{
  fr <- new("flowBunch")
}


# Add default methods
setMethod(
  f = "show",
  signature = signature(object = "flowBunch"),
  definition = function(object)
    fb_print(object, verbose = 1)
)


#' @title fb_info
#'
#' @description Analyze the structure of the given flowBunch object. Returns a
#'   vector of strings.
#'
#' @param fb a flowBunch.
#' @param verbose integer, verbosity level.
#'
#' @importFrom stats quantile
#' @importFrom checkmate assertIntegerish
#' @export

fb_info <- function(
  fb,
  verbose = 1
) {
  assertIntegerish(verbose)
  info <- c()
  if (is.null(fb@pheno)) info <- c(info, "No pheno information.") else {
    info <- c(info, sprintf(
      "flowBunch initialized from %d FCS files in %s", nrow(fb@pheno),
      paste(unique(dirname(fb@pheno$file_name)), collapse = ',')))
    info <- c(info, sprintf(
      "Total cells/rows: %d (%s)", sum(fb@pheno$TOT),
      paste(c("min", "quart1", "med", "quart3", "max"), sep = ":",
            quantile(fb@pheno$TOT), collapse = "/")))
  }
  if (is.null(fb@panel)) info <- c(info, "No panel information.") else {
    info <- c(
      info,
      sprintf("Total channels/columns: %d", nrow(fb@panel)),
      sprintf("FCS column names are\n%s",
              paste(names(fb@panel$fcs_colname), sep = "=",
                    fb@panel$fcs_colname, collapse = ", ")))
  }
  if (is.null(fb@exprs)) info <- c(info, "No expression level.") else {
    info <- c(info, paste0(
      "Exprs is a ", class(fb@exprs), " of ",
      nrow(fb@exprs), "x", ncol(fb@exprs)))
  }
  if (is.null(fb@procs)) info <- c(info, "No input processing information.") else {
    info <- c(info,
              names(fb@procs)
    )
  }
  if (is.null(fb@input)) info <- c(info, "No input information.") else {
    info <- c(info,
      sprintf("Input files: %s", paste0(fb@input$files, collapse = ",")),
      sprintf("Input dirname: %s", fb@input$dirn),
      sprintf("Input pattern: %s", fb@input$pattern)
    )
  }
  if (is.null(fb@output)) info <- c(info,"No output information.\n") else {
    info <- c(info,
      # TODO: add information about output
      "To be added...")
  }
  if (is.null(fb@storage)) info <- c(info,"No storage information.\n") else {
    info <- c(info,
      sprintf("Storage basename: %s", fb@storage$basen),
      sprintf("Storage dirname: %s", fb@storage$dirn),
      sprintf("Storage full path: %s",
              file.path(fb@storage$dirn, fb@storage$basen))
    )
  }
  info
}

#' @title fb_print
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param verbose integer, verbosity level.
#'
#' @importFrom checkmate assertIntegerish
#' @export

fb_print <- function(
  fb,
  verbose = 1
) {
  assertIntegerish(verbose)
  cat(fb_info(fb, verbose = verbose), sep = "\n")
}


#' @title fb_is_valid
#'
#' @description ...
#'
#' @param fb a flowBunch.
#'
#' @importFrom utils str
#' @importFrom checkmate assertClass checkDataFrame
#' @export

fb_is_valid <- function(
  fb
) {
  assertClass(fb, "flowBunch")
  str(fb)
  if (checkDataFrame(fb@panel))
    if (!"fcs_colname" %in% colnames(fb@panel))
      warning("fcs_colname is missing in panel")
  if (checkDataFrame(fb@pheno))
    if (!"file_no" %in% colnames(fb@pheno))
      warning("file_no is missing in pheno")
}


#' @title fb_order_by
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param pheno_col string, column name of the pheno to order the pheno table.
#'
#' @importFrom checkmate assertClass assert testCharacter testIntegerish
#' @export

fb_order_by <- function(
  fb,
  pheno_col
) {
  assertClass(fb, "flowBunch")
  assert(testCharacter(pheno_col), testIntegerish(pheno_col))
  if (is.character(pheno_col)) {
    found <- match(pheno_col, colnames(fb@pheno))
    if (any(is.na(found))) {
      message("Unmatched variables ", paste0(pheno_col[is.na(found)], collapse = ","),
              ". No reordering.")
      return(fb)
    }
    pheno_col <- found
  }
  # test pheno_col is in range of 1:ncol(fb@pheno)
  in_range <- sapply(pheno_col, function(x) x >= 1 && x <= ncol(fb@pheno))
  if (any(!in_range)) {
    message("Out of range indices ", paste0(pheno_col[!in_range], collapse = ","),
            ". No reordering.")
    return(fb)
  }
  # reorder
  oo <- do.call(order, as.list(fb@pheno[,pheno_col, drop = FALSE]))
  fb@pheno <- fb@pheno[oo,]
  # if (!is.null(fb@exprs)) {  }
  # done
  fb
}


#' @title fb_freeze_file_no
#'
#' @description Set the file_no column that assign a unique file number to each
#'   FCS file of the flowBunch.
#'
#' @param fb a flowBunch.
#' @param verbose integer, verbosity level.
#'
#' @importFrom checkmate assertClass assertIntegerish
#' @export

fb_freeze_file_no <- function(
  fb,
  verbose = 1
) {
  assertClass(fb, "flowBunch")
  assertIntegerish(verbose)
  # If no file_no column then create one
  if (!"file_no" %in% colnames(fb@pheno)) {
    if (verbose > 1)
      message("Create a file_no in pheno.")
    fb@pheno$file_no <- 1:nrow(fb@pheno)
  }
  fb
}
