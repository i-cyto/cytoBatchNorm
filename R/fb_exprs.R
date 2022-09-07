#' @title fb_get_exprs
#'
#' @description ...
#'
#' @param fb flowBunch.
#' @param ret string .
#' @param transformed logical.
#'
#' @importFrom data.table as.data.table
#' @importFrom checkmate assertClass assertLogical assertString testMultiClass
#' @export

fb_get_exprs <- function(
  fb,
  ret = c("matrix", "data.frame", "data.table"),
  transformed = TRUE
) {
  assertClass(fb, "flowBunch")
  ret <- match.arg(ret)
  assertLogical(transformed)
  if (testMultiClass(fb@exprs, c("matrix", "data.frame"))) {
    if (transformed == fb@options$transformed) {  # already correct
      result <- fb@exprs
    } else if (transformed) {  # transformation is needed
      if (is.null(fb@options$transforms))
        stop("Please define direct transformations.")
      result <- fb@exprs
      for (j in colnames(fb@exprs)) {  # reverse transformation is needed
        fun_id <- match(j, names(fb@options$transforms))
        if (!is.na(fun_id))
          result[,j] <- (fb@options$transforms[[fun_id]])(result[,j])
      }
    } else {
      if (is.null(fb@options$transforms) || is.null(fb@options$revtransforms))
        stop("Please define transformations.")
    }
  } else {
    stop("Transform: exprs must be a matrix.")
  }
  if (ret == "matrix")
    as.matrix(result) else
      if (ret == "data.frame")
        result else
          if (ret == "data.table")
            as.data.table(result) else
              stop("get_exprs: unknoww format to return")
}
