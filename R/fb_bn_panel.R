#' @title fb_bn_check_channels
#'
#' @description Given a vector of channels, retain those whose the normalization
#'   parameters have been set. If the given vector is NULL, consider all
#'   channels.
#'
#' @param fb a flowBunch.
#' @param channels strings, from fcs_colname of the panel.
#'
#' @importFrom checkmate assertClass
#' @export

fb_bn_check_channels <- function(
    fb,
    channels = NULL
) {
  assertClass(fb, "flowBunch")
  # if no channel, all columns are channels
  # filter channels
  channels_ok <- with(fb@panel, {
    if (is.null(channels)) channels <- fcs_colname
    channels <- intersect(channels, fcs_colname)
    channels <- setdiff(channels, fcs_colname[is.na(batchnorm_method)])
    channels <- setdiff(channels, fcs_colname[batchnorm_method == "none"])
    channels
  })
  if (length(channels_ok) == 0)
    warning("queried channels '", paste0(channels, collapse = ","),
            "' are not matched to fcs_colname")
  channels_ok
}

#' @title fb_bn_set_params
#'
#' @description Set the given batch normalisation method and parameters to the
#'   specified channels.
#'
#' @param fb a flowBunch.
#' @param batchnorm_method String, specify the method.
#' @param batchnorm_params String, specify the parameters of the method.
#' @param overwrite Logical, overwrite is allowed if TRUE (default is FALSE).
#' @param channels .
#'
#' @importFrom checkmate assertClass assertString assertLogical
#' @export

fb_bn_set_params <- function(
  fb,
  batchnorm_method = "percentile_hi",
  batchnorm_params = 0.95,
  overwrite = FALSE,
  channels
) {
  assertClass(fb, "flowBunch")
  assertString(batchnorm_method)
  assertLogical(overwrite, len = 1)
  batchnorm_already <- "batchnorm_method" %in% colnames(fb@panel) &&
    "batchnorm_params" %in% colnames(fb@panel)
  # if channels is missing, all batchnorm_xxx are reset to the given values
  # whether batchnorm_xxx already set or not
  if (missing(channels)) {
    if (batchnorm_already)
      if (!overwrite) {
        message("Normalisation and/or Parameter already in panel. No overwrite.")
        return(fb)
      } else
        message("Normalisation and/or Parameter already in panel. Overwrite.")
    fb@panel$batchnorm_method <- NA
    fb@panel$batchnorm_params <- NA
    channels_ok <- which_is_marker(fb@panel$antigen)
    fb@panel$batchnorm_method[channels_ok] <- batchnorm_method
    fb@panel$batchnorm_params[channels_ok] <- batchnorm_params
  }
  # else, all batchnorm_xxx of the found channels are set to the given values
  # if batchnorm_xxx not present, they are initialized to NA first
  else {
    if (!batchnorm_already) {
      fb@panel$batchnorm_method <- NA
      fb@panel$batchnorm_params <- NA
    }
    id_found <- guess_match_channels(fb, channels)
    for (i in seq(channels)) {
      if (is.na(id_found[i])) {
        message(channels[i], "could not be clearly identified.")
        next
      }
      fb@panel$batchnorm_method[id_found[i]] <- batchnorm_method
      fb@panel$batchnorm_params[id_found[i]] <- batchnorm_params
    }
  }
  fb
}


#' @title fb_bn_split_params
#'
#' @description Split the parameters of batch normalization.
#'
#' @param bn_method string.
#' @param bn_params string.
#'
#' @return a flowBunch object.
#'
#' @export

fb_bn_split_params <- function(
    bn_method,
    bn_params
) {
  bn_params <- strsplit(as.character(bn_params), " *, *")[[1]]
  bn_exclude_zeroes <- which("exclude_zeroes" == bn_params)
  if (length(bn_exclude_zeroes)) bn_params <- bn_params[ -bn_exclude_zeroes ]
  bn_exclude_zeroes <- length(bn_exclude_zeroes) > 0
  bn_transform <- which("transform" == bn_params)
  if (length(bn_transform)) bn_params <- bn_params[ -bn_transform ]
  bn_transform <- length(bn_transform) > 0
  list(
    method = bn_method,
    params = bn_params,
    exclude_zeroes = bn_exclude_zeroes,
    transform = bn_transform)
}

#' @title fb_bn_check_panel
#'
#' @description Carry out sanity checks on the panel slot.
#'
#' @param fb flowBunch
#'
#' @importFrom checkmate assertClass
#' @export

fb_bn_check_panel <- function(
    fb
) {
  assertClass(fb, "flowBunch")
  panel <- fb@panel

  errors <- list()
  # check required columns
  required_cols <- c("transf_method", "transf_params",
                     "batchnorm_method", "batchnorm_params")
  unmatched <- is.na(match(required_cols, colnames(panel)))
  if (any(unmatched))
    errors <- c(errors, paste0(
      "Panel must have columns called '",
      paste0(required_cols[unmatched], collapse = "', '"), "'"))
  # check methods
  available_methods <- c(
    "percentile_hi", "percentile_lohi",
    "percentile_lohi_pos", "quantiles", "none")
  unmatched <- is.na(match(panel$batchnorm_method, c(NA, available_methods)))
  if (any(unmatched))
    errors <- c(errors, paste0(
      "Unknown method:channel  '",
      paste(panel$fcs_colname[unmatched], panel$batchnorm_method[unmatched],
            sep = ":", collapse = "', '"), "'.\nAvailable methods are '",
      paste0(available_methods, collapse = "', '"), "'"))
  # done
  errors
}


#' @title fb_bn_assert_panel
#'
#' @description Carry out sanity checks on the panel slot and throws errors.
#'
#' @param fb flowBunch
#'
#' @export

fb_bn_assert_panel <- function(
    fb
) {
  errors <- fb_bn_check_panel(fb)
  if (length(errors)) {
    stop(errors)
  }
  invisible(fb)
}
