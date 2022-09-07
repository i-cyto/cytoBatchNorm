#' @title fb_tr_set_funcs
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param transforms string, specify the format of data imported in the flowBunch
#' @param revtransforms .
#' @param fcs_colnames .
#'
#' @importFrom checkmate assertClass
#' @export

fb_tr_set_funcs <- function(
  fb,
  transforms,
  revtransforms,
  fcs_colnames
) {
  assertClass(fb, "flowBunch")
  if (missing(fcs_colnames)) {
    fcs_colnames <- fb@panel$fcs_colname[which_is_marker(fb@panel$antigen)]
    if (length(fcs_colnames) == 0)
      stop("Don't know which columns to transform. Check antigens column in the panel table.")
  } else {
    stop("Not implemented yet.")
    # match without NA
    match(fcs_colnames, fb@panel$fcs_colname)
    match(fcs_colnames, fb@panel$antigen)
    if (length(fcs_colnames) == 0)
      stop("Don't know which columns to transform. Check antigens column in the panel table.")
  }
  if (missing(transforms) || missing(revtransforms))
    stop("Direct and reverse transforms must be provided.")
  if (!is.null(fb@options$transforms) || !is.null(fb@options$revtransforms))
    message("Transformations will be overwritten.")
  if (testClass(transforms, "function") && testClass(revtransforms, "function")) {
    fb@options$transforms <-
      sapply(unname(fcs_colnames), function(y) { transforms })
    fb@options$revtransforms <-
      sapply(unname(fcs_colnames), function(y) { revtransforms })
  } else
    stop("Transforms must be functions.")
  fb
}


#' @title fb_tr_set_params
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param transf_method string, the function to apply
#' @param transf_params integer, the cofactor of the transformation
#' @param overwrite logical
#' @param channels .
#'
#' @importFrom checkmate assertClass assertString assertLogical
#' @export

fb_tr_set_params <- function(
  fb,
  transf_method = "asinh",
  transf_params = 5,
  overwrite = FALSE,
  channels
) {
  assertClass(fb, "flowBunch")
  assertCharacter(transf_method, len = 1, min.chars = 4)
  assertLogical(overwrite, len = 1)
  transf_already <- "transf_method" %in% colnames(fb@panel) &&
    "transf_params" %in% colnames(fb@panel)
  # if channels is missing, all transf_xxx are reset to the given values
  # whether transf_xxx already set or not
  if (missing(channels)) {
    if (transf_already)
      if (!overwrite) {
        message("Transform method and/or parameter already in panel. No overwrite.")
        return(fb)
      } else
        message("Transform method and/or parameter already in panel. Overwrite.")
    # determine channels
    channels_ok <- which_is_marker(fb@panel$antigen)
    # erase all transformations
    fb@panel$transf_method <- NA
    fb@panel$transf_params <- NA
    # write defined transformation
    fb@panel$transf_method[channels_ok] <- transf_method
    fb@panel$transf_params[channels_ok] <- transf_params
    # build corresponding functions
    if (transf_method == "asinh") {
      cofactor <- as.numeric(transf_params)
      transforms <- function(x) asinh(x/cofactor)
      revtransforms <- function(x) sinh(x)*cofactor
    }
    channels <- fb@panel$fcs_colname[channels_ok]
    fb@options$transforms <-
      sapply(unname(channels), function(y) { transforms })
    fb@options$revtransforms <-
      sapply(unname(channels), function(y) { revtransforms })
  }
  # else, all transf_xxx of the found channels are set to the given values
  # if transf_xxx not present, they are initialized to NA first
  else {
    # if no transformation defined, setup empty for all channels
    if (!transf_already) {
      fb@panel$transf_method <- NA
      fb@panel$transf_params <- NA
    }
    # find channels and assign functions
    id_founds <- guess_match_channels(fb, channels)
    for (i in seq(channels)) {
      if (is.na(id_founds[i])) {
        message(channels[i], "could not be clearly identified.")
        next
      }
      id_found <- id_founds[i]
      fb@panel$transf_method[id_found] <- transf_method
      fb@panel$transf_params[id_found] <- transf_params
      # build corresponding functions
      if (transf_method == "asinh") {
        cofactor <- as.numeric(transf_params)
        transforms <- function(x) asinh(x/cofactor)
        revtransforms <- function(x) sinh(x)*cofactor
      }
      # assign functions
      channel <- fb@panel$fcs_colname[id_found]
      fb@options$transforms[[channel]] <- transforms
      fb@options$revtransforms[[channel]] <- revtransforms
    }
  }
  fb
}


#' @title fb_tr_update_from_panel
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param channels .
#'
#' @importFrom checkmate assertClass check_true
#' @export

fb_tr_update_from_panel <- function(
  fb,
  channels
) {
  assertClass(fb, "flowBunch")
  transf_defined <- "transf_method" %in% colnames(fb@panel) &&
    "transf_params" %in% colnames(fb@panel)
  assert(check_true(transf_defined))
  transf_method <- fb@panel$transf_method
  transf_params <- fb@panel$transf_params
  if (missing(channels)) {
    channels <- which(!is.na(fb@panel$transf_method))
    channels <- fb@panel$fcs_colname[channels]
  }
  # find channels and assign functions
  id_founds <- guess_match_channels(fb, channels)
  for (i in id_founds) {
    if (is.na(i)) {
      message(channels[i], "could not be clearly identified.")
      next
    }
    # build corresponding functions
    if (transf_method[i] == "asinh") {
      factory_direct <- function(cofactor) {
        force(cofactor)
        function(x) asinh(x/cofactor)
      }
      factory_revers <- function(cofactor) {
        force(cofactor)
        function(x) sinh(x)*cofactor
      }
      cofactor <- as.numeric(transf_params[i])
      transf <- factory_direct(cofactor)
      revtransf <- factory_revers(cofactor)
    } else {
      factory_untransformed <- function() {
        function(x) x
      }
      transf <- factory_untransformed()
      revtransf <- factory_untransformed()
    }
    # assign functions
    channel <- fb@panel$fcs_colname[i]
    fb@options$transforms[[channel]] <- transf
    fb@options$revtransforms[[channel]] <- revtransf
  }
  # else, all transf_xxx of the found channels are set to the given values
  # if transf_xxx not present, they are initialized to NA first

  fb
}


#' @title asinhTransf
#'
#' @param transformId .
#' @param cofactor .
#'
#' @importFrom methods new
#' @importClassesFrom flowCore transform
#' @export

asinhTransf <- function(transformId, cofactor){
  t = new("transform", .Data = function(x) asinh(x / cofactor))
  t@transformationId = transformId
  t
}

#' @title sinhTransf
#'
#' @param transformId .
#' @param cofactor .
#'
#' @importFrom methods new
#' @importClassesFrom flowCore transform
#' @export

sinhTransf <- function(transformId, cofactor){
  t = new("transform", .Data = function(x) sinh(x) * cofactor)
  t@transformationId = transformId
  t
}

#' @title asinhCytof
#'
#' @export

asinhCytof <- asinhTransf("asinhCytof")

#' @title sinhCytof
#'
#' @export

sinhCytof <- sinhTransf("sinhCytof")
