#' @title fb_bn_model_batch
#'
#' @description Model the batch effects from reference samples.
#'
#' @param fb flowBunch.
#' @param channels strings, name of channels to display.
#' @param verbose integer, verbosity level.
#'
#' @details The model defines a function for each channel of each batch that
#'   transforms the intensity so its percentiles are aligned to the ones of the
#'   reference batch. The model retrieves the method and its parameters for each
#'   channel.
#'
#'   Current implemented methods are:
#'
#'   - "none": f(x) = x
#'
#'   - "percentile_hi": f(x) = x
#'
#'   - "percentile_hi": f(x) = (x)/(batHi)*(refHi)
#'
#'   - "percentile_lohi": f(x) = (x-batLo)/(batHi-batLo)*(refHi-refLo)+refLo
#'
#'   - "percentile_lohi_pos": f(x) =
#'   (x-batLo)/(batHi-batLo)*(refHi-refLo)+refLo, and if f(x) < 0 then f(x) = 0
#'
#'   - "quantiles": f(x) = spline_interpolation(x) that each quantile of the
#'   current batch into the corresponding quantiles of the reference batch. By
#'   default quantiles are c(0.01, .2, .4, .6, .8, .9, .99). If a single
#'   positive integer is set, than quantiles will cut the 0..1 percentiles. If a
#'   set of quantiles is defined, they will be used  for the spline.
#'
#' @importFrom stats splinefun
#' @export

fb_bn_model_batch <- function(
    fb,
    channels,
    verbose = 1
) {
  if (verbose) message("Modeling batch effects...")

  # batchnorm params
  batchnorm_method <- fb@panel$batchnorm_method
  batchnorm_params <- fb@panel$batchnorm_params
  names(batchnorm_method) <- names(batchnorm_params) <- fb@panel$fcs_colname

  # get batch id from file no
  fno_to_bid <- factor(fb@pheno$batch_id)
  names(fno_to_bid) <- fb@pheno$file_no
  exprs_bid <- fno_to_bid[as.character(fb@exprs[, "file_no"])]
  rm(fno_to_bid)

  # get file_no for reference file
  ref_numid <- which(fb@pheno$batch_is_ref != "")
  ref_bid <- fb@pheno$batch_id[ref_numid]
  rm(ref_numid)

  # channels
  if (missing(channels)) {
    channels <- which(!is.na(batchnorm_method))
    channels <- fb@panel$fcs_colname[channels]
  }

  # options_keep_source <- options("keep.source")
  # options(keep.source=FALSE)

  models <- list()
  for (chn in channels) {
    models[[chn]] <- list()
    if (verbose) message("  ", chn, appendLF = FALSE)

    # parse batchnorm params
    bnp <- fb_bn_split_params(batchnorm_method[chn], batchnorm_params[chn])

    # get data from a single channel, so a vector
    all_exprs <- fb_get_exprs(fb, "matrix", transformed = bnp[["transform"]])
    # TODO: get one channel at a time because of transformation cost
    all_exprs_chn <- all_exprs[, chn]
    all_exprs_fid <- exprs_bid
    rm(all_exprs)

    # exclude zeroes
    if (bnp[["exclude_zeroes"]]) {
      all_exprs_fid <- all_exprs_fid[all_exprs_chn > 0]
      all_exprs_chn <- all_exprs_chn[all_exprs_chn > 0]
    }

    # build the model
    mod_env <- new.env(parent = baseenv())
    mod_env$ref_bid <- ref_bid
    mod_env$chn <- chn
    mod_env$method <- method <- bnp[["method"]]

    if (method == "none") {

      params <- as.character(unique(all_exprs_fid))
      mod_env$params <- params
      funs <- local({ sapply(params, function(y) {
        function(x) x
      })}, envir = mod_env)

    } else if (method == "percentile_hi") {

      qlo <- 0
      qhi <- max(as.numeric(bnp[["params"]]))
      params <- tapply(all_exprs_chn, all_exprs_fid,
                       quantile, probs = c(qlo, qhi))
      mod_env$params <- params
      funs <- local({ sapply(names(params), function(y) {
        refHi <- params[[ref_bid]][2]
        batHi <- params[[y]][2]
        if (batHi == 0) {
          warning("Infinite scaling for batch ", y, ", channel ", chn,
                  ". No scaling.", call. = FALSE)
          function(x) x
        } else {
          function(x) (x/batHi*refHi)
        }
      })}, envir = mod_env)

    } else if (method == "quantiles") {

      bnp_params <- as.numeric(bnp[["params"]])
      quantileValues <- fb_bn_params_to_quantiles(bnp_params)
      params <- tapply(all_exprs_chn, all_exprs_fid,
                       quantile, probs = quantileValues)
      # params <- tapply(all_exprs_chn, all_exprs_fid,
      #                  function(x) {
      #                    x <- x[x>0]
      #                    quantile(x, probs = quantileValues)
      #                  })
      mod_env$params <- params
      funs <- local({ sapply(names(params), function(y) {
        refQ <- params[[ref_bid]]
        batQ <- params[[y]]
        suppressWarnings(
          spl <- stats::splinefun(batQ, refQ, method = "monoH.FC"))
        spl
      })}, envir = mod_env)

    } else if (method %in%
               c("percentile_lohi", "percentile_lohi_pos")) {

      bnp_params <- as.numeric(bnp[["params"]])
      if (length(bnp_params) == 1) qlo <- 0.40 else
        qlo <- max(min(bnp_params), 0)
      qhi <- min(max(bnp_params), 1)
      params <- tapply(all_exprs_chn, all_exprs_fid,
                       quantile, probs = c(qlo, qhi))
      mod_env$params <- params
      funs <- local({ sapply(names(params), function(y) {
        refLo <- params[[ref_bid]][1]
        batLo <- params[[y]][1]
        refHi <- params[[ref_bid]][2]
        batHi <- params[[y]][2]
        if (batHi == batLo) {
          warning("Infinite scaling for batch ", y, ", channel ", chn,
                  ". No scaling.", call. = FALSE)
          function(x) x
        } else if (method == "percentile_lohi") {
          function(x) (x-batLo)/(batHi-batLo)*(refHi-refLo)+refLo
        } else {
          function(x) pmax((x-batLo)/(batHi-batLo)*(refHi-refLo)+refLo, 0)
        }
      })}, envir = mod_env)

    } else {

      stop("Unknown batchnorm method ", method, " for channel ", chn)

    }

    models[[chn]] <- funs

  }
  # options(options_keep_source)
  if (verbose) message("\nDone")
  temp <- fb@procs$batchnorm_funs
  for (chn in names(models)) {
    temp[[chn]] <- models[[chn]]
  }
  fb@procs$batchnorm_funs <- temp
  fb
}
