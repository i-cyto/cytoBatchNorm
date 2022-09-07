#' @title fb_bn_correct_batch
#'
#' @description Adjust batch effects using the modelling of reference samples.
#'
#' @param fb flowBunch.
#' @param channels strings, name of channels to display.
#' @param verbose integer.
#'
#' @export

fb_bn_correct_batch <- function(
    fb,
    channels,
    verbose = 1
) {
  if (verbose) message("Correcting batch effects...")

  # batchnorm params
  batchnorm_method <- fb@panel$batchnorm_method
  batchnorm_params <- fb@panel$batchnorm_params
  names(batchnorm_method) <- names(batchnorm_params) <- fb@panel$fcs_colname

  # get batch id from file no
  fno_to_bid <- factor(fb@pheno$batch_id)
  names(fno_to_bid) <- fb@pheno$file_no
  exprs_bid <- fno_to_bid[as.character(fb@exprs[, "file_no"])]
  rm(fno_to_bid)

  # channels
  if (missing(channels)) {
    channels <- which(!is.na(batchnorm_method))
    channels <- fb@panel$fcs_colname[channels]
  }

  models <- fb@procs$batchnorm_funs

  for (chn in channels) {
    if (verbose) message("  ", chn, appendLF = FALSE)
    # parse batchnorm params
    bnp <- fb_bn_split_params(batchnorm_method[chn], batchnorm_params[chn])

    # get data
    all_exprs <- fb_get_exprs(fb, "matrix", transformed = bnp[["transform"]])
    # all_exprs_fid <- all_exprs[, "file_no"]
    all_exprs_fid <- exprs_bid

    for (batch in unique(all_exprs_fid)) {
      row_ids <- batch == all_exprs_fid
      # table(row_ids)
      fb@exprs[row_ids, chn] <-
        models[[chn]][[batch]](all_exprs[row_ids, chn])
    }

    # back to original data scale
    if (fb@options$transformed != bnp[["transform"]]) {
      # reverse transformation is needed
      fun_id <- match(chn, names(fb@options$transforms))
      if (!is.na(fun_id))
        fb@exprs[, chn] <- (fb@options$revtransforms[[fun_id]])(fb@exprs[, chn])
    }

  }
  if (verbose) message("\nDone")
  fb
}


#' @title fb_bn_correct_fcs
#'
#' @description Adjust batch effects using the modelling of reference samples.
#'   Apply to all FCS files.
#'
#' @param fb flowBunch.
#' @param hook function, called after each file is processed.
#' @param verbose integer, verbosity level.
#'
#' @export

fb_bn_correct_fcs <- function(
    fb,
    hook = NULL,
    verbose = 1
) {
  if (verbose) message("Correcting batch effects (fcs)...")

  # 1. normalize FCS

  # batchnorm params
  batchnorm_method <- fb@panel$batchnorm_method
  batchnorm_params <- fb@panel$batchnorm_params
  names(batchnorm_method) <- names(batchnorm_params) <- fb@panel$fcs_colname

  # channels
  channels <- which(!is.na(batchnorm_method) & batchnorm_method != "none")
  channels <- fb@panel$fcs_colname[channels]

  # file_ids
  file_nos <- fb@pheno$file_no

  # functions to normalize
  models <- fb@procs$batchnorm_funs

  # finally apply batch adjustment
  for (file_no in file_nos) {
    i <- match(file_no, fb@pheno$file_no)
    batch_id <- fb@pheno$batch_id[i]
    msg <- sprintf("Processing %d: %s of %s", i, fb@pheno$sample_id[i], batch_id)
    if (verbose) message(msg)
    if (!is.null(hook)) hook(msg)
    # read FCS
    my_fb_single <- fb_load_cells(fb, file_ids = file_no, sampling = "none")
    # process channels
    for (chn in channels) {
      if (verbose > 1) message("  ", chn, appendLF = FALSE)
      # parse batchnorm params
      bnp <- fb_bn_split_params(batchnorm_method[chn], batchnorm_params[chn])
      # get data
      all_exprs <- fb_get_exprs(my_fb_single, "matrix", transformed = bnp[["transform"]])
      all_exprs[, chn] <- models[[chn]][[batch_id]](all_exprs[, chn])
      # TODO: manage back transform
      # TODO: build a setter function to push expression in default mode
      # if (fb@options$transformed != bnp[["transform"]]) {
      #   # reverse transformation is needed
      #   fun_id <- match(chn, names(fb@options$transforms))
      #   if (!is.na(fun_id))
      #     fb@exprs[, chn] <- (fb@options$revtransforms[[fun_id]])(fb@exprs[, chn])
      # }
      # TODO: manage zero
      my_fb_single@exprs <- all_exprs
    }
    if (verbose > 1) message()  # append LF
    # write.FCS
    fb_write_fcs(my_fb_single, file_no)
  }

  if (verbose) message("\nDone FCS")

  # 2. Build pheno and panel files
  # the pheno and panel files will be located along with the FCS files

  to_fb <- flowBunch()
  # the storage is the directory of normalized FCS
  to_fb@storage$dirn <- fb_file_path(fb)
  to_fb@storage$basen <- fb@output$fcs$basen
  # the input FCS of this flowBunch are directly in the storage directory
  to_fb@input$dirn <- fb_file_path(to_fb)
  to_fb@input$pattern <- "\\.[fF][cC][sS]$"
  # update and write pheno
  pheno <- fb@pheno
  fn <- gsub(fb@input$pattern, "", basename(pheno$file_name))
  fn <- paste0(fb@output$fcs$prefix, fn, fb@output$fcs$suffix)
  pheno$sample_id <- fn
  fn <- file.path(to_fb@input$dirn, paste0(fn, ".fcs"))
  pheno$file_name <- fn
  to_fb@pheno <- pheno
  fb_write_pheno(to_fb)
  # update and write panel
  panel <- fb@panel
  batch_cols <- grep("^batchnorm_", colnames(panel))
  if (length(batch_cols))
    panel <- panel[, -batch_cols]
  to_fb@panel <- panel
  fb_write_panel(to_fb)

  # 3. Done

  if (verbose) message("\nDone pheno and panel")
  if (!is.null(hook)) hook("Processing done")
  to_fb
}
