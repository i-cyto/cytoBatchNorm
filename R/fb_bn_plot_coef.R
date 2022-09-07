#' @title fb_bn_plot_coef
#'
#' @description Prints a diagnostic graphics of densities along with
#'   percentiles. Loop across all given channels.
#'
#' @param my_fb_ref flowBunch.
#' @param my_fb_ref_adj
#' @param channels strings, name of channels to display.
#' @param calc_mode string, either "diff_tr" or "ratio_un". "diff_tr" computes
#'   difference of transformed intensity. "ratio_un" computes ratio of
#'   untransformed intensity.
#' @param cut_lower_than numerical.
#' @param title string, title at the top of the plot.
#'
#' @note print graphics.
#'
#' @return none.
#'
#' @export

fb_bn_plot_coef <- function(
    my_fb_ref,
    my_fb_ref_adj,
    channels = NULL,
    calc_mode = c("diff_tr", "ratio_un"),
    gg_ncol = 1
) {
  calc_mode <- match.arg(calc_mode)
  fb_bn_assert_pheno(my_fb_ref)
  channels <- fb_bn_check_channels(my_fb_ref, channels)
  if (!length(channels)) return(NULL)

  # get data
  transf <- calc_mode == "diff_tr"
  df_raw <- fb_get_exprs(
    my_fb_ref, "data.frame", transformed = transf)
  df_adj <- fb_get_exprs(
    my_fb_ref_adj, "data.frame", transformed = transf)

  # compute percentiles
  gg_perc_all <- NULL
  for (chan in channels) {
    idx <- guess_match_channels(my_fb_ref, chan)
    channel_name <- my_fb_ref@panel$antigen[idx]

    # get correction
    batch_params <- as.character(my_fb_ref@panel$batchnorm_params[idx])
    # check
    # parse parameters
    percentiles <- as.numeric(strsplit(batch_params, ",\\s*")[[1]])
    # percentiles are extracted from panel that is verified during user entering
    # get effective percentiles
    percentiles <- switch(
      my_fb_ref@panel$batchnorm_method[idx],
      "none" = FALSE,
      "percentile_hi" = max(percentiles),
      "quantiles" = percentiles,
      "percentile_lohi" = if (length(percentiles) == 1) c(0.4, percentiles) else range(percentiles),
      "percentile_lohi_pos" = if (length(percentiles) == 1) c(0.4, percentiles) else range(percentiles)
    )
    # get percentiles
    # get raw intensities of percentiles
    # get transformed intensities of percentiles
    # get corrected transformed intensities of percentiles
    # compute difference
    perc_raw <- tapply(
      df_raw[,chan], df_raw[, "file_no"],
      quantile, probs = percentiles, names = FALSE)
    perc_adj <- tapply(
      df_adj[,chan], df_adj[, "file_no"],
      quantile, probs = percentiles)
    channel_title <- sprintf("%s, %s", channel_name, chan)
    gg_perc_all <- rbind(gg_perc_all, data.frame(
      channel_name = channel_name,
      channel_title = channel_title,
      file_no = rep(names(perc_raw), each = length(percentiles)),
      percentiles = percentiles,
      raw = unlist(perc_raw),
      adj = unlist(perc_adj)
    ))
  }
  # compute scaling coeff
  if (calc_mode == "diff_tr") {
    calc_label <- "difference of transformed intensity"
    gg_perc_all$coeff <- with(gg_perc_all, adj - raw)
  } else {
    calc_label <- "ratio of untransformed intensity"
    gg_perc_all$coeff <- with(gg_perc_all, ifelse(raw > 0, adj / raw, 1))
  }
  # map batch
  mapping <- factor(
    my_fb_ref@pheno$batch_id, levels = my_fb_ref@pheno$batch_id)
  names(mapping) <- my_fb_ref@pheno$file_no
  gg_perc_all$batch_id <- mapping[as.character(gg_perc_all$file_no)]
  # file_no_mapping <- my_fb@pheno[, group_by]
  # names(file_no_mapping) <- my_fb@pheno[, "file_no"]

  # plot percentiles
  # percentiles in decreasing order
  gg_perc_all$percentiles <- factor(
    gg_perc_all$percentiles, levels = sort(unique(gg_perc_all$percentiles), decreasing = TRUE))

  for (chan_name in unique(gg_perc_all$channel_name)) {
    gg_perc <- subset(gg_perc_all, channel_name == chan_name)
    channel_title <- unique(gg_perc$channel_title)

    llim <- range(gg_perc$coeff)
    # gg <- ggplot(gg_perc, aes(batch_id, coeff)) +
    #   geom_col(width = 0.7)
    gg <- ggplot(gg_perc, aes(batch_id, coeff, fill = percentiles)) +
      geom_col(position = position_dodge(width=0.8), width = 0.7) + scale_fill_grey()
    if (calc_mode == "diff_tr") {
      gg <- gg +
        ylim(min(-1, llim[1]), max(1, llim[2])) +
        geom_hline(yintercept = 0, lty = 2)
    } else {
      gg <- gg +
        ylim(min(0, llim[1]), max(2, llim[2])) +
        geom_hline(yintercept = 1, lty = 2)
    }
    gg <- gg  +
      # facet_wrap(~percentiles, ncol = gg_ncol) +
      labs(x = "batch", title = channel_title, y = calc_label) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    print(gg)
  }
}
