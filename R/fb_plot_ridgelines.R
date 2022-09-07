#' @title fb_plot_ridgelines
#'
#' @description Prints a diagnostic graphics of densities along with
#'   percentiles. Loop across all given channels.
#'
#' @param my_fb flowBunch.
#' @param channels strings, name of channels to display.
#' @param cof numerical.
#' @param cut_lower_than numerical.
#' @param title string, title at the top of the plot.
#'
#' @return a flowBunch object.
#'
#' @export

fb_plot_ridgelines <- function(
    my_fb,
    channels,
    channel_names,
    group_by = "sample_id",
    cof = 8,
    cut_lower_than = -5,
    title = NULL
) {
  tmp_exprs <- fb_get_exprs(my_fb, transformed = TRUE, ret = "data.frame")

  if (missing(channels)) {
    if ("batchnorm_method" %in% colnames(my_fb@panel)) {
      channels_idx <- which(!is.na(my_fb@panel$batchnorm_method))
    } else if ("transf_method" %in% colnames(my_fb@panel)) {
      channels_idx <- which(!is.na(my_fb@panel$transf_method))
    } else {
      stop("Panel should have a transf_method or batchnorm_method column.")
    }
    channels <- my_fb@panel$fcs_colname[channels_idx]
  }

  if (missing(channel_names)) {
    channel_names <- my_fb@panel$antigen
    names(channel_names) <- my_fb@panel$fcs_colname
  }

  # TODO: check channels are in the fcs_colname
  # TODO: check channels are in the names of channel_names

  file_no_mapping <- my_fb@pheno[, group_by]
  names(file_no_mapping) <- my_fb@pheno[, "file_no"]

  for (channel in channels)
    dt_plot_ridgelines(
      dta = tmp_exprs,
      channels = channel,
      channel_names = channel_names,
      batch_idn = file_no_mapping,
      batch_lab = group_by,
      cof = cof,
      cut_lower_than = cut_lower_than,
      title = title
    )
}


#' @title dt_plot_ridgelines
#'
#' @description Get the indices of channel names in a flowFrame. It tries to
#'   match them against names, desc of the flowFrame and guess_antigen(desc).
#'
#' @param dta .
#' @param channels .
#' @param batch_col .
#' @param batch_idn .
#' @param probs .
#' @param pdf_file .
#' @param dens_n .
#' @param dens_bw .
#' @param cut_lower_than .
#' @param cof .
#' @param title string, title of the plot.
#'
#' @examples
#' \dontrun{
#' fcs.loc <- system.file("extdata",package="flowCore")
#' samp <- read.flowSet(dir = fcs.loc, pattern = "0+")
#' }
#'
#' @keywords channels
#'
#' @import ggplot2
#' @import ggridges
#' @importFrom flowCore colnames
#' @importFrom utils head write.csv
#' @importFrom stats density spline
#' @importFrom grDevices pdf dev.off
#' @importFrom checkmate assertMultiClass assertCharacter assertString assert checkIntegerish checkCharacter assertNumeric
#' @export

dt_plot_ridgelines <- function(
    dta,
    channels,
    channel_names,
    batch_col = "file_no",
    batch_idn,
    batch_lab = "file",
    probs = c(.2, .4, .6, .7, .8, .85, .9, .95, .97, .99),
    pdf_file = NULL,
    dens_n = 128,
    dens_bw = 0.1,
    cut_lower_than = 0.2,
    cof = 1/3,
    title = NULL
) {
  assertMultiClass(dta, c("data.frame", "matrix"))
  assertCharacter(channels)
  assert(missing(channel_names), checkCharacter(channel_names))
  assertString(batch_col)
  assert(missing(batch_idn), checkIntegerish(batch_idn), checkCharacter(batch_idn))
  assertString(batch_lab)
  assertNumeric(probs, lower = 0, upper = 1, finite = TRUE, min.len = 1, max.len = 20)
  assertString(pdf_file, fixed = "\\.pdf$", ignore.case = TRUE, null.ok = TRUE)
  if (!batch_col %in% colnames(dta)) stop(sprintf(
    "batch_col '%s' not in the column names", batch_col
  ))
  if (missing(channels)) {
    channels <- setdiff(colnames(dta), batch_col)
  } else {
    matched <- match(channels,colnames(dta))
    if (any(is.na(matched))) stop(sprintf(
      "channels '%s' not in the column names", paste(channels[is.na(matched)])
    ))
  }
  if (missing(channel_names)) {
    channel_names <- channels
    names(channel_names) <- channels
  } else if (is.null(names(channel_names))) {
    if (length(channel_names) != length(channels))
      stop("length of channels and channel_names is not the same")
    names(channel_names) <- channels
  } else {
    ok <- channels %in% names(channel_names)
    if (!all(ok))
      stop("channels are not in the names of channel_names ", channels[!ok])
  }
  # convert batch_col into a factor and map names
  dta[,batch_col] = factor(dta[,batch_col])
  # check batch_idn aka the mapping table of the batch_col column
  if (missing(batch_idn))
    batch_idn <- unique(dta[,batch_col])
  if (is.null(names(batch_idn)) && testIntegerish(batch_idn)) {
    tmp <- sprintf("%s%03d", batch_col, batch_idn)
    names(tmp) <- batch_idn
    batch_idn <- tmp
  }
  if (is.null(names(batch_idn)) && testCharacter(batch_idn)) {
    names(batch_idn) <- seq_along(batch_idn)
  }
  if (!all(levels(dta[,batch_col]) %in% names(batch_idn)))
    stop("levels of batch_col ", batch_col, " cannot not be found in batch_idn")
  # graphical output
  if (!is.null(pdf_file)) {
    pdf(pdf_file)  # TODO: tune options
  }
  # change the levels name
  levels(dta[,batch_col]) <- batch_idn[levels(dta[,batch_col])]
  # display
  dta_low <- cut_lower_than
  for (chn in channels) {
    dta_chn = dta[, chn]
    # Density on the full range
    if (any(is.na(dta_chn[dta_chn > dta_low]))) browser()
    dd <- density(dta_chn[dta_chn > dta_low], n = dens_n, bw = dens_bw, na.rm = TRUE)
    dd_cof <- max(dd$y)/cof  # asinh cofactor for scaling the density height
    # qq = quantile(dta_chn[dta_chn > dta_low], probs = c(0.5, 0.99))
    # dd_cof <- max(dd$y) / (qq[2] / qq[1]) / cof
    from_to <- range(c(-1, dd$x))
    dd_bw <- dd$bw
    dd_n <- dd$y > max(dd$y)/1000
    dd_n <- dd$y > 0
    from_to <- range(dd$x[dd_n])
    # Density per batch
    ggxx = NULL  # density curves
    ggqq = NULL  # quantiles across curves
    for (f in 1:nlevels(dta[,batch_col])) {
      file_cells = dta[,batch_col] == levels(dta[,batch_col])[f]
      # quantile using all data
      zz = dta_chn[file_cells]  #  & dta_chn > asinh(0/5)
      qq = quantile(zz, probs = probs)
      # density using data > threshold
      zz = dta_chn[file_cells & dta_chn > dta_low]
      dd = density(zz, n = 256,
                   from = from_to[1], to = from_to[2], bw = dd_bw)
      dd$y = asinh(dd$y/dd_cof)  # density scaling
      # Assemble density
      ggxx = rbind(ggxx, data.frame(
        x = dd$x, height = dd$y, file = f, stringsAsFactors = FALSE))
      # Assemble quantiles, height at the corresponding density
      ggqq = rbind(ggqq, data.frame(
        x = qq, height = spline(dd$x, dd$y, n = length(dd$x), xout = qq)$y,
        file = f, quantile = probs, stringsAsFactors = TRUE))
      ggqq$height = pmax(0, ggqq$height)  # avoid strange effects of spline
    }
    ggqq$quantile = factor(ggqq$quantile, levels = unique(ggqq$quantile), labels = sprintf("%0.2f", unique(ggqq$quantile)))
    # Plot
    graphx <- ggplot(ggxx, aes_string(x="x", y = "file", height = "height", group="file")) +
      geom_hline(aes(yintercept=file), col = "#AAAAAA") +
      geom_ridgeline(scale = 1.5) +
      xlim(-5, 10) + xlab(chn) +
      geom_vline(xintercept = c(0, 2.5, 5, 7.5), lty = 2, col = "#55555599") +
      geom_point(data = ggqq, aes_string(x="x", y="file+height*1.5")) +
      geom_path(data = ggqq, aes_string(x="x", y="file+height*1.5", group = "quantile", color = "quantile"), size = 1, alpha = 0.7) +
      geom_text(data = data.frame(
        x = -5, height = 0.1, file = 1:nlevels(dta[,batch_col]), label = levels(dta[,batch_col]), stringsAsFactors = FALSE),
        aes_string(x="x", y="file+height", label = "label"), hjust = "left", vjust = "bottom", size = 3) +
      #    scale_fill_manual(values = colors) +
      #    guides(colour = guide_legend(override.aes = list(size=5), ncol = 1)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = title, y = batch_lab, x = channel_names[chn])
    print(graphx)
  }
  if (!is.null(pdf_file)) dev.off()
}
