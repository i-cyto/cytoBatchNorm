#' @title fb_bn_init_batch
#'
#' @description Set up the batch by adding 3 columns to the phenotype table:
#'   batch_id, sample_is_ref,	batch_is_ref. The phenotype table contains one row
#'   for each FCS file. The batch_id column designates the batch identifier to
#'   which the FCS file belongs. The sample_is_ref column contains "Y" (yes) if
#'   the FCS file (aka sample) is the replicated reference sample within the
#'   batch. The batch_is_ref designates the sample that is the reference to
#'   which each reference sample will be aligned.
#'
#' @param my_fb flowBunch, the flowBunch to setup.
#' @param batch_pattern string, a regular expression to extract the batch_id.
#' @param ref_sample_pattern string, the directory in which the project resides.
#' @param ... parameters passed to fb_bn_set_params.
#'
#' @return a flowBunch object.
#'
#' @export

fb_bn_init_batch <- function(
    my_fb,
    batch_pattern = NULL,
    ref_sample_pattern = NULL,
    ...
) {
  # add batch information to pheno table, ie annotate FCS files
  my_fb <- fb_bn_init_pheno(my_fb, batch_pattern, ref_sample_pattern)
  # reorder by batch
  my_fb <- fb_order_by(my_fb, "batch_id")
  # add batch information to panel table, ie define markers normalization
  my_fb <- fb_bn_set_params(my_fb, ...)

  my_fb
}
