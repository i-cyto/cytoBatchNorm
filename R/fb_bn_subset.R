#' @title fb_bn_subset_references
#'
#' @description Extract reference samples in each batch and create a new
#'   flowBunch.
#'
#' @param my_fb flowBunch.
#'
#' @return a flowBunch object.
#'
#' @export

fb_bn_subset_references <- function(
    my_fb
) {
  ref_idx <- my_fb@pheno$sample_is_ref == "Y"
  ref_idx[is.na(ref_idx)] <- FALSE
  fba <- my_fb
  fba@pheno <- my_fb@pheno[ref_idx,]
  fba
}
