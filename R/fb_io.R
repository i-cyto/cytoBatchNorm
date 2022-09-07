#' @title fb_file_path
#'
#' @description Give the storage path of the given flowBunch. The project dirname
#'   and basename of the flowBunch are concatenated to locate the storage directory of
#'   the flowBunch.
#'
#' @param fb flowBunch
#' @param ... string, folders to append to the path.
#'
#' @importFrom checkmate assertClass assertString assertDirectory
#' @export

fb_file_path <- function(
    fb,
    ...
) {
  assertClass(fb, "flowBunch")
  proj_name <- fb@storage$basen
  proj_dirname <- fb@storage$dirn
  assertString(proj_name)
  assertDirectory(proj_dirname)
  file.path(proj_dirname, proj_name, ...)
}


#' @title fb_file_name
#'
#' @description Build a file name from the given flowBunch within the storage
#'   folder. The format string is used to build the file name.
#'
#' @param fb flowBunch
#' @param s_format string, the format used in a sprintf. It eases to build
#'   standardized file names.
#'
#' @importFrom checkmate assertClass assertString
#' @export

fb_file_name <- function(
    fb,
    s_format = NULL
) {
  assertClass(fb, "flowBunch")
  assertString(s_format, null.ok = TRUE)
  if (is.null(s_format)) return(NULL)
  if (!grepl("%s", s_format, fixed = TRUE))
    s_format <- paste0("%s", s_format)
  proj_name <- fb@storage$basen
  fb_file_path(fb, sprintf(s_format, proj_name))
}


#' @title fb_write
#'
#' @description Write the given flowBunch as panel, pheno and RData files.
#'
#' @param fb flowBunch
#' @param write_exprs logical, write expressions; default to FALSE.
#'
#' @importFrom checkmate assertClass testNull assertLogical
#' @export

fb_write <- function(
    fb,
    write_exprs = FALSE
) {
  assertClass(fb, "flowBunch")
  assertLogical(write_exprs)
  proj_dir <- fb_file_path(fb)
  if (testNull(proj_dir))
    return(NULL)
  if (!dir.exists(proj_dir))
    dir.create(proj_dir)
  fb_write_pheno(fb)
  fb_write_panel(fb)
  if (!write_exprs)
    fb@exprs <- NULL
  save(fb, file = fb_file_name(fb, "%s-fcsBunch.RData"))
}


#' @title fb_open
#'
#' @description Create a flowBunch from one previously saved to disk. This
#'   function reads the pheno and panel files by default. Alternatively, the
#'   RData file could be used instead.
#'
#' @param project_name string, name of the project in which the flowBunch is.
#' @param project_dir string, the directory in which the project resides.
#' @param from, string, either 'phenopanel' or 'rdata'.
#'
#' @return a flowBunch object.
#'
#' @importFrom checkmate assertString assertDirectory
#' @export

fb_open <- function(
    project_name,
    project_dir,
    from = c('phenopanel', 'rdata')
) {
  assertString(project_name)
  assertDirectory(project_dir)
  from <- match.arg(from)
  # Empty init
  my_fb <- flowBunch()
  # Set storage params
  my_fb@storage$basen <- project_name
  my_fb@storage$dirn <- project_dir
  # Pheno/Panel
  if (from == 'phenopanel') {
    # Default FCS location if relative paths in pheno
    my_fb@input$dirn <- fb_file_path(my_fb)
    # TODO: improve strategy for default locations
    # TODO: check FCS exist or guess new location
    # Read from files and update
    my_fb <- fb_read_pheno(my_fb)
    my_fb <- fb_read_panel(my_fb)
    my_fb <- fb_tr_update_from_panel(my_fb)
  } else if (from == 'rdata') {
    rdata <- fb_file_name(my_fb, "%s-fcsBunch.RData")
    if (!file.exists(rdata)) {
      stop("No fcsBunch at ", dirname(rdata))
    }
    # Return fcsBunch
    tmp_env <- new.env()
    load(rdata, envir = tmp_env)
    get("fb", tmp_env)
  }
  my_fb
}


#' @title fb_reload
#'
#' @description Reload the panel and pheno files of the flowBunch.
#'
#' @param fb flowBunch
#'
#' @importFrom checkmate assertClass
#' @export

fb_reload <- function(
    fb
) {
  assertClass(fb, "flowBunch")
  fb <- fb_read_pheno(fb)
  fb <- fb_read_panel(fb)
  fb <- fb_tr_update_from_panel(fb)
  fb
}
