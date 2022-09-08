#' @title cytoBatchNormGUI
#'
#' @description Run the Graphical User Interface.
#'
#' @param roots string, paths to use for loading FCS files and storing the
#'   resulting FCS files.
#' @param opts list, named values to tune the app.
#' @param ... parameters passed to fb_bn_set_params.
#'
#' @importFrom shiny shinyApp runApp
#' @export

cytoBatchNormGUI <- function(
    roots = c(data = getwd()),
    opts = list(review = TRUE, debug = FALSE),
    ...
) {
  # check
  if (any(sapply(roots, dir.exists) == FALSE)) {
    stop("roots contains one unknown directory.")
  }
  # prepare environment and start app
  source(system.file('shiny/globals.R', package = "cytoBatchNorm"))
  shiny_env <- new.env()
  assign('roots', roots, shiny_env)
  assign('debug', FALSE, shiny_env)
  lapply(names(opts), function(x) assign(x, opts[[x]], shiny_env))
  source(system.file('shiny/ui.R', package = "cytoBatchNorm"))
  source(system.file('shiny/server.R', package = "cytoBatchNorm"))
  environment(ui) <- shiny_env
  environment(server) <- shiny_env
  app <- shinyApp(ui = ui, server = server)
  runApp(app, ...)
}
