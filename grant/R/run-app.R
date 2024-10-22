#' @title The grant explore app
#' @description
#' The app is aimed to provide users an initial exploration about the possible grants they could apply by
#' filtering different categories, years and eligibility. They could also set the award dollar range to
#' meet their funding expectations.

#' @examples
#' \dontrun{
#' run_grant_app()
#' }

#' @export
#'
run_grant_app <- function() {
  app_dir <- system.file("grant-explore", package = "grant")
  shiny::runApp(app_dir, display.mode = "normal")

}
