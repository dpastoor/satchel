#' initialize a satchel directory
#' @param .dir directory
#' @details Defaults to data/derived/satchel relative
#' to working directory
#'
#' Best if run from the console after opening an rstudio project
#' @return path to satchel directory
#' @export
init_satchel_dir <- function(.dir = "data/derived/satchel") {
    if (!dir.exists(.dir)) {
        dir.create(.dir, recursive = TRUE)
        message("satchel directory created at ", normalizePath(.dir))
    }
        message("satchel directory already detected at ", normalizePath(.dir))
    return(.dir)
}
