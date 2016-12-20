#' Carry around your data
#' @importFrom R6 R6Class
#' @name Satchel
#' @details
#'
#' @examples
#' \dontrun{
#' }
NULL

#' @export
Satchel <- R6::R6Class("Satchel",
                    public =
                        list(
                            verbose = NULL,
                            track = NULL,
                            initialize = function(
                                cache_name,
                                dir = NULL, # where model files should be stored
                                track = FALSE,
                                verbose = TRUE,
                                strict = TRUE
                            ) {
                                self$verbose <<- verbose
                                self$track <<- track
                                ## cache folder set to same directory as the sourced script
                                if (is.null(dir)) {
                                    if (strict) {
                                        stop("strict mode on - must explicitly declare the directory to store the cache")
                                    }
                                    dir <- getwd()
                                }
                                dir <- normalizePath(dir, mustWork = FALSE)
                                if (strict) {
                                    # if they are 'manually' setting a modeling dir, make sure it exists
                                    if (!dir.exists(dir)) {
                                        stop(
                                            paste(
                                                "no directory detected at: ",
                                                dir,
                                                "please correct the path or create the folder"
                                            )
                                        )
                                    }
                                } else {
                                    if (!dir.exists(dir)) {
                                        dir.create(dir, recursive = TRUE)
                                        if (self$verbose) {
                                            message(paste("parent directory created at", dir))
                                        }
                                    }
                                }

                                private$dir <<- dir

                                # cache may not be created yet
                                cache_folder <-  normalizePath(file.path(dir, cache_name), mustWork = FALSE)
                                if (!dir.exists(cache_folder)) {
                                    dir.create(cache_folder, recursive = TRUE)
                                }

                                private$cache_location <<- cache_folder

                                if (self$verbose) {
                                    message("global data dir located at: ", private$dir)
                                    message("cache location named: ", basename(private$cache_location))
                                }
                                if (self$track) {
                                    message("automatic gitignore management not yet implemented, sorry!")
                                }
                            },
                            save = function(data, data_name = NULL, metadata = TRUE) {
                                if (is.null(data_name)) {
                                    data_name <- deparse(substitute(data))
                                }
                                if (data_name %in% names(private$data)) {
                                    # don't re-cache if exact object has already been saved
                                    # for now error on side of memory conservation and
                                    if (pryr::address(data) == private$data[[data_name]]$mem_address) {
                                        return(TRUE)
                                    }
                                }
                                saveRDS(data, file.path(private$cache_location, paste0(data_name, ".rds")))
                                info <- tibble::data_frame(
                                    name = data_name,
                                    classes = paste0(class(data), collapse = ", "),
                                    size_mb = as.numeric(pryr::object_size(data))/1000000,
                                    mem_address = pryr::address(data)
                                )
                                private$data[[data_name]] <<- info
                                if (metadata) {
                                    # don't need memory address as won't convey any additional information
                                    info$mem_address <- NULL
                                    writeLines(jsonlite::toJSON(list("info" = info,
                                                          "preview" = head(data),
                                                          "time" = Sys.time()),
                                                          pretty = T
                                                     ),
                                             file.path(private$cache_location, paste0(data_name, "_meta.json"))
                                    )
                                }

                            },
                            use = function(data_name) {
                                if (is.numeric(data_name)) {
                                    warning("be careful referencing models by index as changes could result in subtle bugs,
                                            suggest referring to datasets by name")
                                }
                                data <- TRUE # to implement fetching from tree
                                return(data)
                                },
                            report = function(details = TRUE) {
                                if (details) {
                                    lapply(private$data, dplyr::glimpse)
                                } else {
                                    lapply(names(private$data), print)
                                }
                                return(invisible())

                            }
                        ),
                    private = list(
                        dir = NULL,
                        cache_location = NULL,
                        data = list()

                    )
)
