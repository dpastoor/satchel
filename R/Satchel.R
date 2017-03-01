#' Carry around your data
#'
#'@details
#' methods:
#'  * save(data, data_name) - save data
#'  * use(data_name) - use data saved from a different location
#'  * report() - show information about all data saved in current session
#'  * details() - list all data in the satchel cache directory
#'  * peek(data_dir, data_name) - shows the (approximate) head of a dataset from the output metadata
#' @examples \dontrun{
#' # create a new satchel stored as namespace f1 in the dir data/derived
#' satchel <- Satchel$new("f1", "../data/derived/satchel")
#' satchel$save(Theoph)
#' # new file at ../data/derived/satchel/f1/Theoph.rds
#'
#' # we can also give the files other names
#' satchel$save(Theoph, data_name = "other")
#' satchel$save(Theoph, data_name = "another")
#'
#' # to see all objects saved during the session can check the report
#' satchel$report(details = T)
#'
#' # can see data from any satchel dir by checking what is available
#' satchel$available()
#'
#' # lets say in another file I had saved a file nca_summaries in an nca_analysis namespace
#' # satchel is smart enough to scan all folders for that file
#' nca_summaries <- satchel$use("nca_summaries")
#'
#' # if it finds a conflict, you must explicitly specify which namespace it was saved under
#' nca_summaries <- satchel$use("nca_summaries", "nca_analysis")
#'
#'
#' }
#' @importFrom R6 R6Class
#' @importFrom jsonlite serializeJSON unserializeJSON toJSON fromJSON
#' @name Satchel
NULL

#' @export
Satchel <- R6::R6Class("Satchel",
                    public =
                        list(
                            verbose = NULL,
                            track = NULL,
                            refresh = NULL,
                            initialize = function(
                                cache_name,
                                dir = NULL,
                                track = FALSE,
                                verbose = TRUE,
                                strict = TRUE,
                                refresh = TRUE
                            ) {
                                self$verbose <<- verbose
                                self$track <<- track
                                self$refresh <<- refresh
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
                                size_mb <- tryCatch({
                                        # try to use pryr if possible, however some types like ggplot
                                        # do not work so can fall back to object.size if this errors
                                        as.numeric(pryr::object_size(data))/1000000
                                    },
                                    error = function(e) {
                                        as.numeric(object.size(data))/1000000
                                    })
                                info <- tibble::data_frame(
                                    name = data_name,
                                    classes = paste0(class(data), collapse = ", "),
                                    size_mb = size_mb,
                                    mem_address = pryr::address(data),
                                    type = "object"
                                )
                                private$data[[data_name]] <<- info
                                if (metadata) {
                                    # don't need memory address as won't convey any additional information
                                    info$mem_address <- NULL
                                    data_classes <- c(
                                        "tbl_df",
                                        "data.frame",
                                        "matrix"
                                    )
                                    is_likely_data <- any(class(data) %in% data_classes) || is.vector(data)

                                    if (is_likely_data) {
                                        output <- tryCatch({
                                            jsonlite::toJSON(list("info" = info,
                                                              "json_preview" = head(data),
                                                              "r_preview" = serializeJSON(head(data)),
                                                              "time" = Sys.time()),
                                                              pretty = T
                                                         )
                                        }, error = function(e) {
                                            jsonlite::toJSON(list("info" = info,
                                                              "json_preview" = e$message,
                                                              "r_preview" = serializeJSON(e$message),
                                                              "time" = Sys.time()),
                                                              pretty = T
                                                         )
                                        })
                                    } else {
                                        output <- tryCatch({
                                            jsonlite::toJSON(list("info" = info,
                                                              "json_preview" = "data type likely unsuitable for preview",
                                                              "r_preview" = serializeJSON("data type likely unsuitable for preview"),
                                                              "time" = Sys.time()),
                                                              pretty = T
                                                         )
                                        }, error = function(e) {
                                            jsonlite::toJSON(list("info" = info,
                                                              "json_preview" = e$message,
                                                              "r_preview" = serializeJSON(e$message),
                                                              "time" = Sys.time()),
                                                              pretty = T
                                                         )
                                        })
                                    }
                                    writeLines(output,
                                             file.path(private$cache_location, paste0(data_name, "_meta.json"))
                                    )
                                }

                            },
                             save_from_file = function(path, .f, ..., data_name = NULL) {
                                # check path exists
                                fullpath <- normalizePath(path)
                                if (!file.exists(fullpath)) {
                                    stop(paste("no file detected at:", fullpath))
                                }
                                if (is.null(data_name)) {
                                    data_name <- tools::file_path_sans_ext(basename(path))
                                }
                                rds_name <- file.path(private$cache_location, paste0(data_name, ".rds"))
                                meta_filepath <- file.path(private$cache_location, paste0(data_name, "_meta.json"))
                                file_info <- file.info(fullpath)

                                if (file.exists(meta_filepath)) {
                                    old_meta <- jsonlite::read_json(meta_filepath)
                                    # unfortunately due to the way the list is converted back from json
                                    # the info and fileinfo arrays are converted to be wrapped with a list
                                    # of length 1, so need to also reference into the list each time
                                    old_file_info <- old_meta$info[[1]]$fileinfo[[1]]
                                    if (old_file_info$mtime == file_info$mtime) {
                                        message("file already in cache")
                                        return(FALSE)
                                    }
                                }

                                data <- tryCatch({
                                    .f(path, ...)
                                }, error = function(e) {
                                    stop(paste("error reading data:", e))
                                })
                                saveRDS(data, file.path(private$cache_location, paste0(data_name, ".rds")))
                                size_mb <- tryCatch({
                                        # try to use pryr if possible, however some types like ggplot
                                        # do not work so can fall back to object.size if this errors
                                        as.numeric(pryr::object_size(data))/1000000
                                    },
                                    error = function(e) {
                                        as.numeric(object.size(data))/1000000
                                    })
                                info <- tibble::data_frame(
                                    name = data_name,
                                    classes = paste0(class(data), collapse = ", "),
                                    size_mb = size_mb,
                                    type = "file",
                                    fileinfo = list(fileinfo= file.info(fullpath)[c("mtime", "ctime", "size")])
                                )
                                private$data[[data_name]] <<- info
                                    # don't need memory address as won't convey any additional information
                                    data_classes <- c(
                                        "tbl_df",
                                        "data.frame",
                                        "matrix"
                                    )
                                    is_likely_data <- any(class(data) %in% data_classes) || is.vector(data)

                                    if (is_likely_data) {
                                        output <- tryCatch({
                                            jsonlite::toJSON(list("info" = info,
                                                              "json_preview" = head(data),
                                                              "r_preview" = serializeJSON(head(data)),
                                                              "time" = Sys.time()),
                                                              pretty = T
                                                         )
                                        }, error = function(e) {
                                            jsonlite::toJSON(list("info" = info,
                                                              "json_preview" = e$message,
                                                              "r_preview" = serializeJSON(e$message),
                                                              "time" = Sys.time()),
                                                              pretty = T
                                                         )
                                        })

                                    writeLines(output,
                                             file.path(private$cache_location, paste0(data_name, "_meta.json"))
                                    )
                                    }
                                    return(TRUE)

                            },
                            load_from_file = function(path, .f, ..., data_name = NULL) {
                                # check path exists
                                fullpath <- normalizePath(path)
                                if (!file.exists(fullpath)) {
                                    stop(paste("no file detected at:", fullpath))
                                }
                                if (is.null(data_name)) {
                                    data_name <- tools::file_path_sans_ext(basename(path))
                                }
                                rds_name <- file.path(private$cache_location, paste0(data_name, ".rds"))
                                meta_filepath <- file.path(private$cache_location, paste0(data_name, "_meta.json"))
                                file_info <- file.info(fullpath)

                                if (file.exists(meta_filepath)) {
                                    old_meta <- jsonlite::read_json(meta_filepath)
                                    # unfortunately due to the way the list is converted back from json
                                    # the info and fileinfo arrays are converted to be wrapped with a list
                                    # of length 1, so need to also reference into the list each time
                                    old_file_info <- old_meta$info[[1]]$fileinfo[[1]]
                                    if (old_file_info$mtime == file_info$mtime) {
                                        message("file already in cache, loading from cache!")
                                        return(readRDS(rds_name))
                                    }
                                }

                                data <- tryCatch({
                                    .f(path, ...)
                                }, error = function(e) {
                                    stop(paste("error reading data:", e))
                                })
                                saveRDS(data, file.path(private$cache_location, paste0(data_name, ".rds")))
                                size_mb <- tryCatch({
                                        # try to use pryr if possible, however some types like ggplot
                                        # do not work so can fall back to object.size if this errors
                                        as.numeric(pryr::object_size(data))/1000000
                                    },
                                    error = function(e) {
                                        as.numeric(object.size(data))/1000000
                                    })
                                info <- tibble::data_frame(
                                    name = data_name,
                                    classes = paste0(class(data), collapse = ", "),
                                    size_mb = size_mb,
                                    type = "file",
                                    fileinfo = list(fileinfo= file.info(fullpath)[c("mtime", "ctime", "size")])
                                )
                                private$data[[data_name]] <<- info
                                    # don't need memory address as won't convey any additional information
                                    data_classes <- c(
                                        "tbl_df",
                                        "data.frame",
                                        "matrix"
                                    )
                                    is_likely_data <- any(class(data) %in% data_classes) || is.vector(data)

                                    if (is_likely_data) {
                                        output <- tryCatch({
                                            jsonlite::toJSON(list("info" = info,
                                                              "json_preview" = head(data),
                                                              "r_preview" = serializeJSON(head(data)),
                                                              "time" = Sys.time()),
                                                              pretty = T
                                                         )
                                        }, error = function(e) {
                                            jsonlite::toJSON(list("info" = info,
                                                              "json_preview" = e$message,
                                                              "r_preview" = serializeJSON(e$message),
                                                              "time" = Sys.time()),
                                                              pretty = T
                                                         )
                                        })

                                    writeLines(output,
                                             file.path(private$cache_location, paste0(data_name, "_meta.json"))
                                    )
                                    }
                                    return(data)

                            },
                            use = function(data_name, from = NULL) {
                                if (self$refresh) {
                                    self$available()
                                }
                                if (is.numeric(data_name)) {
                                    warning("be careful referencing models by index as changes could result in subtle bugs,
                                            suggest referring to datasets by name")
                                }
                                if (!is.null(from)) {
                                    # check if from exists as will error otherwise
                                    if (!from %in% names(private$references)) {
                                        stop("no `from` location detected in available data locations")
                                    }
                                }

                                if(is.null(from)) {
                                    references <- private$references
                                } else {
                                    references <- private$references[[from]]
                                }

                                all_objects <- lapply(references, function(.n) {
                                    gsub("\\.rds", "", basename(.n))
                                })
                                obj_matches <- which(data_name == unlist(all_objects))
                                if (!length(obj_matches)) {
                                    stop("could not find any matching objects")
                                }
                                if(length(obj_matches) > 1 && is.null(from)) {
                                    stop("multiple matches found, please specify where the data was specified as well")
                                }
                                data <- readRDS(unlist(references)[obj_matches])
                                return(data)
                                },
                            report = function(details = TRUE) {
                                if (details) {
                                    lapply(private$data, dplyr::glimpse)
                                } else {
                                    lapply(names(private$data), print)
                                }
                                return(invisible())

                            },
                            available = function() {
                                dirs <- list.dirs(private$dir, recursive = F)
                                # drop system folders that start with . and normalize paths
                                dirs <- unlist(lapply(dirs, function(.d) {
                                    if (grepl( "^\\.", basename(.d))) {
                                        return(NULL)
                                    }
                                    return(normalizePath(.d))
                                }))
                                references <- lapply(dirs, function(.d) {
                                    normalizePath(file.path(.d, list.files(.d, pattern = "*.rds")))
                                })
                                private$references <<- setNames(references, basename(dirs))
                                return(lapply(private$references, function(.n) {
                                    gsub("\\.rds", "", basename(.n))
                                }))
                            },
                            auto_refresh = function(.refresh) {
                                if(!missing(.refresh)) {
                                    self$refresh <<- .refresh
                                }
                                return(self$refresh)
                            },
                            preview = function(data_name, from = NULL) {
                                ## this is currently copied and pasted from use() this should be refactored!
                                if (self$refresh) {
                                    self$available()
                                }
                                if (is.numeric(data_name)) {
                                    warning("be careful referencing models by index as changes could result in subtle bugs,
                                            suggest referring to datasets by name")
                                }
                                if (!is.null(from)) {
                                    # check if from exists as will error otherwise
                                    if (!from %in% names(private$references)) {
                                        stop("no `from` location detected in available data locations")
                                    }
                                }

                                if(is.null(from)) {
                                    references <- private$references
                                } else {
                                    references <- private$references[[from]]
                                }

                                all_objects <- lapply(references, function(.n) {
                                    gsub("\\.rds", "", basename(.n))
                                })
                                obj_matches <- which(data_name == unlist(all_objects))
                                if (!length(obj_matches)) {
                                    stop("could not find any matching objects")
                                }
                                if(length(obj_matches) > 1 && is.null(from)) {
                                    stop("multiple matches found, please specify where the data was specified as well")
                                }
                                meta_data_file <- gsub("\\.rds", "_meta.json", unlist(references)[obj_matches])
                                if (!file.exists(meta_data_file)) {
                                    warning("could not find a metadata file at: ", meta_data_file)
                                    return(FALSE)
                                }
                                data <- fromJSON(readr::read_file(meta_data_file))
                                return(unserializeJSON(data$r_preview))
                            }
                        ),
                    private = list(
                        dir = NULL,
                        cache_location = NULL,
                        data = list(),
                        references = list()
                    )
)
