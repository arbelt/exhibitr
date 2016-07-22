#' An S4 class for Exhibit objects.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr detect every is_function
#' @slot title Title for exhibit
#' @slot subtitle Subtitle for exhibit
#' @slot caption Caption for exhibit
#' @slot gens List of exhibit generators. Each generator must be a function that
#'   returns an exhibit component (e.g., a plot).
#' @name exhibitr-class
exhibitr <- setClass("exhibitr",
         slots = list(title = "character",
                      subtitle = "character",
                      caption = "character",
                      gens = "list"),
         validity = function(object){
           if (!(object@gens %>% every(is_function))){
             return("Not all generators are functions")
           }
         })

#' @rdname exhibitr-class
setMethod("initialize",
          signature = c("exhibitr"),
          definition = function(.Object, ...,
                                title = character(0),
                                subtitle = character(0),
                                caption = character(0),
                                gens = list()){
            .Object@title <- title
            .Object@subtitle <- subtitle
            .Object@caption <- caption
            .Object@gens <- c(list(...), gens)
            invisible(.Object)
          })

#' Constructor wrapper for exhibitr
#'
#' @export
#' @name exhibitr
#' @rdname exhibitr-class
exhibitr

#' List exhibit components
#'
#' @export
#' @param object Object to list generators of
setGeneric("xb_gens",
           def = function(object) standardGeneric("xb_gens"))

#' @rdname xb_gens
setMethod("xb_gens",
          signature = "exhibitr",
          definition = function(object) names(object@gens))

#' @rdname exhibitr-class
setMethod("[[",
          signature = c(x = "exhibitr", i = "ANY", j = "ANY" ),
          definition = function(x, i, j = NULL, ...){
            rez <- x@gens[[i]]
            if (!is.null(rez)) return(rez)
            if (i == "plot"){
              rez <- x@gens %>% detect(~ inherits(.x, "xbc_highchart"))
              if (!is.null(rez)) return(rez)
              rez <- x@gens %>% detect(~ inherits(.x, "xbc_plot"))
              if (!is.null(rez)) return(rez)
            }
          })

#' @rdname exhibitr-class
setMethod("[",
          signature = c(x = "exhibitr", i = "ANY", j = "ANY"),
          definition = function(x, i, j = NULL){
            x@gens[i]
          })

#' @rdname exhibitr-class
setMethod("$",
          signature = c("exhibitr"),
          definition = function(x, name){
            x[[as.character(name)]]
          })

#' Class definition for exhibit component generator
#'
#' @export
xbc_gen <- setClass(
  "xbc_gen",
  slots = list(
    shinyOutput = "function",
    shinyRenderer = "function"
  ),
  contains = "function"
)

#' Contructor function for xbc_gen
#'
#' @name xbc_gen
#' @rdname xbc_gen-class
#' @export
xbc_gen

#' S4 Class for plot component generator
#'
#' @export
xbc_plot <- setClass(
  "xbc_plot",
  prototype = prototype(
    shinyOutput = shiny::plotOutput,
    shinyRenderer = shiny::renderPlot
  ),
  contains = "xbc_gen"
)

#' Constructor for xbc_plot
#'
#' @export
#' @rdname xbc_plot-class
#' @name xbc_plot
xbc_plot

#' S4 Class for datatable component generator
#'
#' @export
xbc_datatable <- setClass(
  "xbc_datatable",
  prototype = prototype(
    shinyOutput = DT::dataTableOutput,
    shinyRenderer = DT::renderDataTable
  ),
  contains = c("xbc_gen")
)

#' Constructor for xbc_datatable
#'
#' @export
#' @rdname xbc_datatable-class
#' @name xbc_datatable
xbc_datatable

#' S4 class for highchart component generator
#'
#' @export
xbc_highchart <- setClass(
  "xbc_highchart",
  prototype = prototype(
    shinyOutput = highcharter::highchartOutput,
    shinyRenderer = highcharter::renderHighchart
  ),
  contains = c("xbc_gen")
)

#' Constructor for xbc_highchart
#'
#' @rdname xbc_highchart-class
#' @name xbc_highchart
xbc_highchart

setGeneric("create_ui",
           def = function(object, id, ...) standardGeneric("create_ui"))

setMethod("create_ui",
          signature = c("xbc_gen"),
          definition = function(object, id, ...){
            object@shinyOutput(id, ...)
          })

setGeneric("xb_run",
           def = function(object, ...) standardGeneric("xb_run"))

#' @importFrom purrr map_if
setMethod("xb_run",
          signature("xbc_gen"),
          definition = function(object, ...){
            object@shinyRenderer({
              argz <- list(...) %>% map_if(is.reactive, ~ .x())
              invoke(object, argz)
            })
          })


#' Generate shiny module from exhibit components or objects
#'
#' @export
setGeneric("xb_shiny_module",
           def = function(object, ...) standardGeneric("xb_shiny_module"))

#' @rdname xb_shiny_module
setMethod("xb_shiny_module",
          signature = "xbc_gen",
          definition = function(object, ...){

            .dots <- list(...)

            ui <- function(id, ...){
              ns <- NS(id)
              object@shinyOutput(ns("xbcgen"))
            }

            server <- function(input, output, session, ...){
              output$xbcgen <- xb_run(object, ...)
            }

            list(ui = ui, server = server)
          })

#' @rdname xb_shiny_module
setMethod("xb_shiny_module",
          signature = "NULL",
          definition = function(object, ...){
            list(
              ui = function(...) NULL,
              server = function(...) NULL
            )
          })

#' @rdname xb_shiny_module
#' @importFrom purrr lift_dl discard map map2 lift
#' @param .inner List of functions to be applied to the component outputs.
setMethod("xb_shiny_module",
          signature = "exhibitr",
          definition = function(object, ..., elems = "plot"){
            xbc_list <- object[elems]
            xbc_modules <- xbc_list %>% map(xb_shiny_module)

            ui <- function(id, ..., .inner = identity, .outer = tagList){
              ns <- NS(id)
              ids <- elems %>% map(ns)
              ui_funcs <- xbc_modules %>% map("ui")
              tagz <- map2(ui_funcs, ids, invoke)
              lift(.outer)(tagz)
            }

            server <- function(input, output, session, ...){
              .dots = list(...)
              .f <- function(module, elem){
                cm_args <- c(module$server, elem, .dots)
                invoke(callModule, cm_args)
              }
              map2(xbc_modules, elems, .f)
            }

            list(ui = ui, server = server)
          })

#' Create shiny module with tabs.
#'
#' @export
setGeneric("xb_tabs_module",
           def = function(object, elems = "plot", ...) standardGeneric("xb_tabs_module"))

#' @rdname xb_tabs_module
setMethod("xb_tabs_module",
          signature = "exhibitr",
          definition = function(object, elems = "plot", ..., titles = NULL){

            .dots <- list(...)

            xbc_list <- object[elems]
            xbc_modules <- xbc_list %>% map(xb_shiny_module)

            tabtitles <- titles %||% elems

            ui <- function(id, ..., .outer = tabsetPanel){
              ns <- NS(id)
              ids <- elems %>% map(ns)
              ui_funcs <- xbc_modules %>% map("ui")
              tagz <- map2(ui_funcs, ids, invoke)
              tabargs <- tabtitles %>% map(~ list(title = .x)) %>%
                list(tagz) %>% purrr::transpose()
              tabz <- tabargs %>% map(lift_dl(tabPanel))
              ts_args <- c(tabz, list(...))
              lift_dl(.outer)(ts_args)
            }

            server <- function(input, output, session, ...){
              .dots = list(...)
              .f <- function(module, elem){
                cm_args <- c(module$server, elem, .dots)
                invoke(callModule, cm_args)
              }
              map2(xbc_modules, elems, .f)
            }

            list(ui = ui, server = server)
          })


setGeneric("xb_server_func",
           def = function(object, ..., elems = NULL) standardGeneric("xb_server_func"))

setMethod("xb_server_func",
          signature = "xbc_gen",
          definition = function(object, ...){
            function(input, output, session, ...){
              output$xbcgen <- xb_run(object, ...)
            }
          })

setMethod("xb_server_func",
          signature = "exhibitr",
          definition = function(object, ..., elems = NULL){
            elems <- elems %||% xb_gens(object)
            server <- function(input, output, session, ...){
              .dots <- list(...)
              .f <- function(elem){
                cm_args <- c(xb_server_func(object[[elem]]), elem, .dots)
                invoke(callModule, cm_args)
              }
              map(elems, .f)
            }
            server
          })

xb_server_func.default <- function(object, ..., elems = NULL){
  object$server
}
