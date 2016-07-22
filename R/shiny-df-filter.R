#' Create Shiny component for filtering data
#'
#' @importFrom purrr keep map simplify partial
#' @param data Dataframe
#' @export
shinyDataFilter <- function(data, ...){

  qdots <- eval(substitute(alist(...)))

  filtenv <- new.env()
  filtenv$varnames <- qdots %>% keep(is.name) %>% map(as.character) %>%
    simplify()
  filtenv$slidervars <- character(0)
  lzd <- lazy_dots(...)
  filtenv$data <- data

  ui <- function(id, ...){
    ns <- NS(id)

    funs_list <- list(
      selectize = function(varname, ..., id = varname, label = varname,
                            choices = NULL){
        filtenv$varnames <- c(filtenv$varnames, varname)
        if (is.null(choices)) {
          choices <- filtenv$data[[varname]] %>% unique %>% {if (is.factor(.)) levels(.) else .}
        }
        selectizeInput(inputId = ns(id), label = label, choices = choices, ...)
      },
      slider = function(varname, ..., id = varname, label = varname){
        filtenv$slidervars <- c(filtenv$slidervars, varname)
        datarange <- list(min = min(filtenv$data[[varname]], na.rm = TRUE),
                          max = max(filtenv$data[[varname]], na.rm = TRUE))
        sldr_args <- list(
          inputId = ns(id),
          label = label,
          min = datarange$min,
          max = datarange$max,
          value = c(datarange$min, datarange$max)
        ) %>% { invoke(update_list, list(., ...)) }
        invoke(sliderInput, sldr_args)
      }
    )

    ui_elems <- lazy_eval(lzd, c(funs_list, filtenv))

    invoke(tagList, ui_elems)
  }

  server <- function(input, output, session, ...){
    ret <- reactive({
      subs_args <- list()
      for (n in filtenv$varnames){
        subs_args[[n]] <- input[[n]]
      }
      for (n in filtenv$slidervars){
        subs_args[[n]] <- new(
          "NumericRangeSubset",
          min = min(input[[n]]),
          max = max(input[[n]])
        )
      }
      invoke(partial(subset2_, data = filtenv$data), subs_args %>% map_if(is.reactive, ~ .x()))
    })
    ret
  }

  list(ui = ui, server = server)

}

#' @importFrom lazyeval lazy_eval lazy_dots interp
#' @importFrom pryr named_dots
subset2_ <- function(data, ...){
  ret <- data
  mydots <- pryr::named_dots(...)
  for (i in seq_along(mydots)){
    idx <- run_subset(mydots[[i]], ret[[names(mydots)[i]]])
    ret <- ret[idx,]
  }
  ret
}


SubsetExpr <- setClass("SubsetExpr")

NumericRangeSubset <- setClass(
  "NumericRangeSubset",
  slots = list(min = "numeric", max = "numeric"),
  contains = "SubsetExpr")

MemberOfSubset <- setClass(
  "MemberOfSubset",
  slots = list(members = "vector"),
  contains = "SubsetExpr"
)

setGeneric(
  name = "run_subset",
  def = function(object, data){ standardGeneric("run_subset") }
)

setMethod(
  f = "run_subset",
  signature = c(object = "MemberOfSubset", data = "vector"),
  definition = function(object, data){
    data %in% object@members
  }
)

setMethod(
  f = "run_subset",
  signature = c(object = "NumericRangeSubset", data = "vector"),
  definition = function(object, data){
    data >= object@min & data <= object@max
  }
)

setMethod(
  f = "run_subset",
  signature = c(object = "vector", data = "vector"),
  definition = function(object, data){
    data %in% object
  }
)
