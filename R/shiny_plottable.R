#' Create plottable object from exhibit
#'
#' @return plottable List of 2 components, \code{ui} and \code{server}
#' @import shiny
#' @importFrom purrr %||%
#' @importFrom purrr invoke map
#' @export
shiny_plottable <- function(exhibit, data = NULL){
  plotterfn <- exhibit$ggplot %||% function(data, ...) NULL

  ui <- function(id){
    ns <- NS(id)
    tagList(
      plotOutput(ns("ggplot")),
      DT::dataTableOutput(ns("datatable"))
    )
  }

  .data <- data

  server <- function(input, output, session, data = .data){
    output$ggplot <- renderPlot({
      data_ <- if (is.reactive(data)) data() else data
      p <- invoke(plotterfn, list(data = data_))
      p
    })
    output$datatable <- DT::renderDataTable({
      data_ <- if (is.reactive(data)) data() else data
      DT::datatable(data_)
    })
  }

  list(ui = ui, server = server)
}
