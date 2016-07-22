library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(exhibitr)
library(DT)

data(iris)

df_module <- shinyDataFilter(iris, selectize("Species", multiple = TRUE,
                                             options = list(plugins = list("remove_button"))),
                             slider("Sepal.Length"))

plot_component <- xbc_plot(function(data){
  ggplot(data) + aes(Sepal.Width, Sepal.Length, colour = Species) + geom_point()
})

dt_component <- xbc_datatable(function(data) DT::datatable(data, style = "bootstrap"))

plot_exhibit <- exhibitr(gens = list(plot = plot_component, data = dt_component))

plot_module <- xb_tabs_module(plot_exhibit, elems = c("plot", "data"), titles = c("My Plot", "Data"))

ui <- fluidPage(
  titlePanel(tags$h1("Hello")),
  fluidRow(
    column(
      3,
      wellPanel(
        df_module$ui("myfilter")
      )
    ),
    column(
      9,
      plot_module$ui("myplot")
    )
  ),
  theme = shinytheme("spacelab")
)

server <- function(input, output, session){
  filtdata <- callModule(df_module$server, "myfilter")
  callModule(plot_module$server, "myplot", data = filtdata)
}

shinyApp(ui = ui, server = server)
