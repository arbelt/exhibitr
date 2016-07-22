library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(exhibitr)
library(DT)
p_load(shinydashboard, purrr)

data(iris)

# Create the data filter module. This just outputs a tagList, so you'll probably
# want to stick it in a panel to make it look nicer.
df_module <- shinyDataFilter(
  iris,
  selectize("Species", multiple = TRUE, options = list(plugins = list("remove_button"))),
  slider("Sepal.Width"))

# Function to generate the plot we want.
plot_component <- xbc_plot(function(data){
  ggplot(data) + aes(Sepal.Width, Sepal.Length, colour = Species) + geom_point()
})

# Function to generate the datatable.
dt_component <- xbc_datatable(function(data) DT::datatable(data, style = "bootstrap"))

# Putting them together and creating an "exhibit"
plot_exhibit <- exhibitr(gens = list(plot = plot_component, data = dt_component))

# Getting a shiny module from that exhibit. This step could probably be elided
# in the future.
plot_module <- xb_tabs_module(plot_exhibit, elems = c("plot", "data"), titles = c("My Plot", "Data"))

ui <- dashboardPage(
  dashboardHeader(title = "Exhibitr"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        df_module$ui("myfilter"),
        width = 3),
      plot_module$ui("myplot", .outer = partial(tabBox, width = 9))
      )
  )
)

server <- function(input, output, session){
  filtdata <- callModule(df_module$server, "myfilter")
  callModule(plot_module$server, "myplot", data = filtdata)
}

shinyApp(ui = ui, server = server)
