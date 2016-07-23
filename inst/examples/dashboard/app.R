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
  slider("Sepal.Width", step = 0.1),
  slider("Sepal.Length", step = 0.1)
)

# Function to generate the plot we want.
#
# In general it doesn't hurt to add the ... to the signature since it allows
# calls with more arguments to go through, which is a good thing given all the
# manual call construction happening behind the scenes.
plot_component <- xbc_plot(function(data, ...){
  ggplot(data) + aes(Sepal.Width, Sepal.Length, colour = Species) + geom_point()
})

# Function to generate the datatable.
dt_component <- xbc_datatable(
  function(data, ...) DT::datatable(data, style = "bootstrap"))

# Putting them together and creating an "exhibit"
plot_exhibit <- exhibitr(plot = plot_component, data = dt_component)

# Getting a shiny module from that exhibit. This step could probably be elided
# in the future. [IN PROGRESS: NEW API]
#
## plot_module <- xb_tabs_module(plot_exhibit, elems = c("plot", "data"), titles = c("My Plot", "Data"))

ui <- dashboardPage(
  dashboardHeader(title = "Exhibitr"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        df_module$ui("myfilter"),
        width = 3
      ),

      ## This is a good candidate for some helper functions. Turns out that we
      ## can just map over tagLists on the UI side, which is convenient since it
      ## means we won't need to define separate UI functions for every variant
      ## (e.g., tabs); instead, these can be added afterwards.

      ## Probably don't want to make the user jump through the call construction
      ## hoops here though; better to have a call like

      ## wrap_tags(xb_ui, titles =  ...)

      local({
        tagz <- xb_ui(plot_exhibit, "myplot")
        tabargs <- list(title = c("Plot", "Data") ) %>%
          c(list(tagz)) %>% purrr::transpose()
        invoke_map(tabPanel, tabargs) %>%
          c(width = 9) %>%
          (lift_dl(tabBox))
      })

      ## plot_module$ui("myplot", .outer = partial(tabBox, width = 9))
    ),

    ## The convenience is a little clearer without the list manipulation. This
    ## just inserts the tagList without mapping over it and puts it in a box.

    fluidRow(
      box(
        xb_ui(plot_exhibit, "myplot2"),
        width = 12, title = "Flat untabbed layout",
        solidHeader = TRUE)
    )

  )
)

server <- function(input, output, session){

  ## Should probably change the filter API to match the exhibit API...

  filtdata <- callModule(df_module$server, "myfilter")

  ## Code supporting the commented calls is still in the library, but will
  ## probably be removed. Previously the thinking was to generate an
  ## intermediary Shiny module object, but this ends up just feeling like an
  ## extra step we don't need. Instead provide an xb_module function that
  ## mirrors the built-in callModule.

  ## callModule(plot_module$server, "myplot", data = filtdata)
  ## callModule(xb_server_func(plot_exhibit), "myplot", data = filtdata)

  xb_module(plot_exhibit, "myplot", data = filtdata, elems = c("plot", "data"))
  xb_module(plot_exhibit, "myplot2", data = filtdata, elems = c("plot", "data"))
}

shinyApp(ui = ui, server = server)
