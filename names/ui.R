
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyUI(fluidPage(

  # Application title
  titlePanel("Buscador de IDs"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("name",
                "Name"),
      helpText('Type in a name, or part of a name. Capitalization does not matter. The "|" symbol means "or" in your search.'),
      textInput("permid",
                "permid"),
      helpText('You can also type in a permid, or part of a permid.')),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput('text1'),
      DT::dataTableOutput('table1')
    )
  )
))
