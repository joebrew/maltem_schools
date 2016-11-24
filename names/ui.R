
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(leaflet)

shinyUI(fluidPage(

  # Application title
  titlePanel("Buscador de IDs"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("name",
                "Name"),
      textInput("name2",
                "Name 2"),
      textInput('dob',
                'dob'),
      textInput("permid",
                "permid"),
      helpText('You can also type in a permid, or part of a permid.')),

    # Show a plot of the generated distribution
    mainPanel(
      h1(textOutput('text2')),
      textOutput('text1'),
      DT::dataTableOutput('table1'),
      checkboxInput('map',
                    'show map?',
                    value = FALSE),
      leafletOutput('map')
    )
  )
))
