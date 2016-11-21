
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(dplyr)

source('global.R')

shinyServer(function(input, output) {

  df <- reactive({
    x <- census %>%
      filter(grepl(toupper(input$name),
                   name)) %>% 
      filter(grepl(as.character(input$permid),
                   as.character(permid),
                   fixed = TRUE)) %>%
      dplyr::select(permid, 
                    name, 
                    dob,
                    sex,
                    geo)
    
    x
  })
  output$text1 <-
    renderText({
      paste0(nrow(df()), ' possible matches found.')
    })
  
  output$table1 <-
    DT::renderDataTable({
      if(is.null(input$name)){
        return(data_frame(' ' = 'Input a name'))
      } else {
        return(
          df()
        )
      }
      
    })

})
