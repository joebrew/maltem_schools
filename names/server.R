
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(dplyr)
library(leaflet)

source('global.R')

shinyServer(function(input, output) {

  df <- reactive({
    x <- census %>%
      filter(grepl(toupper(input$name),
                   name)) %>% 
      filter(grepl(toupper(input$name2),
                   name)) %>%
      filter(grepl(as.character(input$permid),
                   as.character(permid),
                   fixed = TRUE)) %>%
      filter(grepl(as.character(input$dob),
                   as.character(format(dob, '%d-%m-%y')),
                   fixed = TRUE)) %>%
      dplyr::select(permid, 
                    name, 
                    dob,
                    sex,
                    geo) %>%
      mutate(dob = format(dob, '%d-%m-%y'))
    
    x
  })
  output$text1 <-
    renderText({
      paste0(nrow(df()), ' possible matches found.')
    })
  
  output$text2 <- 
    renderText({
      if(nrow(df()) == 1){
        paste0(df()$permid)
      }
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
  
  output$map1 <- 
    renderLeaflet({
      if(input$map){
        x <- df()
        leaflet(data = x) %>%
          addTiles() %>%
          addMarkers(lng = ~x,
                     lat = ~y,
                     popup = paste0(x$permid, ' ', x$name))
        
      }
    })

})
