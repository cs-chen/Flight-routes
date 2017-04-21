#Final Project

library(shiny)
library(readr)
library(leaflet)
library(dplyr)
library(countrycode)




shinyUI(navbarPage(
  
  # Application title
  "Flight Routes", 
  
  tabPanel("Routes", 
  
    sidebarPanel(
       selectInput("continent", "Choose the departure continent:",
                   choices = c("Africa", "Americas", "Asia", "Europe", "Oceania")),
       selectInput("country", "Departure country", ""),
       hr(),
       selectInput("continent2", "Choose the arrival continent:", ""),
       selectInput("country2", "Arrival country", ""),
       actionButton("action", "Show Plot")
       ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", leafletOutput("plot")),
        tabPanel("Stats", plotOutput("bar")),
        tabPanel("Summary Table", tableOutput("table")))
    )
  ),
  tabPanel("Airports",
    
      sidebarPanel(
        selectInput("cat", "Choose the category:",
                    c("Domestic", "International")),
        selectInput("type", "Choose the type of flights:", c("Arrival", "Departure")),
        actionButton("action2", "Show Plot")
      )
    ,
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("gg")),
        tabPanel("Summary Table", tableOutput("tab"))
      )
    )
           ),
  tabPanel("Airlines",
           
           sidebarPanel(
             selectInput("cat2", "Choose the category:",
                         c("Domestic", "International")),
             actionButton("action3", "Show Plot")
           )
           ,
           mainPanel(
             tabsetPanel(
               tabPanel("Plot", plotOutput("gg2")),
               tabPanel("Summary Table", tableOutput("tab2"))
             )
           )
  )
))
