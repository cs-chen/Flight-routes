#Final Project

library(shiny)
library(readr)
library(dplyr)
library(geosphere)
library(leaflet)
library(sp)
library(countrycode)
library(RColorBrewer)

    
shinyServer(function(input, output, session) {   
  withProgress(message = "Loading data", value = 0, {
  #Read data
    airport <- read.table("airports.dat", sep = ",", stringsAsFactors = F)
    names(airport) <- c("airport_id", "name", "city", "country", "IATA", "ICAO", "lat", "long", "alt", "timezone", "DST", "tz", "type", "source")
    
    
    airline <- read.table("airlines.dat", sep = ",", stringsAsFactors = F)
    names(airline) <- c("airline", "name", "alias", "IATA", "ICAO", "callsign", "country", "active")
    df <- data.frame(country = names(table(airline$country)), color = colorRampPalette(brewer.pal(6, "Set1"))(278), stringsAsFactors = F)
    
    airline <- airline %>%
      left_join(df, by = "country") 
    
    
    route <- read.table("routes.dat", sep = ",", stringsAsFactors = F)
    names(route) <- c("airline", "airline_id", "source_airport", "airport_id", "dest_airport", "dest_airport_id", "codeshare", "stops", "equipment")
    
    route <- route %>%
      filter(airport_id != "\\N" & dest_airport_id != "\\N") %>%
      mutate(identifier = c(1:n()))
    
    incProgress(0.5, detail = "reading...")
    
    #Combine routes with airport latitude and longitude
    airport$airport_id <- as.character(airport$airport_id)
    route_source <- route %>%
      select(airline, airline_id, source_airport, airport_id, stops, equipment, identifier) %>%
      left_join(airport, by = "airport_id") %>%
      mutate(continent = countrycode(country, "country.name", "continent"))
    
    incProgress(0.65, detail = "reading...")
    
    route_dest <- route %>%
      select(airline, airline_id, dest_airport, dest_airport_id, stops, equipment, identifier) %>%
      left_join(airport, by = c("dest_airport_id" = "airport_id")) %>%
      mutate(continent = countrycode(country, "country.name", "continent"))
    
    incProgress(0.75, detail = "matching countries")
    
    route_all <- inner_join(na.omit(route_source), na.omit(route_dest), by = "identifier")
    
    
  country <- route_all[ , c(10, 21)] %>%
    distinct(country.x, .keep_all = T) %>%
    arrange(continent.x)
  
  Africa <- country %>% filter(continent.x == "Africa") %>% select(country.x) %>% arrange(country.x) 
  
  Americas <- country %>% filter(continent.x == "Americas") %>% select(country.x)%>% arrange(country.x)
  
  Asia <- country %>% filter(continent.x == "Asia") %>% select(country.x)%>% arrange(country.x)
  
  Europe <- country %>% filter(continent.x == "Europe") %>% select(country.x)%>% arrange(country.x)
  
  Oceania <- country %>% filter(continent.x == "Oceania") %>% select(country.x)%>% arrange(country.x)
  
  })
  countryselect <- reactive({
    withProgress(message = "Retrieving data", value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("retrieving countries", i))
        
        Sys.sleep(0.1)
      }
    })
    
    mydata <- get(input$continent)
    mydata$country.x
  }) 
  
  observe({
    
    updateSelectInput(session, "country", choices = countryselect())
    
  })
  
  continentselect <- reactive({
    withProgress(message = "Updating data", value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("retrieving continents", i))
        
        Sys.sleep(0.1)
      }
    })
    continent2 <- route_all %>%
      filter(country.x == input$country) %>%
      distinct(continent.y, .keep_all = T) %>%
      arrange(continent.y)
      
    continent2$continent.y
    
  }) 
  
  observe({
    updateSelectInput(session, "continent2", choices = continentselect())
  })
  
  
  
  countryselect2 <- reactive({
    country2 <- route_all %>%
    filter(continent.y == input$continent2) %>%
    filter(country.x == input$country) %>%
    distinct(country.y, .keep_all = T) %>%
    arrange(country.y)
  
    country2$country.y
  }) 
  
  
  observe({
    
    updateSelectInput(session, "country2", choices = countryselect2())
    
  })
  
#__________________________________________________________________
  
  output$bar <- renderPlot({
    withProgress(message = "Loading data", value = 0, {
    #Selection
    airline$airline <- as.character(airline$airline)
    route_req <- route_all %>%
      filter(country.x == input$country & country.y == input$country2) %>%
      left_join(airline, by = c("airline_id.x" = "airline")) %>%
      group_by(name.x) %>%
      mutate(source_num = n()) %>%
      group_by(name.y) %>%
      mutate(dest_num = n())
    incProgress(0.3, detail = paste("gathering data"))
    
    #Data for flight numbers summary
    route_sum <- route_req %>%
      select(city.x, name.x, long.x, lat.x, source_num, city.y, name.y, long.y, lat.y, dest_num, identifier)
    
    route_sum_source <- route_sum[ , c(1:5, 11)] %>%
      distinct(name.x, .keep_all = T) %>%
      mutate(type = rep("Departure", n())) 
    names(route_sum_source) <- c("city", "name", "long", "lat", "number", "id", "type")  
    route_sum_source$id <- as.character(route_sum_source$id)
    incProgress(0.6, detail = paste("gathering data"))
    route_sum_dest <- route_sum[ , 6:11] %>%
      distinct(name.y, .keep_all = T) %>%
      mutate(type = rep("Arrival", n()))
    names(route_sum_dest) <- c("city", "name", "long", "lat", "number", "id", "type")
    route_sum_dest$id <- as.character(route_sum_dest$id)
    route_sum_all <- rbind(as.data.frame(route_sum_source), as.data.frame(route_sum_dest))
    
    library(ggplot2)
    ggplot(route_sum_all) + geom_bar(aes(name, number, fill = type), stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } )
  })
  
#______________________________________________________________
  
  output$table <- renderTable({
    withProgress(message = "Loading data", value = 0, {
    #Selection
    airline$airline <- as.character(airline$airline)
    route_req <- route_all %>%
      filter(country.x == input$country & country.y == input$country2) %>%
      left_join(airline, by = c("airline_id.x" = "airline")) %>%
      group_by(name.x) %>%
      mutate(source_num = n()) %>%
      group_by(name.y) %>%
      mutate(dest_num = n())
    incProgress(0.3, detail = paste("gathering data"))
    
    #Data for flight numbers summary
    route_sum <- route_req %>%
      select(city.x, name.x, long.x, lat.x, source_num, city.y, name.y, long.y, lat.y, dest_num, identifier)
    
    route_sum_source <- route_sum[ , c(1:5, 11)] %>%
      distinct(name.x, .keep_all = T) %>%
      mutate(type = rep("Departure", n())) 
    names(route_sum_source) <- c("city", "name", "long", "lat", "number", "id", "type")  
    route_sum_source$id <- as.character(route_sum_source$id)
    incProgress(0.6, detail = paste("gathering data"))
    route_sum_dest <- route_sum[ , 6:11] %>%
      distinct(name.y, .keep_all = T) %>%
      mutate(type = rep("Arrival", n()))
    names(route_sum_dest) <- c("city", "name", "long", "lat", "number", "id", "type")
    route_sum_dest$id <- as.character(route_sum_dest$id)
    
    route_sum_all <- rbind(as.data.frame(route_sum_source), as.data.frame(route_sum_dest))
      return(route_sum_all)
    })
    
  })
  
#___________________________________________________________________
  observeEvent(input$action, {
  output$plot <- renderLeaflet({
    withProgress(message = "Making Plot", value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("retrieving data", i))
     Sys.sleep(0.1)
      }
    })   
        
    #Selection
    
    if(!is.null(input$country) & !is.null(input$country2)){ 
    airline$airline <- as.character(airline$airline)
    route_req <- route_all %>%
      filter(country.x == input$country & country.y == input$country2) %>%
      left_join(airline, by = c("airline_id.x" = "airline")) %>%
      group_by(name.x) %>%
      mutate(source_num = n()) %>%
      group_by(name.y) %>%
      mutate(dest_num = n())}
    
    
    #Data for flight numbers summary
    if(!is.null(route_req)){
    route_sum <- route_req %>%
      select(city.x, name.x, long.x, lat.x, source_num, city.y, name.y, long.y, lat.y, dest_num)
    
    route_sum_source <- route_sum[ , 1:5] %>%
      distinct(name.x, .keep_all = T) 
      
    route_sum_dest <- route_sum[ , 6:10] %>%
      distinct(name.y, .keep_all = T) 
     }
    
    
    
    #Plotting
    #Generate spatial line data
    if(!is.null(route_req)){
    for (i in 1:length(route_req$identifier)) {
      if(!exists("inter")){
        inter <- gcIntermediate(c(route_req$long.x[i], route_req$lat.x[i]),
                                c(route_req$long.y[i], route_req$lat.y[i]),
                                n = 100, addStartEnd = T, sp = T) 
      }
      if(exists("inter")) {
        inter1 <- gcIntermediate(c(route_req$long.x[i], route_req$lat.x[i]),
                                 c(route_req$long.y[i], route_req$lat.y[i]),
                                 n = 100, addStartEnd = T, sp = T)
        inter <- rbind(inter, inter1)
        rm(inter1)
      }
    } }
    
    
    #Generate leaflet map
    isolate(
    leaflet() %>% 
      addTiles() %>% 
      addPolylines(data = inter, weight = 1.5, color = route_req$color, group = "Flight",
                   highlight = highlightOptions(color = "red", weight = 5, bringToFront = T), 
                   label = paste(route_req$name, 
                                 paste(route_req$city.x, route_req$city.y, sep = " to "), sep = " : ")) %>%
      addLegend("bottomright", title = "Airline country origin", labels = unique(route_req$country), colors = unique(route_req$color), opacity = 1) %>%
      addCircles(route_sum_source$long.x, route_sum_source$lat.x, radius = 1.5^route_sum_source$source_num/50, label = paste(route_sum_source$name.x, route_sum_source$source_num, sep = " : "), group = "Source Airport") %>%
      addCircles(route_sum_dest$long.y, route_sum_dest$lat.y, radius = 1.4^route_sum_dest$dest_num/10, color = "red", label = paste(route_sum_dest$name.y, route_sum_dest$dest_num, sep = " : "), group = "Destination Airport") %>%
      addLayersControl(
        overlayGroups = c("Flight", "Source Airport", "Destination Airport"),
        options = layersControlOptions(collapse = F)))
    
     
    }) 
  })
  
#_______________________________________________________________________

  typeselect <- reactive({
    withProgress(message = "Updating data", value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("retrieving data", i))
        
        Sys.sleep(0.1)
      }
    })
    
    if(input$cat == "International"){
    airport_sum_source <- route_all %>%
      filter(country.x != country.y)} 
    else {
      airport_sum_source <- route_all %>%
        filter(country.x == country.y)
    }
      if(input$type == "Departure"){
      airport_sum_source <- airport_sum_source %>%  
      group_by(name.x) %>%
      mutate(sum = n()) %>%
      select(name.x, city.x, country.x, continent.x, sum) %>%
      distinct(name.x, .keep_all = T) %>%
      arrange(desc(sum))}
        else {
        airport_sum_source <- airport_sum_source %>%  
        group_by(name.y) %>%
          mutate(sum = n()) %>%
          select(name.y, city.y, country.y, continent.y, sum) %>%
          distinct(name.y, .keep_all = T) %>%
          arrange(desc(sum))
      }
    tab1 <- airport_sum_source[1:10, ]
    names(tab1) <- c("name", "city", "country", "continent", "sum")
    return(as.data.frame(tab1))
    
    }
    )  
  observeEvent(input$action2, {  
  output$gg <- renderPlot({
    withProgress(message = "Plotting", value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("retrieving data", i))
        
        Sys.sleep(0.1)
      }
    })
    isolate(
    ggplot(typeselect()) + geom_bar(aes(name, sum, fill = country), stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("Top 10 Airports:") + ylab("Number of Routes") +
      xlab("Airport") + facet_wrap(~continent, scales = "free"))
  }
    
  )})
  
  output$tab <- renderTable({
    typeselect()
  })
#_________________________________________________________________
  
  typeselect2 <- reactive({
    withProgress(message = "Updating data", value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("retrieving data", i))
        
        Sys.sleep(0.1)
      }
    })
    names(airline)[1] <- c("airline_id.x")
    airline$airline_id.x <- as.numeric(airline$airline_id.x)
    route_all$airline_id.x <- as.numeric(route_all$airline_id.x)
    if(input$cat2 == "International"){
      airline_sum_source <- route_all %>%
        left_join(airline, by = "airline_id.x") %>%
        filter(country.x != country.y)} 
    else {
      airline_sum_source <- route_all %>%
        left_join(airline, by = "airline_id.x") %>%
        filter(country.x == country.y)
    }
    withProgress(message = "Plotting", value = 0, {
      
        incProgress(0.5, detail = paste("retrieving data"))
        
        
      airline_sum_source <- airline_sum_source %>%  
        group_by(name) %>%
        mutate(sum = n()) %>%
        select(name, alias, country, sum) %>%
        distinct(name, .keep_all = T) %>%
        arrange(desc(sum)) %>%
        mutate(continent = countrycode(country, "country.name", "continent"))
      
      incProgress(0.7, detail = paste("retrieving data"))
    
      tab2 <- airline_sum_source[1:10, ] 
     Sys.sleep(0.1)
      
    })
    return(as.data.frame(tab2))
    
  }
  )   
  observeEvent(input$action3, {
  output$gg2 <- renderPlot({
    withProgress(message = "Plotting", value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("retrieving data", i))
        
        Sys.sleep(0.1)
      }
    })
    isolate(
    ggplot(typeselect2()) + geom_bar(aes(name, sum, fill = country), stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("Top 10 Airlines:") + ylab("Number of Routes") +
      xlab("Airport") + facet_wrap(~continent, scales = "free"))
  }
  
  )})
  
  output$tab2 <- renderTable({
    typeselect2()
  })
  
})   
