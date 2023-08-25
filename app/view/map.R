box::use(
  shiny[div,NS,moduleServer, reactive,tags],
  semantic.dashboard[box,valueBoxOutput,renderValueBox, valueBox,icon],
  leaflet[
    leafletOutput, renderLeaflet,
    leaflet, setView, addTiles,fitBounds,
  ],
  
)

box::use(
  app/logic/rhinos[leaflet_map,],
)

#' @export
ui<- function(id){
  ns<- NS(id)
  div(class="row",
      box(leafletOutput(ns("main_map"), height = '45rem'), width = 16),
      
      valueBoxOutput(ns("deer_sightings_number"), width = 6),
      
      valueBoxOutput(ns("Countries_with_sightings"), width = 5),
      valueBoxOutput(ns("Unique_deer_species"), width = 5)
  )
  
}



#' @export
server<- function(id, data){
  moduleServer(id, function(input, output, session){
  
    map <- reactive({
     
      data <- data()
     
      if(nrow(data$data())<1){
        map <- leaflet(data = NULL) |>
          addTiles()  |>setView(lng = 0, lat = 0, zoom = 2)
      }
      else if(nrow(data$data())==1){
        map <- leaflet_map(data = data, image = data$data()$identifier.y, vernacularename = data$data()$vernacularName, 
                           eventdate = data$data()$eventDate , scientificname = data$data()$scientificName,
                           rightsHolder = data$data()$rightsHolder.y, locality = data$data()$locality , 
                           license = data$data()$license.y, licenselink = data$data()$license.x) |>
          
          setView(lng = mean(data$data()$decimalLongitude), lat = mean(data$data()$decimalLatitude), zoom = 12)
        # fitBounds(~min(decimalLongitude), ~min(decimalLatitude), ~max(decimalLongitude), ~max(decimalLatitude)) 
      }
      else{
        map<- leaflet_map(data = data, image = data$data()$identifier.y, vernacularename = data$data()$vernacularName, 
                          eventdate = data$data()$eventDate , scientificname = data$data()$scientificName,
                          rightsHolder = data$data()$rightsHolder.y, locality = data$data()$locality , 
                          license = data$data()$license.x, licenselink = data$data()$license.y)  |>
          fitBounds(~min(decimalLongitude), ~min(decimalLatitude), ~max(decimalLongitude), ~max(decimalLatitude))
      }
      # browser()
      return(map)
    })
    
    output$main_map <- renderLeaflet({
      map()
    })
    
    output$deer_sightings_number <- renderValueBox({
      
      div(class="padded-container",
          valueBox(
            value = length(unique(data()$data()$gbifID)),
            subtitle = tags$span(class = "ui big green pointing label", "Total Deer Sightings"),
            icon = icon("green eye"),
            color = "green",
            width = 5,
            size = 'large')
      )
    })
    
    output$Unique_deer_species <- renderValueBox({
      
      
      # browser()
      div(class="padded-container",
          valueBox(
            value = length(unique(data()$data()$scientificName)),
            # value = length(unique(filtered_data()$scientificName)),
            subtitle = tags$span(class = "ui big green pointing label", "Unique Deer Species"),
            icon = icon("green dna"),
            color = "green",
            width = 5,
            size = 'large')
      )
    })
    
    output$Countries_with_sightings <- renderValueBox({
      
      div(class="padded-container",
          valueBox(
            value = length(unique(data()$data()$level0Name)),
            subtitle = tags$span(class = "ui big green pointing label", "Countries with Deer Sightings"),
            icon = icon("green globe europe"),
            color = "green",
            width = 5,
            size = 'large')
      )
    })
    
    
    
    
    
  })
}