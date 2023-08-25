box::use(
  vroom[vroom],
  dplyr[mutate, distinct, filter,],
  leaflet,
  leaflet[makeIcon],
  crosstalk,
  htmltools
)

#' @export
dataInput<- function(){
  occurrence_data <- vroom::vroom("data/occurrence.txt") |> as.data.frame() 
  multimedia_data <- vroom::vroom("data/multimedia.txt") |> as.data.frame() 
  deer_data <- merge(occurrence_data, multimedia_data, by = "gbifID", all.x = TRUE, all.y =T)  |>
    distinct(identifier.x, .keep_all = TRUE) 
  # deer_data <- deer_data|> filter(continent== 'EUROPE')
  deer_data$eventDate <- gsub(" UTC", "", deer_data$eventDate) 
  deer_data <- deer_data[!is.na(deer_data$identifier.x) & !is.na(deer_data$rightsHolder.y) & !is.na(deer_data$level0Name ), ]
  deer_data_subset <-subset(deer_data, select = c(gbifID , license.x,license.y, eventDate  , continent , stateProvince , locality ,
                                                  decimalLatitude, decimalLongitude , scientificName , vernacularName , identifier.x,
                                                  identifier.y,rightsHolder.x,rightsHolder.y, level0Name,references.x ))
  sharedData <- crosstalk::SharedData$new(deer_data_subset)
  return(sharedData)
}


#' @export
data_filter<- function(deerdata, vernacular_Name, country, start_date, end_date){
  
  filter_data<-deerdata$data() |>
              filter(vernacularName %in% vernacular_Name & eventDate >= start_date & 
                              eventDate <= end_date & level0Name %in% country )
  sharedData <- crosstalk::SharedData$new(filter_data)
  return(sharedData)
}



#' @export
customIcon <- function(imagePath, iconWidth, iconHeight) {
  leaflet::makeIcon(
    iconUrl = imagePath,
    iconWidth = iconWidth,
    iconHeight = iconHeight
  )
}


#' @export
leaflet_map<- function(data, image, vernacularename, eventdate , scientificname,
                       rightsHolder, locality , license, licenselink){
  map <-leaflet::leaflet(data)|> 
    leaflet::addTiles()|>
    
    # setView(lng = mean(data$decimalLongitude), lat = mean(data$decimalLatitude), zoom = 4)
    
    leaflet::addMarkers(
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      icon = customIcon("https://appsilon.github.io/semantic.dashboard/reference/figures/hexsticker.png", 
                        iconWidth = 30, iconHeight = 30),
      popup = 
        paste(
          "<div class='ui card '>",
          "<div class='fixed-height-container covered-fit-container'>",
          "<img src=", image, " >",
          "</div>",
          " <br/>",
          "<div class='content'>",
          "<div class='ui big green ribbon label'>", vernacularename,
          "</div>",
          "</div>",
          "<div class='description'>",
          "<div class='ui large list'>",
          "<div class='content'>",
          "<div class='item'>",
          "<i class='calendar alternate outline icon'>" ,"</i>",
          'Date:',format(as.Date(eventdate), "%Y %b %d"),
          "</div>",
          "<div class='item'>",
          "<i class='dna icon'>", "</i>",
          scientificname,
          " </div>",
          "<div class='item'>",
          "<i class='camera retro icon'>", "</i>",
          'Image Rights Holder:', rightsHolder,
          "</div>",
          "<div class='item'>",
          "<i class='map marker alternate icon'>", "</i>",
          'Locality:', locality,
          "</div>",
          "<div class='item'>",
          "<i class='copyright outline icon'>", "</i>",
          
          "<a href=",  license , "target='_blank'>", licenselink, "</a>",
          "</div>",
          "</div>",
          "</div>",
          "</div>",
          "<br/>",
          "</div>"
        )
    )
  return(map)
}

#' target="_blank" is used to prevent the app to reload when click on hyperlink in table
#' it will open the link in new tab or window

#' @export
make_hyperlink = function(myurl, mytext) {
  paste0('<a href="', myurl, '" target="_blank">', mytext, '</a>')
}

#' @export
make_hyperlink_img<- function(myurl,myimage) {
  paste('<a href="', myurl, '" target="_blank">',
        "<img class='ui mini spaced image', src=",myimage,">",'</a>')
}

#' @export
table_hyperlink<- function(df){
  my_table<- df$data()|>
    mutate(
      identifier.x=make_hyperlink(myurl = references.x, mytext = identifier.x),
      license.x = make_hyperlink(myurl = license.y, mytext = license.x),
      Image = make_hyperlink_img(myurl =identifier.y, myimage=identifier.y )
      
    )|>
    subset(select= c(gbifID, identifier.x,scientificName, vernacularName,locality, eventDate ,
                     rightsHolder.y, rightsHolder.x,Image, license.x )) |>
    mutate(scientificName = gsub("\\(.*\\)", "", scientificName)) |>
    mutate(locality = gsub("\\(.*\\)", "", locality))
  
    colnames(my_table)<- c("GBIF ID", "Identifier", "Scientific Name", "Vernacular Name", "Locality",
                         "Date Of Sighting", "Rights Holder (Indivisual)", "Rights Holder (Organization)",
                         "Image Link", "Licence Link")
  return(my_table)
}

