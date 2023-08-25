box::use(
  shiny[moduleServer, NS, fluidRow,  h1, reactive, tags, div,tagList],
  semantic.dashboard[
    dashboardPage,
    dashboardHeader, dashboardBody, dashboardSidebar,
    sidebarMenu, menuItem,box,tabItems,tab_item,icon,
  ],
  reactable
)

box::use(
  app/logic/rhinos[customIcon, leaflet_map, table_hyperlink, make_hyperlink_img,make_hyperlink, data_filter],

  app/view[map,table]
  
)

grid <- function(...) div(class = 'grid',...)
card <- function(...) div(class = 'card',...)
deer_data<- readRDS('data/occurence_combined.rds')

#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    dashboardHeader(left = h1(class = 'header_title', "Deer Obervation Europe"), show_menu_button = FALSE, 
                    color = 'green',inverted = TRUE),
    dashboardSidebar(size = 'wide',
      sidebarMenu(
      menuItem(tabName = "map", text = "Map", icon = icon('map')),
      tags$br(),
      menuItem(tabName = "table", text = "Table", icon = icon("table")),
      tags$br(),
      div(class = 'padded-container ',
         
          shiny.semantic::selectInput(ns("vernacularName"), 
                                      label = tags$span(class = "ui green pointing below label", "Select Vernacular Name:"), 
                                      choices = unique(deer_data$data()$vernacularName),
                                      selected = 'European Roe Deer',
                                      multiple = T,
                                      type= "ui fluid search selection multiple dropdown 
                                    semantic-select-input shiny-bound-input"
                                      
          )
          
      ),
      
      tags$br(),
      div(class = 'padded-container',
          shiny.semantic::selectInput(ns("country_names"), 
                                      label = tags$span(class = "ui green pointing below label", "Select one/more country:"), 
                                      choices = unique(deer_data$data()$level0Name),
                                      selected = 'Italy',
                                      multiple = T,
                                      type= "ui fluid search selection multiple 
                                dropdown semantic-select-input shiny-bound-input" #Arguments passed to dropdown_input.
                                      
          )
      ),
      
      tags$br(),
      div(class="padded-container",
               div(class = "ui green pointing below label", "Select start date:"),
               shiny.semantic::calendar(ns('startDate'), 
                                       # label = tags$div(class = "ui green pointing below label", "Select start date:"), 
                                       value  = '2022-01-01',
                                       min = min(deer_data$data()$eventDate),
                                       max = max(deer_data$data()$eventDate),
                                       placeholder="Select one date only"
                 
               ),
               tags$br(),
               div(class = "ui green pointing below label", "Select end date:"),
               shiny.semantic::calendar(ns('endDate'), 
                                        # label = tags$div(class = "ui green pointing below label", "Select End date:"),
                                        value  = '2022-10-27',
                                        min = min(deer_data$data()$eventDate),
                                        max = max(deer_data$data()$eventDate),
                                        placeholder="Select one date only"
                                        # icon = 'calander',
                                        # icon_name = 'calander'
               )
      )
    )),
    dashboardBody(
      fluidRow(
        tabItems(
          
          tab_item( tabName = 'map',
          # grid(
            map$ui(ns('map'))    
          # )
          ),
          tab_item(
            tabName = 'table',
            box(
                table$ui(ns('table'))
              , width = 16, height='auto')
          )
          
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data<- reactive({
      data_filter(deerdata=deer_data, vernacular_Name = input$vernacularName, country = input$country_names, 
                  start_date = input$startDate , end_date = input$endDate)
    })

    table_output<- reactive({
      table_hyperlink(df =data())
     
    })
    
    map$server('map', data)
    table$server('table', data=table_output)
    
    
    
  })
}