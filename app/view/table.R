box::use(
  shiny,
  shiny[reactive],
  semantic.dashboard[box],
  reactable[reactable, renderReactable,colDef,reactableOutput ],
  reactable,
  
)

box::use(
  app/logic/rhinos,
)

#' @export
ui<- function(id){
  ns<- shiny$NS(id)
  shiny::tagList(
    
    reactableOutput(ns("table"))
  )
  
}



#' @export
server<- function(id,data){
  shiny$moduleServer(id, function(input, output, session){
  
    output$table <- renderReactable({
      # DT::datatable(filtered_data()$daat(), extensions = 'Scroller', options = list(deferRender = TRUE, scrollY = 200, scroller = TRUE))
      
      reactable$reactable(data(), columns = list(
        
        "Licence Link"  = colDef(html = TRUE),
        Identifier = colDef(html = TRUE),
        "Image Link" = colDef(html = TRUE)
      )
      )
    })
    
    
  })
}