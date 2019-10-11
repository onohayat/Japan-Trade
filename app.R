

ui <- fluidPage(  
  # Create a title for app
  column(3, titlePanel("Japanese Trade")),
  column(12, 
         # Create page for export map with slider input
         tabsetPanel( tabPanel("Export Map",
                               sliderInput(inputId = "Years", h3("Choose the Year"), 
                                           min = 1962, max = 2017, 
                                           value=1962, sep="", ticks = FALSE),
                               leafletOutput("Map",height="600px")),
                      # Create page for import map with slider input
                      tabPanel("Import Map",
                               sliderInput(inputId = "Years2", h3("Choose the Year"), 
                                           min = 1962, max = 2017, 
                                           value=1962, sep="", ticks = FALSE),
                               leafletOutput("Map2", height="600px")),
                      # Create page for graphing export and import values for user's preferred country
                      tabPanel("Individual Country",
                               selectizeInput("country", label = "Choose a Country", 
                                              choices = AllCountriesAllYears$Destination
                                              [AllCountriesAllYears$export_val!="na"],
                                              options=list(create = FALSE)),
                               plotlyOutput("Plot", height="600px")),   
                      # Create page for comparing export and import values for different regions
                      tabPanel("Region",
                               fluidRow(column(width=12, 
                                               plotlyOutput("Plot2", height="600px")))) ))) 

server <- function(input, output){
  # Create map for export
  qpal <- colorNumeric("YlOrBr", data1.2$log2, reverse = F)
  output$Map <- renderLeaflet( {
    leaflet(WorldCountry) %>% 
      addTiles() %>% 
      addPolygons(fillColor = 
                    ~qpal(data1.2$log2[data1.2$Years==input$Years]), 
                  weight=2, color="black", fillOpacity=0.7, 
                  label=~paste(data1.2$Destination[data1.2$Years==input$Years],
                               data1.2$export_val[data1.2$Years==input$Years])) %>%
      setView(lng = 0, lat = 0, zoom = 2)} )
  # Create map for import
  qpal <- colorNumeric("YlOrBr", data1.2$log, reverse = F)
  output$Map2 <- renderLeaflet( {
    leaflet(WorldCountry) %>% 
      addTiles() %>% 
      addPolygons(fillColor = 
                    ~qpal(data1.2$log[data1.2$Years==input$Years2]), 
                  weight=2, color="black", fillOpacity=0.7, 
                  label=~paste(data1.2$Destination[data1.2$Years==input$Years2]
                               ,data1.2$import_val[data1.2$Years==input$Years2])) %>%
      setView(lng = 0, lat = 0, zoom = 2)} )
  # Name axes for line/bar graphs
  x <- list(title="Year")
  y <- list(title="Value")
  x2 <- list(title="")
  y2 <- list(title="Value")
  # Create line graph for individual countries
  output$Plot <- renderPlotly( {
    plot_ly(data1.2,x=~Years[data1.2$Destination==input$country]) %>%
      add_lines(y=~export_val[data1.2$Destination==input$country], 
                name = "Export" ) %>%
      add_lines(y=~import_val[data1.2$Destination==input$country], 
                name = "Import") %>%
      layout( title = ~paste("Trade between Japan and", input$country),
              xaxis = x, yaxis = y) }) 
  # Create bar graph for comparing regions
  output$Plot2 <- renderPlotly( {
    plot_ly(data2, x=~fct_reorder(Region,Export)) %>%
      layout(xaxis = list(title=x2), 
             yaxis= list(title=y2,inputautorange=TRUE)) %>%
      add_bars(y=~Export/sum.Export., frame=~Years, name ="Export") %>%
      add_bars(y=~Import/sum.Import., frame=~Years, name= "Import") })
}


shinyApp(ui, server)
