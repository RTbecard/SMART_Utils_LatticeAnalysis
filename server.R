## Load packages
require(sp)
require(rgdal)
require(rgeos)
library(shiny)

## Source external functions
source("helperFunctions.R",local = T)

## Save output as global variable
results <- list()

shinyServer(function(input, output) {
  ## Save patrols data
  patrols.in <- reactive({
    input$patrols
  })
  
  basemap <- reactive({
    input$basemap
  })
  
  zone <- reactive({
    input$utmZone
  })

  filename <- reactive({
    input$filename
  })
  
  cellType <- reactive({
    input$cellType
  })
  
  size <- reactive({
    return(as.numeric(input$size))
  })
    
  observeEvent(input$analyse, {
    message("Preparing Files")

    ## Make temporary folder if it doesn"t exist
    if(!dir.exists("temp")){
      unlink("temp",recursive = T)
      dir.create("temp")
    }
    
    ## Copy patrol files to temp directory
    for(i in 1:length(patrols.in()$name)){
      file.rename(patrols.in()$datapath[i],
                paste("temp",patrols.in()$name[i],sep = "/"))
    }
    
    ## Copy map files to temp directory
    for(i in 1:length(basemap()$name)){
      file.rename(basemap()$datapath[i],
                paste("temp",basemap()$name[i],sep = "/"))
    }

    patrol <- readOGR(dsn="./temp",
                      layer=substr(patrols.in()$name[1],1,
                                   nchar(patrols.in()$name[1])-4),
                      stringsAsFactors = options(stringsAsFactors = T))
    map <- readOGR(dsn="./temp",
                   layer=substr(basemap()$name[1],1,
                                nchar(basemap()$name[1])-4),
                   stringsAsFactors = options(stringsAsFactors = T))
    message("Running Analysis... Please wait.")

    ## Run analysis
    results <<- tryCatch({
      LatticedAnalysis(patrols = patrol,
                     map = map,
                     cellDiameter = size(),
                     cellType = cellType(),
                     zone = zone())
      }, error = function(err) {
        stop("Error! Analysis halted!!")
      })
    message("Plotting results...")
    
    ## Plot results
    output$plot1 <- renderPlot(spplot(results[[1]],
                                      zcol = "distance.patrolled",
                                      main = "Distance Patrolled per Cell",
                                      scales = list(draw = T)))
    output$plot2 <- renderPlot(spplot(results[[1]],
                                      zcol = "Patrol.Visits",
                                      scales = list(draw = T),
                                      main = "Visits by patrols"))
    output$plot3 <- renderPlot(spplot(results[[1]],
                                      zcol = "Percent.coverage",
                                      scales = list(draw = T),
                                      main = paste0("Percent Coverage per
                                                Region (Total Area covered: ",
                                                results[[2]],"%)")))
  })
  
  observeEvent(input$download, {
    ## Make temporary folder if it doesn"t exist
    if(!dir.exists("results")){
      unlink("results",recursive = T)
      dir.create("results")
    }
    
    message("Converting to shape files...")
    writeOGR(results[[1]], 
             driver="ESRI Shapefile",
             overwrite_layer=TRUE,
             dsn = "./results",
             layer = filename())
    message("Files downloaded can be
            found in the \"Results\" folder")
  })
})
