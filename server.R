## Load packages
require(sp)
require(rgdal)
require(rgeos)
require(shiny)
require(XML)

## Source external functions
source("helperFunctions.R",local = T)

shinyServer(function(input, output) {
  
  analysisType <- reactive({
    input$analysisType
  })
  
  patrols.in <- reactive({
    input$patrols
  })
  
  encounters.in <- reactive({
    input$encounters
  })
  
  basemap <- reactive({
    input$basemap
  })

  metadata <- reactive({
    input$metadata
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
  
  period <- reactive({
    return(as.numeric(input$period))
  })
    
  observeEvent(input$analyse, {
    message("Preparing Files")
    
    ## Load area map
    renameShapeFiles(basemap()$name,basemap()$datapath)
    map <- loadShapeFile(basemap()$name)

    
    if(analysisType() == "Patrol Cover per Region"){
      
      ## Load patrol data
      renameShapeFiles(patrols.in()$name,patrols.in()$datapath)
      patrol <- loadShapeFile(patrols.in()$name)
      
      ## Run analysis
      message("Running Analysis... Please wait.")
      assign('results', PatrolCoverage(patrols = patrol,
                                           map = map,
                                           cellDiameter = size(),
                                           cellType = cellType(),
                                           zone = zone())
                 ,envir = .GlobalEnv)
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
                                        main = "Percent Coverage per
                                                Region"))
    }else if(analysisType() == "Encounter rates over time"){
      ## Load patrol waypoints
      patrol.points <- loadPatrolData(metadata()$datapath)
      patrol.tracks <- createPatrolTracks(patrol.points)
      
      message("Running Analysis... Please wait.")
      PatrolZipLocations <- metadata()$datapath
      results <- EncounterRates(patrol.points = patrol.points,
                                patrol.tracks = patrol.tracks,
                                map = map,
                                cellDiameter = size(),
                                cellType = cellType(),
                                zone = zone(),
                                period = period(),
                                analysisType = analysisType())
      
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
    }
  })
  
  observeEvent(input$download, {
    ## Make temporary folder if it doesn"t exist
    if(!dir.exists("results")){
      unlink("results",recursive = T)
      dir.create("results")
    }
    if(exists('results')){
      message("Converting to shape files...")
      writeOGR(results[[1]], 
               driver="ESRI Shapefile",
               overwrite_layer=TRUE,
               dsn = "./results",
               layer = filename())
      message("Files downloaded can be
              found in the \"Results\" folder")
    }else{
      message("You must first run an anlysis before downloading!")
    }
  })
})
