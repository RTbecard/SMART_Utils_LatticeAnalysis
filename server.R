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

  grid <- reactive({
    input$grid
  })
    
  zone <- reactive({
    input$utmZone
  })

  regionName <- reactive({
    input$regionName
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
    message("---Loading Files---")
    
    ## Load area map
    renameShapeFiles(basemap()$name,basemap()$datapath)
    map <- loadShapeFile(basemap()$name)
  
    ## Load patrol data
    renameShapeFiles(patrols.in()$name,patrols.in()$datapath)
    patrol <- loadShapeFile(patrols.in()$name)
  
    ## Load user defined grid data
    grid <- NULL
    if(cellType() == 'User Defined'){
      renameShapeFiles(grid()$name,grid()$datapath)
      grid <- loadShapeFile(grid()$name)}
        
    ## Run analysis
    message("Running Analysis... Please wait.")
    assign('results', PatrolCoverage(patrols = patrol,
                                     map = map,
                                     cellDiameter = size(),
                                     cellType = cellType(),
                                     zone = zone(),
                                     regionName = regionName(),
                                     cellLattice = grid)
               ,envir = .GlobalEnv)
    
    ## Plot results
    output$plot1 <- renderPlot(spplot(results[[1]],
                                      zcol = "distance",
                                      main = "Distance Patrolled per Cell",
                                      scales = list(draw = T)))
    output$plot2 <- renderPlot(spplot(results[[1]],
                                      zcol = "visits",
                                      scales = list(draw = T),
                                      main = "Visits by patrols"))
    output$plot3 <- renderPlot(spplot(results[[1]],
                                      zcol = "Percent.coverage",
                                      scales = list(draw = T),
                                      main = "Percent Coverage per
                                              Region"))
  })
  
  observeEvent(input$download, {
    ## Make temporary folder if it doesn"t exist
    if(!dir.exists("results")){
      unlink("results",recursive = T)
      dir.create("results")
    }
    if(exists('results')){
      message("Converting to shape files...")
      writeOGR(results$shapeFile, 
               driver="ESRI Shapefile",
               overwrite_layer=TRUE,
               dsn = "./results",
               layer = filename())
      write.csv(file = paste0('./results/',filename(),'.csv'), x = results$resultsTable)
      message("Files downloaded can be
              found in the \"Results\" folder")
    }else{
      message("You must first run an anlysis before downloading!")
    }
  })
})
