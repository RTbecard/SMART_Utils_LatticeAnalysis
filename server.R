## Load packages
require(sp)
require(rgdal)
require(rgeos)
require(shiny)
require(XML)
require(maptools)
require(RColorBrewer)
require(shinyjs)
require(utils)
## Source external functions
source("helperFunctions.R",local = T)

## Override default maximum file size
options(shiny.maxRequestSize=30*1024^2) 

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
  
  cellType <- reactive({
    input$cellType
  })
  
  size <- reactive({
    return(as.numeric(input$size))
  })
  
  observeEvent(input$analyse, {
    withCallingHandlers({
      ## Clear shiny console
      shinyjs::html(id = "console", html = "")
      
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
                                        scales = list(draw = T),
                                        col.regions = colorRampPalette(brewer.pal(9,'Blues'))(101),
                                        lwd = 0.3))
      browser()
      colors <- colorRampPalette(brewer.pal(9,'Blues'))(101)
      colors[1] <- '#888888'
      output$plot2 <- renderPlot(spplot(results[[1]],
                                        zcol = "visits",
                                        scales = list(draw = T),
                                        main = "Visits by patrols",
                                        col.regions = colors,
                                        lwd = 0.3))
      output$plot3 <- renderPlot(spplot(results[[1]],
                                        zcol = "Percent.coverage",
                                        scales = list(draw = T),
                                        main = "Percent Coverage per
                                                Region",
                                        colorkey=list(at=0:100),
                                        at = 0:100,
                                        col.regions = colorRampPalette(brewer.pal(9,'Blues'))(101),
                                        lwd = 0.3))},
      message = function(m) {
        shinyjs::html(id = "console", html = m$message, add = TRUE)
      },
      warning = function(m) {
        shinyjs::html(id = "console", html = m$message,add = TRUE)
      }
    )
  })
  
  ## Download results as zip file
  output$download <- downloadHandler(
    filename = paste0('Results ', Sys.Date(),format(Sys.time(), " %H-%M"),'.zip'),
    content = function(file){
      
      ## Delete and remake results folder if already exists
      if(dir.exists("results")){
        unlink("results",recursive = T)
      }
      dir.create("results")
      
      ## Create shape files
      message("Converting results to shape file...")
      writeOGR(results$shapeFile, 
               driver="ESRI Shapefile",
               overwrite_layer=TRUE,
               dsn = "./results",
               layer = paste0('results ',Sys.Date(),format(Sys.time(), " %H-%M")))
      write.csv(file = paste0('./results/','results ',Sys.Date(),format(Sys.time(), " %H-%M"),'.csv'), x = results$resultsTable)
      
      ## Create zip file for download
      if(length(dir('results')) == 0){
        message('No results available for download.  Run the analysis before downloading results.')
      }else{
        zip(file,files = paste('results',dir('results'),sep = '/'))
      }
    })
})
