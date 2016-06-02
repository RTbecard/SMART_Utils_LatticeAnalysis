library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("SMART Utilities: Gridded Analysis Workaround"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      p("Load your exported shapefiles from a
        SMART patrol query and your SMART base
        map layer outlining the patrol regions.
        You can then run an automated hexagon
        lattice or square grid analysis.  For
        the region analysis, summary statistics
        will be done for each region with a
        uniqe name attribute."),
      h4("Files"),
      p("For each shape file, select all 5
        files types exported by SMART for
        each upload (.dbf, .fix, .prj,
        .shp, .shx)"),
      
      h4("Settings"),
      selectInput("analysisType",
                  "Analysis Type",c("Patrol Cover per Region",
                                "Encounter rates over time"),
                  "Patrol Cover per Region"),
      fileInput("basemap",
                "Area Map (Shape file)",
                accept=c(".dbf", ".fix",
                         ".prj", ".shp", ".shx"),
                multiple = T),
      
      conditionalPanel(
        condition = "input.analysisType == \"Encounter rates over time\"",
        fileInput("metadata","Patrol metadata (.zip files)",
                  accept=c(".zip"),
                  multiple = T),
        textInput("period", "Analysis periods (days)"),
        fileInput("encounters",
                  "Encounters (Shape file)",
                  accept=c(".dbf", ".fix",
                           ".prj", ".shp", ".shx"),
                  multiple = T)),

      conditionalPanel(
                condition = "input.analysisType == \"Patrol Cover per Region\"",     
                fileInput("patrols",
                            "Patrols (Shape file)",
                            accept=c(".dbf", ".fix",
                           ".prj", ".shp", ".shx"),
                  multiple = T)
      ),

      selectInput("cellType",
                  "Cell Type",c("Hexagon Lattice",
                                "Square Grid"),"Hexagon lattice"),
      conditionalPanel(
        condition = "input.cellType == \"Square Grid\"",
        textInput("utmZone","UTM Zone (example: \"33\")")),
      textInput("size", "Cell Diameter (Decimal Degrees
                for hexagon lattice, meters for square grid)"),
      actionButton("analyse","Start Analysis"),
      textInput("filename", "Output file name"),
      actionButton("download","Download Results")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition = "input.analysisType == \"Patrol Cover per Region\"",
        tabsetPanel(
          tabPanel("Distance Patroled/Cell",
                   plotOutput("plot1",width = "700px",height = "700px")),
          tabPanel("Patrol visits/Cell",
                   plotOutput("plot2",width = "700px",height = "700px")),
          tabPanel("Region % Patrol Coverage",
                   plotOutput("plot3",width = "700px",height = "700px"))
        )
      )
    )
  )
))
