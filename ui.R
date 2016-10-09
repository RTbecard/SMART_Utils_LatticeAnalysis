library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("SMART Utilities: Gridded Analysis Workaround"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h4("Files"),
      p("For each shape file, select all 5
        files types exported by SMART for
        each upload (.dbf, .fix, .prj,
        .shp, .shx)"),
      fileInput("basemap",
                "Area Map (Shape file)",
                accept=c(".dbf", ".fix",
                         ".prj", ".shp", ".shx"),
                multiple = T),
      textInput("regionName","Attribute name defining regions"),
      p('The basemap will be used as a template to clip grids or lattices used in the analysis.  You can export a shape file from your SMART basemap.  To analyse the percent cover per region, you must specify the name of the attribute which acts as a unique identifier for regions.  The outline of this layer will be used to clip the resulting lattice/grid used in analysis.'),
      fileInput("patrols",
                "Patrols (Shape file)",
                accept=c(".dbf", ".fix",
                         ".prj", ".shp", ".shx"),
                multiple = T),
      p('Here you should use the shape file returned when exporting a patrol query in SMART.'),
      selectInput("cellType",
                  "Cell Type",c("Hexagon Lattice","Square Grid","User Defined"),"Hexagon lattice"),
      conditionalPanel(
        condition = "input.cellType == \"Square Grid\"",
        textInput("utmZone","UTM Zone (example: \"33\")")),
      conditionalPanel(
        condition = "input.cellType == \"User Defined\"",
        fileInput("grid",
                  "User Defined Grid (Shape file)",
                  accept=c(".dbf", ".fix",
                           ".prj", ".shp", ".shx"),
                  multiple = T)),
      conditionalPanel(
        condition = "input.cellType == \"Hexagon Lattice\" || input.cellType == \"Square Grid\"",
        textInput("size", "Cell Diameter (Decimal Degrees
                  for hexagon lattice, meters for square grid)")),
      p('Hexagon analysis will create a hexagon lattice using a lat/lon projection.  Square Grid will use UTM projections for which you have to supply the grid number, and user defined grids will use the projection defined in the user supplied shape file.'),
      actionButton("analyse","Start Analysis"),
      downloadButton("download","Download Results")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
          tabPanel("Distance Patroled/Cell",
                   plotOutput("plot1",width = "700px",height = "700px")),
          tabPanel("Patrol visits/Cell",
                   plotOutput("plot2",width = "700px",height = "700px")),
          tabPanel("Region % Patrol Coverage",
                   plotOutput("plot3",width = "700px",height = "700px"))
          ),
      shinyjs::useShinyjs(),
      verbatimTextOutput("console")
      )
    )
  )
)
