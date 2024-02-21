library(shiny)
library(RELAN)
#library(xml2) need this package for read_xml()?
library(devtools)

load_all(".")

# Define UI
ui <- fluidPage(
  titlePanel("RELAN"),

  sidebarLayout(
    sidebarPanel(
      helpText("Convert your ELAN file into a",
                 code("data.frame"),
                 "that can be easily processed with R by using the RELAN package."),

    fileInput("eafUpload",
              label = "Upload your .eaf file(s) here",
              multiple = TRUE,
              accept = ".eaf")
    ),

    mainPanel(
      tableOutput("RDataFrame")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$RDataFrame <- renderTable({
    file <- input$eafUpload
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "eaf", "Please upload an eaf file"))

    #use RELAN package at this point? extract_annotations(read_xml("files/280117_10_Hamid_Clandividing.eaf"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
