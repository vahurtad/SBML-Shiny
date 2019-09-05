library(shiny)
library(DT)
library(httr)
library(XML)
library(xml2)
library(RCurl)
library(dplyr)
library(shinythemes)

#------------------

downloadSBML <- function(modelID){
    biomodels <- "https://www.ebi.ac.uk/biomodels/model/download"
    url <- paste(biomodels,modelID, sep = "/")
    print(url)
    result <- GET(url,
                  content_type("application/xml "),
                  accept("application/octet-stream"),
                  query = list(filename = paste(modelID,'_url.xml',sep = "")),
                  write_disk("temp.xml", overwrite=TRUE))
    
    return(result)
}


getSBML <-function(modelID){

    biomodels <- "https://www.ebi.ac.uk/biomodels/model/download"
    url <- paste(biomodels,modelID, sep = "/")
    xData <- getURL(paste(paste(url,"filename=",sep = "?"),modelID,'_url.xml',sep=""))
    #xml_out <- xmlParse(xData)
    pg <- read_xml(xData)
    xml_ns_strip(pg)
    
    # get all the <species>
    all_children <- xml_child(pg,search=1)
    species <- xml_find_all(all_children, "//species")

    # extract and clean 
    species_text <- trimws(xml_text(species))
    species_name <- xml_attr(species, "name")
    species_id <- xml_attr(species, "id")
    species_initialAmount <- xml_attr(species, "initialAmount")
    
    species <- tibble(text = species_text ,name = species_name ,id = species_id, initialAmount = species_initialAmount)
    return(species)
}


#---------------------

# Define UI for application 
ui <- fluidPage(
    theme = shinytheme("slate"),
    # Application title
    titlePanel("SBML"),

    sidebarLayout(
        sidebarPanel(
            textInput("modelID","Input SBML Model:","BIOMD0000000002")
        ),
        mainPanel(
            'Showing for: ',
            textOutput("xml_file_name"),
            dataTableOutput("xml_output")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- reactive({
        if(input$modelID == ""){
            data <- "Need Model ID"
        } else {
          data <- input$modelID
        }
    })

    output$xml_file_name <- renderText(data())
    
    output$xml_output <- renderDataTable({datatable(getSBML(input$modelID))})
}

# Run the application 
shinyApp(ui = ui, server = server)
