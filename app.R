library(shiny)
library(DT)
library(httr)
library(XML)
library(xml2)
library(RCurl)
library(dplyr)
library(shinythemes)
library(shinydashboard)

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


getSBML <- function(modelID){

    biomodels <- "https://www.ebi.ac.uk/biomodels/model/download"
    url <- paste(biomodels,modelID, sep = "/")
    xData <- getURL(paste(paste(url,"filename=",sep = "?"),modelID,'_url.xml',sep=""))
    #xml_out <- xmlParse(xData)
    pg <- read_xml(xData)
    xml_ns_strip(pg)
    
    # get all the <species>
    all_children <- xml_child(pg,search=1)
    return(all_children)
}

getParameters <- function(model){
    
    param <- xml_find_all(model, "//parameter")
    
    # extract and clean 
    param_text <- trimws(xml_text(param))
    param_name <- xml_attr(param, "name")
    param_id <- xml_attr(param, "id")
    param_value <- xml_attr(param, "value")
    
    species <- tibble(text = param_text ,name = param_name ,id = param_id, value = param_value)
    return(species)
}

getSpecies <- function(model){
    
    species <- xml_find_all(model, "//species")
    
    # extract and clean
    species_name <- xml_attr(species, "name")
    species_id <- xml_attr(species, "id")
    species_initialAmount <- xml_attr(species, "initialAmount")
    
    species <- tibble(name = species_name ,id = species_id, initialAmount = species_initialAmount)
    return(species)
}

#---------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        textInput("modelID","Input SBML Model:","BIOMD0000000002"),
        menuItem("model",tabName = 'model'),
        menuItem('species', tabName = 'species'),
        menuItem('parameter', tabName = 'param'),
        menuItem('reaction', tabName = 'reaction'),
        menuItem('ODE', tabName = 'ode')
        
    )
)

body <- dashboardBody(
    
    tabItems(
        tabItem(tabName = "model",
                'Showing for: ',
                textOutput("xml_file_name")
        ),
        
        tabItem(tabName = "species",
                h2("Species"),
                dataTableOutput("xml_species")
        ),
        tabItem(tabName = "param",
                h2("Parameters"),
                dataTableOutput("xml_param")
        ),
        
        tabItem(tabName = "reaction",
                h2("Widgets tab content")
               
        ),
        tabItem(tabName = "ode",
                h2("Widgets tab content")
                
        )
    )
)

# Define UI for application 
ui <- dashboardPage(
        dashboardHeader(title = "SBML UI."),
        sidebar,
        body
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
    model_data <- reactive(getSBML(input$modelID))
    
    output$xml_file_name <- renderText(data())
    output$xml_all <- model_data
    render_all <- reactive(getSpecies(model_data))
    output$xml_species <- renderDataTable({datatable(getSpecies(getSBML(input$modelID)))})
    output$xml_param <- renderDataTable({datatable(getParameters(getSBML(input$modelID)))})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
