#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)


getSBML <- function(modelID){
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

getSBML("BIOMD0000000002")
library(XML)

xml_parse <- xmlParse(paste(getwd(),"temp.xml",sep="/"))
#xmlToList(xml_parse)
#xmlRoot(xml_parse)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
