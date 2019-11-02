#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

DF <- read_rds("Indeed.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Indeed.com Data Scientist"),

    selectInput("city", 
                label = "Choose a city",
                choices = c("New York", "Los Angeles",
                            "Chicago", "Houston","Phoenix"),
                selected = "New York"),
    
    selectInput("var", 
                label = "What would you like to look at?",
                choices = c("Number of Jobs", "Job Ratings",
                            "Skills", "Salary"),
                selected = "Number of Jobs"),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot1"),
           plotOutput("distPlot2")
           
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot1 <- renderPlot({
        if(input$var== "Number of Jobs"){
            # generate bins based on input$bins from ui.R
            x    <- DF$company_review
            bins <- seq(min(x), max(x), length.out = 20)
            
            # draw the histogram with the specified number of bins
            hist(x, breaks = bins, col = 'darkgray', border = 'white')
        }

    })
    
    output$distPlot2 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- rexp(100)
        bins <- seq(min(x), max(x), length.out = 20)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
