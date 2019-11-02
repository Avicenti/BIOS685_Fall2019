#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

DF <- readRDS("Indeed.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Indeed.com Data Scientist"),

    selectInput("city", 
                label = "Choose a city",
                choices = c("New York, NY", "Los Angeles, CA",
                            "Chicago, IL", "Houston, TX","Phoenix, AZ"),
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
        if(input$var== "Job Ratings"){
            # generate bins based on input$bins from ui.R
            x <- subset(DF, job_location==input$city) 
            bins <- seq(0,5,.5)
            # draw the histogram with the specified number of bins
            hist(x$company_review, col = 'darkgray', border = 'white', breaks = bins,main=input$city, xlab="Company Ratings" )
        }
        
        if(input$var== "Number of Jobs"){
            ggplot(data = DF) + 
                geom_bar(mapping = aes(x = DF$job_location))
        }

    })
    
    output$distPlot2 <- renderPlot({
        
        if(input$var== "Job Ratings"){
            
            City<-c("New York, NY", "Los Angeles, CA","Chicago, IL", "Houston, TX","Phoenix, AZ")
            # generate bins based on input$bins from ui.R
            Average_Rating <- c(mean(subset(DF, job_location=="New York, NY")$company_review,na.rm=TRUE),mean(subset(DF, job_location=="Los Angeles, CA")$company_review,na.rm=TRUE),
                   mean(subset(DF, job_location=="Chicago, IL")$company_review,na.rm=TRUE),mean(subset(DF, job_location=="Houston, TX")$company_review,na.rm=TRUE),
                   mean(subset(DF, job_location=="Phoenix, AZ")$company_review,na.rm=TRUE))
            df2<-data.frame(City,Average_Rating)
            # draw the histogram with the specified number of bins
            ggplot(df2,aes(City,Average_Rating)) +geom_col()+labs(title = "Average Employer Rating By City")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
