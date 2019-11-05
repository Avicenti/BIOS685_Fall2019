#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("tidyverse")

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
                            "Skills"),
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
            x <- subset(DF, job_location==input$city) 
            bins <- seq(0,5,.5)
            hist(x$company_review, col = 'darkgray', border = 'white', breaks = bins,main=input$city, xlab="Company Ratings" )
        }
        
        else if(input$var== "Skills"){
            
            skills <- c("machine learning", "python","linux","sas", "R", "matlab","sql","hadoop")
            x <- subset(DF, job_location==input$city)
            skill_total <- c(sum(x$ML),sum(x$py),sum(x$lin),sum(x$sas),sum(x$r),sum(x$matl),sum(x$sql),sum(x$had))
            df2<-data.frame(skills,skill_total)
            # draw the histogram with the specified number of bins
            ggplot(df2,aes(x= reorder(skills,-skill_total), y = skill_total)) +geom_col()+labs(title = "Skill Distribution By City", x='Skill', y='Count')
        } 
        
        else if(input$var== "Number of Jobs"){
            
            ggplot(data = DF) + 
                
                geom_bar(mapping = aes(x = DF$job_location)) + labs(title = "Job Count By City", x='City', y='Count')
            
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
            ggplot(df2,aes(x = reorder(City,-Average_Rating), y =Average_Rating)) +geom_col()+labs(title = "Average Employer Rating By City", x='City', y='Rating')
        }
        
        else if(input$var== "Skills"){
            
            skills <- c("machine learning", "python","linux","sas", "R", "matlab","sql","hadoop")
            skill_total <- c(sum(DF$ML),sum(DF$py),sum(DF$lin),sum(DF$sas),sum(DF$r),sum(DF$matl),sum(DF$sql),sum(DF$had))
            df2<-data.frame(skills,skill_total)
            # draw the histogram with the specified number of bins
            ggplot(df2,aes(x= reorder(skills,-skill_total), y = skill_total)) +geom_col()+labs(title = "Total Skill Distribution", x='Skill', y='Count')
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
