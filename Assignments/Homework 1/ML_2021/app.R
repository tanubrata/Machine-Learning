#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(DT)
library(ggplot2)
data(iris)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Analyze Iris Dataset: Machine Learning - FALL2021"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "var", 
                        "Select variable:",
                        choices = c("Sepal.Length"=1,
                                    "Sepal.Width" = 2,
                                    "Petal.Length"=3,
                                    "Petal.Width"=4),
                        selected = 1
            ),
            
            sliderInput(inputId = "bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h3("Histogram with Normal Distribution"),
            textOutput("text1"),
            plotOutput("hist"),
            
            h3("Boxplot of Iris Dataset grouped by Species"),
            textOutput("text2"),
            plotOutput("bar"),
            
            h3("Summary of Iris Dataset: Mean, Median, IQR & SD"),
            textOutput("text3"),
            verbatimTextOutput("summary"),
            
            textOutput("text4"),
            tableOutput("summary1"),
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$text1 <- renderText({
        paste(" This is a histogram of each variable 
              from the Iris dataset. The curve in red is the normal distribution.
              Set the variable and number of bins from left panel for visualization.")
    })
    
    output$hist <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- as.numeric(input$var)
        bins <- seq(0, max(iris[,x]), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        histogram = hist(iris[,x], breaks = bins, col = 'darkgray',
                         border = 'white', main = "Histogram with Distribution Curve",
                         xlab=names(iris[x]))
        
        xrange <- seq(min(iris[,x]), max(iris[,x]), length = 40) 
        yrange <- dnorm(xrange, mean = mean(iris[,x]), sd = sd(iris[,x])) 
        yrange <- yrange * diff(histogram$mids[1:2]) * length(iris[,x]) 
        
        lines(xrange, yrange, col = "red", lwd = 2)
    })
    
    output$text2 <- renderText({
        paste(" This is a Barplot for each variable based on Species 
              from the Iris dataset. Change the variable from the left 
              panel to see the plot for that variable and grouped by Species.")
    })
    
    output$bar <- renderPlot({
        # generate bins based on input$bins from ui.R
        column <- as.numeric(input$var)
        
        #bins <- seq(0, max(iris[,x]), length.out = input$bins + 1)
        
        #iris.mean <- aggregate(iris, by = list(iris$Species), FUN = mean)
        #iris.mean = iris.mean[,c(2,3,4,5,1)]
        boxplot(iris[,column]~iris[,5], ylab = colnames(iris[,column]), xlab = "Species")
        
        # draw the histogram with the specified number of bins
        #ggplot(iris, aes(x = Species,y = iris[,column])) + geom_bar(stat = "identity")
        
    })
    
    output$text3 <- renderText({
        
        paste(" This is a Summary for each variable based on Species 
              from the Iris dataset. Change the variable from the left 
              panel to see the data for that variable and grouped by Species.")
    })
    
    output$summary <- renderPrint({
        column    <- as.numeric(input$var)
        str1 = print(tapply(iris[,column], iris$Species, summary))
    })
    
    output$text4 <- renderText({
        
        paste("The above summary table shows mostly everything based on 
        variable selected and grouped by Species but it does not give IQR, though we can get it from
        subtracting 1st Quantile from 3rd Quantile to get it. I also tried another way shown below 
        but it is kind of not organized very well but it changes based on the variable selected for both the ways. 
        Since the above one is missing IQR and SD, that is why I used the one below and the one below is more specific."
        )
    })
    
    output$summary1 <- renderTable({
        column    <- as.numeric(input$var)
        y <- list(iris$Species)
        data.frame(
        #summary(iris[,as.numeric(input$var)])
        #IQR(iris[,as.numeric(input$var)])
        #str1 = print(tapply(iris[,column], iris$Species, summary))
        #str3 = print("Mean")
        setNames(aggregate(iris[,column], y, mean), c("Species", "Mean_values")),
        #colnames(Mean) <- c("Species", "Mean_values"),
        
        #str4 = print("Median")
        setNames(aggregate(iris[,column], y, median),c("Species", "Median_values")),
        
        #str6 = print("IQR")
        setNames(aggregate(iris[,column], y, IQR), c("Species", "IQR")),
        
        #str8 = print("SD")
        setNames(aggregate(iris[,column], y, sd), c("Species", "SD"))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
