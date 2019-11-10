#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Three Investing Scenarios"),

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
     #   sidebarPanel(
    fluidRow(
        column(3,  
          sliderInput(inputId = "uvar",
                        label = "Initial Amount",
                        min = 0,
                        max = 10000,
                        step = 100,
                        pre = "$",
                        value = 1000),
            sliderInput(inputId = "vvar",
                        label = "Annual Contribution",
                        min = 0,
                        max = 5000,
                        step = 100,
                        pre = "$",
                        value = 200),
            sliderInput(inputId = "gvar",
                        label = "Annual Growth Rate(in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 2)),
        
            column(3, 
            sliderInput(inputId = "mu1",
                        label = "High Yield rate(in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 2),
            sliderInput(inputId = "mu2",
                        label = "Fixed Income rate(in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 5),
            sliderInput(inputId = "mu3",
                        label = "US Equity rate(in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 10)),

            column(3,
            sliderInput(inputId = "sigma1",
                        label = "High Yield volatility(in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 0.1),
            sliderInput(inputId = "sigma2",
                        label = "Fixed Income volatility(in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 4.5),
            sliderInput(inputId = "sigma3",
                        label = "US Equity volatility(in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 15)),
        
        column(3,
            sliderInput(inputId = "year",
                        label = "Years",
                        min = 0,
                        max = 50,
                        step = 1,
                        value = 20),
            numericInput(inputId = "seed", label = "Random Seed", 12345),
            
            selectInput(inputId = "dataset", 
                        label = "Choose a dataset:",
                        choices = c("Yes", "No"), 
                        selected  = "Yes")),
    

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput(outputId = "distPlot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    output$distPlot <- renderPlot({
        set.seed(input$seed)
        
        
        # generate the amount for High Yield\
        v <- input$vvar
        g <- input$gvar
        
         y1 = rep(input$uvar, input$year +1)
        u1 <- input$uvar
        rate_bonds <- input$mu1/100   # High Yield annual avg return 
        vol_bonds <-  input$sigma1/100   # U.S. annual volatility 
        #r1 <- rnorm(1, mean = rate_bonds, sd = vol_bonds)
        for (i in 1:input$year){
            r1 <- rnorm(1, mean = rate_bonds, sd = vol_bonds)
            y1[i+1] <- u1*(1+r1) + v*(1+ g/100)**(i-1) 
            u1<- y1[i+1]
        }
        
        
        y2 = rep(input$uvar, input$year+1)
        u2 <- input$uvar
        rate_bonds <- input$mu2/100
        vol_bonds <-  input$sigma2/100 
        #r2 <- rnorm(1, mean = rate_bonds, sd = vol_bonds)
        
        for (i in 1:input$year){
            r2 <- rnorm(1, mean = rate_bonds, sd = vol_bonds)
            y2[i+1] <- u2*(1+r2) + v*((1+ g/100)**(i-1)) 
            u2<- y2[i+1] 
        }
        
        
        y3 = rep(input$uvar, input$year+1)
        u3 <- input$uvar
        rate_bonds <- input$mu3/100  
        vol_bonds <-  input$sigma3/100
        #r3 <- rnorm(1, mean = rate_bonds, sd = vol_bonds)
        
        for (i in 1:input$year){
            r3 <- rnorm(1, mean = rate_bonds, sd = vol_bonds)
            y3[i+1] <- u3*(1+r3) + v*((1+ g/100)**(i-1))
            u3<- y3[i+1]
        }
        
        
        
        dat <- data.frame(
            year = rep(0:input$year, 3),
            amount = c(y1,y2,y3),
            name = factor(rep(c("high_yield","us_bonds","us_stocks"), each = input$year+1 ))
        )
        
        
        if(input$dataset  == "Yes"){
        ggplot(data = dat, aes(x = year, y = amount, fill = name, color = name)) +
            geom_area(position="stack",  alpha = 0.6) +
            ggtitle("Three indices")+
            geom_point(size = 1)+
            facet_grid(.~name)
        }else{
        ggplot(data = dat, aes(x = year, y = amount, group = name, color = name)) +
            geom_path(aes(color = name)) +
            ggtitle("Three indices")+
            geom_point(size = 1)+
            ggtitle("Three indices") 
            
            
    }
        
        
        
        
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
