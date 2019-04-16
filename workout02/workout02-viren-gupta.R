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
   titlePanel("Workout 2 - Viren Gupta"),
   
   fluidRow(
     column(4,  sliderInput("initial",
                            label = "Initial Amount",
                            min = 0,
                            max = 100000,
                            value = 1000,
                            step = 500), 
              sliderInput("contrib",
                        label = "Annual Contribution",
                        min = 0,
                        max = 50000,
                        value = 2000,
                        step = 500)
            ),
        column(4,  sliderInput("rrate",
                          label = "Return Rate (in %)",
                                   min = 0,
                                   max = 20,
                                   value = 5,
                                   step = 0.1), 
                   sliderInput("grate",
                               label = "Growth Rate (in %)",
                               min = 0,
                               max = 20,
                               value = 2,
                               step = 0.1)      
                
   ),
   column(4,  sliderInput("years",
                          label = "Years",
                          min = 0,
                          max = 50,
                          value = 20,
                          step = 1), 
          selectInput("facet",
                      label = "Facet?",
                      choices = c("No", "Yes")
            
          )   
          
   )
   ),
   mainPanel(
     h4("Timelines"),
     plotOutput("time_plot"),
     h4("Balances"),
     tableOutput("balances")
   )
)     


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$balances <- renderTable({
    no_contrib_list <- c(input$initial * ((1 + (input$rrate/100)) ** 0))
    fixed_contrib_list <- c(input$initial)
    growing_contrib_list <- c(input$initial)
    for (i in 1:input$years) {
      no_contrib_list[i + 1] <- input$initial * ((1 + input$rrate/100) ** i)
      fixed_contrib_list[i + 1] <- no_contrib_list[i + 1] + input$contrib * ((((1 + (input$rrate/100)) ** i) -  1) / (input$rrate/100))
      growing_contrib_list[i + 1] <- no_contrib_list[i + 1] + input$contrib * ((((1 + input$rrate/100) ** i) - ((1 + input$grate/100) ** i))/ ((input$rrate - input$grate)/100))
    }
    balances <- data.frame(year = 0:input$years, no_contrib =  no_contrib_list, fixed_contrib = fixed_contrib_list, growing_contrib = growing_contrib_list)
  })
  
  output$time_plot <- renderPlot({
    no_contrib_list <- c(input$initial * ((1 + (input$rrate/100)) ** 0))
    fixed_contrib_list <- c(input$initial)
    growing_contrib_list <- c(input$initial)
    for (i in 1:input$years) {
      no_contrib_list[i + 1] <- input$initial * ((1 + input$rrate/100) ** i)
      fixed_contrib_list[i + 1] <- no_contrib_list[i + 1] + input$contrib * ((((1 + (input$rrate/100)) ** i) -  1) / (input$rrate/100))
      growing_contrib_list[i + 1] <- no_contrib_list[i + 1] + input$contrib * ((((1 + input$rrate/100) ** i) - ((1 + input$grate/100) ** i))/ ((input$rrate - input$grate)/100))
    }
    temp <- data.frame(year = 0:input$years, types = c(rep("no_contrib", input$years + 1),rep("fixed_contrib", input$years + 1), rep("growing_contrib", input$years + 1)) , value = c(no_contrib_list, fixed_contrib_list, growing_contrib_list))
    if (input$facet == "No") {
      ggplot(data = temp, aes(x = year, y = value, group = types)) +
        geom_point(aes(color = types)) +
        geom_path(aes(color = types)) +
        ggtitle("Three modes of investing")
    } else {
      ggplot(data = temp, aes(x = year, y = value, group = types, fill = types)) +
        geom_point(aes(color = types)) +
        geom_area(alpha = 0.4) +
        geom_path(aes(color = types)) +
        ggtitle("Three modes of investing") +
        facet_wrap(~ types, ncol = 3)
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

