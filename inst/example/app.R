#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reactor)

# Define UI for application that draws a histogram
ui <- navbarPage(title = 'Reactor Test',
          tabPanel('Old Faithful',
             # Application title
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
                   titlePanel("Old Faithful Geyser Data"),
                   plotOutput("distPlot")
                )
             )
          ),
          tabPanel('Reactor', reactorUI('faithful'))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   data <- reactive({ faithful })

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- data()[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })

   # add the reactor module
   r <- reactorModule('faithful')
}

# Run the application
shinyApp(ui = ui, server = server)
