library(shiny)


# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Exploring Color Blindness"),


    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("What is Color Blindness?", plotOutput("colorblindness")),
                  tabPanel("Are You Color Blind?", verbatimTextOutput("areyou")),
                  tabPanel("Color Blindness Sliding Scale", tableOutput("scale"))


    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

}

shinyApp(ui, server)

runApp("~/shinyapp")
