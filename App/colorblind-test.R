library(shiny)
library(imager)
library(colourpicker)

# load image
pics <- tibble::tribble(
  ~name, ~ id,
  "red-green test", "super-mario",
  "blue-yellow test", "starry-night",
  "general", "colorful-flowers"
)

# ui
## Add sliding size scale for plot?
ui <- fluidPage(
  fluidRow(
    column(5,
           selectInput("id", "Pick a test",
                       choices = setNames(pics$id, pics$name)),
           imageOutput("photo")
    ),
    column(2,
           colourInput("col1", "Choose 1st color", palette = "limited"),
           colourInput("col3", "Choose 3rd color", palette = "limited"),
           actionButton("act", "Check!", class = "btn-lg btn-success btn-block")
    ),
    column(2,
           colourInput("col2", "Choose 2nd color", palette = "limited"),
           colourInput("col4", "Choose 4th color", palette = "limited")
    )
  ),
  fluidRow(
    textOutput("result")
  )
)


# server
server <- function(input, output, session){

  output$photo <- renderImage({
    list(
      src = normalizePath(file.path(filename = "/home/guest/Project 2/project-2-stats_r_us/Image",
                                    paste0(input$id, ".jpeg"))),
      width = 300,
      height = 200
    )
  }, deleteFile = FALSE)

  observeEvent(input$act, {cat("Showing", input$id)})

  output$result <- renderText({
    paste("You chose",
          input$col1, input$col2, input$col3, input$col4,
          "as the dominant color")
      })
  }



# app
shinyApp(ui = ui, server = server)






