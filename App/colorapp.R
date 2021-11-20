library(shiny)


# Define UI for app = create layout ----

ui <- navbarPage("Exploring Color Blindness",

                 navbarMenu("What is Color Blindness?",

                            tabPanel("Why should you care?"),

                            tabPanel("What does it look like?")),


                 tabPanel("Are You Color Blind?"),

                 tabPanel("Sliding Scale of Color Blindness")

)

# Define server logic ----

server <- function(input, output) {

}


# run app -----

shinyApp(ui, server)

