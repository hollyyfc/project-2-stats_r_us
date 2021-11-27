library(shiny)
library(ggplot2)


# constants, data, functions, etc. ----

colors <- read_csv("~/R/project-2-stats_r_us/Data/wikipedia_x11_colors.csv")
colors <- colors %>%
  rename(r = `Red (8 bit)`,
         b = `Blue (8 bit)`,
         g = `Green (8 bit)`) %>%
  select(Name, r, b, g)

# Define UI for app = create layout ----

ui <- navbarPage("Exploring Color Blindness",

                 navbarMenu("What is Color Blindness?",

                            tabPanel("Why should you care?"),

                            tabPanel("What does it look like?")),


                 tabPanel("Are You Color Blind?"),

                 tabPanel("Sliding Scale of Color Blindness",
                          sliderInput(inputId = "x", label = "Severity",
                                      min = 0, max = 1, value = 0),

                          selectInput("filter", "Type of Colorblindess:",
                                      c("Protanopia" = "protanopia",
                                        "Deuteranopia" = "deuteranopia",
                                        "Tritanopia" = "tritanopia",
                                        "Monochromatism" = "monochromatism")),

                          selectInput("image", "Image:",
                                      c("Mario Brothers" = "mario",
                                        "Parrot" = "parrot",
                                        "Starry Night" = "starry",
                                        "Ball Pit" = "balls")),
                          mainPanel(plotOutput("plot1")))


)

# Define server logic ----

server <- function(input, output) {
  output$plot1 <- renderPlot({
    p <- ggplot(data=colors,aes(x=r,y=b,color="red")) +
      geom_point()
    print(p)
  })
}



# run app -----

shinyApp(ui, server)

