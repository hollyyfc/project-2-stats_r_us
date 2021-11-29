#packages ----
library(shiny)
library(imager)
library(colourpicker)
library(palmerpenguins)
library(ggplot2)
library(tidyverse)
library(scales)
library(magick)
library(ggplotify)
library(patchwork)
library(ggtern)

#constants ----
#matrix values for c, RGB to LMS. c = change
c.c = 0.31399022
c.cc = 0.63951294
c.ccc = 0.04649755
cc.c = 0.15537241
cc.cc = 0.75789446
cc.ccc = 0.08670142
ccc.c = 0.01775239
ccc.cc = 0.10944209
ccc.ccc = 0.87256922

#matrix values for t, LMS to RGB. u = undo (u = inv(c))
u.u = 5.47221206
u.uu = -4.6419601
u.uuu = 0.16963708
uu.u = -1.1252419
uu.uu = 2.29317094
uu.uuu = -0.1678952
uuu.u = 0.02980165
uuu.uu = -0.19318073
uuu.uuu = 1.16364789

#matrix values for p, p = protanopia
#missing values are either O or 1
p.pp = 1.05118294
p.ppp = -0.05116099

#matrix values for d, d = deuteranopia
#missing values are either O or 1
dd.d = 0.9513092
dd.ddd = 0.04866992

#matrix values for t, t = tritanopia
#missing values are either O or 1
ttt.t = -0.86744736
ttt.tt = 1.86727089

#hex functions ----

proHex <- function(hex) {

  rgb.array = col2rgb(hex)

  r1 = rgb.array[1] / 255
  g1 = rgb.array[2] / 255
  b1 = rgb.array[3] / 255

  l = r1*c.c      + g1*c.cc   + b1*c.ccc
  m = r1*cc.c     + g1*cc.cc  + b1*cc.ccc
  s = r1*ccc.c    + g1*ccc.cc + b1*ccc.ccc

  l1 =              m*p.pp    + s*p.ppp
  m1 =              m
  s1 =                          s

  rNew = l1*u.u   + m1*u.uu   + s1*u.uuu
  gNew = l1*uu.u  + m1*uu.uu  + s1*uu.uuu
  bNew = l1*uuu.u + m1*uuu.uu + s1*uuu.uuu

  rNew = round(case_when(rNew > 1 ~ 1, rNew < 0 ~ 0, TRUE ~ rNew) * 255)
  gNew = round(case_when(gNew > 1 ~ 1, gNew < 0 ~ 0, TRUE ~ gNew) * 255)
  bNew = round(case_when(bNew > 1 ~ 1, bNew < 0 ~ 0, TRUE ~ bNew) * 255)

  newHex = toupper(rgb2hex(rNew, gNew, bNew))
  return(newHex)
}

#ui and server ----

# ui
## Add sliding size scale for plot?
ui <- fluidPage(
  fluidRow(

    column(2, colourInput("col1", "Choose 1st color"), submitButton("Update", icon("refresh"))),
    column(2, colourInput("col2", "Choose 2nd color")),
    column(2, colourInput("col3", "Choose 3rd color")),
    column(2, colourInput("col4", "Choose 4th color")),
    column(2, colourInput("col5", "Choose 5th color")),
    column(2, colourInput("col6", "Choose 6th color"),

    )
  ),
  fluidRow(
    column(6,
           plotOutput(outputId = "plot1")),
    column(6,
           plotOutput(outputId = "plot2")),
    column(6,
           plotOutput(outputId = "plot3")),
    column(6,
           plotOutput(outputId = "plot4")),
    column(6,
           plotOutput(outputId = "plot5")),
    column(6,
           plotOutput(outputId = "plot6"))
    ),
  fluidRow(
    column(12,
           verbatimTextOutput("result"))
  )
)


# server
server <- function(input, output, session){

  # Output plots
  # output$plot1 <- renderPlot({
  #   ggplot(data = penguins,
  #          aes(x = flipper_length_mm,
  #              y = body_mass_g)) +
  #     geom_point(aes(color = species,
  #                    shape = species),
  #                size = 3,
  #                alpha = 0.8) +
  #     theme_minimal() +
  #     scale_color_manual(values = c(input$col1,input$col2,input$col3))
  #   })

  # Output bars
  colorsquares <- data.frame(
    xmin = seq(1, 6),
    xmax = seq(2, 7),
    ymin = rep(0, 6),
    ymax = rep(1, 6)
  )

  squareplot <-
    ggplot(colorsquares, aes(fill=factor(xmin))) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              color = "black", show.legend = F) +
    coord_fixed() +
    theme_void() +
    theme(plot.title = element_text(size = 25, hjust = 0.5, vjust = 3))

  output$plot1 <- renderPlot({

      squareplot +
      scale_fill_manual(values = c("blue", "red", "yellow", "green", "purple", "black")) +
      labs(title = "test")
  })

  output$plot2 <- renderPlot({

      squareplot +
      scale_fill_manual(values = c("blue", "red", "yellow", "green", "purple", "black")) +
      labs(title = "test")
  })

  output$plot3 <- renderPlot({

      squareplot +
      scale_fill_manual(values = c("blue", "red", "yellow", "green", "purple", "black")) +
      labs(title = "test")
  })

  output$plot4 <- renderPlot({

      squareplot +
      scale_fill_manual(values = c("blue", "red", "yellow", "green", "purple", "black")) +
      labs(title = "test")
  })


  output$plot5 <- renderPlot({

      squareplot +
      scale_fill_manual(values = c("blue", "red", "yellow", "green", "purple", "black")) +
      labs(title = "test")
  })

  output$plot6 <- renderPlot({

      squareplot +
      scale_fill_manual(values = c("blue", "red", "yellow", "green", "purple", "black")) +
      labs(title = "test")
  })




  # Output result text
  output$result <- renderText({paste(
    input$col1, " ", input$col2, " ", input$col3, "...", "are the original colors\n",
    proHex(input$col1), " ", proHex(input$col2), " ", proHex(input$col3), "...", "are the protanaisfdfaef colors", sep = "") })
}



# app
shinyApp(ui = ui, server = server)






