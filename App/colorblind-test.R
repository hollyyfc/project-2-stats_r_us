#packages ----
library(shiny)
library(imager)
library(colourpicker)
library(ggplot2)
library(tidyverse)
library(scales)
library(magick)
library(ggplotify)
library(patchwork)
library(shinyalert)
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

deutHex <- function(hex) {

  rgb.array = col2rgb(hex)

  r1 = rgb.array[1] / 255
  g1 = rgb.array[2] / 255
  b1 = rgb.array[3] / 255

  l = r1*c.c      + g1*c.cc   + b1*c.ccc
  m = r1*cc.c     + g1*cc.cc  + b1*cc.ccc
  s = r1*ccc.c    + g1*ccc.cc + b1*ccc.ccc

  l1 = l
  m1 = l*dd.d                 + s*dd.ddd
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

triHex <- function(hex) {

  rgb.array = col2rgb(hex)

  r1 = rgb.array[1] / 255
  g1 = rgb.array[2] / 255
  b1 = rgb.array[3] / 255

  l = r1*c.c      + g1*c.cc   + b1*c.ccc
  m = r1*cc.c     + g1*cc.cc  + b1*cc.ccc
  s = r1*ccc.c    + g1*ccc.cc + b1*ccc.ccc

  l1 = l
  m1 =            + m
  s1 = l*ttt.t    + m*ttt.tt

  rNew = l1*u.u   + m1*u.uu   + s1*u.uuu
  gNew = l1*uu.u  + m1*uu.uu  + s1*uu.uuu
  bNew = l1*uuu.u + m1*uuu.uu + s1*uuu.uuu

  rNew = round(case_when(rNew > 1 ~ 1, rNew < 0 ~ 0, TRUE ~ rNew) * 255)
  gNew = round(case_when(gNew > 1 ~ 1, gNew < 0 ~ 0, TRUE ~ gNew) * 255)
  bNew = round(case_when(bNew > 1 ~ 1, bNew < 0 ~ 0, TRUE ~ bNew) * 255)

  newHex = toupper(rgb2hex(rNew, gNew, bNew))
  return(newHex)
}

monoHex <- function(hex) {

  rgb.array = col2rgb(hex)

  r1 = rgb.array[1] / 255
  g1 = rgb.array[2] / 255
  b1 = rgb.array[3] / 255

  l = r1*c.c      + g1*c.cc   + b1*c.ccc
  m = r1*cc.c     + g1*cc.cc  + b1*cc.ccc
  s = r1*ccc.c    + g1*ccc.cc + b1*ccc.ccc

  l1 =                          s
  m1 =                          s
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
ui <- fluidPage(
  titlePanel("Are You Colorblind?"),

  ### FORMAT TEXTS!!!!!
  "Want to check if you might have colorblindness? Take this test!",
  "\nFirst, randomly choose six different colors. Then, navigate to the sidebar\nsection where
  you choose an unknown type of colorblindness.",
  "\nNext, compare the newly generated plots.",
  "\nFinally, choose the green button on top or red button below it
  to check your results.",

  HTML("<h4> Step 1 </h4>"),
  fluidRow(
    column(2, colourInput("col1", "Choose 1st color:")),
    column(2, colourInput("col2", "Choose 2nd color:")),
    column(2, colourInput("col3", "Choose 3rd color:")),
    column(2, colourInput("col4", "Choose 4th color:")),
    column(2, colourInput("col5", "Choose 5th color:")),
    column(2, colourInput("col6", "Choose 6th color:")),
    style='padding:0px; padding-top:0px; padding-bottom:10px'),

  sidebarLayout(
    sidebarPanel(
      HTML("<h4> Step 2 </h4>"),
      radioButtons("colorblind", "Choose an unknown type of colorblindness:",
                   c("A", "B", "C", "D")),
      "\n-----------------------------------------------------------------------",
      HTML("<h4> Step 3 </h4>"),
      fluidRow(
        HTML("<b>After comparing the two plots, check your result by hitting one of
      the buttons below:<b>"),
        style='padding-top:5px; padding-bottom:10px; padding-left:20px; padding-right:20px'),
      useShinyalert(),
      actionButton("correct", "I can see very different colors in both plots.",
                   icon("laugh-wink"), class = "btn-success btn-block"),
      actionButton("check", "After comparison, I see similar colors in both plots.",
                   icon("flushed"), class = "btn-danger btn-block "),
      fluidRow(
        "***Disclaimer: This test is not a professional diagnosis.***",
        style='padding-top:20px; padding-bottom:10px; padding-left:20px; padding-right:10px')),

    mainPanel(
      ### MARGIN LAYOUT!!!
      fluidRow(
        column(6,
               offset = 0,
               style='padding:0px; padding-top:0px; padding-bottom:0px',
               plotOutput(outputId = "graph1")),
        column(6,
               offset = 0,
               style='padding:0px; padding-top:0px; padding-bottom:0px',
               plotOutput(outputId = "graph2"),
               plotOutput(outputId = "plot2"))
      )
    )
  )
)


# server
server <- function(input, output, session){

  ## Output bars ----
  colorsquares <- data.frame(xmin = seq(1, 6), xmax = seq(2, 7),
                             ymin = rep(0, 6), ymax = rep(1, 6))

  squareplot <-
    ggplot(colorsquares, aes(fill=factor(xmin))) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              color = "black", show.legend = F) +
    coord_fixed() +
    theme_void() +
    theme(plot.title = element_text(size = 25, hjust = 0.5, vjust = 3)) +
    labs(title = "Colorblind Plot")


  ## Output art ----

  build_art <- function(points, angle, adjustment) {
    tibble(i = 1:points,
           t = (1:points) * angle + adjustment,
           x = sin(t),
           y = cos(t),
           g = sample(6, points, TRUE))}

  art <- build_art(angle = pi * (3 - sqrt(5)), points = 500, adjustment = 0) %>%
    ggplot(aes(x = x * t, y = y * t)) +
    geom_point(aes(size = factor(g), color = factor(g), fill = factor(g)),
               alpha = 0.5, shape = "square filled", show.legend = FALSE) +
    coord_equal() +
    theme_void()

  output$graph1 <- renderPlot({

    plotA <-
      squareplot +
      scale_fill_manual(values = c(input$col1, input$col2, input$col3,
                                   input$col4, input$col5, input$col6)) +
      labs(title = "Original Plot")

    plotB <-
      art +
      scale_fill_manual(values = c(input$col1, input$col2, input$col3,
                                   input$col4, input$col5, input$col6)) +
      scale_color_manual(values = c(input$col1, input$col2, input$col3,
                                    input$col4, input$col5, input$col6))
    plotA / plotB

  })


  observe({

    if (input$colorblind == "A") {

      output$graph2 <- renderPlot({
        plotA <-
          squareplot +
          scale_fill_manual(values = c(proHex(input$col1), proHex(input$col2),
                                       proHex(input$col3), proHex(input$col4),
                                       proHex(input$col5), proHex(input$col6)))
        plotB <-
          art +
          scale_fill_manual(values = c(proHex(input$col1), proHex(input$col2),
                                       proHex(input$col3), proHex(input$col4),
                                       proHex(input$col5), proHex(input$col6)))+
          scale_color_manual(values = c(proHex(input$col1), proHex(input$col2),
                                        proHex(input$col3), proHex(input$col4),
                                        proHex(input$col5), proHex(input$col6)))
        plotA / plotB
      })
    }

    else if (input$colorblind == "B") {

      output$graph2 <- renderPlot({
        plotA <-
          squareplot +
          scale_fill_manual(values = c(deutHex(input$col1), deutHex(input$col2),
                                       deutHex(input$col3), deutHex(input$col4),
                                       deutHex(input$col5), deutHex(input$col6)))

        plotB <-
          art +
          scale_fill_manual(values = c(deutHex(input$col1), deutHex(input$col2),
                                       deutHex(input$col3), deutHex(input$col4),
                                       deutHex(input$col5), deutHex(input$col6)))+
          scale_color_manual(values = c(deutHex(input$col1), deutHex(input$col2),
                                        deutHex(input$col3), deutHex(input$col4),
                                        deutHex(input$col5), deutHex(input$col6)))

        plotA / plotB
      })
    }

    else if (input$colorblind == "C") {

      output$graph2 <- renderPlot({

        plotA <-
          squareplot +
          scale_fill_manual(values = c(triHex(input$col1), triHex(input$col2),
                                       triHex(input$col3), triHex(input$col4),
                                       triHex(input$col5), triHex(input$col6)))

        plotB <-
          art +
          scale_fill_manual(values = c(triHex(input$col1), triHex(input$col2),
                                       triHex(input$col3), triHex(input$col4),
                                       triHex(input$col5), triHex(input$col6)))+
          scale_color_manual(values = c(triHex(input$col1), triHex(input$col2),
                                        triHex(input$col3), triHex(input$col4),
                                        triHex(input$col5), triHex(input$col6)))
        plotA / plotB
      })
    }

    else {



      output$graph2 <- renderPlot({

        plotA <-
          squareplot +
          scale_fill_manual(values = c(monoHex(input$col1), monoHex(input$col2),
                                       monoHex(input$col3), monoHex(input$col4),
                                       monoHex(input$col5), monoHex(input$col6)))

        plotB <-
          art +
          scale_fill_manual(values = c(monoHex(input$col1), monoHex(input$col2),
                                       monoHex(input$col3), monoHex(input$col4),
                                       monoHex(input$col5), monoHex(input$col6)))+
          scale_color_manual(values = c(monoHex(input$col1), monoHex(input$col2),
                                        monoHex(input$col3), monoHex(input$col4),
                                        monoHex(input$col5), monoHex(input$col6)))
        plotA / plotB
      })
    }

  })


  ## Output result text ----

  observeEvent(input$correct, {

    if (input$colorblind == "A") {
      type_color <- "protanopia"}
    else if (input$colorblind == "B") {
      type_color <- "deuteranopia"}
    else if (input$colorblind == "C") {
      type_color <- "tritanopia"}
    else {
      type_color <- "monochromatism"}

    shinyalert(
      "Congratulations!",
      paste0("You are showing no symptoms of", " ", type_color, "."),
      type = "success")
  })


  observeEvent(input$check, {

    if (input$colorblind == "A") {
      type_color <- "protanopia"}
    else if (input$colorblind == "B") {
      type_color <- "deuteranopia"}
    else if (input$colorblind == "C") {
      type_color <- "tritanopia"}
    else {
      type_color <- "monochromatism"}

    shinyalert(
      paste0("Oops! You may have symptoms of", " ", type_color, "."),
      "Try doing the test again with different colors \nor seek a professional colorblindness test.",
      type = "error")
  })

}



# app
shinyApp(ui = ui, server = server)


