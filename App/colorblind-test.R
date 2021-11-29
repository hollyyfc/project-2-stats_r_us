library(shiny)
library(imager)
library(colourpicker)
library(palmerpenguins)
library(ggplot2)

library(tidyverse)
library(scales)
library(imager)
library(magick)
library(ggplotify)
library(patchwork)
library(ggtern)


# load image
pics <- tibble::tribble(
  ~name, ~ id,
  "red-green test", "super-mario",
  "blue-yellow test", "starry-night",
  "general", "colorful-flowers"
)

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

  rNew = round(case_when(rNew > 1 ~ 1, rNew < 0 ~ 0, TRUE ~ rNew)) * 255
  gNew = round(case_when(gNew > 1 ~ 1, gNew < 0 ~ 0, TRUE ~ gNew)) * 255
  bNew = round(case_when(bNew > 1 ~ 1, bNew < 0 ~ 0, TRUE ~ bNew)) * 255

  newHex = rgb2hex(rNew, gNew, bNew)
  return(newHex)
}


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
           actionButton("act", "Check!", class = "btn-success btn-block")
    ),
    column(2,
           colourInput("col2", "Choose 2nd color", palette = "limited"),
           colourInput("col4", "Choose 4th color", palette = "limited"),
           submitButton("Update View", icon("refresh"))
    )
  ),
  fluidRow(
    verbatimTextOutput("result"),
    column(5,
           plotOutput(outputId = "plot1")),
    column(5,
           plotOutput(outputId = "plot2"))
    )
)


# server
server <- function(input, output, session){

  # Output photo
  output$photo <- renderImage({
    list(
      src = normalizePath(file.path(filename = "~/R/project-2-stats_r_us/Image/",
                                    paste0(input$id, ".jpeg"))),
      width = 450,
      height = 300
    )
  }, deleteFile = FALSE)

  # Output result
  # observeEvent(input$act,{
  #   output$result <- renderText({ paste("You chose",
  #                                       input$col1, input$col2, input$col3, input$col4,
  #                                       "as the dominant color of",
  #                                       input$id) })
  # }, once = TRUE)

  # Output result text
  output$result <- renderText({ paste("You chose",
                                      input$col1, input$col2, input$col3, input$col4,
                                      "as the dominant color of",
                                      input$id) })

  output$plot1 <- renderPlot({
    ggplot(data = penguins,
           aes(x = flipper_length_mm,
               y = body_mass_g)) +
      geom_point(aes(color = species,
                     shape = species),
                 size = 3,
                 alpha = 0.8) +
      theme_minimal() +
      scale_color_manual(values = c(input$col1,input$col2,input$col3))
    })

  output$plot2 <- renderPlot({
    ggplot(data = penguins,
           aes(x = flipper_length_mm,
               y = body_mass_g)) +
      geom_point(aes(color = species,
                     shape = species),
                 size = 3,
                 alpha = 0.8) +
      theme_minimal() +
      scale_color_manual(values = c(proHex(input$col1),
                                    proHex(input$col2),
                                    proHex(input$col3)))
  })


  # values<- reactiveValues()
  # values$df<- data.frame()
  #
  # observeEvent(input$act, {
  #   pic <- input$id
  #   col1 <- input$col1
  #   col2 <- input$col2
  #   col3 <- input$col3
  #   col4 <- input$col4
  #   score <- 0
  #
  #   colorlist <- list(col1, col2, col3, col4)
  #
  #   # new_row <- data.frame(rank,name,college,gender,team,score)
  #   #
  #   # values$df<- rbind(values$df, new_row)
  #   # values$df<- values$df[order(values$df$score,decreasing=TRUE),]
  #   # values$df$rank<- 1:nrow(values$df)
  # })
  #
  # output$test <- renderPrint({
  #   df <- data.frame()
  #   for (team_name in unique(values$df$team)){ #this does NOT work
  #     local({
  #       rank <- 0
  #       team <- team_name
  #       score <- format(mean(values$df[values$df$team==team_name,]$score), digits=4)
  #
  #       new_row<- data.frame(rank, team, score)
  #
  #       df <<- rbind(df, new_row)
  #       df <<- df[order(df$score,decreasing=TRUE),]
  #       df$rank <<- 1:nrow(df)
  #     })
  #   }
  #   return(df)
  # })

}



# app
shinyApp(ui = ui, server = server)






