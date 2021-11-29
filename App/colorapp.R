# packages  ----

library(shiny)
library(ggplot2)
library(tidyverse)
library(scales)
library(imager)
library(magick)
library(ggplotify)
library(patchwork)
library(shinyalert)
library(shinybusy)

# data and images ----

balls <- load.image('~/R/project-2-stats_r_us/Image/ball.jpeg')
flowers <- load.image('~/R/project-2-stats_r_us/Image/flower.jpeg')
mario <- load.image('~/R/project-2-stats_r_us/Image/super-mario.jpeg')
parrots <- load.image(system.file("extdata/parrots.png", package = "imager"))
starry <- load.image('~/R/project-2-stats_r_us/Image/starry-night.jpeg')

# sliderFunctions.rmd ----

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

protanopia <- function(r, g, b) {
  r1 = r
  g1 = g
  b1 = b

  l = r1*c.c      + g1*c.cc   + b1*c.ccc
  m = r1*cc.c     + g1*cc.cc  + b1*cc.ccc
  s = r1*ccc.c    + g1*ccc.cc + b1*ccc.ccc

  l1 = l*(1-x)    + m*p.pp*x  + s*p.ppp*x
  m1 =              m
  s1 =                          s

  rNew = l1*u.u   + m1*u.uu   + s1*u.uuu
  gNew = l1*uu.u  + m1*uu.uu  + s1*uu.uuu
  bNew = l1*uuu.u + m1*uuu.uu + s1*uuu.uuu

  rNew = case_when(rNew > 1 ~ 1, rNew < 0 ~ 0, TRUE ~ rNew)
  gNew = case_when(gNew > 1 ~ 1, gNew < 0 ~ 0, TRUE ~ gNew)
  bNew = case_when(bNew > 1 ~ 1, bNew < 0 ~ 0, TRUE ~ bNew)

  return(rgb(rNew, gNew, bNew))

}

deuteranopia <- function(r, g, b) {
  r1 = r
  g1 = g
  b1 = b

  l = r1*c.c      + g1*c.cc   + b1*c.ccc
  m = r1*cc.c     + g1*cc.cc  + b1*cc.ccc
  s = r1*ccc.c    + g1*ccc.cc + b1*ccc.ccc

  l1 = l
  m1 = l*dd.d*x   + m*(1-x)   + s*dd.ddd*x
  s1 =                          s

  rNew = l1*u.u   + m1*u.uu   + s1*u.uuu
  gNew = l1*uu.u  + m1*uu.uu  + s1*uu.uuu
  bNew = l1*uuu.u + m1*uuu.uu + s1*uuu.uuu

  rNew = case_when(rNew > 1 ~ 1, rNew < 0 ~ 0, TRUE ~ rNew)
  gNew = case_when(gNew > 1 ~ 1, gNew < 0 ~ 0, TRUE ~ gNew)
  bNew = case_when(bNew > 1 ~ 1, bNew < 0 ~ 0, TRUE ~ bNew)

  return(rgb(rNew, gNew, bNew))
}

tritanopia <- function(r, g, b) {
  r1 = r
  g1 = g
  b1 = b

  l = r1*c.c      + g1*c.cc    + b1*c.ccc
  m = r1*cc.c     + g1*cc.cc   + b1*cc.ccc
  s = r1*ccc.c    + g1*ccc.cc  + b1*ccc.ccc

  l1 = l
  m1 =            + m
  s1 = l*ttt.t*x  + m*ttt.tt*x + s*(1-x)

  rNew = l1*u.u   + m1*u.uu    + s1*u.uuu
  gNew = l1*uu.u  + m1*uu.uu   + s1*uu.uuu
  bNew = l1*uuu.u + m1*uuu.uu  + s1*uuu.uuu

  rNew = case_when(rNew > 1 ~ 1, rNew < 0 ~ 0, TRUE ~ rNew)
  gNew = case_when(gNew > 1 ~ 1, gNew < 0 ~ 0, TRUE ~ gNew)
  bNew = case_when(bNew > 1 ~ 1, bNew < 0 ~ 0, TRUE ~ bNew)

  return(rgb(rNew, gNew, bNew))
}

monochromatism <- function(r, g, b) {
  r1 = r
  g1 = g
  b1 = b

  l = r1*c.c      + g1*c.cc    + b1*c.ccc
  m = r1*cc.c     + g1*cc.cc   + b1*cc.ccc
  s = r1*ccc.c    + g1*ccc.cc  + b1*ccc.ccc

  l1 = l*(1-x)                 + s*x
  m1 =            + m*(1-x)    + s*x
  s1 =                         + s

  rNew = l1*u.u   + m1*u.uu    + s1*u.uuu
  gNew = l1*uu.u  + m1*uu.uu   + s1*uu.uuu
  bNew = l1*uuu.u + m1*uuu.uu  + s1*uuu.uuu

  rNew = case_when(rNew > 1 ~ 1, rNew < 0 ~ 0, TRUE ~ rNew)
  gNew = case_when(gNew > 1 ~ 1, gNew < 0 ~ 0, TRUE ~ gNew)
  bNew = case_when(bNew > 1 ~ 1, bNew < 0 ~ 0, TRUE ~ bNew)

  return(rgb(rNew, gNew, bNew))
}



# Define UI for app = create layout ----

ui <- navbarPage(em("Exploring Color Blindness"),

                 tabPanel("Why Should You care about Color Blindness?",
                          HTML(paste0(
                          "<p>Vision problems affect 1,430,176,980 Americans.",
                          "<p>The eight most prevalent types of vision problems include myopia,",
                          "<p>cataracts, hyperopia,diabetic tetinopathy, vision impairment,",
                          "<p>glaucoma, AMD (Age-Related Macular Degeneration), and blindness.",

                          "<p>Color blindness is a type of vision impairment; vision impairment",
                          "<p>affects almost 50 million people, which is over 80 times the",
                          "<p>population of Durham! 1 in 12 men and 1 in 200 women are color",
                          "<p> blind, which accounts for 300 million people in the world! ",
                          "<p>Color blindness is a widespread problem that is usually caused",
                          "<p> by genetics, diabetes, multiple sclerosis, or aging. Colorblindness",
                          "<p> affects so many yet is not talked about enough!",
                          "<p>Let's start the conversation here.",

                          "<p><b>Here are some examples of common types of colorblindness"))),


                 tabPanel("Are You Color Blind?"),

                 tabPanel("Sliding Scale of Color Blindness",
                          sidebarPanel(

                            selectInput(inputId = "imageInput",
                                        label = "Image:",
                                        list("Ball Pit" = "balls",
                                             "Flowers" = "flowers",
                                             "Mario Brothers" = "mario",
                                             "Parrots" = "parrots",
                                             "Starry Night" = "starry")),

                            radioButtons(inputId = "filterInput",
                                        label = "Colorblindess:",
                                        list("Protanopia" = 1,
                                             "Deuteranopia" = 2,
                                             "Tritanopia" = 3,
                                             "Monochromatism" = 4),
                                        selected = 1),

                            sliderInput(inputId = "xInput",
                                        label = "Severity:",
                                        min = 0, max = 1, value = 1),),

                          mainPanel(plotOutput("plotSlider")))


)

# Define server logic ----

server <- function(input, output, session) {

  imageUpdated <- reactive({return(as.name(input$imageInput))})
  filterUpdated <- reactive({return(input$filterInput)})
  xUpdated <- reactive({return(input$xInput)})

  output$plotSlider <- renderPlot({

    #if-statements are hardcoded to override fxn pass
    if      (imageUpdated() == "balls")   {pic <<- balls}
    else if (imageUpdated() == "flowers") {pic <<- flowers}
    else if (imageUpdated() == "mario")   {pic <<- mario}
    else if (imageUpdated() == "parrots") {pic <<- parrots}
    else                                  {pic <<- starry}
    #filter below, same approach w global vars
    if      (filterUpdated() == 1) {colorblindness <<- protanopia}
    else if (filterUpdated() == 2) {colorblindness <<- deuteranopia}
    else if (filterUpdated() == 3) {colorblindness <<- tritanopia}
    else                           {colorblindness <<- monochromatism}
    #good news x was easy
    x <<- xUpdated()

    old <- as.ggplot(expression(plot(pic,
                                     rescale = FALSE, axes = FALSE))) +
           coord_fixed() +
           labs(title = "Original") +
           theme(plot.title = element_text(size = 20, hjust = .5, vjust = -4))

    new <- as.ggplot(expression(plot(pic, colorscale = colorblindness,
                                     rescale = FALSE, axes = FALSE))) +
           coord_fixed() +
           labs(title = "Filtered") +
           theme(plot.title = element_text(size = 20, hjust = .5, vjust = -4))

    old + new
  })
}


# run app -----
shinyApp(ui, server)


