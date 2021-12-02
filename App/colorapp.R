# Load packages ----------------------------------------------------------------

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
library(waffle)
library(colourpicker)
library(ggtern)



# Preparations:-----------------------------------------------------------------
## Tab 1) Why color blindness matters?----
### Stored text for  -----

text1 <- "<h1> What is color blindness? </h1>

<h5> Color blindness affects approximately 1 in every 12 men and 1 in every 200 women.
Worldwide, that works out to about 300 million people - same as the population of
the United States. Color blindness affects a significant portion of the population,
yet it is not often talked about. Let's continue that conversation here!</br></h5>

<h4>  ✨CAUSES ✨  </h4>

<h5> Color blindness is almost always inherited genetically, from the mother's X
chromosome, which is why it affects so many more men than women. However, it can also
develop as a result of other diseases like diabetes or multiple sclerosis, or can be
established over time as a result of aging or medication. <h5> </br>


<h4>  ✨KINDS ✨  </h4>

<h5> While there are seven kinds of colorblindness, we will be focusing on the four most
common: protanopia, deuteranopia, tritanopia, and monochromatism: </br>
<br><em><u>Protanopia</u></em> is a type of colorblindness where the L-cone (also
known as the red cone or the long-wavelength cone) is completely missing.
People with protanopia are unable to perceive red and green. </br>
<br><em><u>Deuteranopia</u></em> is a type of colorblindness where those affected
cannot perceive green; it is caused by the absence of the M-cone (also known as
the green cone or the medium-wavelength cone). </br>
<br><em><u>Tritanopia</u></em> affected people cannot distinguish between blue and
yellow due to missing S-cones (blue cones, short-wavelength cones). </br>
<br><em><u>monochromatism</u></em> is a type of colorblindness in which one perceives
all colors as varying shades of gray. In other words, people with monochromatism
cannot perceive color at all. Monochromatism is characterized by a lack of all
cones that perceive color. </h5> "

### Data visualization for why colorblindness matters -----

of1000 = c("Color Blind Males"=80,
           "Normal Vision Males"=420,
           "Color Blind Females"=4,
           "Normal Vision Feales"=496)

cbplot <- waffle(of1000,
                 rows = 25,
                 size=1,
                 colors=c("darkblue", "dodgerblue2", "#CC0000", "lightcoral"))







## Tab 2) Color blindness test ----
### Define constants for matrices -----

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



### Hex functions for HEX converting -----
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



## Tab3)
### Art functions -----

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
  theme(plot.title = element_text(size = 25, hjust = 0.5, vjust = 3)) +
  labs(title = "Colorblind")

# Output art
build_art <- function(points,
                      angle,
                      adjustment
) {

  tibble(
    i = 1:points,
    t = (1:points) * angle + adjustment,
    x = sin(t),
    y = cos(t),
    g = sample(6, points, TRUE)
  )
}

art <-
  build_art(
    angle = pi * (3 - sqrt(5)),
    points = 500,
    adjustment = 0
  ) %>%
  ggplot(aes(x = x * t, y = y * t)) +
  geom_point(aes(size = g, color = factor(g), fill = factor(g)),
             alpha = 0.5, shape = "square filled", show.legend = FALSE) +
  coord_equal() +
  theme_void()







## Tab 3) Color blindness Filter ----
### Load data and images -----
balls <- load.image('~/R/project-2-stats_r_us/Image/ball.jpeg')
flowers <- load.image('~/R/project-2-stats_r_us/Image/flower.jpeg')
mario <- load.image('~/R/project-2-stats_r_us/Image/super-mario.jpeg')
parrots <- load.image(system.file("extdata/parrots.png", package = "imager"))
starry <- load.image('~/R/project-2-stats_r_us/Image/starry-night.jpeg')

### Slider functions for RGB converting(with constants) -----
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



# Define UI for app ------------------------------------------------------------

ui <- navbarPage(theme = shinytheme("united"), em("Exploring Color Blindness"),

                 # Tab 1 ----
                 tabPanel("Why Should You Care About Color Blindness?",
                          fluidRow(
                            column(width=6,
                                   HTML(text1)),

                            column(width=6,
                                   plotOutput("cbplot")))),


                 # Tab 2 ----
                 tabPanel("Are You Color Blind?",
                          titlePanel("Color Blind Test"),

                          HTML(
                          "Want to check if you have colorblindness or color
                          deficiency? Take this test! </br>
                          First, randomly choose six colors.
                          Then, navigate to the sidebar section where you choose
                          an unknown type of colorblindness. </br>
                          Next, compare the newly generated plots.</br>
                          Finally, choose the green button on top or red
                          button below it to check your results."
                          ),

                          HTML("<h4> Step 1 </h4>"),
                          fluidRow(
                            column(2, colourInput("col1", "Choose 1st color")),
                            column(2, colourInput("col2", "Choose 2nd color")),
                            column(2, colourInput("col3", "Choose 3rd color")),
                            column(2, colourInput("col4", "Choose 4th color")),
                            column(2, colourInput("col5", "Choose 5th color")),
                            column(2, colourInput("col6", "Choose 6th color")),
                            style = 'padding:0px; padding-top:0px; padding-bottom:10px'
                          ),

                          sidebarLayout(
                            sidebarPanel(
                              HTML("<h4> Step 2 </h4>"),
                              radioButtons("colorblind",
                                           "Choose an unknown type of colorblindness:",
                                           c("A", "B", "C", "D")),
                              "-----------------------------------------------------------------------",
                              HTML("<h4> Step 3 </h4>"),
                              fluidRow(
                                HTML("<b>After comparing the two plots, check
                                your result by hitting one of the buttons below:</b>"),
                                style='padding-top:5px; padding-bottom:10px; padding-left:20px; padding-right:20px'
                              ),
                              useShinyalert(),
                              actionButton("correct",
                                           "I can see very different colors in both plots.",
                                           icon("laugh-wink"),
                                           class = "btn-success btn-block"),
                              actionButton("check",
                                           "I see similar colors in both plots.",
                                           icon("flushed"),
                                           class = "btn-danger btn-block "),
                              fluidRow(
                                HTML("<b>***Disclaimer: This test is not a
                                     professional diagnosis.***</b>"),
                                style='padding-top:20px; padding-bottom:10px; padding-left:20px; padding-right:10px')
                            ),

                            mainPanel(
                              fluidRow(
                                column(6,
                                       offset = 0,
                                       style='padding:0px; padding-top:0px; padding-bottom:0px',
                                       plotOutput(outputId = "graph1")),
                                column(6,
                                       offset = 0,
                                       style='padding:0px; padding-top:0px; padding-bottom:0px',
                                       plotOutput(outputId = "graph2"))
                              )
                            )
                          )
                 ),

                 # Tab 3 ----
                 tabPanel("Color Blindness Filter",
                          titlePanel("Color Blindness Filter"),

                          HTML(
                          "Hopefully, you have tested normal for your color vision.
                          However, understanding what the world looks like in
                          the eye of people suffering from colorblindness is
                          also important.</br>
                          Here, we build a colorblindness filter that simulate
                          the color vision of different types and degrees of
                          colorblindness.</br>
                          By choosing from four colorblindness types and
                          adjusting the sliding scale, we have two plots
                          generated for comparison. </br>
                          Besides built-in images, you can also <u>upload
                          your own</u> to try the filter!</br>"
                          ),

                          sidebarLayout(
                          sidebarPanel(

                            selectInput(inputId = "imageInput",
                                        label = "Image:",
                                        list("Ball Pit" = "balls",
                                             "Flowers" = "flowers",
                                             "Mario Brothers" = "mario",
                                             "Parrots" = "parrots",
                                             "Starry Night" = "starry")),

                            fileInput(inputId = "fileInput",
                                      label = "Upload JPG:",
                                      accept = ".jpg"),

                            checkboxInput(inputId = "usePic",
                                          label = "Use Upload"),

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

                          mainPanel(shinycssloaders::withSpinner(plotOutput("plotSlider")))
                          )),

                 # Tab 4 ----
                 tabPanel("The Math: Explained"),

                 # Tab 5 ----
                 tabPanel("Writeup & Acknowledgements")

)




# Define server logic ----------------------------------------------------------
server <- function(input, output, session) {

  # Tab 1 ----
  output$cbplot <- renderPlot({
    cbplot
  })

  # Tab 2 -----
  c1 <- reactive({return(input$col1)})
  c2 <- reactive({return(input$col2)})
  c3 <- reactive({return(input$col3)})
  c4 <- reactive({return(input$col4)})
  c5 <- reactive({return(input$col5)})
  c6 <- reactive({return(input$col6)})
  cbindLet <- reactive({return(input$colorblind)})

  output$graph1 <- renderPlot({

   plotA <- squareplot +
            scale_fill_manual(values = c(c1(), c2(), c3(),
                                   c4(), c5(), c6())) +
            labs(title = "Original")

   plotB <- art +
            scale_fill_manual(values = c(c1(), c2(), c3(),
                                         c4(), c5(), c6())) +
            scale_color_manual(values = c(c1(), c2(), c3(),
                                          c4(), c5(), c6()))

   plotA / plotB
  })



output$graph2 <- renderPlot({

    if (cbindLet() == "A") {
    plotA <- squareplot +
             scale_fill_manual(values = c(proHex(c1()), proHex(c2()),
                                          proHex(c3()), proHex(c4()),
                                          proHex(c5()), proHex(c6())))
    plotB <- art +
             scale_fill_manual(values = c(proHex(c1()), proHex(c2()),
                                          proHex(c3()), proHex(c4()),
                                          proHex(c5()), proHex(c6())))+
             scale_color_manual(values = c(proHex(c1()), proHex(c2()),
                                          proHex(c3()), proHex(c4()),
                                          proHex(c5()), proHex(c6())))
    plotA / plotB
    }


    else if (cbindLet() == "B") {
    plotA <- squareplot +
             scale_fill_manual(values = c(deutHex(c1()), deutHex(c2()),
                                           deutHex(c3()), deutHex(c4()),
                                           deutHex(c5()), deutHex(c6())))
    plotB <- art +
             scale_fill_manual(values = c(deutHex(c1()), deutHex(c2()),
                                          deutHex(c3()), deutHex(c4()),
                                          deutHex(c5()), deutHex(c6())))+
             scale_color_manual(values = c(deutHex(c1()), deutHex(c2()),
                                           deutHex(c3()), deutHex(c4()),
                                           deutHex(c5()), deutHex(c6())))
    plotA / plotB
}

    else if (cbindLet() == "C") {
    plotA <- squareplot +
             scale_fill_manual(values = c(triHex(c1()), triHex(c2()),
                                          triHex(c3()), triHex(c4()),
                                          triHex(c5()), triHex(c6())))

    plotB <- art +
             scale_fill_manual(values = c(triHex(c1()), triHex(c2()),
                                          triHex(c3()), triHex(c4()),
                                          triHex(c5()), triHex(c6())))+
             scale_color_manual(values = c(triHex(c1()), triHex(c2()),
                                           triHex(c3()), triHex(c4()),
                                           triHex(c5()), triHex(c6())))
    plotA / plotB
    }

    else {
    plotA <- squareplot +
             scale_fill_manual(values = c(monoHex(c1()), monoHex(c2()),
                                          monoHex(c3()), monoHex(c4()),
                                          monoHex(c5()), monoHex(c6())))

    plotB <- art +
             scale_fill_manual(values = c(monoHex(c1()), monoHex(c2()),
                                          monoHex(c3()), monoHex(c4()),
                                          monoHex(c5()), monoHex(c6())))+
             scale_color_manual(values = c(monoHex(c1()), monoHex(c2()),
                                           monoHex(c3()), monoHex(c4()),
                                           monoHex(c5()), monoHex(c6())))
    plotA / plotB
    }
})

  ## Output result text

  observeEvent(input$correct, {

    if (cbindLet() == "A") {
      type_color <- "protanopia"}
    else if (cbindLet() == "B") {
      type_color <- "deuteranopia"}
    else if (cbindLet() == "C") {
      type_color <- "tritanopia"}
    else {
      type_color <- "monochromatism"}

    shinyalert(
      "Congratulations!",
      paste0("You are showing no symptoms of", " ", type_color, "."),
      type = "success")
  })


  observeEvent(input$check, {

    if (cbindLet() == "A") {
      type_color <- "protanopia"}
    else if (cbindLet() == "B") {
      type_color <- "deuteranopia"}
    else if (cbindLet() == "C") {
      type_color <- "tritanopia"}
    else {
      type_color <- "monochromatism"}

    shinyalert(
      paste0("Oops! You may have symptoms of", " ", type_color, "."),
      "Try doing the test again with different colors \nor seek a professional colorblindness test.",
      type = "error")
  })

  # Tab 3 ----

  fileUpload <- reactive({return(input$fileInput)})
  fileThere <- reactive({return(input$usePic)})

  imageUpdated <- reactive({return(as.name(input$imageInput))})
  filterUpdated <- reactive({return(input$filterInput)})
  xUpdated <- reactive({return(input$xInput)})

  output$plotSlider <- renderPlot({

    #if-statements are hardcoded to override fxn pass
    if (fileThere() & !is.null(fileUpload())){
      temp <- fileUpload()
      fileUploadDouble <- load.image(temp$datapath)
      pic  <<- fileUploadDouble}
    else if (imageUpdated() == "balls")   {pic <<- balls}
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



# Run app ----------------------------------------------------------------------
shinyApp(ui, server)

