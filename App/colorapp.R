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
library(shinythemes)



# Preparations:-----------------------------------------------------------------
## Tab 1) Why color blindness matters?----
### Stored text for  -----

text1 <- "<h1> What is color blindness? </h1>
<h5> Color blindness affects approximately 1 in every 12 men and 1 in every 200 women.
Worldwide, that works out to about 300 million people - same as the population of
the United States. Color blindness affects a significant portion of the population,
yet it is not often talked about. Let's continue that conversation here!</br></br></h5>
<h4>  ✨CAUSES ✨  </h4>
<h5> Color blindness is almost always inherited genetically, from the mother's X
chromosome, which is why it affects so many more men than women. However, it can also
develop as a result of other diseases like diabetes or multiple sclerosis, or can be
established over time as a result of aging or medication. <h5> </br>
<h4>  ✨TYPES ✨  </h4>
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
<br><em><u>Monochromatism</u></em> is a type of colorblindness in which one perceives
all colors as varying shades of gray. In other words, people with monochromatism
cannot perceive color at all. Monochromatism is characterized by a lack of all
cones that perceive color. </h5> "


text2 <-
"
<h1> Math Behind Colorblindness Filters </h1>


<h5> Any picture on a screen is made up of many tiny pixels coded to specific red, green, and blue values (RGB values).
These pixels are stored as RGB values in 3 different ways:
three integers from 0 to 255 coresponsing to RGB values,
three decimals from 0 to 1 corresponding to a ratio of RGB values,
or a hexidecimal string such as #FFFFFF.
To maintain consistent, all of the filter functions convert these representations of RGB into the 0 to 1 syntax. </br></h5>

<h4>  Step 1  </h4>
<h5> Although computers interpret color through RGB values,
humans interpret color through three different cone receptors that detect long, medium, and short wavelengths of light.
This is known as LMS space.
Although RGB and LMS are similar based on corresponsing wavelengths of light,
LMS space differs because the three cones have overlap in the light wavelengths they can perceive.
Therefore, the matrix conversions on the righthand side are needed to account for the overlap.
The first step shows converting RGB space to LMS space with transformation matrix T. <h5> </br>

<h4>  Step 2  </h4>
<h5> The next step is to convert the original LMS values to colorblind LMS values.
The first row in this step shows simulation matrix S to show what protanopia colorblindess,
or a long cone defiecncy, looks like.
The next row in this step is important to understand the math final row.
This middle row shows how to use an identity matrix for simulation matrix S to create no change in LMS values.
The final row relates the original protanopia matrix to the identity matrix with proporiton x in order to create
a relationship between these two matrices.
This allows us to represent protanomaly, or a partial deficiency in long cones with value x.
Therefore, the filter defines a measurement of 75% protanopia to when x is equal to .75.
Simulation matrices S for the other types of colorblindess are defined below.
<h5> </br>

<h4>  Step 3  </h4>
<h5> The final step uses the inverse of transformation matrix T from the step one.
This matrix converts from LMS space to back to RGB space.
In conclusion, RGB values were converted to LMS, an LMS colorblindness filter is applied, then the LMS is converted back
into RGB values for the computer to display.
<h5> </br>
"

tab5text <- "
<h3>Introduction</h3>

  <p>The goal we decided to tackle for our final project was to design an R-shiny
based web app to increase awareness and understanding of colorblindness. In this
writeup, we will give some background on colorblindness and the relevance of our
project, discuss the functionality of the application itself, and discuss the
audience and overall impact of the site.</p>

  <p>1 in 12 men, and 1 in 200 women are color blind, which accounts for 300 million
people around the world. Color blindness is widespread, and is usually caused by
genetics, though it can also be caused by diabetes, multiple sclerosis, or aging.
There are 7 types of colorblindness, and we chose to focus on the four most
common ones (following in order). Protanopia (commonly known as “red-green
colorblindness”), Deuteranopia (cannot perceive green), Tritanopia (“blue-yellow
colorblindness”), and monochromatism (no color perception). We do want to note
here that the prevalence of colorblindness has been well documented in the
western world, namely white, European and American women and men. There is some
research that indicates heredity and prevalence of colorblindness differ in
populations around the world, so we want to acknowledge this bias in our data.</p>

  <p>Having taken a class on data visualizations now, we have learned more than ever
about the importance of color: in art and aesthetic, but also in education and
research settings. This is why we chose our topic for the final project. Our
project has three main parts in its overall goal. (1) Educating users about
color blindness by (2) allowing them to put themselves in the shoes of a
colorblind person and (3) identifying users who may potentially be affected by
one of the four main types of colorblindness.</p>

  <h4>Math section</h4>


  <h4>Colorblind test</h4>

  <p>The “Color Blind Test'' tab provides important visual information for our users
to understand different types of color blindness through navigating and
participating in a simple colorblind test. By saying “simple”, we stress that our
test is not a professional diagnosis, so users should only take the results as a
reminder of potential color deficiency, not a medical diagnosis.</p>

  <p>The logic of our design is to let users spot the color differences between an
original plot and a treated colorblind plot. However, unlike the traditional
Ishihara color test with some colorful dots and a number in the middle, our test
adds interactivity, where color choice and type of color blindness are user-defined.
With six color inputs, we generate an original plot that incorporates the original
chosen colors. We also generate a colorblind plot in the same shape as the original
one, but with our predefined color blindness filters being applied. Here, our
filter algorithm is the same as the one used in the colorblind filter tab but
it converts HEX color code instead of RGB. We chose to employ generative art
as a strategy to carry the randomness of colors and color blindness choices
while having a consistent aesthetic. We also have two mutated stacked bar
charts at the top of the art plots indicating the color choices that users
make along with the titles. We keep the filtered plot named “Colorblind” instead
of “Protanopia” or “Tritanopia” because we don’t want to tell the users on which
type of colorblindness they are testing.</p>

  <p> After the two plots are generated, users report if they see distinct color
differences between the plots by clicking on either the green (meaning they can
see “very different colors”) or red (meaning they see “similar colors”) button.
When they hit the button, a corresponding result message will pop up with the
information of what type of color blindness is represented and a general
suggestion. One note about the buttons is that we specifically phrased our
language as “very different” and “similar.” We made this distinction very clear
as to not overly diagnose users as colorblind and cause unnecessary worry.</p>

  <p>One limitation of this test is the freedom of color choices: what if users
choose all six colors that look the same in both colorblind and non-colorblind
vision? Some combinations of six color inputs do not present problems for specific
types of color blindness, which makes the two plots similar (or even identical,
in rare cases). This is why added a notice encouraging users to choose their colors
randomly, instead of focusing on shades of one or two colors.</p>

  <h4>Colorblind filter </h4>

  <p>As the second part of our interactive panel, the third tab on our website,
“Colorblind Filter” lets users understand what the world looks like through
the eyes of a colorblind person. It simulates what is seen for different types
of colorblindness at different severities. Users can choose from several built-in
images, selected for the wide range of colors they display, or upload their own.</p>

  <p>The filter works by taking the input image (either from built-in images, or
  user-uploaded) and applying the matrix transformations described above to the
RGB values in the image. Each different type of colorblindness takes a different
transformation. Moving the sliding scale for severity of colorblindness changes
the scale of the transformation taking place, allowing users to see what less
intense versions of different types of colorblindness might look like. Less
severe protanopia is called a “protanomaly” (which is actually the most common
kind of colorblindness), less severe deuteranopia is deuteranomaly, less severe
tritanopia is tritanomaly. Monochromatism is the most severe type of colorblindness.</p>

  <h3>Impact</h3>

  <p>The audience of our site is people who want to learn more about color blindness,
people looking to be tested for color blindness, and people who want to make
their visualizations more accessible. Beyond generally gaining perspective
about color blindness, we hope that users are encouraged to stop and think
about the colors in their day-to-day life they may take for granted. We hope
that our app will enable users to consider accessibility concerns in their lives.
For example, after viewing our app, we hope that users will utilize
colorblind-friendly colors in their next data visualizations in school or at work.
We want to portray how important it is that we keep colorblind people in mind
when trying to create meaningful visualizations. Here at Duke and especially in
the Statistics Department, students make several visualizations weekly. We hope
that our app will emphasize how important it is to use accessibility-focused
functions and color schemes, such as scale_viridis(), to ensure colorblind
viewers can understand and appreciate your visualizations. For example, in our
waffle chart on the first tab of our website, we utilized colors from Paul Tol’s
Colorblind Safe Colour Schemes, which were specifically developed for accessibility
purposes. We are excited to see users of our website follow our lead to create
a more inclusive society here at Duke and beyond.</p>
"


citations <-
c(tags$a(href="https://ixora.io/projects/colorblindness/color-blindness-simulation-research/",
         "https://ixora.io/projects/colorblindness/color-blindness-simulation-research/"),
tags$a(href="https://psychology.wikia.org/wiki/LMS_color_space",
       "https://psychology.wikia.org/wiki/LMS_color_space"),
tags$a(href="https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html",
       "https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html"),
tags$a(href="https://www.bardoptical.com/color-blindness-protanopia/",
       "https://www.bardoptical.com/color-blindness-protanopia/"),
tags$a(href="https://www.rdocumentation.org/packages/imager/versions/0.41.2",
       "https://www.rdocumentation.org/packages/imager/versions/0.41.2"),
tags$a(href="https://brew.sh/",
       "https://brew.sh/"),
tags$a(href="https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh",
       "https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"),
tags$a(href="https://stackoverflow.com/questions/25726276/visualize-a-list-of-colors-palette-in-r",
       "https://stackoverflow.com/questions/25726276/visualize-a-list-of-colors-palette-in-r"),
tags$a(href="https://www.rapidtables.com/web/color/RGB_Color.html",
       "https://www.rapidtables.com/web/color/RGB_Color.html"),
tags$a(href="https://data.world/dilumr/color-names/workspace/file?filename=wikipedia_x11_colors.csv",
       "https://data.world/dilumr/color-names/workspace/file?filename=wikipedia_x11_colors.csv"),
tags$a(href="https://www.researchgate.net/figure/A-comparison-of-the-visible-color-spectrum-in-common-types-of-color-blindness_fig2_316448379",
       "https://www.researchgate.net/figure/A-comparison-of-the-visible-color-spectrum-in-common-types-of-color-blindness_fig2_316448379"))

ournames <- "<em>This project was created by Parker Dingman, Holly Cui, Sophie
Dalldorf, and Kate Neal under the instruction of Professor Mine Cetinkaya-Rundel
for the Fall 2021 section of STA313: Advanced Data Visualization at Duke Univeristy.</em>"

space <-
  HTML(paste(" ", " ", " ", " ", " ", sep="<br/>"))

smallspace <-
  HTML(paste(" ", " ", sep="<br/>"))

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

### Data visualizations -----

of1000 = c("Color Blind Males"=80,
           "Normal Vision Males"=420,
           "Color Blind Females"=4,
           "Normal Vision Feales"=496)

cbplot <- waffle(of1000,
                 rows = 25,
                 size = 1,
                 colors = c("#66CCEE", "#4477AA", "#CCBB44", "#EE6677")) +
  labs(title = "Colorblindness in 1000 people",
       caption = "Source: NIH National Library of Medicine") +
  theme(plot.title = element_text(hjust = 0.5, size = 27))


# Define UI for app ------------------------------------------------------------

ui <- navbarPage(theme = shinytheme("united"), em("Exploring Color Blindness"),

                 # Tab 1 ----
                 tabPanel("Why Should You Care About Color Blindness?",
                          fluidRow(
                            column(width = 6,
                                   HTML(text1)),

                            column(width = 6,
                                   space,
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
                          Next, compare the newly generated plots.
                          Finally, choose the green button on top or red
                          button below it to check your results."
                          ),

                          HTML("<h4> Step 1 </h4>
                               <u>**Note: please choose colors as <b>randomly</b>
                               as possible.**</u>"),
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
                            "Whether or not you show symptoms of color deficiency,
                            understanding what the world looks like from
                          the eye of people suffering from colorblindness is
                           important.</br>
                          Below is an interactive colorblindness filter that simulates
                          the vision for different types and serverities of
                          colorblindness.</br>
                          Choose from four colorblindness types and the severity
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
                                          min = 0, max = 100,
                                          post = "%", value = 100),),

                            mainPanel(shinycssloaders::withSpinner(plotOutput("plotSlider")))
                          )),

                 # Tab 4 ----
                 tabPanel("The Math: Explained",
                          fluidRow(
                            column(width=6,
                                   HTML(text2)),

                            column(width=6,
                                   plotOutput("cbmathplot")))),

                 # Tab 5 ----
                 tabPanel("Writeup & Acknowledgements",
                          fluidRow(
                            column(width = 8,
                                   HTML(ournames, tab5text),
                                   smallspace,
                                   h4("References"),
                                   tags$a(href="https://ixora.io/projects/colorblindness/color-blindness-simulation-research/",
                                          "https://ixora.io/projects/colorblindness/color-blindness-simulation-research/"),
                                   smallspace,
                                   tags$a(href="https://psychology.wikia.org/wiki/LMS_color_space",
                                          "https://psychology.wikia.org/wiki/LMS_color_space"),
                                   smallspace,
                                   tags$a(href="https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html",
                                          "https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html"),
                                   smallspace,
                                   tags$a(href="https://www.bardoptical.com/color-blindness-protanopia/",
                                          "https://www.bardoptical.com/color-blindness-protanopia/"),
                                   smallspace,
                                   tags$a(href="https://www.rdocumentation.org/packages/imager/versions/0.41.2",
                                          "https://www.rdocumentation.org/packages/imager/versions/0.41.2"),
                                   smallspace,
                                   tags$a(href="https://brew.sh/",
                                          "https://brew.sh/"),
                                   smallspace,
                                   tags$a(href="https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh",
                                          "https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"),
                                   smallspace,
                                   tags$a(href="https://stackoverflow.com/questions/25726276/visualize-a-list-of-colors-palette-in-r",
                                          "https://stackoverflow.com/questions/25726276/visualize-a-list-of-colors-palette-in-r"),
                                   smallspace,
                                   tags$a(href="https://www.rapidtables.com/web/color/RGB_Color.html",
                                          "https://www.rapidtables.com/web/color/RGB_Color.html"),
                                   smallspace,
                                   tags$a(href="https://data.world/dilumr/color-names/workspace/file?filename=wikipedia_x11_colors.csv",
                                          "https://data.world/dilumr/color-names/workspace/file?filename=wikipedia_x11_colors.csv"),
                                   smallspace,
                                   tags$a(href="https://www.researchgate.net/figure/A-comparison-of-the-visible-color-spectrum-in-common-types-of-color-blindness_fig2_316448379",
                                          "https://www.researchgate.net/figure/A-comparison-of-the-visible-color-spectrum-in-common-types-of-color-blindness_fig2_316448379"),
                                   smallspace),


                            column(width = 4,
                                   )))

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
    #good news x works
    x <<- (xUpdated() / 100)

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

  # Tab 4 ----
  output$cbmathplot <- renderImage({

    cbmathJPG <- normalizePath(file.path('~/R/project-2-stats_r_us/Image/cb_math.jpg'))
    list(src = cbmathJPG, width = "85%", height = "170%")}, deleteFile = FALSE)


}



# Run app ----------------------------------------------------------------------
shinyApp(ui, server)