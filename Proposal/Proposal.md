Project Proposal
================
Team: Stats R Us

**Background information:**

``` r
library(tidyr)
library(tidyverse)
```

    ## Warning in system("timedatectl", intern = TRUE): running command 'timedatectl'
    ## had status 1

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ dplyr   1.0.7
    ## ✓ tibble  3.1.5     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1
    ## ✓ purrr   0.3.4

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(ggplot2)
library("httr")
library("readxl")
GET("https://query.data.world/s/m334npjqtx64m4ztmznpfjbencph6k", write_disk(tf <- tempfile(fileext = ".xlsx")))
```

    ## Response [https://download.data.world/file_download/datacrunch/prevalence-of-vision-problems-in-the-u-s/Prevelance%20of%20Eye%20Diseases%20in%20US.xlsx?auth=eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OnNvcGhpZWRhbGxkb3JmIiwiaXNzIjoiYWdlbnQ6c29waGllZGFsbGRvcmY6Ojg2OWFjYzY4LTYxZjYtNGE1Yy1iNzdiLTBlMTIwMzIzNzkyNCIsImlhdCI6MTYzNjY4MDcwMSwicm9sZSI6WyJ1c2VyIiwidXNlcl9hcGlfYWRtaW4iLCJ1c2VyX2FwaV9lbnRlcnByaXNlX2FkbWluIiwidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6ZmFsc2UsInVybCI6ImE5MjAxMGY3MzJjNzQyZWE2M2UwNWZmM2VhMjEzNGEwNzU4ZmI2ZDAifQ.YEB-l3lhe4tpjKfz_yr1dvP6LLMuDeICFHZqq0GAUrXwQPmDoiog2pFs7ddX_bO8-BF5PJATuZ7RoxOxar8vzA]
    ##   Date: 2021-11-12 04:02
    ##   Status: 200
    ##   Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
    ##   Size: 2.37 MB
    ## <ON DISK>  /tmp/Rtmp8GP8lE/file26901a5c5f6a.xlsx

``` r
vision_data <- read_excel(tf)
```

``` r
vision_data <- vision_data %>%
  group_by(vision_problem) %>%
  summarise(num_cases = sum(number_of_cases))
```

``` r
ggplot(data = vision_data, aes(x = reorder(vision_problem, -num_cases), y = num_cases, fill = vision_problem)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = num_cases), check_overlap = TRUE, vjust = -.3, size = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_viridis_d() +
  scale_y_continuous(name = "Total Number of Cases",
                     breaks = c(0, 100000000, 200000000, 300000000, 400000000, 500000000, 600000000),
                     labels = c("0", "100M", "200M", "300M", "400M", "500M", "600M")) +
  labs(title = "Prevelance of Vision Problems in the U.S.", caption = "Data sourced from data.world")
```

![](Proposal_files/figure-gfm/background-plots-1.png)<!-- -->

Vision problems affect 1,430,176,980 Americans. As we can see above, the
eight most prevalent types of vision problems include myopia, cataracts,
hyperopia, diabetic tetinopathy, vision impairment, glaucoma, AMD
(Age-Related Macular Degeneration), and blindness. Myopia affects the
most people at 500 million, and blindness affects the least at just over
20 million. Color blindness is a type of vision impairment; as we can
see in the plot above, vision impairment affects almost 50 million
people, which is over 80 times the population of Durham!

1 in 12 men and 1 in 200 women are color blind, which accounts for 300
million people in the world! Color blindness is a widespread problem
that is usually caused by genetics, diabetes, multiple sclerosis, or
aging. Because color blindness affects so many people and is not talked
about nearly enough, our group wanted to explore how R can be used to
give a platform to this worldwide, common condition. We will use our
skills in R to enable users to understand how color blind individuals
view the world differently. Subsequently, we hope to foster a desire to
learn more about the color blind community and the challenges that come
with being visually impaired.

**1. A one sentence description of your high-level goal.**

We want to build a Shiny app to help users understand color blindness by
building “filters” for photos representing different types of color
deficiency.

**2. A one to two paragraph description of your goals, including your
motivation.**

In class, we learned about scale\_color\_vidiris and
scale\_fill\_viridis, two functions in ggplot that are designed to make
colored graphs more accessible to color-blind viewers. All of us were
very interested in further exploring accessibility concerns for the
colorblind community, so we would like to implement a sliding color
scale to allow noncolorblind viewers to understand these accessibility
concerns. This sliding scale would apply different filters for different
types of colorblindness, such as deuteranopia, protanopia, and
tritanopia. By exploring different types, we will play around with the
color use and theme design as well as raise awareness for the variety of
colorblindness out there. We think it is extremely important for members
of the noncolorblind community to understand accessibility issues in the
plots and images they make. We can all move towards more accessible and
equitable visualizations with our project.

One package that we will use in this app will be the `imager` library,
which contains a large array of functions for working with image data.
As we allow users to input images and filter them for different types of
color blindness, as well as creating a colorblind test, the
functionality of this package will be helpful. A dataset we may choose
to incorporate when talking about the significance of our project can be
found at
<https://data.world/datacrunch/prevalence-of-vision-problems-in-the-u-s>,
in order to compare the prevalence of commonly known vision problems to
rates of less commonly known types of color blindness, statistics about
which will be referenced from <https://iristech.co/statistics/>. We also
found background information on colorblindness at
<https://www.colourblindawareness.org/colour-blindness/>.

![](/home/guest/Project%202/project-2-stats_r_us/Image/Color-blind-grid.jpeg)

**3. Plan of attack**

DUE DATES:

Proposal due November 5.

Revised proposal due November 12.

Write-up and presentation due December 3.

Proposal (Due November 5):

-   One sentence description: Kate (done by 11/1)
-   Motivation: Sophie (done by 11/1)
-   Weekly plan of attack: Sophie and Kate (done by 11/1)
-   Goals: Parker and Holly (done by 11/1)
-   Organization: Holly (done by 11/1)
-   Dates:
    -   Finish rough draft of all of our parts by end of day on 11/1
    -   All members read over entire rough draft and make edits by 11/3
    -   Final draft of proposal complete by end of day 11/3

Revised Proposal (Due November 12):

-   Review comments individually on 11/9
-   Meet on 11/10 to revise proposal
-   Close all comments by 11/11
-   Submit updated proposal by end of day 11/11

Write-Up (Due December 3):

-   Introduction (by 11/13):
    -   Sophie and Parker
-   Sliding Color Scale (by 11/17):
    -   As a team: study room reserved for week of 11/15
    -   Find images that are conducive to our goals
        -   Parker
-   Conclusion (by 11/25):
    -   Holly and Kate

Website (Due December 3):

-   Our project is a Shiny App and therefore is a website
-   We will add a page to our website by the end of November including
    our Write-Up Sophie

Presentation (Due December 3):

-   Our presentation will include a demo of our website
-   Slides including our motivation and significance of the social
    impact of understanding color blindness
    -   Make slides by end of November:
        -   Kate and Holly
-   Entire team practice and make final edits to presentation and
    write-up on 12/2
    -   Link group study 3 reserved 7-11pm

**4. The final organization of your project repository.**

There will be five folders created in total. Two main folders are under
the project repository, one containing the major RMD file with the Shiny
app design and the other including all images we will be using to work
on the different types of filters. We will also upload datasets to Data
folder if we need further analysis. A separate presentation folder will
also be created using xaringan. Proposal file is under its own folder.

Folder structure:

-   App (.rmd, .html (website), README describing sections of website
    and user interface)

-   Image (images that will be used on website and in the app)

-   Data (including potential datasets used for basic plot analysis)

-   Proposal (containing Proposal.rmd and .md)

-   Presentation (using xaringan)
