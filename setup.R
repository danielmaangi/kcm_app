library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)
library(shiny)
library(thematic)
library(bslib)
library(rsconnect)
library(shinythemes)
library(tidyverse)
library(data.table)
library(lubridate)
library(patchwork)
library(scales)
library(wesanderson)
library(pins)
library(bslib)

#renv::install("rlang")
#renv::install("gt")
#renv::snapshot()

library(gt)

# renv::remove("datimutils")
# renv::install("pepfar-datim/datimutils")
# restart session
library(datimutils)

base_url <- "https://kcmdashboard.org/"
dataSet <- "WFU6M2XN4W4"
orgunit <- "HfVjCurKxh2"


# Visualization theme
theme_set(
  theme_minimal(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_blank()
    ) 
)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# Visualization theme
theme_set(
  theme_minimal(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.caption = element_text(hjust = 0, face = "italic", size = 10),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.title = element_blank()
    ) 
)


des <- fread("metadata/hiv_krcs_gc7.txt") %>%
  rename(Indicator = description)



