library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(dplyr)
library(tibble)
library(readxl)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(lemon)
library(ggvis)
library(rgdal)
library(mosaic)
library(ggmap)
library(mapproj)
library(munsell)

options(gsubfn.engine="R")


# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")

