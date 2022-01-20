library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(dplyr)
library(tibble)
library(sp)
library(rgdal)
library(raster) # funkcija crop
library(rgeos) # funkcija gBuffer
library(tidyverse)
library(cluster)
library(ggalt)
library(readxl)



options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")