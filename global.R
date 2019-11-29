
library(shiny)
library(tidyverse)
library(htmltools)
library(networkD3)
library(DT)
library(plotly)
library(shinydashboard)
library(ggbiplot)
library(sunburstR)
library(RColorBrewer)
library(colorspace)
library(comorbidity)
library(nomensland)

sites <<- c('750100042', '750100075')
strctr <<- "/mnt/commons/Groupe_de_travail/GH_PMSI/INFORMATIQUE/GIT/remi/tableaux_de_bord/structures.xlsx"
datapath <<- "/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/GENRSA/"


