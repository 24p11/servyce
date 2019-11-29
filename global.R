
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

#paramètres:

#renseigner ici les finess des hopitaux pour lesquels les données ont été importées avec dimRactivité:
sites <<- c('750100042', '750100075')

#renseigner ici le path au fichier de structures des hopitaux pour lesquels les données ont été importées avec dimRactivité (format du fichier structure disponible avec dimRactivité):
strctr <<- "/mnt/commons/Groupe_de_travail/GH_PMSI/INFORMATIQUE/GIT/remi/tableaux_de_bord/structures.xlsx"

#renseigner ici le path aux données de remontée (fichiers .Rdata) produites par dimRactivité
datapath <<- "/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/GENRSA/"


