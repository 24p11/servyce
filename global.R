
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

onStop(session = NULL, fun = function() {
  rm(anno)
  rm(mese)
  rm(aconsol)
  rm(mconsol)
  rm(naselect)
  rm(pmctmono)
  rm(pmctmono_ante)
  rm(pmctmono_consol)
  rm(rsa)
  rm(rum)
  rm(structures)
  rm(tarifs)
  rm(unita)
  rm(unitemed)
  } )
