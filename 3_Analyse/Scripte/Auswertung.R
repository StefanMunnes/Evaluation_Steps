######################################################
# Skript zur Auswertung der STePs-Evaluation
#
# Stefan Munnes (munnes@wzb.eu)
#
######################################################


### Präambel
library(tidyverse)
library(likert)
library(RColorBrewer)
library(colorspace)
library(scales)
library(stringr)
library(grid)
library(gridExtra)
library(jpeg)


### Arbeitsverzeichnis setzen
setwd("/home/steps/Evalutation_Steps")

rm(list = ls())

source("3_Analyse/Scripte/_labels.R")
source("3_Analyse/Scripte/_functions.R")

beberlin <- readJPEG("1_AMC/bbst_gs_deutsch/_pics/beberlin.jpg")
steps    <- readJPEG("1_AMC/bbst_gs_deutsch/_pics/steps.jpg")


### 1. Daten aufbereiten ###
# Datensätze laden
projects <- basename(grep("bbst_*|bwb_*",
  list.dirs("1_AMC", recursive = F), value = T))

for (i in projects) {
  file <- paste0("1_AMC/", i, "/exports/", i, ".csv")

  if (file.exists(file))
    assign(i, read.csv2(file, stringsAsFactor = T, colClasses = "character"))
}

source("3_Analyse/Scripte/bbst_gs_deutsch.R")
source("3_Analyse/Scripte/bbst_gs_englisch.R")
source("3_Analyse/Scripte/bbst_gs_mathe.R")
source("3_Analyse/Scripte/bbst_gs_nawi.R")
source("3_Analyse/Scripte/bbst_iss_mathe.R")
source("3_Analyse/Scripte/bbst_iss_info.R")
source("3_Analyse/Scripte/bbst_iss_englisch.R")
source("3_Analyse/Scripte/bbst_iss_physik.R")
source("3_Analyse/Scripte/es_englisch.R")
source("3_Analyse/Scripte/es_mathe.R")
source("3_Analyse/Scripte/es_info.R")
source("3_Analyse/Scripte/es_quali.R")
