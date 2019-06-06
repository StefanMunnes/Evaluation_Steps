######################################################
# Skript zur Auswertung der STePs-Evaluation
#
# Stefan Munnes (munnes@wzb.eu)
# 15.05.2019
#
######################################################



### Präambel
install.packages("likert")
install.packages("sjPlot")
install.packages("RColorBrewer")


library(tidyverse)
library(likert)
library(RColorBrewer)
# library(sjPlot)
# library(xlsx)

setwd("/home/kalle/Dokumente/Arbeit_extern/Steps")

### 1. Daten aufbereiten
# Datensätze laden

projects <- basename(grep("bbst_*|bwb_*",
  list.dirs("1_AMC-Fragebögen", recursive = F), value = T))

projects

for (i in projects) {
  file <- paste0(i, "/exports/", i, ".csv")

  if (file.exists(file))
    assign(i, read.csv2(file, stringsAsFactor = T, colClasses = "character"))
}


source("Auswertung/Scripte/plot_functions.R")

source("Auswertung/Scripte/labels.R")

source("Auswertung/Scripte/bbst_gs_deutsch.R")


# write.xlsx("../Dokumente/Arbeit_extern/SBJF/Auswertung/bbst_gs_deutsch")

bbst_gs_englisch <- bbst_gs_englisch %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bbst_gs_mathe <- bbst_gs_mathe %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bbst_gs_nawi <- bbst_gs_nawi %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bbst_iss_englisch <- bbst_iss_englisch %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bbst_iss_info <- bbst_iss_info %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bbst_iss_mathe <- bbst_iss_mathe %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bbst_iss_physik <- bbst_iss_physik %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bwb_englisch <- bwb_englisch %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bwb_info <- bwb_info %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bwb_mathe <- bwb_mathe %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))

bwb_quali <- bwb_quali %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))


str(bbst_gs_deutsch)
