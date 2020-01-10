######################################################
# Skript zur Auswertung der FIRST STEPS Evaluation
#
# Stefan Munnes (munnes@wzb.eu)
#
######################################################


### Präambel
library(tidyverse)
library(grid)
library(gridExtra)
library(jpeg)
library(ggpubr)


### Arbeitsverzeichnis setzen
setwd("/home/steps/Evalutation_Steps")

rm(list = ls())


# Erhebungszeitraum festlegen
fs_jahr <- "19-20_1"


# Ordner für Grafiken erstellen
if (!dir.exists(paste0("4_Ergebnisse/Grafiken/first_steps/", fs_jahr))) {
  dir.create(paste0("4_Ergebnisse/Grafiken/first_steps/", fs_jahr))
}


# externe Grafiken für den Report
beberlin <- readJPEG("1_AMC/bbst_gs_deutsch/_pics/beberlin.jpg")
arrow    <- readJPEG("1_AMC/bbst_gs_deutsch/_pics/arrow.jpg")


# Labels für Variablenbeschriftung laden
source("3_Analyse/Scripte/first_steps/fs_labels.R")
notelab <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6)



# Daten laden und aufbereiten für Paten/-schaft
csvs_pat_p <- list.files(paste0("3_Analyse/Scans/first_steps/", fs_jahr, "/pat_p"),
                   pattern = "*.csv",
                   include.dirs = T,
                   recursive = T)

data_pat_p <- bind_rows(lapply(csvs_pat_p, function(csv)
                         read.csv2(paste0("3_Analyse/Scans/first_steps/", fs_jahr, "/pat_p/", csv)))) %>%
          mutate(Note = as.character(Note)) %>%
          filter(Note != "-1") %>%
          select(starts_with("TICKED.")) %>%
          rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", ""))) %>%
          gather(frg, note) %>%
          transmute(frage = recode_factor(frg, !!!varlab_pat, .ordered = T),
                    frage_p = recode_factor(frg, !!!varlab_pat_p, .ordered = T),
                    frage_q = recode_factor(frg, !!!varlab_pat_q, .ordered = T),
                    note  = as.numeric(recode_factor(note, !!!notelab)),
                    part  = "p")


csvs_pat_q <- list.files(paste0("3_Analyse/Scans/first_steps/", fs_jahr, "/pat_q"),
                     pattern = "*.csv",
                     include.dirs = T,
                     recursive = T)

data_pat_q <- bind_rows(lapply(csvs_pat_q, function(csv)
                        read.csv2(paste0("3_Analyse/Scans/first_steps/", fs_jahr, "/pat_q/", csv)))) %>%
         mutate(Note = as.character(Note)) %>%
         filter(Note != "-1") %>%
         select(starts_with("TICKED.")) %>%
         rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", ""))) %>%
         gather(frg, note) %>%
         transmute(frage = recode_factor(frg, !!!varlab_pat, .ordered = T),
                   frage_p = recode_factor(frg, !!!varlab_pat_p, .ordered = T),
                   frage_q = recode_factor(frg, !!!varlab_pat_q, .ordered = T),
                   note  = as.numeric(recode_factor(note, !!!notelab)),
                   part  = "q")

data.final_pat <- rbind(data_pat_p, data_pat_q)
data.final_pat[grepl("v0", data.final_pat$frage) & data.final_pat$note == 2 & !is.na(data.final_pat$note), "note"] <- 0 # Dummy Codierung


### Daten laden und aufbereiten für Patensystem
csvs_sys_p <- list.files(paste0("3_Analyse/Scans/first_steps/", fs_jahr, "/sys_p"),
                   pattern = "*.csv",
                   include.dirs = T,
                   recursive = T)

data_sys_p <- bind_rows(lapply(csvs_sys_p, function(csv)
                         read.csv2(paste0("3_Analyse/Scans/first_steps/", fs_jahr, "/sys_p/", csv)))) %>%
          mutate(Note = as.character(Note)) %>%
          filter(Note != "-1") %>%
          select(starts_with("TICKED.")) %>%
          rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", ""))) %>%
          gather(frg, note) %>%
          transmute(frage = recode_factor(frg, !!!varlab_sys, .ordered = T),
                    frage_p = recode_factor(frg, !!!varlab_sys_p, .ordered = T),
                    frage_q = recode_factor(frg, !!!varlab_sys_q, .ordered = T),
                    note  = as.numeric(recode_factor(note, !!!notelab)),
                    part  = "p")


csvs_sys_q <- list.files(paste0("3_Analyse/Scans/first_steps/", fs_jahr, "/sys_q"),
                     pattern = "*.csv",
                     include.dirs = T,
                     recursive = T)

data_sys_q <- bind_rows(lapply(csvs_sys_q, function(csv)
                        read.csv2(paste0("3_Analyse/Scans/first_steps/", fs_jahr, "/sys_q/", csv)))) %>%
         mutate(Note = as.character(Note)) %>%
         filter(Note != "-1") %>%
         select(starts_with("TICKED.")) %>%
         rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", ""))) %>%
         gather(frg, note) %>%
         transmute(frage = recode_factor(frg, !!!varlab_sys, .ordered = T),
                   frage_p = recode_factor(frg, !!!varlab_sys_p, .ordered = T),
                   frage_q = recode_factor(frg, !!!varlab_sys_q, .ordered = T),
                   note  = as.numeric(recode_factor(note, !!!notelab)),
                   part  = "q")

data.final_sys <- rbind(data_sys_p, data_sys_q)
data.final_sys[grepl("v0", data.final_sys$frage) & data.final_sys$note == 2 & !is.na(data.final_sys$note), "note"] <- 0 # Dummy Codierung
data.final_sys[grepl("v7", data.final_sys$frage) & data.final_sys$note == 2 & !is.na(data.final_sys$note), "note"] <- 0 # Dummy Codierung
data.final_sys[grepl("v8", data.final_sys$frage) & data.final_sys$note == 2 & !is.na(data.final_sys$note), "note"] <- 0 # Dummy Codierung


###############################################################################
########################## Grafische Berichte #################################
###############################################################################


############ Vergleichsberichte ################

### pat_p_q ###
source("3_Analyse/Scripte/first_steps/fs_pat_p_q.R")

### sys_p_q ###
source("3_Analyse/Scripte/first_steps/fs_sys_p_q.R")

############### Einzelberichte ##################

### pat_p ###
source("3_Analyse/Scripte/first_steps/fs_pat_p.R")

### pat_q ###
source("3_Analyse/Scripte/first_steps/fs_pat_q.R")

### sys_p ###
source("3_Analyse/Scripte/first_steps/fs_sys_p.R")

### sys_q ###
source("3_Analyse/Scripte/first_steps/fs_sys_q.R")

# exit
