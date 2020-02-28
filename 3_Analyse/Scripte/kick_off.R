######################################################
# Skript zur Auswertung der KICK OFF Evaluation
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
library(openxlsx)

### Arbeitsverzeichnis setzen
setwd("/home/steps/Evalutation_Steps")

rm(list = ls())


# externe Grafiken für den Report
beberlin <- readJPEG("1_AMC/bbst_gs_deutsch/_pics/beberlin.jpg")
steps    <- readJPEG("1_AMC/bbst_gs_deutsch/_pics/steps.jpg")


# Labels
varlab <- c(
  "v1" = "1. Die Veranstaltung war klar und übersichtlich strukturiert.",
  "v2" = "2. Die Veranstaltung versetzt mich in die Lage,\ndie Inhalte selbständig zu vertiefen.",
  "v3" = "3. Das fachliche Niveau der Veranstaltung empfand\nich als angemessen.",
  "v4" = "4. Das Erlernte und Erfahrene sind für mich beruflich von Nutzen.",
  "v5" = "5. Es gab genügend Zeit für den allgemeinen bzw.\nfachlichen Austausch.",
  "v6" = "6. Die Lehrveranstaltung begann und endete pünktlich.",
  "v7" = "7. Die Organisation des Programms QuerBer wurde\nin der Begrüßungsveranstaltung verständlich dargestellt.",
  "v8" = "8. Die Informationen waren über den Bildungsserver\nproblemlos abrufbar.",
  "v9" = "9. ... hat Ziele und Struktur der Veranstaltung\nnachvollziehbar dargestellt.",
  "v10" = "10. ... ging, soweit wie möglich, auf Wünsche und Fragen der\nTeilnehmenden ein und hat Anregungen aufgegriffen.",
  "v11" = "11. ... gestaltete die Lehrveranstaltung interessant\nund abwechslungsreich.")

notelab <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6)

partlab <- c("Zusammenfassende Einschätzung\nzur Veranstaltung", "Der Dozent/ die Dozentin ...")


# Datensätze laden
csvs <- str_replace(list.files("3_Analyse/Scans/kick_off/",
                               pattern = "*.csv",
                               include.dirs = T,
                               recursive = T),
                    ".csv", "")

# lade alle (noch unbearbeiteten) CSV-Dateien in eine Liste als data.frames
data.list <- sapply(csvs, function(x)
                      read.csv2(paste0("3_Analyse/Scans/kick_off/", x, ".csv"),
                                stringsAsFactor = T,
                                colClasses = "character"),
                   simplify = F,
                   USE.NAMES = T)

data <- bind_rows(data.list, .id = "id") %>%
  mutate(Note = as.character(Note),
         group = id) %>%
  filter(Note != "-1") %>%
  select(id, group, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", ""))) %>%
  separate(id, c("kurs", "name", "zeit"), sep = "/") %>%
  gather(frage, note, starts_with("v")) %>%
  mutate(frage = recode_factor(frage, !!!varlab, .ordered = T),
         note  = as.numeric(recode_factor(note, !!!notelab)),
         part  = ifelse(grepl("(9|10|11)", frage), 2, 1),
         part  = recode_factor(part, !!!partlab, .ordered = T))



source("3_Analyse/Scripte/kick_off/ko_einzelberichte.R")
source("3_Analyse/Scripte/kick_off/ko_personen.R")
source("3_Analyse/Scripte/kick_off/ko_kurse.R")


### Excel-Tabellen
tbl.alle <- data %>%
  group_by(group) %>%
  summarise(Kurs = first(kurs),
            Lehrperson = first(name),
            Teilnehmende = n()/11,
            Note = round(mean(note, na.rm = 1), 1)) %>%
  select(-group)

tbl.kurse <- data %>%
  group_by(kurs) %>%
  summarise(Kurskennung = first(kurs),
            Teilnehmende = n()/11,
            Note = round(mean(note, na.rm = 1), 1)) %>%
  select(-kurs)

tbl.personen <- data %>%
  group_by(name) %>%
  summarise(Lehrperson = first(name),
            Teilnehmende = n()/11,
            Note = round(mean(note, na.rm = 1), 1)) %>%
  select(-name)

excel <- createWorkbook("kick_off")

addWorksheet(excel, "Einzelberichte")
addWorksheet(excel, "Kurse")
addWorksheet(excel, "Lehrpersonen")

writeData(excel, "Einzelberichte", tbl.alle)
writeData(excel, "Kurse", tbl.kurse)
writeData(excel, "Lehrpersonen", tbl.personen)

saveWorkbook(excel, "4_Ergebnisse/Tabellen/kick_off/Gesamtbericht.xlsx", overwrite = T)


# exit
