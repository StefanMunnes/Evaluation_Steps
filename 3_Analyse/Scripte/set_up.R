######################################################
# Skript zur Auswertung der SET UP Evaluation
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


# externe Grafiken für den Report
beberlin <- readJPEG("1_AMC/bbst_gs_deutsch/_pics/beberlin.jpg")
steps    <- readJPEG("1_AMC/bbst_gs_deutsch/_pics/steps.jpg")


# Labels
varlab <- c(
  "v1" = "1. Die Veranstaltung war klar und übersichtlich strukturiert.",
  "v2" = "2. Die Veranstaltung versetzt mich in die Lage,\ndie Inhalte selbständig zu vertiefen.",
  "v3" = "3. Das fachliche Niveau der Veranstaltung empfand\nich als angemessen.",
  "v4" = "4. Das Geschehen in der Veranstaltung entsprach\nder Ankündigung auf dem Bildungsserver.",
  "v5" = "5. Das Erlernte und Erfahrene sind für mich beruflich von Nutzen.",
  "v6" = "6. Es gab genügend Zeit für den allgemeinen bzw.\nfachlichen Austausch.",
  "v7" = "7. Die Lehrveranstaltung begann und endete pünktlich.",
  "v8" = "8. Die Veranstaltung würde ich weiterempfehlen.",
  "v9" = "9. ... hat Ziele und Struktur der Veranstaltung\nnachvollziehbar dargestellt.",
  "v10" = "10. ... ging, soweit wie möglich, auf Wünsche und Fragen der\nTeilnehmenden ein und hat Anregungen aufgegriffen.",
  "v11" = "11. ... gestaltete die Lehrveranstaltung interessant\nund abwechslungsreich.")

notelab <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6)

partlab <- c("Zusammenfassende Einschätzung\nzur Veranstaltung", "Der Dozent/ die Dozentin ...")


# Datensätze laden
su_ids <- basename(list.dirs("3_Analyse/Scans/set_up", recursive = F))

csvs <- str_replace(list.files("3_Analyse/Scans/set_up/",
                               pattern = "*.csv",
                               include.dirs = T,
                               recursive = T),
                    ".csv", "")

pdfs <- str_replace(list.files("4_Ergebnisse/Grafiken/set_up/",
                               pattern = "*.pdf",
                               include.dirs = T,
                               recursive = T),
                    ".pdf", "")

to.use <- setdiff(csvs, pdfs)
# to.use <- csvs  # wenn alle PDF-Berichte überschrieben werden sollen


raw <- sapply(to.use, function(x) read.csv2(paste0("3_Analyse/Scans/set_up/", x, ".csv"),
                                            stringsAsFactor = T,
                                            colClasses = "character"),
              simplify = F,
              USE.NAMES = T)

raw.2 <- lapply(raw, function(x) x %>%
  filter(Note != "-1") %>%
  select(starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", ""))))

names(raw.2) <- to.use


final <- lapply(raw.2, function(x)
  gather(x, frage, note) %>%
  transmute(frage = recode_factor(frage, !!!varlab, .ordered = T),
            note  = as.numeric(recode_factor(note, !!!notelab)),
            part  = ifelse(grepl("(9|10|11)", frage), 2, 1),
            part  = recode_factor(part, !!!partlab, .ordered = T)))



# Bericht als PDF mit 2 Grafiken und Durchschnittsnote

for (i in names(final)) {

  file <- paste0("4_Ergebnisse/Grafiken/set_up/", i, ".pdf")

  su_id <- unlist(strsplit(names(final[i]), "/"))[1]
  datetime <- unlist(strsplit(names(final[i]), "/"))[2]

  date <- format.Date(as.Date(datetime, format = "%Y%m%d"), format = "%d.%m.%Y")
  time <- sub("([[:digit:]]{2,2})$", ":\\1", str_sub(datetime, -4, -1))

  teilnehmende <- nrow(final[[i]])/11

  schnitt <- round(mean(final[[i]][["note"]], na.rm = T), digits = 1)


  if (!dir.exists(paste0("4_Ergebnisse/Grafiken/set_up/", su_id))) {
    dir.create(paste0("4_Ergebnisse/Grafiken/set_up/", su_id))
  }

  
  par(oma = c(1, 1, 0, 1))
  par(mar = c(1, 1, 0, 1))

  plot(0:10, asp = 0.6, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

  text(5.5, 7, "Feedback", cex = 2.2)
  text(5.5, 6, "zum SET UP", cex = 1.5)
  text(5.5, 5, su_id, cex = 1.2)
  text(5.5, 3, paste0(date, "  -  ", time), cex = 1)
  text(5.5, 2, paste0("Teilnehmende: ", teilnehmende), cex = 1)

  text(5.5, 0, paste0("Durchschnittliche Bewertung: ", schnitt), cex = 1.2)

  rasterImage(beberlin, 1, 9, 4.5, 10)
  rasterImage(steps, 7.5, 9 , 10, 10)

  plot.title <- recordPlot()

  dev.off()

  plot.1 <- final[[i]] %>% filter(grepl("(^(1. )|2|3|4|5|6|7|8)", frage), between(note, 1, 6)) %>%
    ggplot(aes(y = note, x = reorder(frage, desc(frage)))) +
    coord_flip() +
    #geom_jitter(width = .15, height = 0.08, cex = 2, color = alpha("mediumpurple3", 0.4)) +
    #geom_dotplot(binaxis = "y", binwidth = 1, stackdir = "center", dotsize = 0.05) +
    geom_count(color = alpha("mediumpurple3", 0.66), show.legend = F)  +
    scale_size_area(max_size = 6) +
    stat_summary(fun.y = mean, geom = "crossbar",
                 aes(ymax = ..y..,
                     ymin = ..y..),
                 color = "indianred2",
                 width = 0.5) +
    scale_y_continuous(name = "",
                       limits = c(1, 6),
                       breaks = c(1, 2, 3, 4, 5, 6)) +
    labs(title = "Zusammenfassende Einschätzung\nzur Veranstaltung", x = "") +
    theme_light() +
    theme(axis.text = element_text(size = 10),
          plot.title = element_text(face = "bold", size = 12, colour = "black"))


  plot.2 <- final[[i]] %>% filter(grepl("(9|10|11)", frage), between(note, 1, 6)) %>%
    ggplot(aes(y = note, x = reorder(frage, desc(frage)))) +
    coord_flip() +
    geom_count(color = alpha("mediumpurple3", 0.66), show.legend = F)  +
    scale_size_area(max_size = 6) +
    stat_summary(fun.y = mean, geom = "crossbar",
                 aes(ymax = ..y..,
                     ymin = ..y..),
                 color = "indianred2",
                 width = 0.5) +
    scale_y_continuous(name = "",
                       limits = c(1, 6),
                       breaks = c(1, 2, 3, 4, 5, 6)) +
    labs(title = "Der Dozent/ die Dozentin ...", x = "") +
    theme_light() +
    theme(axis.text = element_text(size = 10),
          plot.title = element_text(face = "bold", size = 12, colour = "black"))


  plot.graph <- ggarrange(plot.1, plot.2, ncol = 1, align = "v", heights = c(2, 1.1))


  pdf(file, width = 8.3, height = 11.7, onefile=F)

  print(ggarrange(plot.title, plot.graph, ncol = 1,  heights = c(1.6, 2), widths = c(2, 1.6)))

  dev.off()
}

# exit
