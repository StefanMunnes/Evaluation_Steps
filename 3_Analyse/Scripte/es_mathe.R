lab.kurs <- c(
  "A" = "Ergänzungs- und Erweiterungsstudium Mathe WB-ES Ma 17/18",
  "B" = "Ergänzungs- und Erweiterungsstudium Mathe WB-ES Ma 18/19"
)

lev.kurs <- c(
  "Ergänzungs- und Erweiterungsstudium Mathe WB-ES Ma 17/18",
  "Ergänzungs- und Erweiterungsstudium Mathe WB-ES Ma 18/19"
)


# Variablen bereinigen
es_mathe_c <- bwb_mathe %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))

es_mathe_1 <- es_1.recode(es_mathe_c)



################
#### PLOTS #####
################

dir.create("4_Ergebnisse/Grafiken/es_mathe", showWarnings = F)

kurse <- c("18/19")

for (i in kurse) {

  if (i == "17/18") {

    file   <- "4_Ergebnisse/Grafiken/es_mathe/es_mathe_1718.pdf"
    anzahl <- paste0("Teilnehmende: ", nrow(es_mathe_c[grepl(i, es_mathe_1$kurs), ]))
    kursnr <- paste0("ES Ma 17/18-1")

    write.csv(es_mathe_c, "4_Ergebnisse/Tabellen/es_mathe_1718.csv", na = "")
  }
  else if (i == "18/19") {

  	file   <- "4_Ergebnisse/Grafiken/es_mathe/es_mathe_1819.pdf"
		anzahl <- paste0("Teilnehmende: ", nrow(es_mathe_c[grepl(i, es_mathe_1$kurs), ]))
		kursnr <- paste0("ES Ma 18/19-1")

		write.csv(es_mathe_c, "4_Ergebnisse/Tabellen/es_mathe_1819.csv", na = "")
  }

  plot.erstes <- plot.pie(es_mathe_1, i, erstes, T,
    "Besuchen Sie zum ersten Mal eine berufsbegleitende Weiterbildung?")

  plot.absolv <- plot.bar.absolv(es_mathe_c, es_mathe_1)

  plot.aufmerk_weit <- plot.bar.aufmerk_weit(es_mathe_c, es_mathe_1)

  plot.gruend <- plot.bar.gruend(es_mathe_c, es_mathe_1)

  plot.zufried <- plot.bar.div(es_mathe_1, i, zufried,
  	"Sind Sie insgesamt mit der Weiterbildung zufrieden?",
  	"sehr", "gar nicht")

  plot.stunden_weit <- plot.bar(es_mathe_1, i, stunden_weit, T,
    "Wie viele Stunden wenden Sie für diese Weiterbildung pro Woche auf \n(Vor- und Nachbereitung etc., ohne Präsenzzeiten in der Veranstaltung)?")


  ### plots for batteries
  # Fachvorlesung
  es_mathe_fv <- batterie.recode(es_mathe_c, i, "fv")
  names(es_mathe_fv) <- items_9

  # Fachdidaktik
  es_mathe_fd <- batterie.recode(es_mathe_c, i, "fd")
  names(es_mathe_fd) <- items_13

  ### Übung
  es_mathe_ue <- batterie.recode(es_mathe_c, i, "ue")
  names(es_mathe_ue) <- items_11

  ### Seminar
  es_mathe_se <- batterie.recode(es_mathe_c, i, "se")
  names(es_mathe_se) <- items_se_11


  # Fachvorlesung
  plot.fv <- plot.likert(es_mathe_fv,
  	"Fragen zu Struktur und Ablauf der Fachvorlesung")

  # Fachdidaktik
  plot.fd <- plot.likert(es_mathe_fd,
  	"Fragen zu Struktur und Ablauf der Fachdidaktik-Seminare")

  # Übung
  plot.ue <- plot.likert(es_mathe_ue,
  	"Fragen zu Struktur und Ablauf der (Präsenz-) Übungen")

  # Seminar
  plot.se <- plot.likert(es_mathe_se,
  	"Fragen zu Struktur und Ablauf des Seminars")


  plot.empfehl <- plot.pie(es_mathe_1, i, empfehl, T,
    "Würden Sie diese Weiterbildung weiterempfehlen?")

  plot.andere <- plot.bar.andere(es_mathe_c, es_mathe_1)

  plot.geschl <- plot.pie(es_mathe_1, i, geschl, T,
  	"Geschlecht")

  plot.alter <- plot.pie(es_mathe_1, i, alter, T,
  	"Alter")

  plot.schultyp <- plot.bar(es_mathe_1, i, schultyp, T,
  	"An welchem Schultyp sind Sie eingesetzt?")

  plot.klassen <- plot.bar.klassen(es_mathe_c, es_mathe_1)

  ## plots to pdf
  pdf(file, width = 9, height = 8)

  plot(0:10, type = "n", xaxt= "n", yaxt= "n", bty= "n", xlab = "", ylab = "")

  text(5.5, 8, "Evaluation", cex = 2)
  text(5.5, 7, "Ergänzungs- und Erweiterungsstudium")
  text(5.5, 6, kursnr)

  text(5.5, 4, anzahl)

  rasterImage(beberlin, 1, 0, 4.5, 1)
  rasterImage(steps, 6.5, 0 , 9, 1)


  grid.arrange(plot.erstes, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.absolv, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.aufmerk_weit, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.gruend, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.zufried, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.stunden_weit, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  grid.arrange(plot.fv, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.fd, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.ue, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.se, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  grid.arrange(plot.empfehl, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.andere, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.geschl, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.alter, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.schultyp, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.klassen, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  dev.off()
}
