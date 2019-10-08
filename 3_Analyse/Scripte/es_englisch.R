lab.kurs <- c(
  "A" = "Ergänzungs- und Erweiterungsstudium Englisch WB-ES Eng 17/18",
  "B" = "Ergänzungs- und Erweiterungsstudium Englisch WB-ES Eng 18/19"
)

lev.kurs <- c(
  "Ergänzungs- und Erweiterungsstudium Englisch WB-ES Eng 17/18",
  "Ergänzungs- und Erweiterungsstudium Englisch WB-ES Eng 18/19"
)


# Variablen bereinigen
es_englisch_c <- bwb_englisch %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))

es_englisch_1 <- es_1.recode(es_englisch_c)



################
#### PLOTS #####
################

dir.create("4_Ergebnisse/Grafiken/es_englisch", showWarnings = F)

kurse <- "18/19"

for (i in kurse) {

  file   <- "4_Ergebnisse/Grafiken/es_englisch/es_englisch_1819.pdf"
  anzahl <- paste0("Teilnehmende: ", nrow(es_englisch_1))
  kursnr <- paste0("ES Eng 18/19-1")

  write.csv(es_englisch_c, "4_Ergebnisse/Tabellen/es_englisch_1819.csv", na = "")


  plot.erstes <- plot.pie(es_englisch_1, i, erstes, T,
    "Besuchen Sie zum ersten Mal eine berufsbegleitende Weiterbildung?")

  plot.absolv <- plot.bar.absolv(es_englisch_c, es_englisch_1)

  plot.aufmerk_weit <- plot.bar.aufmerk_weit(es_englisch_c, es_englisch_1)

  plot.gruend <- plot.bar.gruend(es_englisch_c, es_englisch_1)

  plot.zufried <- plot.bar.div(es_englisch_1, i, zufried,
  	"Sind Sie insgesamt mit der Weiterbildung zufrieden?",
  	"sehr", "gar nicht")

  plot.stunden_weit <- plot.bar(es_englisch_1, i, stunden_weit, T,
    "Wie viele Stunden wenden Sie für diese Weiterbildung pro Woche auf \n(Vor- und Nachbereitung etc., ohne Präsenzzeiten in der Veranstaltung)?")


  # Weiterbildung
	es_englisch_wb <- batterie.recode(es_englisch_c, i, "wb")
	names(es_englisch_wb) <- items_wb

  # Fachdidaktik
	es_englisch_fd <- batterie.recode(es_englisch_c, i, "fd")
	names(es_englisch_fd) <- items_13

	### Linguistik
	es_englisch_lk <- batterie.recode(es_englisch_c, i, "lk")
	names(es_englisch_lk) <- items_13

	### Kulturrwissenschaft
	es_englisch_kw <- batterie.recode(es_englisch_c, i, "kw")
	names(es_englisch_kw) <- items_13

	### Literaturwissenschaft
	es_englisch_lw <- batterie.recode(es_englisch_c, i, "lw")
	names(es_englisch_lw) <- items_13

	### Sprache
	es_englisch_sp <- batterie.recode(es_englisch_c, i, "sp")
	names(es_englisch_sp) <- items_13


  # Weiterbildung
  plot.wb <- plot.likert(es_englisch_wb,
  	"Fragen zur Struktur und Organisation der Weiterbildung")

  # Fachdidaktik
  plot.fd <- plot.likert(es_englisch_fd,
  	"Fragen zur Struktur und Ablauf in der Fachdidaktik")

	# Linguistik
	plot.lk <- plot.likert(es_englisch_lk,
		"Fragen zur Struktur und Ablauf in der Linguistik")

  # Kulturwissenschaft
  plot.kw <- plot.likert(es_englisch_kw,
  	"Fragen zur Struktur und Ablauf in der Kulturwissenschaft")

  # Literaturwissenschaft
  plot.lw <- plot.likert(es_englisch_lw,
  	"Fragen zur Struktur und Ablauf in der Literaturwissenschaft")

  # Sprache
  plot.sp <- plot.likert(es_englisch_sp,
  	"Fragen zur Struktur und Ablauf in der Sprache")


  plot.empfehl <- plot.pie(es_englisch_1, i, empfehl, T,
    "Würden Sie diese Weiterbildung weiterempfehlen?")

  plot.andere <- plot.bar.andere(es_englisch_c, es_englisch_1)

  plot.geschl <- plot.pie(es_englisch_1, i, geschl, T,
  	"Geschlecht")

  plot.alter <- plot.pie(es_englisch_1, i, alter, T,
  	"Alter")

  plot.schultyp <- plot.bar(es_englisch_1, i, schultyp, T,
  	"An welchem Schultyp sind Sie eingesetzt?")

  plot.klassen <- plot.bar.klassen(es_englisch_c, es_englisch_1)


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

  grid.arrange(plot.wb, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.fd, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.lk, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.kw, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.lw, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.sp, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  grid.arrange(plot.empfehl, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.andere, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.geschl, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.alter, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.schultyp, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.klassen, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  dev.off()
}
