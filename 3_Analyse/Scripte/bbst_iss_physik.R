lab.kurs <- c(
	"A" = "Berufsbegleitende Studien Physik ISS/Gymnasien/Berufliche Schulen 18/19")

lev.kurs <- c(
	"Berufsbegleitende Studien Physik ISS/Gymnasien/Berufliche Schulen 18/19")

l.fragen.gs_physik <- c(
  "Das Gesamtkonzept für die bbSt Physik mit der Aufteilung in die Bereiche Fachphysik, Fachdidaktik und Praktikum ist zielführend.",
  "Die Inhalte für die bbSt Physik sind aufeinander abgestimmt.",
  "Meine persönlichen Erwartungen an die Ausbildung werplot.spden/wurden erfüllt.")



# Variablen bereinigen
bbst_iss_physik_c <- bbst_iss_physik %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))


bbst_iss_physik_1 <- bbst_1.recode(bbst_iss_physik_c)



################
#### PLOTS #####
################

dir.create("4_Ergebnisse/Grafiken/bbst_iss_physik", showWarnings = F)


kurse <- lev.kurs


for (i in kurse) {

	file   <- "4_Ergebnisse/Grafiken/bbst_iss_physik/bbst_iss_physik_1819.pdf"
	anzahl <- paste0("Teilnehmende: ", nrow(bbst_iss_physik_c))
	kursnr <- "bbSt Phy ISS/G 18/19-1"

	write.csv(bbst_iss_physik_c, "4_Ergebnisse/Tabellen/bbst_iss_physik_1819.csv", na = "")

	plot.kurs <- bbst_iss_physik_1 %>%
		filter(!is.na((kurs)), str_detect(kurs, pattern = i)) %>%
		select(kurs) %>%
		ggplot(aes(x = reorder(kurs, desc(kurs)), fill = kurs)) +
			geom_bar(width = 0.6) +
			coord_flip() +
			scale_fill_manual(values = colors.n8) +
			theme_minimal() +
			labs(x = NULL, y = "Anzahl", title = "Kurse") +
			theme(legend.position = "none",
						plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
						axis.ticks.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.major.y = element_blank())


  plot.geschl <- plot.pie(bbst_iss_physik_1, i, geschl, T,
  	"Geschlecht")

  plot.alter <- plot.pie(bbst_iss_physik_1, i, alter, T,
  	"Alter")

  plot.fach <- plot.pie(bbst_iss_physik_1, i, fach, T,
  	"Welche Fachrichtung haben Sie studiert?")

  plot.erfahr <- plot.bar(bbst_iss_physik_1, i, erfahr, T,
  	"Wie viele Jahre Berufserfahrung haben Sie in der studierten Fachrichtung?")

  plot.querein <- plot.bar(bbst_iss_physik_1, i, querein, T,
  	"Sind Sie bereits vor Ihrem Quereinstieg in Ausbildungsbereichen tätig gewesen \n(z.B. als Trainer/in, im Bereich Nachhilfe, als Chorleitung etc.)?")

  plot.aufmerk_ausb <- plot.bar(bbst_iss_physik_1, i, aufmerk_ausb, T,
  	"Wie wurden Sie auf diese Ausbildungsmöglichkeit aufmerksam?")

  plot.schultyp <- plot.bar(bbst_iss_physik_1, i, schultyp, T,
  	"An welchem Schultyp sind Sie eingesetzt?")

	plot.klassen <- plot.bar.klassen(bbst_iss_physik_c, bbst_iss_physik_1)

  plot.stunden <- plot.bar(bbst_iss_physik_1, i, stunden, T,
  	"Wie viele Stunden eigenständigen Unterricht erteilen Sie pro Woche?")

  plot.stunden_fach <- plot.bar(bbst_iss_physik_1, i, stunden_fach, T,
  	"Wie viele Stunden davon unterrichten Sie Ihr anerkanntes Fach?")

  plot.stunden_stud <- plot.bar(bbst_iss_physik_1, i, stunden_stud, T,
  	"Wie viele Stunden unterrichten Sie bereits in dem Fach,\nwelches Sie gerade in den berufsbegleitenden Studien belegen?")

	plot.einsatz <- plot.bar.einsatz(bbst_iss_physik_c, bbst_iss_physik_1)

  plot.leitung <- plot.pie(bbst_iss_physik_1, i, leitung, T,
  	"Fühlen Sie sich seitens der Schulleitung ausreichend unterstützt?")

  plot.belast <- plot.bar.div(bbst_iss_physik_1, i, belast,
  	"Wie hoch ist Ihre gefühlte Belastung durch die berufsbegleit. Studien und Schule?",
  	"völlig in Ordnung", "maximale Belastung")

  plot.verein <- plot.bar.div(bbst_iss_physik_1, i, verein,
  	"Wie gelingt Ihnen die Vereinbarkeit von berufsbegleitenden Studien und Familie?",
  	"sehr gut", "ungenügend")

  plot.quali <- plot.bar.div(bbst_iss_physik_1, i, quali,
  	"Wie empfanden Sie die Qualität der Beratung im Vorfeld der Ausbildung?",
  	"sehr gut", "ungenügend")

  plot.fehlt <- plot.bar.fehlt(bbst_iss_physik_c, bbst_iss_physik_1)


  ### plots for batteries

	# Fragen zu den berufsbegleitenden Studien
	bbst_iss_physik_fragen <- batterie.recode(bbst_iss_physik_c, i, "fragen")
	names(bbst_iss_physik_fragen) <- l.fragen.gs_physik

	### Fachphysik
	bbst_iss_physik_fp <- batterie.recode(bbst_iss_physik_c, i, "fp")
	names(bbst_iss_physik_fp) <- items_13

	# Fachdidaktik
	bbst_iss_physik_fd <- batterie.recode(bbst_iss_physik_c, i, "fd")
	names(bbst_iss_physik_fd) <- items_13

	### Praktikum
	bbst_iss_physik_pr <- batterie.recode(bbst_iss_physik_c, i, "pr")
	names(bbst_iss_physik_pr) <- items_13


  # Fragen zu den berufsbegleitenden Studien
  plot.fragen <- plot.likert(bbst_iss_physik_fragen,
  	"Fragen zu den berufsbegleitenden Studien Physik \nfür die Grundschule")

  # Fachphysik
  plot.fp <- plot.likert(bbst_iss_physik_fp,
  	"Fragen zur Struktur und Ablauf in der Fachdphysik")

	# Fachdidaktik
	plot.fd <- plot.likert(bbst_iss_physik_fd,
		"Fragen zur Struktur und Ablauf in der Fachdidaktik")

  # Praktikum
  plot.pr <- plot.likert(bbst_iss_physik_pr,
  	"Fragen zur Struktur und Ablauf des Praktikums")



  ## plots to pdf
  pdf(file, width = 9, height = 8)

  plot(0:10, type = "n", xaxt= "n", yaxt= "n", bty= "n", xlab = "", ylab = "")

  text(5.5, 8, "Evaluation", cex = 2)
  text(5.5, 7, "Berufsbegleitende Studien")
  text(5.5, 6, kursnr)

  text(5.5, 4, anzahl)

  rasterImage(beberlin, 1, 0, 4.5, 1)
  rasterImage(steps, 6.5, 0 , 9, 1)


	grid.arrange(plot.geschl, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.alter, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.fach, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.erfahr, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.querein, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.aufmerk_ausb, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.schultyp, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.klassen, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.stunden, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.stunden_fach, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.stunden_stud, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.einsatz, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.leitung, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.belast, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.verein, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.quali, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.fehlt, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  grid.arrange(plot.fragen, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  grid.arrange(plot.fp, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.fd, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.pr, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  dev.off()

}
