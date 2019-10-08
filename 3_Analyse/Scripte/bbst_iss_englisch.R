lab.kurs <- c(
	"A" = "Berufsbegleitende Studien Englisch ISS/Gymnasien/Berufliche Schulen 17/18",
	"B" = "Berufsbegleitende Studien Englisch ISS/Gymnasien/Berufliche Schulen 18/19")

lev.kurs <- c(
	"Berufsbegleitende Studien Englisch ISS/Gymnasien/Berufliche Schulen 17/18",
	"Berufsbegleitende Studien Englisch ISS/Gymnasien/Berufliche Schulen 18/19")

l.fragen.iss_englisch <- c(
  "Das Gesamtkonzept für die bbSt Englisch mit der Aufteilung in die Bereiche Fachdidaktik, Linguistik, Kulturwissenschaft, Literaturwissenschaft und Sprache ist zielführend.",
  "Die Inhalte für die bbSt Englisch sind aufeinander abgestimmt.",
  "Meine persönlichen Erwartungen an die Ausbildung werplot.spden/wurden erfüllt.")



# Variablen bereinigen
bbst_iss_englisch_c <- bbst_iss_englisch %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))


bbst_iss_englisch_1 <- bbst_1.recode(bbst_iss_englisch_c)



################
#### PLOTS #####
################

dir.create("4_Ergebnisse/Grafiken/bbst_iss_englisch", showWarnings = F)


kurse <- c("17/18", "18/19")


for (i in kurse) {

	if (i == "17/18") {
  	file   <- "4_Ergebnisse/Grafiken/bbst_iss_englisch/bbst_iss_englisch_1718.pdf"
		anzahl <- paste0("Teilnehmende: ", nrow(bbst_iss_englisch_c[grepl(i, bbst_iss_englisch_1$kurs), ]))
		kursnr <- paste0("bbSt Eng ISS/G ", i, "-1")

		write.csv(bbst_iss_englisch_c, "4_Ergebnisse/Tabellen/bbst_iss_englisch_1718.csv", na = "")

	}
	else if (i == "18/19") {
  	file   <- "4_Ergebnisse/Grafiken/bbst_iss_englisch/bbst_iss_englisch_1819.pdf"
		anzahl <- paste0("Teilnehmende: ", nrow(bbst_iss_englisch_c[grepl(i, bbst_iss_englisch_1$kurs), ]))
		kursnr <- paste0("bbSt Eng ISS/G ", i, "-1")

		write.csv(bbst_iss_englisch_c, "4_Ergebnisse/Tabellen/bbst_iss_englisch_1819.csv", na = "")
	}

  plot.geschl <- plot.pie(bbst_iss_englisch_1, i, geschl, T,
  	"Geschlecht")

  plot.alter <- plot.pie(bbst_iss_englisch_1, i, alter, T,
  	"Alter")

  plot.fach <- plot.pie(bbst_iss_englisch_1, i, fach, T,
  	"Welche Fachrichtung haben Sie studiert?")

  plot.erfahr <- plot.bar(bbst_iss_englisch_1, i, erfahr, T,
  	"Wie viele Jahre Berufserfahrung haben Sie in der studierten Fachrichtung?")

  plot.querein <- plot.bar(bbst_iss_englisch_1, i, querein, T,
  	"Sind Sie bereits vor Ihrem Quereinstieg in Ausbildungsbereichen tätig gewesen \n(z.B. als Trainer/in, im Bereich Nachhilfe, als Chorleitung etc.)?")

  plot.aufmerk_ausb <- plot.bar(bbst_iss_englisch_1, i, aufmerk_ausb, T,
  	"Wie wurden Sie auf diese Ausbildungsmöglichkeit aufmerksam?")

  plot.schultyp <- plot.bar(bbst_iss_englisch_1, i, schultyp, T,
  	"An welchem Schultyp sind Sie eingesetzt?")

	plot.klassen <- plot.bar.klassen(bbst_iss_englisch_c, bbst_iss_englisch_1)

  plot.stunden <- plot.bar(bbst_iss_englisch_1, i, stunden, T,
  	"Wie viele Stunden eigenständigen Unterricht erteilen Sie pro Woche?")

  plot.stunden_fach <- plot.bar(bbst_iss_englisch_1, i, stunden_fach, T,
  	"Wie viele Stunden davon unterrichten Sie Ihr anerkanntes Fach?")

  plot.stunden_stud <- plot.bar(bbst_iss_englisch_1, i, stunden_stud, T,
  	"Wie viele Stunden unterrichten Sie bereits in dem Fach,\nwelches Sie gerade in den berufsbegleitenden Studien belegen?")

	plot.einsatz <- plot.bar.einsatz(bbst_iss_englisch_c, bbst_iss_englisch_1)

  plot.leitung <- plot.pie(bbst_iss_englisch_1, i, leitung, T,
  	"Fühlen Sie sich seitens der Schulleitung ausreichend unterstützt?")

  plot.belast <- plot.bar.div(bbst_iss_englisch_1, i, belast,
  	"Wie hoch ist Ihre gefühlte Belastung durch die berufsbegleit. Studien und Schule?",
  	"völlig in Ordnung", "maximale Belastung")

  plot.verein <- plot.bar.div(bbst_iss_englisch_1, i, verein,
  	"Wie gelingt Ihnen die Vereinbarkeit von berufsbegleitenden Studien und Familie?",
  	"sehr gut", "ungenügend")

  plot.quali <- plot.bar.div(bbst_iss_englisch_1, i, quali,
  	"Wie empfanden Sie die Qualität der Beratung im Vorfeld der Ausbildung?",
  	"sehr gut", "ungenügend")

  plot.fehlt <- plot.bar.fehlt(bbst_iss_englisch_c, bbst_iss_englisch_1)


  ### plots for batteries
	# Fragen zu den berufsbegleitenden Studien
	bbst_iss_englisch_fragen <- batterie.recode(bbst_iss_englisch_c, i, "fragen")
	names(bbst_iss_englisch_fragen) <- l.fragen.iss_englisch

	# Fachdidaktik
	bbst_iss_englisch_fd <- batterie.recode(bbst_iss_englisch_c, i, "fd")
	names(bbst_iss_englisch_fd) <- items_13

	### Linguistik
	bbst_iss_englisch_lk <- batterie.recode(bbst_iss_englisch_c, i, "lk")
	names(bbst_iss_englisch_lk) <- items_13

	### Kulturrwissenschaft
	bbst_iss_englisch_kw <- batterie.recode(bbst_iss_englisch_c, i, "kw")
	names(bbst_iss_englisch_kw) <- items_13

	### Literaturwissenschaft
	bbst_iss_englisch_lw <- batterie.recode(bbst_iss_englisch_c, i, "lw")
	names(bbst_iss_englisch_lw) <- items_13

	### Sprache
	bbst_iss_englisch_sp <- batterie.recode(bbst_iss_englisch_c, i, "sp")
	names(bbst_iss_englisch_sp) <- items_13


  # Fragen zu den berufsbegleitenden Studien
  plot.fragen <- plot.likert(bbst_iss_englisch_fragen,
  	"Fragen zu den berufsbegleitenden Studien Englisch \nfür die Grundschule")

  # Fachdidaktik
  plot.fd <- plot.likert(bbst_iss_englisch_fd,
  	"Fragen zur Struktur und Ablauf in der Fachdidaktik")

	# Linguistik
	plot.lk <- plot.likert(bbst_iss_englisch_lk,
		"Fragen zur Struktur und Ablauf in der Linguistik")

  # Kulturwissenschaft
  plot.kw <- plot.likert(bbst_iss_englisch_kw,
  	"Fragen zur Struktur und Ablauf in der Kulturwissenschaft")

  # Literaturwissenschaft
  plot.lw <- plot.likert(bbst_iss_englisch_lw,
  	"Fragen zur Struktur und Ablauf in der Literaturwissenschaft")

  # Sprache
  plot.sp <- plot.likert(bbst_iss_englisch_sp,
  	"Fragen zur Struktur und Ablauf in der Sprache")



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

  grid.arrange(plot.fd, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.lk, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.kw, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.lw, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.sp, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  dev.off()

}
