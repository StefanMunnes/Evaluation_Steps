lab.kurs <- c(
	"A" = "Berufsbegleitende Studien Informatik ISS/Gymnasien/Berufliche Schulen 17/18",
	"B" = "Berufsbegleitende Studien Informatik ISS/Gymnasien/Berufliche Schulen 18/19")

lev.kurs <- c(
	"Berufsbegleitende Studien Informatik ISS/Gymnasien/Berufliche Schulen 17/18",
	"Berufsbegleitende Studien Informatik ISS/Gymnasien/Berufliche Schulen 18/19")


l.fragen.iss_info <- c(
  "Das Gesamtkonzept für die bbSt Informatik mit der Aufteilung in die Bereiche Fachvorlesung, Übung, Seminar und Praktika ist zielführend.",
  "Die Inhalte für die bbSt Informatik sind aufeinander abgestimmt.",
  "Meine persönlichen Erwartungen an die Ausbildung werden/wurden erfüllt.")



# Variablen bereinigen
bbst_iss_info_c <- bbst_iss_info %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))


bbst_iss_info_1 <- bbst_1.recode(bbst_iss_info_c)

# nur temporär, falsche codierung im tex-File schon behoben, noch nicht aktualisiert
names(bbst_iss_info_c)[names(bbst_iss_info_c) == "v22a"] <- "fragen_1"
names(bbst_iss_info_c)[names(bbst_iss_info_c) == "v22b"] <- "fragen_2"
names(bbst_iss_info_c)[names(bbst_iss_info_c) == "v22c"] <- "fragen_3"


write.csv(bbst_iss_info_c, "4_Ergebnisse/Tabellen/bbst_iss_info.csv", na = "")


################
#### PLOTS #####
################

dir.create("4_Ergebnisse/Grafiken/bbst_iss_info", showWarnings = F)


kurse <- as.vector(unique(na.omit(bbst_iss_info_1$kurs)))


for (i in kurse) {

	if (grepl("17/18", i)) {
		file   <- "4_Ergebnisse/Grafiken/bbst_iss_info/bbst_iss_info_1718.pdf"
		anzahl <- paste0("Teilnehmende: ",
										 nrow(bbst_iss_info_1[grepl(i, bbst_iss_info_1$kurs), ]))
	  kursnr <- "bbSt Info ISS/G 17/18-1"
	}

	else if (grepl("18/19", i)) {
		file   <- "4_Ergebnisse/Grafiken/bbst_iss_info/bbst_iss_info_1819.pdf"
		anzahl <- paste0("Teilnehmende: ",
										 nrow(bbst_iss_info_1[grepl(i, bbst_iss_info_1$kurs), ]))
	  kursnr <- "bbSt Info ISS/G 18/19-1"
	}


  plot.geschl <- plot.pie(bbst_iss_info_1, i, geschl, T,
  	"Geschlecht")

  plot.alter <- plot.pie(bbst_iss_info_1, i, alter, T,
  	"Alter")

  plot.fach <- plot.pie(bbst_iss_info_1, i, fach, T,
  	"Welche Fachrichtung haben Sie studiert?")

  plot.erfahr <- plot.bar(bbst_iss_info_1, i, erfahr, T,
  	"Wie viele Jahre Berufserfahrung haben Sie in der studierten Fachrichtung?")

  plot.querein <- plot.bar(bbst_iss_info_1, i, querein, T,
  	"Sind Sie bereits vor Ihrem Quereinstieg in Ausbildungsbereichen tätig gewesen \n(z.B. als Trainer/in, im Bereich Nachhilfe, als Chorleitung etc.)?")

  plot.aufmerk_ausb <- plot.bar(bbst_iss_info_1, i, aufmerk_ausb, T,
  	"Wie wurden Sie auf diese Ausbildungsmöglichkeit aufmerksam?")

  plot.schultyp <- plot.bar(bbst_iss_info_1, i, schultyp, T,
  	"An welchem Schultyp sind Sie eingesetzt?")

	plot.klassen <- plot.bar.klassen(bbst_iss_info_c, bbst_iss_info_1)

  plot.stunden <- plot.bar(bbst_iss_info_1, i, stunden, T,
  	"Wie viele Stunden eigenständigen Unterricht erteilen Sie pro Woche?")

  plot.stunden_fach <- plot.bar(bbst_iss_info_1, i, stunden_fach, T,
  	"Wie viele Stunden davon unterrichten Sie Ihr anerkanntes Fach?")

  plot.stunden_stud <- plot.bar(bbst_iss_info_1, i, stunden_stud, T,
  	"Wie viele Stunden unterrichten Sie bereits in dem Fach,\nwelches Sie gerade in den berufsbegleitenden Studien belegen?")

	plot.einsatz <- plot.bar.einsatz(bbst_iss_info_c, bbst_iss_info_1)

  plot.leitung <- plot.pie(bbst_iss_info_1, i, leitung, T,
  	"Fühlen Sie sich seitens der Schulleitung ausreichend unterstützt?")

  plot.belast <- plot.bar.div(bbst_iss_info_1, i, belast,
  	"Wie hoch ist Ihre gefühlte Belastung durch die berufsbegleit. Studien und Schule?",
  	"völlig in Ordnung", "maximale Belastung")

  plot.verein <- plot.bar.div(bbst_iss_info_1, i, verein,
  	"Wie gelingt Ihnen die Vereinbarkeit von berufsbegleitenden Studien und Familie?",
  	"sehr gut", "ungenügend")

  plot.quali <- plot.bar.div(bbst_iss_info_1, i, quali,
  	"Wie empfanden Sie die Qualität der Beratung im Vorfeld der Ausbildung?",
  	"sehr gut", "ungenügend")

  plot.fehlt <- plot.bar.fehlt(bbst_iss_info_c, bbst_iss_info_1)


  ### plots for batteries
	# Fragen zu den berufsbegleitenden Studien
	bbst_iss_info_fragen <- batterie.recode(bbst_iss_info_c, i, "fragen")
	names(bbst_iss_info_fragen) <- l.fragen.iss_info


	# Fachvorlesung
	bbst_iss_info_fv_1 <- batterie.recode(bbst_iss_info_c, i, "fv_1")
	names(bbst_iss_info_fv_1) <- items_9

	bbst_iss_info_fv_2 <- batterie.recode(bbst_iss_info_c, i, "fv_2")
	names(bbst_iss_info_fv_2) <- items_9

	bbst_iss_info_fv_3 <- batterie.recode(bbst_iss_info_c, i, "fv_3")
	names(bbst_iss_info_fv_3) <- items_9

	bbst_iss_info_fv_4 <- batterie.recode(bbst_iss_info_c, i, "fv_4")
	names(bbst_iss_info_fv_4) <- items_9

	bbst_iss_info_fv_5 <- batterie.recode(bbst_iss_info_c, i, "fv_5")
	names(bbst_iss_info_fv_5) <- items_9

	# Übungen
	bbst_iss_info_ue_1 <- batterie.recode(bbst_iss_info_c, i, "ue_1")
	names(bbst_iss_info_ue_1) <- items_9

	bbst_iss_info_ue_2 <- batterie.recode(bbst_iss_info_c, i, "ue_2")
	names(bbst_iss_info_ue_2) <- items_9

	bbst_iss_info_ue_3 <- batterie.recode(bbst_iss_info_c, i, "ue_3")
	names(bbst_iss_info_ue_3) <- items_9

	bbst_iss_info_ue_4 <- batterie.recode(bbst_iss_info_c, i, "ue_4")
	names(bbst_iss_info_ue_4) <- items_9

	bbst_iss_info_ue_5 <- batterie.recode(bbst_iss_info_c, i, "ue_5")
	names(bbst_iss_info_ue_5) <- items_9

	# Seminare
	bbst_iss_info_se_1 <- batterie.recode(bbst_iss_info_c, i, "se_1")
	names(bbst_iss_info_se_1) <- items_9

	# Praktika
	bbst_iss_info_pr_1 <- batterie.recode(bbst_iss_info_c, i, "pr_1")
	names(bbst_iss_info_pr_1) <- items_9

	bbst_iss_info_pr_2 <- batterie.recode(bbst_iss_info_c, i, "pr_2")
	names(bbst_iss_info_pr_2) <- items_9


	# Fragen zu den berufsbegleitenden Studien
	plot.fragen <- plot.likert(bbst_iss_info_fragen,
		"Fragen zu den berufsbegleitenden Studien Informatik \nfür ISS/Gymnasien/Berufliche Schulen")


	if (grepl("17/18", i)) {

		# Fachvorlesung
		plot.fv_1 <- plot.likert.sub(bbst_iss_info_fv_1,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Datenstrukturen und Datenabstraktion/ALP III")

		plot.fv_2 <- plot.likert.sub(bbst_iss_info_fv_2,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Datenbanksysteme")

		# Übungen
		plot.ue_1 <-plot.likert.sub(bbst_iss_info_ue_1,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Datenstrukturen und Datenabstraktion/ALP III")

		plot.ue_2 <-plot.likert.sub(bbst_iss_info_ue_2,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Datenbanksysteme")

		# Seminare
		plot.se_1 <-plot.likert.sub(bbst_iss_info_se_1,
			"Fragen zur Struktur und Ablauf der Seminare",
			"Rechnernetze")

			# Praktikum
		plot.pr_1 <-plot.likert.sub(bbst_iss_info_pr_1,
			"Fragen zur Struktur und Ablauf der Praktika",
			"Unterrichtsbezogenes Softwarepraktikum")

		plot.pr_2 <-plot.likert.sub(bbst_iss_info_pr_2,
			"Fragen zur Struktur und Ablauf der Praktika",
			"Unterrichtsbezogenes Datenbankpraktikum")

	}

	if (grepl("18/19", i)) {

		# Fachvorlesung
		plot.fv_1 <- plot.likert.sub(bbst_iss_info_fv_1,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Funktionale Programmierung/ALP I")

		plot.fv_2 <- plot.likert.sub(bbst_iss_info_fv_2,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Rechnerstrukturen")

		plot.fv_3 <- plot.likert.sub(bbst_iss_info_fv_3,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Imperative und objektorientierte Programmierung/ALP II")

		plot.fv_4 <- plot.likert.sub(bbst_iss_info_fv_4,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Rechnerorganisation")

		plot.fv_5 <- plot.likert.sub(bbst_iss_info_fv_5,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Grundlagen der Theoretischen Informatik")

		# Übungen
		plot.ue_1 <-plot.likert.sub(bbst_iss_info_ue_1,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Funktionale Programmierung/ALP I")

		plot.ue_2 <-plot.likert.sub(bbst_iss_info_ue_2,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Rechnerstrukturen")

		plot.ue_3 <-plot.likert.sub(bbst_iss_info_ue_3,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Imperative und objektorientierte Programmierung/ALP II")

		plot.ue_4 <-plot.likert.sub(bbst_iss_info_ue_4,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Rechnerorganisation")

		plot.ue_5 <-plot.likert.sub(bbst_iss_info_ue_5,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Grundlagen der Theoretischen Informatik")

		# Seminare
		plot.se_1 <-plot.likert.sub(bbst_iss_info_se_1,
			"Fragen zur Struktur und Ablauf der Seminare",
			"Betriebssystemwerkzeuge")

	}


  ## plots to pdf
  pdf(file, width = 9, height = 8)

  plot(0:10, type = "n", xaxt= "n", yaxt= "n", bty= "n", xlab = "", ylab = "")

  text(5.5, 8, "Evaluation", cex = 2)
  text(5.5, 7, "Berufsbegleitende Studien")
  text(5.5, 6, kursnr)

  text(5.5, 4, anzahl)

  rasterImage(beberlin, 1, 0, 4.5, 1)
  rasterImage(steps, 6.5, 0 , 9, 1)


	if (i == "Mathe" & length(kurse) > 1) {
  	grid.arrange(plot.kurs, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	}

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

	if (grepl("17/18", i)) {

		grid.arrange(plot.fv_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.fv_2, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_2, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.se_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.pr_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.pr_2, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

	}

	if (grepl("18/19", i)) {

		grid.arrange(plot.fv_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.fv_2, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.fv_3, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.fv_4, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.fv_5, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_2, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_3, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_4, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_5, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.se_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

	}

  dev.off()
}
