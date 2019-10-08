lab.kurs <- c(
	"A" = "Ergänzungs- und Erweiterungsstudium Informatik WB-ES Inf 17/18",
	"B" = "Ergänzungs- und Erweiterungsstudium Informatik WB-ES Inf 18/19")

lev.kurs <- c(
	"Ergänzungs- und Erweiterungsstudium Informatik WB-ES Inf 17/18",
	"Ergänzungs- und Erweiterungsstudium Informatik WB-ES Inf 18/19")



# Variablen bereinigen
es_info_c <- bwb_info %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))


es_info_1 <- es_1.recode(es_info_c)

# nur temporär, falsche codierung im tex-File schon behoben, noch nicht aktualisiert
# names(es_info_c)[names(es_info_c) == "v22a"] <- "fragen_1"
# names(es_info_c)[names(es_info_c) == "v22b"] <- "fragen_2"
# names(es_info_c)[names(es_info_c) == "v22c"] <- "fragen_3"


write.csv(es_info_c, "4_Ergebnisse/Tabellen/es_info.csv", na = "")


################
#### PLOTS #####
################

dir.create("4_Ergebnisse/Grafiken/es_info", showWarnings = F)

kurse <- as.vector(unique(na.omit(es_info_1$kurs)))

for (i in kurse) {

	if (grepl("17/18", i)) {
		file   <- "4_Ergebnisse/Grafiken/es_info/es_info_1718.pdf"
		anzahl <- paste0("Teilnehmende: ",
										 nrow(es_info_1[grepl(i, es_info_1$kurs), ]))
	  kursnr <- "ES Inf 17/18-1"
	}

	else if (grepl("18/19", i)) {
		file   <- "4_Ergebnisse/Grafiken/es_info/es_info_1819.pdf"
		anzahl <- paste0("Teilnehmende: ",
										 nrow(es_info_1[grepl(i, es_info_1$kurs), ]))
	  kursnr <- "ES Inf 18/19-1"
	}


	plot.erstes <- plot.pie(es_info_1, i, erstes, T,
		"Besuchen Sie zum ersten Mal eine berufsbegleitende Weiterbildung?")

	plot.absolv <- plot.bar.absolv(es_info_c, es_info_1)

  plot.aufmerk_weit <- plot.bar.aufmerk_weit(es_info_c, es_info_1)

  plot.gruend <- plot.bar.gruend(es_info_c, es_info_1)

	plot.zufried <- plot.bar.div(es_info_1, i, zufried,
		"Sind Sie insgesamt mit der Weiterbildung zufrieden?",
		"sehr", "gar nicht")

	plot.stunden_weit <- plot.bar(es_info_1, i, stunden_weit, T,
		"Wie viele Stunden wenden Sie für diese Weiterbildung pro Woche auf \n(Vor- und Nachbereitung etc., ohne Präsenzzeiten in der Veranstaltung)?")


  ### plots for batteries
	# Fachvorlesung
	es_info_fv_1 <- batterie.recode(es_info_c, i, "fv_1")
	names(es_info_fv_1) <- items_9

	es_info_fv_2 <- batterie.recode(es_info_c, i, "fv_2")
	names(es_info_fv_2) <- items_9

	es_info_fv_3 <- batterie.recode(es_info_c, i, "fv_3")
	names(es_info_fv_3) <- items_9

	es_info_fv_4 <- batterie.recode(es_info_c, i, "fv_4")
	names(es_info_fv_4) <- items_9

	es_info_fv_5 <- batterie.recode(es_info_c, i, "fv_5")
	names(es_info_fv_5) <- items_9

	# Fachdidaktik
	es_info_fd_1 <- batterie.recode(es_info_c, i, "fd_1")
	names(es_info_fd_1) <- items_13

	es_info_fd_2 <- batterie.recode(es_info_c, i, "fd_2")
	names(es_info_fd_2) <- items_13

	# Übungen
	es_info_ue_1 <- batterie.recode(es_info_c, i, "ue_1")
	names(es_info_ue_1) <- items_9

	es_info_ue_2 <- batterie.recode(es_info_c, i, "ue_2")
	names(es_info_ue_2) <- items_9

	es_info_ue_3 <- batterie.recode(es_info_c, i, "ue_3")
	names(es_info_ue_3) <- items_9

	es_info_ue_4 <- batterie.recode(es_info_c, i, "ue_4")
	names(es_info_ue_4) <- items_9

	es_info_ue_5 <- batterie.recode(es_info_c, i, "ue_5")
	names(es_info_ue_5) <- items_9

	# Seminare
	es_info_se_1 <- batterie.recode(es_info_c, i, "se_1")
	names(es_info_se_1) <- items_9

	# Praktika
	es_info_pr_1 <- batterie.recode(es_info_c, i, "pr_1")
	names(es_info_pr_1) <- items_9

	es_info_pr_2 <- batterie.recode(es_info_c, i, "pr_2")
	names(es_info_pr_2) <- items_9


	if (grepl("17/18", i)) {

		# Fachvorlesung
		plot.fv_1 <- plot.likert.sub(es_info_fv_1,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Datenstrukturen und Datenabstraktion/ALP III")

		plot.fv_2 <- plot.likert.sub(es_info_fv_2,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Datenbanksysteme")

		plot.fv_3 <- plot.likert.sub(es_info_fv_3,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Nichtsequentielle und verteilte Programmierung/ALP IV")

		# Fachdidaktik
		plot.fd_1 <- plot.likert.sub(es_info_fd_1,
			"Fragen zu Struktur und Ablauf in der Fachdidaktik",
			"Fachdidaktik Informatik")

		plot.fd_2 <- plot.likert.sub(es_info_fd_2,
			"Fragen zu Struktur und Ablauf in der Fachdidaktik",
			"Analyse fachlichen Lernens")

		# Übungen
		plot.ue_1 <-plot.likert.sub(es_info_ue_1,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Datenstrukturen und Datenabstraktion/ALP III")

		plot.ue_2 <-plot.likert.sub(es_info_ue_2,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Datenbanksysteme")

		plot.ue_3 <-plot.likert.sub(es_info_ue_3,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Nichtsequentielle und verteilte Programmierung/ALP IV")

		# Seminare
		plot.se_1 <-plot.likert.sub(es_info_se_1,
			"Fragen zur Struktur und Ablauf der Seminare",
			"Rechnernetze")

			# Praktikum
		plot.pr_1 <-plot.likert.sub(es_info_pr_1,
			"Fragen zur Struktur und Ablauf der Praktika",
			"Unterrichtsbezogenes Softwarepraktikum")

		plot.pr_2 <-plot.likert.sub(es_info_pr_2,
			"Fragen zur Struktur und Ablauf der Praktika",
			"Unterrichtsbezogenes Datenbankpraktikum")

	}

	if (grepl("18/19", i)) {

		# Fachvorlesung
		plot.fv_1 <- plot.likert.sub(es_info_fv_1,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Funktionale Programmierung/ALP I")

		plot.fv_2 <- plot.likert.sub(es_info_fv_2,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Rechnerstrukturen")

		plot.fv_3 <- plot.likert.sub(es_info_fv_3,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Imperative und objektorientierte Programmierung/ALP II")

		plot.fv_4 <- plot.likert.sub(es_info_fv_4,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Rechnerorganisation")

		plot.fv_5 <- plot.likert.sub(es_info_fv_5,
			"Fragen zu Struktur und Ablauf der Fachvorlesungen",
			"Grundlagen der Theoretischen Informatik")

		# Übungen
		plot.ue_1 <-plot.likert.sub(es_info_ue_1,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Funktionale Programmierung/ALP I")

		plot.ue_2 <-plot.likert.sub(es_info_ue_2,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Rechnerstrukturen")

		plot.ue_3 <-plot.likert.sub(es_info_ue_3,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Imperative und objektorientierte Programmierung/ALP II")

		plot.ue_4 <-plot.likert.sub(es_info_ue_4,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Rechnerorganisation")

		plot.ue_5 <-plot.likert.sub(es_info_ue_5,
			"Fragen zu Struktur und Ablauf der Übungen",
			"Grundlagen der Theoretischen Informatik")

		# Seminare
		plot.se_1 <-plot.likert.sub(es_info_se_1,
			"Fragen zur Struktur und Ablauf der Seminare",
			"Betriebssystemwerkzeuge")

	}

	plot.empfehl <- plot.pie(es_info_1, i, empfehl, T,
  "Würden Sie diese Weiterbildung weiterempfehlen?")

  plot.andere <- plot.bar.andere(es_info_c, es_info_1)

  plot.geschl <- plot.pie(es_info_1, i, geschl, T,
  	"Geschlecht")

  plot.alter <- plot.pie(es_info_1, i, alter, T,
  	"Alter")

  plot.schultyp <- plot.bar(es_info_1, i, schultyp, T,
  	"An welchem Schultyp sind Sie eingesetzt?")

  plot.klassen <- plot.bar.klassen(es_info_c, es_info_1)


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

	if (grepl("17/18", i)) {

		grid.arrange(plot.fv_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.fv_2, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.fv_3, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.fd_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.fd_2, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_2, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
		grid.arrange(plot.ue_3, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
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

	grid.arrange(plot.empfehl, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.andere, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.geschl, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.alter, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.schultyp, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.klassen, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  dev.off()
}
