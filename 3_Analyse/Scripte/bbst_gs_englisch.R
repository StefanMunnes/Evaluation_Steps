kursname <- "bbSt Eng GS 18/19-1"
kursfile <- "bbst_gs_englisch_1819"

lab.kurs <- c(
	"A" = "Berufsbegleitende Studien Englisch Grundschule")

lev.kurs <- c(
	"Berufsbegleitende Studien Englisch Grundschule")

l.fragen.gs_englisch <- c(
  "Das Gesamtkonzept für die bbSt Englisch mit der Aufteilung in die Bereiche Fachdidaktik, Literaturwissenschaft, Linguistik und Sprache ist zielführend.",
  "Die Inhalte in den Bereichen Fachdidaktik, Literaturwissenschaft und Linguistik sind aufeinander abgestimmt.",
  "Meine persönlichen Erwartungen an die Ausbildung werplot.spden/wurden erfüllt.")



# Variablen bereinigen
bbst_gs_englisch_c <- bbst_gs_englisch %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))


bbst_gs_englisch_1 <- bbst_1.recode(bbst_gs_englisch_c)



################
#### PLOTS #####
################

dir.create("4_Ergebnisse/Grafiken/bbst_gs_englisch", showWarnings = F)

if (length(lev.kurs) > 1) {
	kurse <- c("Englisch", lev.kurs)
} else {
	kurse <- "Englisch"
}

for (i in kurse) {

	if (i == "Englisch") {
  	file   <- paste0("4_Ergebnisse/Grafiken/bbst_gs_englisch/", kursfile , ".pdf")
		anzahl <- paste0("Teilnehmende: ", nrow(bbst_gs_englisch_c))
		kursnr <- kursname

		write.csv(bbst_gs_englisch_c, paste0("4_Ergebnisse/Tabellen/", kursfile, ".csv"), na = "")

		if (length(lev.kurs) > 1) {
			plot.kurs <- bbst_gs_englisch_1 %>%
				filter(!is.na((kurs)), str_detect(kurs, pattern = i)) %>%
				select(kurs) %>%
				ggplot(aes(x = reorder(kurs, desc(kurs)), fill = kurs)) +
					geom_bar(width = 0.6) +
					coord_flip() +
					scale_fill_manual(values = colors.n14) +
					theme_minimal() +
					labs(x = NULL, y = "Anzahl", title = "Kurse") +
					theme(legend.position = "none",
								plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
								axis.ticks.x = element_blank(),
								panel.grid.minor.x = element_blank(),
								panel.grid.major.y = element_blank())
		}
	}
	else {
		file   <- paste0("4_Ergebnisse/Grafiken/bbst_gs_englisch/", kursfile, " ", str_sub(i, -1, -1), ".pdf")
		anzahl <- paste0("Teilnehmende: ", nrow(bbst_gs_englisch_1[grepl(i, bbst_gs_englisch_1$kurs), ]))
	  kursnr <- paste0(kursname, " ", str_sub(i, -1, -1))
	}



  plot.geschl <- plot.pie(bbst_gs_englisch_1, i, geschl, T,
  	"Geschlecht")

  plot.alter <- plot.pie(bbst_gs_englisch_1, i, alter, T,
  	"Alter")

  plot.fach <- plot.pie(bbst_gs_englisch_1, i, fach, T,
  	"Welche Fachrichtung haben Sie studiert?")

  plot.erfahr <- plot.bar(bbst_gs_englisch_1, i, erfahr, T,
  	"Wie viele Jahre Berufserfahrung haben Sie in der studierten Fachrichtung?")

  plot.querein <- plot.bar(bbst_gs_englisch_1, i, querein, T,
  	"Sind Sie bereits vor Ihrem Quereinstieg in Ausbildungsbereichen tätig gewesen \n(z.B. als Trainer/in, im Bereich Nachhilfe, als Chorleitung etc.)?")

  plot.aufmerk_ausb <- plot.bar(bbst_gs_englisch_1, i, aufmerk_ausb, T,
  	"Wie wurden Sie auf diese Ausbildungsmöglichkeit aufmerksam?")

  plot.schultyp <- plot.bar(bbst_gs_englisch_1, i, schultyp, T,
  	"An welchem Schultyp sind Sie eingesetzt?")

	plot.klassen <- plot.bar.klassen(bbst_gs_englisch_c, bbst_gs_englisch_1)

  plot.stunden <- plot.bar(bbst_gs_englisch_1, i, stunden, T,
  	"Wie viele Stunden eigenständigen Unterricht erteilen Sie pro Woche?")

  plot.stunden_fach <- plot.bar(bbst_gs_englisch_1, i, stunden_fach, T,
  	"Wie viele Stunden davon unterrichten Sie Ihr anerkanntes Fach?")

  plot.stunden_stud <- plot.bar(bbst_gs_englisch_1, i, stunden_stud, T,
  	"Wie viele Stunden unterrichten Sie bereits in dem Fach,\nwelches Sie gerade in den berufsbegleitenden Studien belegen?")

	plot.einsatz <- plot.bar.einsatz(bbst_gs_englisch_c, bbst_gs_englisch_1)

  plot.leitung <- plot.pie(bbst_gs_englisch_1, i, leitung, T,
  	"Fühlen Sie sich seitens der Schulleitung ausreichend unterstützt?")

  plot.belast <- plot.bar.div(bbst_gs_englisch_1, i, belast,
  	"Wie hoch ist Ihre gefühlte Belastung durch die berufsbegleit. Studien und Schule?",
  	"völlig in Ordnung", "maximale Belastung")

  plot.verein <- plot.bar.div(bbst_gs_englisch_1, i, verein,
  	"Wie gelingt Ihnen die Vereinbarkeit von berufsbegleitenden Studien und Familie?",
  	"sehr gut", "ungenügend")

  plot.quali <- plot.bar.div(bbst_gs_englisch_1, i, quali,
  	"Wie empfanden Sie die Qualität der Beratung im Vorfeld der Ausbildung?",
  	"sehr gut", "ungenügend")

  plot.fehlt <- plot.bar.fehlt(bbst_gs_englisch_c, bbst_gs_englisch_1)


  ### plots for batteries
	# Fragen zu den berufsbegleitenden Studien
	bbst_gs_englisch_fragen <- batterie.recode(bbst_gs_englisch_c, i, "fragen")
	names(bbst_gs_englisch_fragen) <- l.fragen.gs_englisch

	# Fachdidaktik
	bbst_gs_englisch_fd <- batterie.recode(bbst_gs_englisch_c, i, "fd")
	names(bbst_gs_englisch_fd) <- items_13

	### Literaturwissenschaft
	bbst_gs_englisch_lw <- batterie.recode(bbst_gs_englisch_c, i, "lw")
	names(bbst_gs_englisch_lw) <- items_13

	### Linguistik
	bbst_gs_englisch_lk <- batterie.recode(bbst_gs_englisch_c, i, "lk")
	names(bbst_gs_englisch_lk) <- items_13

	### Sprache
	bbst_gs_englisch_sp <- batterie.recode(bbst_gs_englisch_c, i, "sp")
	names(bbst_gs_englisch_sp) <- items_13


  # Fragen zu den berufsbegleitenden Studien
  plot.fragen <- plot.likert(bbst_gs_englisch_fragen,
  	"Fragen zu den berufsbegleitenden Studien Englisch \nfür die Grundschule")

  # Fachdidaktik
  plot.fd <- plot.likert(bbst_gs_englisch_fd,
  	"Fragen zur Struktur und Ablauf in der Fachdidaktik")

  # Literaturwissenschaft
  plot.lw <- plot.likert(bbst_gs_englisch_lw,
  	"Fragen zur Struktur und Ablauf in der Literaturwissenschaft")

  # Linguistik
  plot.lk <- plot.likert(bbst_gs_englisch_lk,
  	"Fragen zur Struktur und Ablauf in der Linguistik")

  # Linguistik
  plot.sp <- plot.likert(bbst_gs_englisch_sp,
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


	if (i == "Englisch" & length(kurse) > 1) {
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

  grid.arrange(plot.fd, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.lw, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.lk, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	grid.arrange(plot.sp, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  dev.off()

}
