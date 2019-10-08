
lab.kurs <- c(
	"A" = "Berufsbegleitende Studien Naturwissenschaften Grundschule A",
	"B" = "Berufsbegleitende Studien Naturwissenschaften Grundschule B")

lev.kurs <- c(
	"Berufsbegleitende Studien Naturwissenschaften Grundschule A",
	"Berufsbegleitende Studien Naturwissenschaften Grundschule B")


l.fragen.gs_nawi <- c(
  "Das Gesamtkonzept für die bbSt Naturwissenschaften ist überzeugend.",
  "Die Inhalte sind aufeinander abgestimmt.",
  "Meine persönlichen Erwartungen an die Ausbildung werden/wurden erfüllt.")



# Variablen bereinigen
bbst_gs_nawi_c <- bbst_gs_nawi %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))


bbst_gs_nawi_1 <- bbst_1.recode(bbst_gs_nawi_c)



################
#### PLOTS #####
################

dir.create("4_Ergebnisse/Grafiken/bbst_gs_nawi", showWarnings = F)

kursname <- "bbSt Nawi GS 18/19-1"
kursfile <- "bbst_gs_nawi_1819"

if (length(lev.kurs) > 1) {
	kurse <- c("Naturwissenschaften", as.vector(unique(na.omit(bbst_gs_nawi_1$kurs))))
} else {
	kurse <- "Naturwissenschaften"
}

for (i in kurse) {

	if (i == "Naturwissenschaften") {
  	file   <- paste0("4_Ergebnisse/Grafiken/bbst_gs_nawi/", kursfile , ".pdf")
		anzahl <- paste0("Teilnehmende: ", nrow(bbst_gs_nawi_c))
		kursnr <- kursname

		write.csv(bbst_gs_nawi_c, paste0("4_Ergebnisse/Tabellen/", kursfile, ".csv"), na = "")

		if (length(lev.kurs) > 1) {
			plot.kurs <- bbst_gs_nawi_1 %>%
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
		}
	}
	else {
		file   <- paste0("4_Ergebnisse/Grafiken/bbst_gs_nawi/", kursfile, " ", str_sub(i, -1, -1), ".pdf")
		anzahl <- paste0("Teilnehmende: ", nrow(bbst_gs_nawi_1[grepl(i, bbst_gs_nawi_1$kurs), ]))
	  kursnr <- paste(kursname, str_sub(i, -1, -1), sep = " ")
	}

  plot.geschl <- plot.pie(bbst_gs_nawi_1, i, geschl, T,
  	"Geschlecht")

  plot.alter <- plot.pie(bbst_gs_nawi_1, i, alter, T,
  	"Alter")

  plot.fach <- plot.pie(bbst_gs_nawi_1, i, fach, T,
  	"Welche Fachrichtung haben Sie studiert?")

  plot.erfahr <- plot.bar(bbst_gs_nawi_1, i, erfahr, T,
  	"Wie viele Jahre Berufserfahrung haben Sie in der studierten Fachrichtung?")

  plot.querein <- plot.bar(bbst_gs_nawi_1, i, querein, T,
  	"Sind Sie bereits vor Ihrem Quereinstieg in Ausbildungsbereichen tätig gewesen \n(z.B. als Trainer/in, im Bereich Nachhilfe, als Chorleitung etc.)?")

  plot.aufmerk_ausb <- plot.bar(bbst_gs_nawi_1, i, aufmerk_ausb, T,
  	"Wie wurden Sie auf diese Ausbildungsmöglichkeit aufmerksam?")

  plot.schultyp <- plot.bar(bbst_gs_nawi_1, i, schultyp, T,
  	"An welchem Schultyp sind Sie eingesetzt?")

	plot.klassen <- plot.bar.klassen(bbst_gs_nawi_c, bbst_gs_nawi_1)

  plot.stunden <- plot.bar(bbst_gs_nawi_1, i, stunden, T,
  	"Wie viele Stunden eigenständigen Unterricht erteilen Sie pro Woche?")

  plot.stunden_fach <- plot.bar(bbst_gs_nawi_1, i, stunden_fach, T,
  	"Wie viele Stunden davon unterrichten Sie Ihr anerkanntes Fach?")

  plot.stunden_stud <- plot.bar(bbst_gs_nawi_1, i, stunden_stud, T,
  	"Wie viele Stunden unterrichten Sie bereits in dem Fach,\nwelches Sie gerade in den berufsbegleitenden Studien belegen?")

	plot.einsatz <- plot.bar.einsatz(bbst_gs_nawi_c, bbst_gs_nawi_1)

  plot.leitung <- plot.pie(bbst_gs_nawi_1, i, leitung, T,
  	"Fühlen Sie sich seitens der Schulleitung ausreichend unterstützt?")

  plot.belast <- plot.bar.div(bbst_gs_nawi_1, i, belast,
  	"Wie hoch ist Ihre gefühlte Belastung durch die berufsbegleit. Studien und Schule?",
  	"völlig in Ordnung", "maximale Belastung")

  plot.verein <- plot.bar.div(bbst_gs_nawi_1, i, verein,
  	"Wie gelingt Ihnen die Vereinbarkeit von berufsbegleitenden Studien und Familie?",
  	"sehr gut", "ungenügend")

  plot.quali <- plot.bar.div(bbst_gs_nawi_1, i, quali,
  	"Wie empfanden Sie die Qualität der Beratung im Vorfeld der Ausbildung?",
  	"sehr gut", "ungenügend")

  plot.fehlt <- plot.bar.fehlt(bbst_gs_nawi_c, bbst_gs_nawi_1)


  ### plots for batteries
	# Fragen zu den berufsbegleitenden Studien
	bbst_gs_nawi_fragen <- batterie.recode(bbst_gs_nawi_c, i, "fragen")
	names(bbst_gs_nawi_fragen) <- l.fragen.gs_nawi

	# Kompetenzerwerb
	bbst_gs_nawi_ke <- batterie.recode(bbst_gs_nawi_c, i, "ke")
	names(bbst_gs_nawi_ke) <- items_ke

	### Studiengeschehen
	bbst_gs_nawi_sg <- batterie.recode(bbst_gs_nawi_c, i, "sg")
	names(bbst_gs_nawi_sg) <- items_sg


  # Fragen zu den berufsbegleitenden Studien
  plot.fragen <- plot.likert(bbst_gs_nawi_fragen,
  	"Fragen zu den berufsbegleitenden Studien Naturwissenschaften \nfür die Grundschule")

  # Kompetenzerwerb
	plot.ke <- likert(bbst_gs_nawi_ke) %>%
  	likert.bar.plot(group.order = names(.$items),
  		plot.percent.low = F, plot.percent.high = F,
  		colors = colors.likert, wrap = 60, legend = NULL) +
  	labs(title = "Fragen zum Kompetenzerwerb") +
  	ylab("Zustimmung in Prozent") +
  	theme_light() +
  	theme(legend.position="bottom",
  				legend.text = element_text(size = 9),
  				legend.key.size = unit(.5, "cm"),
          plot.title = element_text(hjust = 0, size = 14, face = "bold")
				  )

  # Studiengeschehen
  plot.sg <- plot.likert(bbst_gs_nawi_sg,
  	"Fragen zur Struktur und Studiengeschehen")


  ## plots to pdf
  pdf(file, width = 9, height = 8)

  plot(0:10, type = "n", xaxt= "n", yaxt= "n", bty= "n", xlab = "", ylab = "")

  text(5.5, 8, "Evaluation", cex = 2)
  text(5.5, 7, "Berufsbegleitende Studien")
  text(5.5, 6, kursnr)

  text(5.5, 4, anzahl)

  rasterImage(beberlin, 1, 0, 4.5, 1)
  rasterImage(steps, 6.5, 0 , 9, 1)


	if (i == "Naturwissenschaften" & length(kurse) > 1) {
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

  grid.arrange(plot.ke, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.sg, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  dev.off()

}
