lab.kurs <- c(
  "A" = "Sonderpädagogische Zusatzqualifikation - Förderschwerpunkt Autismus",
  "B" = "Sonderpädagogische Zusatzausbildung für Pädagogische Unterrichtshilfen 17/18",
  "C" = "Qualifizierung Durchgängige Sprachbildung unter Beachtung neu zugewanderter Schüler/innen",
  "D" = "Qualifizierung Sprachbildungskoordinatorin bzw. Sprachbildungskoordinator",
  "E" = "Weiterbildungslehrgang Ethik",
  "F" = "Weiterbildungslehrgang Gesellschaftswissenschaften in den Klassenstufen 5 und 6",
  "G" = "Weiterbildungslehrgang Mathematik",
  "H" = "Weiterbildungslehrgang Musik",
  "I" = "Weiterbildungslehrgang Psychologie",
  "J" = "Weiterbildungslehrgang Theater Sek. I/Darstellendes Spiel Sek. II",
  "K" = "Weiterbildungslehrgang Schwimmen",
  "L" = "Zusatzqualifikation Facherzieherin und Facherzieher für Integration Gruppe A",
  "M" = "Zusatzqualifikation Facherzieherin und Facherzieher für Integration Gruppe B",
  "N" = "Zusatzqualifikation Facherzieherin und Facherzieher für Integration Gruppe C",
  "O" = "Berufsbegleitende Weiterbildung für die Veranstaltung Erweiterungsstudium Sonderpädagogik in der Fachrichtung Lernen/ Emotional und soziale Entwicklung",
  "Z" = "Sonderpädagogische Zusatzausbildung für Pädagogische Unterrichtshilfen 18/19")

lev.kurs <- c(
  "Sonderpädagogische Zusatzqualifikation - Förderschwerpunkt Autismus",
  "Sonderpädagogische Zusatzausbildung für Pädagogische Unterrichtshilfen 17/18",
  "Qualifizierung Durchgängige Sprachbildung unter Beachtung neu zugewanderter Schüler/innen",
  "Qualifizierung Sprachbildungskoordinatorin bzw. Sprachbildungskoordinator",
  "Weiterbildungslehrgang Ethik",
  "Weiterbildungslehrgang Gesellschaftswissenschaften in den Klassenstufen 5 und 6",
  "Weiterbildungslehrgang Mathematik",
  "Weiterbildungslehrgang Musik",
  "Weiterbildungslehrgang Psychologie",
  "Weiterbildungslehrgang Theater Sek. I/Darstellendes Spiel Sek. II",
  "Weiterbildungslehrgang Schwimmen",
  "Zusatzqualifikation Facherzieherin und Facherzieher für Integration Gruppe A",
  "Zusatzqualifikation Facherzieherin und Facherzieher für Integration Gruppe B",
  "Zusatzqualifikation Facherzieherin und Facherzieher für Integration Gruppe C",
  "Berufsbegleitende Weiterbildung für die Veranstaltung Erweiterungsstudium Sonderpädagogik in der Fachrichtung Lernen/ Emotional und soziale Entwicklung",
  "Sonderpädagogische Zusatzausbildung für Pädagogische Unterrichtshilfen 18/19")


# Variablen bereinigen
es_quali_c <- bwb_quali %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))

# nur temporär, falsche codierung im tex-File schon behoben, noch nicht aktualisiert
names(es_quali_c)[names(es_quali_c) == "v13"] <- "geschl"
names(es_quali_c)[names(es_quali_c) == "v14"] <- "alter"
names(es_quali_c)[names(es_quali_c) == "v15"] <- "schultyp"
names(es_quali_c)[names(es_quali_c) == "v16"] <- "klassen"


# Variablen bereinigen
es_quali_neu_c <- bwb_quali_neu %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))

# nur temporär, falsche codierung im tex-File schon behoben, noch nicht aktualisiert
names(es_quali_neu_c)[names(es_quali_neu_c) == "v13"] <- "geschl"
names(es_quali_neu_c)[names(es_quali_neu_c) == "v14"] <- "alter"
names(es_quali_neu_c)[names(es_quali_neu_c) == "v15"] <- "schultyp"
names(es_quali_neu_c)[names(es_quali_neu_c) == "v16"] <- "klassen"

# nur temporär, da SonderPädagogik bisher nur im quali_neu
es_quali_c <- es_quali_c[,order(colnames(es_quali_c),decreasing=T)]
es_quali_neu_c <- es_quali_neu_c[,order(colnames(es_quali_neu_c),decreasing=T)]
es_quali_c <- rbind(es_quali_c, es_quali_neu_c)


# PU 17/18 und 18/19 kann im Bogen nicht unterschieden werden, daher hier händisch
# 18/19
es_quali_c[es_quali_c$Prüfung > 242 & es_quali_c$Prüfung <= 295, "kurs"] <- "Z"

es_quali_1 <- es_1.recode(es_quali_c)


write.csv(es_quali_c, "4_Ergebnisse/Tabellen/es_quali_1819.csv", na = "")

################
#### PLOTS #####
################

dir.create("4_Ergebnisse/Grafiken/es_quali", showWarnings = F)

kurse <- c(as.vector(unique(na.omit(es_quali_1$kurs))),
           "Zusatzqualifikation Facherzieherin und Facherzieher für Integration")

for (i in kurse) {

  if (i == "Sonderpädagogische Zusatzqualifikation - Förderschwerpunkt Autismus") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/Q-Aut_1819.pdf"
    kurstyp <- "Qualifizierung"
    kursnr  <- "Q-Aut 18/19-1"
  }
  if (i == "Sonderpädagogische Zusatzausbildung für Pädagogische Unterrichtshilfen 17/18") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/PU_1718.pdf"
    kurstyp <- "Pädagogische Unterrichtshilfen"
    kursnr  <- "PU 17/18-1"
  }
  if (i == "Sonderpädagogische Zusatzausbildung für Pädagogische Unterrichtshilfen 18/19") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/PU_1819.pdf"
    kurstyp <- "Pädagogische Unterrichtshilfen"
    kursnr  <- "PU 18/19-1"
  }
  if (i == "Qualifizierung Durchgängige Sprachbildung unter Beachtung neu zugewanderter Schüler/innen") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/Q-DSB_1819.pdf"
    kurstyp <- "Qualifizierung"
    kursnr  <- "Q-DSB 18/19-1"
  }
  if (i == "Qualifizierung Sprachbildungskoordinatorin bzw. Sprachbildungskoordinator") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/Q-SBK_1819.pdf"
    kurstyp <- "Qualifizierung"
    kursnr  <- "Q-SBK 18/19-1"
  }
  if (i == "Weiterbildungslehrgang Ethik") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/L-Et_1819.pdf"
    kurstyp <- "Weiterbildungslehrgang"
    kursnr  <- "L-Et 18/19-1"
  }
  if (i == "Weiterbildungslehrgang Gesellschaftswissenschaften in den Klassenstufen 5 und 6") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/L-Gew_1819.pdf"
    kurstyp <- "Weiterbildungslehrgang"
    kursnr  <- "L-Gewi 18/19-1"
  }
  if (i == "Weiterbildungslehrgang Mathematik") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/L-Ma_1819.pdf"
    kurstyp <- "Weiterbildungslehrgang"
    kursnr  <- "L-Ma 18/19-1"
  }
  if (i == "Weiterbildungslehrgang Musik") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/L-Mu_1819.pdf"
    kurstyp <- "Weiterbildungslehrgang"
    kursnr  <- "L-Mu 18/19-1"
  }
  if (i == "Weiterbildungslehrgang Psychologie") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/L-Psy_1819.pdf"
    kurstyp <- "Weiterbildungslehrgang"
    kursnr  <- "L-Psy 18/19-1"
  }
  if (i == "Weiterbildungslehrgang Theater Sek. I/Darstellendes Spiel Sek. II") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/L-DS_1819.pdf"
    kurstyp <- "Weiterbildungslehrgang"
    kursnr  <- "L-DS 18/19-1"
  }
  if (i == "Weiterbildungslehrgang Schwimmen") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/L-Schw_1819.pdf"
    kurstyp <- "Weiterbildungslehrgang"
    kursnr  <- "L-Schw 18/19-1"
  }
  if (i == "Berufsbegleitende Weiterbildung für die Veranstaltung Erweiterungsstudium Sonderpädagogik in der Fachrichtung Lernen/ Emotional und soziale Entwicklung") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/ES_SoPäd_LE_1718"
    kurstyp <- "Erweiterungsstudium"
    kursnr  <- "ES SoPäd L/E 17/18-1"
  }
  if (i == "Zusatzqualifikation Facherzieherin und Facherzieher für Integration Gruppe A") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/FE_Int_1819_A.pdf"
    kurstpy <- "Facherzieherin/Facherzieher für Integration"
    kursnr  <- "FE f Int 18/19-1 A"
  }
  if (i == "Zusatzqualifikation Facherzieherin und Facherzieher für Integration Gruppe B") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/FE_Int_1819_B.pdf"
    kurstpy <- "Facherzieherin/Facherzieher für Integration"
    kursnr  <- "FE f Int 18/19-1 B"
  }
  if (i == "Zusatzqualifikation Facherzieherin und Facherzieher für Integration Gruppe C") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/FE_Int_1819_C.pdf"
    kurstpy <- "Facherzieherin/Facherzieher für Integration"
    kursnr  <- "FE f Int 18/19-1 C"
  }

  if (i == "Zusatzqualifikation Facherzieherin und Facherzieher für Integration") {
    file    <- "4_Ergebnisse/Grafiken/es_quali/FE_Int_1819.pdf"
    kurstpy <- "Facherzieherin/Facherzieher für Integration"
    kursnr  <- "FE f Int 18/19-1"

    plot.kurs <- es_quali_1 %>%
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

  anzahl <- paste0("Teilnehmende: ", nrow(es_quali_1[grepl(i, es_quali_1$kurs), ]))


  plot.erstes <- plot.pie(es_quali_1, i, erstes, T,
    "Besuchen Sie zum ersten Mal eine berufsbegleitende Weiterbildung?")

  plot.absolv <- plot.bar.absolv(es_quali_c, es_quali_1)

  plot.aufmerk_weit <- plot.bar.aufmerk_weit(es_quali_c, es_quali_1)

  plot.gruend <- plot.bar.gruend(es_quali_c, es_quali_1)

  plot.zufried <- plot.bar.div(es_quali_1, i, zufried,
  	"Sind Sie insgesamt mit der Weiterbildung zufrieden?",
  	"sehr", "gar nicht")

  plot.stunden_weit <- plot.bar(es_quali_1, i, stunden_weit, T,
    "Wie viele Stunden wenden Sie für diese Weiterbildung pro Woche auf \n(Vor- und Nachbereitung etc., ohne Präsenzzeiten in der Veranstaltung)?")


  # Fragen zur Struktur und Organisation der Weiterbildung
	es_quali_v7 <- batterie.recode(es_quali_c, i, "v7")
	names(es_quali_v7) <- c(
    "Die koordinierende Leitung hat zu Beginn den Ablauf und die Ziele der Weiterbildung erläutert.",
    "Das Maßnahmengeschehen entspricht der Ankündigung.",
    "Die Planung der Lehrveranstaltungen ist klar und übersichtlich.",
    "Die Inhalte der Weiterbildung sind aufeinander abgestimmt.",
    "Die Dozierenden schaffen Transparenz in Bezug auf Leistungsanforderungen und -bewertung.",
    "Die Gruppengröße der Weiterbildung ist angemessen."
  )

  # Fragen zum Maßnahmengeschehen
	es_quali_v8 <- batterie.recode(es_quali_c, i, "v8")
	names(es_quali_v8) <- c(
    "Fragen, Erfahrungen und Anregungen der Teilnehmenden werden in den Lehrveranstaltungen aufgegriffen.",
    "Es gibt in den Lehrveranstaltungen ausreichend Übungsmöglichkeiten.",
    "Die Gestaltung der Lehrveranstaltungen ist abwechslungsreich.",
    "Es gibt genügend Zeit für den allgemeinen/fachlichen Austausch.",
    "Die Dozierenden erreichen, dass alle Teilnehmenden aktiv teilnehmen können.",
    "Die Lehrveranstaltungen beginnen und enden pünktlich.",
    "Die Teilnehmenden erscheinen pünktlich."
  )

	### Fragen zur Kompetenzerweiterung
	es_quali_v9 <- batterie.recode(es_quali_c, i, "v9")
	names(es_quali_v9) <- c(
    "Ich erwerbe bzw. vertiefe in der Weiterbildungsmaßnahme fachliche Kompetenzen.",
    "Das in der Weiterbildung Erlernte und Erfahrene ist für mich beruflich von Nutzen.",
    "Ich fühle mich gut auf den erfolgreichen Abschluss der Weiterbildung vorbereitet.",
    "Ich fühle mich gut auf die künftige Tätigkeit in der  Schule/Unterrichtstätigkeit vorbereitet.",
    "Meine persönlichen Erwartungen an die Weiterbildungsmaßnahme werden erfüllt."
  )

	### Fragen zu Materialien der Weiterbildung
	es_quali_v10 <- batterie.recode(es_quali_c, i, "v10")
	names(es_quali_v10) <- c(
    "Studienmaterial oder Literaturhinweise zur weiterführenden Arbeit werden zur Verfügung gestellt bzw. erarbeitet.",
    "Das Weiterbildungsmaterial ist aktuell und informativ."
  )


  # Fragen zur Struktur und Organisation der Weiterbildung
    plot.v7 <- likert(es_quali_v7) %>%
    	likert.bar.plot(group.order = names(.$items),
    		plot.percent.low = F, plot.percent.high = F,
    		colors = colors.likert, wrap = 40, legend = NULL) +
    	labs(title = "Fragen zur Struktur und Organisation der Weiterbildung") +
    	ylab("") +
    	theme_light() +
    	theme(legend.position="none",
    				legend.text = element_text(size = 9),
    				legend.key.size = unit(.5, "cm"),
            plot.title = element_text(hjust = 0, size = 14, face = "bold")
  				  )

  # Fragen zum Maßnahmengeschehen
  plot.v8 <- plot.likert(es_quali_v8,
  	"Fragen zum Maßnahmengeschehen")

	# Fragen zur Kompetenzerweiterung
  plot.v9 <- likert(es_quali_v9) %>%
    	likert.bar.plot(group.order = names(.$items),
    		plot.percent.low = F, plot.percent.high = F,
    		colors = colors.likert, wrap = 40, legend = NULL) +
    	labs(title = "Fragen zur Kompetenzerweiterung") +
    	ylab("") +
    	theme_light() +
    	theme(legend.position="none",
    				legend.text = element_text(size = 9),
    				legend.key.size = unit(.5, "cm"),
            plot.title = element_text(hjust = 0, size = 14, face = "bold")
  				  )

  # Fragen zu Materialien der Weiterbildung
  plot.v10 <- plot.likert(es_quali_v10,
  	"Fragen zu Materialien der Weiterbildung")


  plot.empfehl <- plot.pie(es_quali_1, i, empfehl, T,
    "Würden Sie diese Weiterbildung weiterempfehlen?")

  plot.andere <- plot.bar.andere(es_quali_c, es_quali_1)

  plot.geschl <- plot.pie(es_quali_1, i, geschl, T,
    "Geschlecht")

  plot.alter <- plot.pie(es_quali_1, i, alter, T,
    "Alter")

  plot.schultyp <- plot.bar(es_quali_1, i, schultyp, T,
    "An welchem Schultyp sind Sie eingesetzt?")

  plot.klassen <- plot.bar.klassen(es_quali_c, es_quali_1)


  ## plots to pdf
  pdf(file, width = 9, height = 8)

  plot(0:10, type = "n", xaxt= "n", yaxt= "n", bty= "n", xlab = "", ylab = "")

  text(5.5, 8, "Evaluation", cex = 2)
  text(5.5, 7, kurstyp)
  text(5.5, 6, kursnr)

  text(5.5, 4, anzahl)

  rasterImage(beberlin, 1, 0, 4.5, 1)
  rasterImage(steps, 6.5, 0 , 9, 1)

  if (i == "Zusatzqualifikation Facherzieherin und Facherzieher für Integration") {
  	grid.arrange(plot.kurs, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
	}

  grid.arrange(plot.erstes, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.absolv, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.aufmerk_weit, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.gruend, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.zufried, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.stunden_weit, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  grid.arrange(plot.v7, plot.v8, ncol = 1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.v9, plot.v10, ncol = 1, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  grid.arrange(plot.empfehl, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.andere, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.geschl, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.alter, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.schultyp, bottom = anzahl, vp=viewport(width=0.9, height=0.9))
  grid.arrange(plot.klassen, bottom = anzahl, vp=viewport(width=0.9, height=0.9))

  dev.off()
}
