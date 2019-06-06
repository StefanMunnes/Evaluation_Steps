l.kurs <- c(
	"A" = "Berufsbegleitende Studien Deutsch Grundschule A",
	"B" = "Berufsbegleitende Studien Deutsch Grundschule B",
	"C" = "Berufsbegleitende Studien Deutsch Grundschule C",
	"D" = "Berufsbegleitende Studien Deutsch Grundschule D",
	"E" = "Berufsbegleitende Studien Deutsch Grundschule E",
	"F" = "Berufsbegleitende Studien Deutsch Grundschule F",
	"G" = "Berufsbegleitende Studien Deutsch Grundschule G",
	"H" = "Berufsbegleitende Studien Deutsch Grundschule H",
	"I" = "Berufsbegleitende Studien Deutsch Grundschule I")


# Variablen bereinigen
bbst_gs_deutsch <- bbst_gs_deutsch %>%
  select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")),
    funs(str_replace(., "TICKED.", "")))


bbst_gs_deutsch_allgemein <- bbst_gs_deutsch %>%
  mutate(
    alter        = recode_factor(bbst_gs_deutsch$alter, !!!l.alter,
			.ordered = T),
    aufmerk_ausb = recode_factor(bbst_gs_deutsch$aufmerk_ausb,
			!!!l.aufmerk_ausb),
    geschl       = recode_factor(bbst_gs_deutsch$geschl, !!!l.geschl,
			.ordered = T),
    fach         = recode_factor(bbst_gs_deutsch$fach, !!!l.fach),
    erfahr       = recode_factor(bbst_gs_deutsch$erfahr, !!!l.erfahr,
			.ordered = T),
    querein      = recode_factor(bbst_gs_deutsch$querein, !!!l.querein),
    schultyp     = recode_factor(bbst_gs_deutsch$schultyp, !!!l.schultyp),
    klassen      = recode_factor(bbst_gs_deutsch$klassen, !!!l.klassen,
			.ordered = T),
    stunden      = recode_factor(bbst_gs_deutsch$stunden, !!!l.stunden,
			.ordered = T),
    stunden_fach = recode_factor(bbst_gs_deutsch$stunden_fach,
			!!!l.stunden_fach, .ordered = T),
    stunden_stud = recode_factor(bbst_gs_deutsch$stunden_stud,
			!!!l.stunden_stud, .ordered = T),
    einsatz      = recode_factor(bbst_gs_deutsch$einsatz, !!!l.einsatz),
    leitung      = recode_factor(bbst_gs_deutsch$leitung, !!!l.leitung),
    fehlt        = recode_factor(bbst_gs_deutsch$fehlt, !!!l.fehlt),
    belast       = recode_factor(bbst_gs_deutsch$belast, !!!l.belast,
			.ordered = T),
    verein       = recode_factor(bbst_gs_deutsch$verein, !!!l.verein,
			.ordered = T),
    quali        = recode_factor(bbst_gs_deutsch$quali, !!!l.quali,
			.ordered = T)
	)


bbst_gs_deutsch_kurs <- bbst_gs_deutsch %>%
	mutate(kurs = recode_factor(bbst_gs_deutsch$kurs, !!!l.kurs, .ordered = T))

# Fragen zu den berufsbegleitenden Studien
bbst_gs_deutsch_fragen <- select(bbst_gs_deutsch, starts_with("fragen")) %>%
	mutate_all(funs(recode_factor(.,  !!!l.skala))) %>%
	mutate_all(funs(ordered(., levels = order)))

names(bbst_gs_deutsch_fragen) <- fragen

# Fachdidaktik
bbst_gs_deutsch_fd <- select(bbst_gs_deutsch, starts_with("fd")) %>%
	mutate_all(funs(recode_factor(.,  !!!l.skala))) %>%
	mutate_all(funs(ordered(., levels = order)))

names(bbst_gs_deutsch_fd) <- items_13

### Literaturwissenschaft
bbst_gs_deutsch_lw <- select(bbst_gs_deutsch, starts_with("lw")) %>%
	mutate_all(funs(recode_factor(.,  !!!l.skala))) %>%
	mutate_all(funs(ordered(., levels = order)))

names(bbst_gs_deutsch_lw) <- items_13

### Linguistik
bbst_gs_deutsch_lk <- select(bbst_gs_deutsch, starts_with("lk")) %>%
	mutate_all(funs(recode_factor(.,  !!!l.skala))) %>%
	mutate_all(funs(ordered(., levels = order)))

names(bbst_gs_deutsch_lk) <- items_13


################
#### PLOTS #####
################

### allover plot options
colors <- paste0(rev(brewer.pal(4, "RdBu")), "CC") # add transparency



p.kurs <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$kurs,
	"Kurs")

p.geschl <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$geschl,
	"Geschlecht")

p.alter <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$alter,
	"Alter")

p.fach <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$fach,
	"Welche Fachrichtung haben Sie studiert?")

p.erfahr <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$erfahr,
	"Wie viele Jahre Berufserfahrung haben Sie in der studierten Fachrichtung?")

p.querein <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$querein,
	"Sind Sie bereits vor Ihrem Quereinstieg in Ausbildungsbereichen tätig gewesen \n(z.B. als Trainer/in, im Bereich Nachhilfe, als Chorleitung etc.)?")

p.aufmerk_ausb <- plot1(bbst_gs_deutsch_allgemein,
	bbst_gs_deutsch_allgemein$aufmerk_ausb,
	"Wie wurden Sie auf diese Ausbildungsmöglichkeit aufmerksam?")

p.schultyp <- plot1(bbst_gs_deutsch_allgemein,
	bbst_gs_deutsch_allgemein$schultyp,
	"An welchem Schultyp sind Sie eingesetzt?")

p.klassen <- plot1(bbst_gs_deutsch_allgemein,	bbst_gs_deutsch_allgemein$klassen,
	"In welchen Klassenstufen sind Sie überwiegend eingesetzt? \n(Mehrfachantworten möglich)")

p.stunden <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$stunden,
	"Wie viele Stunden eigenständigen Unterricht erteilen Sie pro Woche?")

p.stunden_fach <- plot1(bbst_gs_deutsch_allgemein,
	bbst_gs_deutsch_allgemein$stunden_fach,
	"Wie viele Stunden davon unterrichten Sie Ihr anerkanntes Fach?")

p.stunden_stud <- plot1(bbst_gs_deutsch_allgemein,
	bbst_gs_deutsch_allgemein$stunden_stud,
	"Wie viele Stunden unterrichten Sie bereits in dem Fach, welches Sie gerade in den berufsbegleitenden Studien belegen?")

p.einsatz <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$einsatz,
	"Welcher zusätzliche Einsatz wird von Ihnen seitens der Schule erwartet? \n(Mehrfachantworten möglich)")

p.leitung <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$leitung,
	"Fühlen Sie sich seitens der Schulleitung ausreichend unterstützt?")

p.belast <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$belast,
	"Wie hoch ist Ihre gefühlte Belastung durch die berufsbegleitenden Studien und Schule?")

p.verein <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$verein,
	"Wie gelingt Ihnen die Vereinbarkeit von berufsbegleitenden Studien und Familie?")

p.quali <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$quali,
	"Wie empfanden Sie die Qualität der Beratung im Vorfeld der Ausbildung?")

p.fehlt <- plot1(bbst_gs_deutsch_allgemein, bbst_gs_deutsch_allgemein$fehlt,
	"Welche Aspekte der Beratung fehlten Ihrer Meinung nach?")



### plots for batteries
# Fragen zu den berufsbegleitenden Studien

plot.fragen <- plot.likert(bbst_gs_deutsch_fragen,
	"Fragen zu den berufsbegleitenden Studien Deutsch für die Grundschule")

# Fachdidaktik
plot.fd <- plot.likert(bbst_gs_deutsch_fd,
	"Fragen zur Struktur und Ablauf in der Fachdidaktik")

# Literaturwissenschaft
plot.lw <- likert(bbst_gs_deutsch_lw,
	"Fragen zur Struktur und Ablauf in der Literaturwissenschaft")

# Linguistik
plot.lk <- likert(bbst_gs_deutsch_lk,
	"Fragen zur Struktur und Ablauf in der Linguistik")


### plots to pdf
pdf("/home/kalle/Dokumente/Arbeit_extern/SBJF/Auswertung/Grafiken/bbst_gs_deutsch.pdf", width = 9, height = 8)

p.kurs
p.geschl
p.alter
p.fach
p.erfahr
p.querein
p.aufmerk_ausb
p.schultyp
p.klassen
p.stunden
p.stunden_fach
p.stunden_stud
p.einsatz
p.leitung
p.belast
p.verein
p.quali
p.fehlt

plot.fragen

plot.fd
plot.lw
plot.lk


dev.off()
