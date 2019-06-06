l.alter <- c(
  "A" = "30 Jahre und jünger",
  "B" = "31 bis 40 Jahre",
  "C" = "41 bis 50 Jahre",
  "D" = "51 bis 60 Jahre",
  "E" = "61 Jahre und älter")

l.aufmerk_ausb <- c(
  "A" = "Eigenrecherche",
	"B" = "Familie/Freunde/Bekannte",
	"C" = "Fernsehen",
	"D" = "Gewerkschaft",
	"E" = "Internet/Intranet",
	"F" = "Kollegium",
	"G" = "Schulleitung",
	"H" = "Zeitung",
	"I" = "andere")

l.geschl <- c(
	"A" =	"männlich",
	"B" =	"weiblich")

l.fach <- c(
	"A" = "Naturwissenschaft",
	"B" = "Geisteswissenschaft",
	"C" = "Ingenieurwesen",
	"D" = "Sprache",
	"E" = "Musik",
	"F" = "Sport",
	"G" = "andere")

l.erfahr <- c(
	"A" = "keine",
	"B" = "1 bis 5 Jahre",
	"C" = "6 bis 10 Jahre",
	"D" = "11 bis 15 Jahre",
	"E" = "mehr als 15 Jahre")

l.querein <- c(
	"A" = "ja",
	"B" = "nein")

l.schultyp <- c(
	"A" = "Grundschule",
	"B" = "Integrierte Sekundarschule",
	"C" = "Gymnasium",
	"D" = "berufliche Schule",
	"E" = "Schule mit sonderpädagogischem Förderschwerpunkt",
	"F" = "Gemeinschaftsschule")

l.klassen <- c(
	"A" = "Klassenstufe 1 bis 3",
	"B" = "Klassenstufe 4 bis 6",
	"C" = "Klassenstufe 7 bis 10",
	"D" = "Klassenstufe 11 bis 12/13")

l.stunden <- c(
	"A" = "weniger als 5 Stunden",
	"B" = "5 bis 10 Stunden",
	"C" = "11 bis 15 Stunden",
	"D" = "16 bis 19 Stunden",
	"E" = "mehr als 19 Stunden")

l.stunden_fach <- c(
	"A" = "weniger als 25% meines Unterrichts",
	"B" = "25% bis 50% meines Unterrichts",
	"C" = "50% bis 75% meines Unterrichts",
	"D" = "mehr als 75% meines Unterrichts")

l.stunden_stud <- c(
	"A" = "weniger als 2 Stunden pro Woche",
	"B" = "3 bis 4 Stunden pro Woche",
	"C" = "5 bis 6 Stunden pro Woche",
	"D" = "mehr")

l.einsatz <- c(
	"A" = "Übernahme von Klassenleitungen",
	"B" = "Elternsprechabende",
	"C" = "Gremienarbeit",
	"D" = "(Fach-) Konferenzen/Dienstberatungen",
	"E" = "Arbeiten am SchiC",
	"F" = "Fortbildungen",
	"G" = "verantwortlich für Brandschutz",
	"H" = "verantwortlich für IT",
	"I" = "Planung/Durchführung/Begleitung von Klassenfahrten/Ausflügen/Wandertagen",
	"J" = "Planung/Durchführung/Begleitung von Feierlichkeiten/Veranstaltungen",
	"K" = "Planung/Durchführung/Begleitung von Projekttagen",
	"L" = "andere außerschulische Aktivitäten",
	"M" = "sonderpädagogische Aufgaben (Diagnostik, Beratung, Hospitation, Feststellungsverfahren)",
	"N" = "weiteres")

l.leitung <- c(
	"A" = "ja",
	"B" = "nein")

l.fehlt <- c(
  "A" = "Es gab keine Beratung.",
  "B" = "konkrete Angaben zum Verlauf der Ausbildung",
  "C" = "Erklärungen zur Fächerauswahl",
  "D" = "Rechte/Pflichten der Quereinsteigenden",
  "E" = "konkrete Ansprechpartner/innen",
  "F" = "andere Aspekte")

l.belast <- c(
  "A" = "völlig in Ordnung",
  "B" = "2",
  "C" = "3",
  "D" = "4",
  "E" = "5",
  "F" = "maximale Belastung")

l.verein <- c(
  "A" = "sehr gut",
  "B" = "2",
  "C" = "3",
  "D" = "4",
  "E" = "5",
  "F" = "ungenügend",
  "G" = "nicht zutreffend")

l.quali <- c(
  "A" = "sehr gut",
  "B" = "2",
  "C" = "3",
  "D" = "4",
  "E" = "5",
  "F" = "ungenügend")


l.skala <- c(
  "A" = "trifft voll zu",
  "B" = "trifft überwiegend zu",
  "C" = "trifft weniger zu",
  "D" = "trifft gar nicht zu",
  "E" = "keine Angabe")

order <- c("trifft voll zu", "trifft überwiegend zu",
           "trifft weniger zu", "trifft gar nicht zu")



fragen <- c(
  "Das Gesamtkonzept für die bbSt Deutsch mit der Aufteilung in die Bereiche Fachdidaktik, Literaturwissenschaft und Linguistik ist zielführend.",
  "Die Inhalte in den Bereichen Fachdidaktik, Literaturwissenschaft und Linguistik sind aufeinander abgestimmt.",
  "Meine persönlichen Erwartungen an die Ausbildung werden/wurden erfüllt."
)


items_13 <- c(
  "Die Planung der Lehrveranstaltungen ist klar und übersichtlich.",
  "Der Ablauf der Lehrveranstaltungen entspricht der Ankündigung.",
  "Die Gestaltung der Lehrveranstaltungen ist abwechslungsreich.",
  "Die/der Dozierende sorgt dafür, dass alle Teilnehmenden aktiv teilnehmen können.",
  "Die/der Dozierende sorgt für Transparenz in Bezug auf Leistungsanforderungen und -bewertung.",
  "Fragen, Erfahrungen und Anregungen der Teilnehmenden werden in den Veranstaltungen aufgegriffen.",
  "Es gibt ausreichend Übungsmöglichkeiten.",
  "Die Lehrveranstaltungen beginnen und enden pünktlich.",
  "Die Teilnehmenden erscheinen pünktlich.",
  "Das Ausbildungsmaterial ist aktuell und informativ.",
  "Über Literatur und zusätzliche Materialien wird informiert.",
  "Das vermittelte und erworbene Fachwissen ist als Grundlagenwissen relevant.",
  "Das Fachwissen stellt eine Unterstützung bei der Unterrichtsvorbereitung dar."
)
