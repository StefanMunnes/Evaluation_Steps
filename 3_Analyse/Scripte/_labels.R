lab.alter <- c(
  "A" = "30 Jahre und jünger",
  "B" = "31 bis 40 Jahre",
  "C" = "41 bis 50 Jahre",
  "D" = "51 bis 60 Jahre",
  "E" = "61 Jahre und älter")

lab.aufmerk_ausb <- c(
  "A" = "Eigenrecherche",
	"B" = "Familie/Freunde/Bekannte",
	"C" = "Fernsehen",
	"D" = "Gewerkschaft",
	"E" = "Internet/Intranet",
	"F" = "Kollegium",
	"G" = "Schulleitung",
	"H" = "Zeitung",
	"I" = "andere")

lab.geschl <- c(
	"A" =	"männlich",
	"B" =	"weiblich")

lab.fach <- c(
	"A" = "Naturwissenschaft",
	"B" = "Geisteswissenschaft",
	"C" = "Ingenieurwesen",
	"D" = "Sprache",
	"E" = "Musik",
	"F" = "Sport",
	"G" = "andere")

lab.erfahr <- c(
	"A" = "keine",
	"B" = "1 bis 5 Jahre",
	"C" = "6 bis 10 Jahre",
	"D" = "11 bis 15 Jahre",
	"E" = "mehr als 15 Jahre")

lab.querein <- c(
	"A" = "ja",
	"B" = "nein")

lab.schultyp <- c(
	"A" = "Grundschule",
	"B" = "Integrierte Sekundarschule",
	"C" = "Gymnasium",
	"D" = "berufliche Schule",
	"E" = "Schule mit sonderpädagogischem Förderschwerpunkt",
	"F" = "Gemeinschaftsschule")

lab.klassen <- c(
	"Klassenstufe 1 bis 3",
	"Klassenstufe 4 bis 6",
	"Klassenstufe 7 bis 10",
	"Klassenstufe 11 bis 12/13")

lab.stunden <- c(
	"A" = "weniger als 5 Stunden",
	"B" = "5 bis 10 Stunden",
	"C" = "11 bis 15 Stunden",
	"D" = "16 bis 19 Stunden",
	"E" = "mehr als 19 Stunden")

lab.stunden_fach <- c(
	"A" = "weniger als 25% meines Unterrichts",
	"B" = "25% bis 50% meines Unterrichts",
	"C" = "50% bis 75% meines Unterrichts",
	"D" = "mehr als 75% meines Unterrichts")

lab.stunden_stud <- c(
	"A" = "weniger als 2 Stunden pro Woche",
	"B" = "3 bis 4 Stunden pro Woche",
	"C" = "5 bis 6 Stunden pro Woche",
	"D" = "mehr")

lab.einsatz= c(
	"Übernahme von Klassenleitungen",
	"Elternsprechabende",
	"Gremienarbeit",
	"(Fach-) Konferenzen/Dienstberatungen",
	"Arbeiten am Schichtplan",
	"Fortbildungen",
	"verantwortlich für Brandschutz",
	"verantwortlich für IT",
	"Planung/Begleitung von Klassenfahrten/Wandertagen",
	"Planung/Begleitung von Feierlichkeiten/Veranstaltungen",
	"Planung/Begleitung von Projekttagen",
	"andere außerschulische Aktivitäten",
	"sonderpädagogische Aufgaben (Diagnostik, etc.)",
	"weiteres")

lab.leitung <- c(
	"A" = "ja",
	"B" = "nein")

lab.fehlt <- c(
  "A" = "Es gab keine Beratung",
  "B" = "konkrete Angaben zum Verlauf der Ausbildung",
  "C" = "Erklärungen zur Fächerauswahl",
  "D" = "Rechte/Pflichten der Quereinsteigenden",
  "E" = "konkrete Ansprechpartner/innen",
  "F" = "andere Aspekte")

lab.belast <- c(
  "A" = "völlig in Ordnung",
  "B" = "2",
  "C" = "3",
  "D" = "4",
  "E" = "5",
  "F" = "maximale Belastung")

lab.verein <- c(
  "A" = "sehr gut",
  "B" = "2",
  "C" = "3",
  "D" = "4",
  "E" = "5",
  "F" = "ungenügend")

lab.quali <- c(
  "A" = "sehr gut",
  "B" = "2",
  "C" = "3",
  "D" = "4",
  "E" = "5",
  "F" = "ungenügend")

lab.skala <- c(
  "A" = "trifft voll zu",
  "B" = "trifft überwiegend zu",
  "C" = "trifft weniger zu",
  "D" = "trifft gar nicht zu",
  "E" = "keine Angabe")

lab.erstes <- c(
  "A" = "Ja",
  "B" = "Nein")

lab.absolv <- c(
  "A" = "Ergänzungs- und Erweiterungsstudium Englisch",
  "B" = "Ergänzungs- und Erweiterungsstudium Informatik",
  "C" = "Ergänzungs- und Erweiterungsstudium Mathematik",
  "D" = "Erweiterungsstudium Sonderpädagogik",
  "E" = "Ergänzungs- und Erweiterungsstudium WAT",
  "F" = "Weiterbildungslehrgang Englisch",
  "G" = "Weiterbildungslehrgang Ethik",
  "H" = "Weiterbildungslehrgang Gesellschaftswissenschaften",
  "I" = "Weiterbildungslehrgang Mathematik",
  "J" = "Weiterbildungslehrgang Naturwissenschaften",
  "K" = "Weiterbildungslehrgang Psychologie",
  "L" = "Weiterbildungslehrgang Schwimmen",
  "M" = "Weiterbildungslehrgang Theater/Darstellendes Spiel",
  "N" = "Qualifizierung Beratungslehrkraft für den schulpsychologischen Dienst (BSD",
  "O" = "Qualifizierung Deutsche Gebärdensprache",
  "P" = "Qualifizierung Sprachbildungskoordinatorin bzw. Sprachbildungskoordinator",
  "Q" = "Qualifizierung Durchgängige Sprachbildung",
  "R" = "Qualifizierung Unterrichts- und Schulentwicklung für die inklusive Schule",
  "S" = "Sonderpädagogische Zusatzausbildung für Pädagogische Unterrichtshilfen",
  "T" = "Zusatzqualifikation Facherzieherin und Facherzieher für Integration",
  "U" = "andere")

lab.aufmerk_weit <- c(
  "A" = "durch den Besuch einer anderen Weiterbildung/Fortbildung",
  "B" = "Eigenrecherche/ Interesse",
  "C" = "Information durch Schulleitung",
  "D" = "Internet/Intranet",
  "E" = "Kollegium",
  "F" = "Rundschreiben",
  "G" = "sonstiges")

lab.gruend <- c(
  "A" = "weil ich mich persönlich weiterentwickeln möchte",
  "B" = "weil ich mich beruflich weiterbilden möchte",
  "C" = "weil ich dieses Fach bereits fachfremd unterrichte",
  "D" = "zur Höhergruppierung",
  "E" = "weil diese Qualifikation an unserer Schule gebraucht wird",
  "F" = "weil diese Qualifikation zu unserem Schulprofil passt",
  "G" = "andere Gründe")

lab.zufried <- c(
  "A" = "1",
  "B" = "2",
  "C" = "3",
  "D" = "4",
  "E" = "5",
  "F" = "6")

lab.stunden_weit <- c(
  "A" = "1",
  "B" = "2",
  "C" = "3",
  "D" = "4",
  "E" = "5",
  "F" = "6",
  "G" = "mehr")

lab.empfehl <- c(
  "A" = "Ja",
  "B" = "Nein")

lab.andere <- c(
  "A" = "Kein Interesse",
  "B" = "Deutsche Gebärdensprache",
  "C" = "Englisch",
  "D" = "Ethik",
  "E" = "Facherzieherin und Facherzieher für Integration",
  "F" = "Geschichte",
  "G" = "Gesellschaftswissenschaften",
  "H" = "Informatik",
  "I" = "Inklusion",
  "J" = "Mathematik",
  "K" = "Musik",
  "L" = "Naturwissenschaften",
  "M" = "Pädagogische Unterrichtshilfen",
  "N" = "Philosophie",
  "O" = "Politik",
  "P" = "Psychologie/Schulpsychologie",
  "Q" = "Schwimmen",
  "R" = "Sprachbildung",
  "S" = "Sonderpädagogik",
  "T" = "Theater/Darstellendes Spiel",
  "U" = "WAT",
  "V" = "andere")


lev.andere <- c(
  "Kein Interesse",
  "Deutsche Gebärdensprache",
  "Englisch",
  "Ethik",
  "Facherzieherin und Facherzieher für Integration",
  "Geschichte",
  "Gesellschaftswissenschaften",
  "Informatik",
  "Inklusion",
  "Mathematik",
  "Musik",
  "Naturwissenschaften",
  "Pädagogische Unterrichtshilfen",
  "Philosophie",
  "Politik",
  "Psychologie/Schulpsychologie",
  "Schwimmen",
  "Sprachbildung",
  "Sonderpädagogik",
  "Theater/Darstellendes Spiel",
  "WAT",
  "andere")

lev.empfehl <- c(
  "Ja",
  "Nein")

lev.stunden_weit <- c(
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "mehr")

lev.zufried <- c(
  "1",
  "2",
  "3",
  "4",
  "5",
  "6")

lev.gruend <- c(
  "weil ich mich persönlich weiterentwickeln möchte",
  "weil ich mich beruflich weiterbilden möchte",
  "weil ich dieses Fach bereits fachfremd unterrichte",
  "zur Höhergruppierung",
  "weil diese Qualifikation an unserer Schule gebraucht wird",
  "weil diese Qualifikation zu unserem Schulprofil passt",
  "andere Gründe")

lev.aufmerk_weit <- c(
  "durch den Besuch einer anderen Weiterbildung/Fortbildung",
  "Eigenrecherche/ Interesse",
  "Information durch Schulleitung",
  "Internet/Intranet",
  "Kollegium",
  "Rundschreiben",
  "sonstiges")

lev.absolv <- c(
  "Ergänzungs- und Erweiterungsstudium Englisch",
  "Ergänzungs- und Erweiterungsstudium Informatik",
  "Ergänzungs- und Erweiterungsstudium Mathematik",
  "Erweiterungsstudium Sonderpädagogik",
  "Ergänzungs- und Erweiterungsstudium WAT",
  "Weiterbildungslehrgang Englisch",
  "Weiterbildungslehrgang Ethik",
  "Weiterbildungslehrgang Gesellschaftswissenschaften",
  "Weiterbildungslehrgang Mathematik",
  "Weiterbildungslehrgang Naturwissenschaften",
  "Weiterbildungslehrgang Psychologie",
  "Weiterbildungslehrgang Schwimmen",
  "Weiterbildungslehrgang Theater/Darstellendes Spiel",
  "Qualifizierung Beratungslehrkraft für den schulpsychologischen Dienst (BSD",
  "Qualifizierung Deutsche Gebärdensprache",
  "Qualifizierung Sprachbildungskoordinatorin bzw. Sprachbildungskoordinator",
  "Qualifizierung Durchgängige Sprachbildung",
  "Qualifizierung Unterrichts- und Schulentwicklung für die inklusive Schule",
  "Sonderpädagogische Zusatzausbildung für Pädagogische Unterrichtshilfen",
  "Zusatzqualifikation Facherzieherin und Facherzieher für Integration",
  "andere")

lev.alter <- c(
  "30 Jahre und jünger",
  "31 bis 40 Jahre",
  "41 bis 50 Jahre",
  "51 bis 60 Jahre",
  "61 Jahre und älter")

lev.aufmerk_ausb <- c(
  "Eigenrecherche",
	"Familie/Freunde/Bekannte",
	"Fernsehen",
	"Gewerkschaft",
	"Internet/Intranet",
	"Kollegium",
	"Schulleitung",
	"Zeitung",
	"andere")

lev.geschl <- c(
	"männlich",
	"weiblich")

lev.fach <- c(
	"Naturwissenschaft",
	"Geisteswissenschaft",
	"Ingenieurwesen",
	"Sprache",
	"Musik",
	"Sport",
	"andere")

lev.erfahr <- c(
	"keine",
	"1 bis 5 Jahre",
	"6 bis 10 Jahre",
	"11 bis 15 Jahre",
	"mehr als 15 Jahre")

lev.querein <- c(
	"ja",
	"nein")

lev.schultyp <- c(
	"Grundschule",
	"Integrierte Sekundarschule",
	"Gymnasium",
	"berufliche Schule",
	"Schule mit sonderpädagogischem Förderschwerpunkt",
	"Gemeinschaftsschule")

lev.stunden <- c(
	"weniger als 5 Stunden",
	"5 bis 10 Stunden",
	"11 bis 15 Stunden",
	"16 bis 19 Stunden",
	"mehr als 19 Stunden")

lev.stunden_fach <- c(
	"weniger als 25% meines Unterrichts",
	"25% bis 50% meines Unterrichts",
	"50% bis 75% meines Unterrichts",
	"mehr als 75% meines Unterrichts")

lev.stunden_stud <- c(
	"weniger als 2 Stunden pro Woche",
	"3 bis 4 Stunden pro Woche",
	"5 bis 6 Stunden pro Woche",
	"mehr")

lev.einsatz <- c(
  "Übernahme von Klassenleitungen",
	"Elternsprechabende",
	"Gremienarbeit",
	"(Fach-) Konferenzen/Dienstberatungen",
	"Arbeiten am Schichtplan",
	"Fortbildungen",
	"verantwortlich für Brandschutz",
	"verantwortlich für IT",
	"Planung/Begleitung von Klassenfahrten/Wandertagen",
	"Planung/Begleitung von Feierlichkeiten/Veranstaltungen",
	"Planung/Begleitung von Projekttagen",
	"andere außerschulische Aktivitäten",
	"sonderpädagogische Aufgaben (Diagnostik, etc.)",
	"weiteres")

lev.leitung <- c(
	"ja",
	"nein")

lev.fehlt <- c(
  "Es gab keine Beratung",
  "konkrete Angaben zum Verlauf der Ausbildung",
  "Erklärungen zur Fächerauswahl",
  "Rechte/Pflichten der Quereinsteigenden",
  "konkrete Ansprechpartner/innen",
  "andere Aspekte")


lev.belast <- c(
  "völlig in Ordnung",
  "2",
  "3",
  "4",
  "5",
  "maximale Belastung")


lev.verein <- c(
  "sehr gut",
  "2",
  "3",
  "4",
  "5",
  "ungenügend")


lev.quali <- c(
  "sehr gut",
  "2",
  "3",
  "4",
  "5",
  "ungenügend")


lev.erstes <- c(
  "Ja",
  "Nein")


order <- c(
  "trifft voll zu",
  "trifft überwiegend zu",
  "trifft weniger zu",
  "trifft gar nicht zu")


items_11 <- c(
  "Die Planung der Lehrveranstaltungen ist klar und übersichtlich.",
  "Der Ablauf der Lehrveranstaltungen entspricht der Ankündigung.",
  "Die Gestaltung der Lehrveranstaltungen ist abwechslungsreich.",
  "Die Veranstaltungszeit der Lehrveranstaltungen wird effizient genutzt.",
  "Die Gruppengröße ist für die Fachvorlesung angemessen.",
  "Die Lehrveranstaltungen beginnen und enden pünktlich.",
  "Die Teilnehmenden erscheinen pünktlich.",
  "Das Ausbildungsmaterial ist aktuell und informativ.",
  "Über Literatur und zusätzliche Materialien wird informiert.",
  "Das vermittelte und erworbene Fachwissen ist als Grundlagenwissen relevant.",
  "Das Fachwissen stellt eine Unterstützung bei der Unterrichtsvorbereitung dar."
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

items_ke <- c(
  "Das vermittelte und erworbene Fachwissen ist als Grundlagenwissen relevant für das Lehramt an Grundschulen.",
  "Das Fachwissen stellt eine Unterstützung bei der Unterrichtsvorbereitung im Fach Naturwissenschaften in der Grundschule dar.",
  "Das Fachwissen entsprechend der Rahmenlehrpläne der SEK-I gibt Sicherheit für den Unterricht in der Grundschule.",
  "Es werden verschiedene naturwissenschaftliche Untersuchungsmethoden wie das Beobachten, Vergleichen, Ordnen und Experimentieren durchgeführt.",
  "Es werden Denk- und Argumentationsweisen wie das Bilden von Hypothesen und das Herstellen von Zusammenhängen (beispielsweise Wenn-dann-Beziehungen, Proportionalitäten) vermittelt.",
  "Es wird mit Geräten und Chemikalien sachgerecht umgegangen.",
  "Es werden Elemente der Mathematik beim Umgang mit Größen, dem Erfassen von Messwerten und dem Anwenden einfacher mathematischer Verfahren berücksichtigt.",
  "Die Arbeit mit Modellen wird durchgeführt.",
  "Zur Beschreibung von Phänomenen aus Natur und Technik wird auf die Nutzung der Fachsprache Wert gelegt.",
  "Es werden Sachtexte, grafische Darstellungen, Modelle und andere Medien in die Veranstaltungen einbezogen.",
  "Fachinformationen werden in den Veranstaltungen bewertet und Erklärungen nachvollzogen und reflektiert."
)

items_sg <- c(
  "Die Studienplanung ist klar und übersichtlich.",
  "Das Studiengeschehen entspricht der Ankündigung.",
  "Die Gestaltung der Veranstaltungen ist abwechslungsreich.",
  "Die/der Dozierende sorgt dafür, dass alle Teilnehmenden aktiv teilnehmen können.",
  "Die/der Dozierende sorgt für Transparenz in Bezug auf Leistungsanforderungen und -bewertung.",
  "Fragen, Erfahrungen und Anregungen der Teilnehmenden werden in den Veranstaltungen aufgegriffen.",
  "Es gibt ausreichend Übungsmöglichkeiten.",
  "Es gibt genügend Zeit für den allgemeinen/ fachlichen Austausch.",
  "Die Lehrveranstaltungen beginnen und enden pünktlich.",
  "Die Teilnehmenden erscheinen pünktlich.",
  "Das Ausbildungsmaterial ist aktuell und informativ.",
  "Mit den zur Verfügung gestellten Materialien können die Inhalte der Veranstaltungen wiederholt und vertieft werden."
)

items_wb <- c(
  "Die koordinierende Leitung hat zu Beginn den Ablauf und die Ziele der Weiterbildung erläutert.",
  "Die Planung der Lehrveranstaltungen ist klar und übersichtlich.",
  "Die Inhalte der Weiterbildung sind sinnvoll aufeinander abgestimmt.",
  "Die Dozierenden schaffen Transparenz in Bezug auf Leistungsanforderungen und -bewertung.",
  "Das in der Weiterbildung Erlernte und Erfahrene ist für mich beruflich von Nutzen.",
  "Ich erwerbe bzw. vertiefe in der Weiterbildungsmaßnahme fachliche Kompetenzen.",
  "Ich fühle mich gut auf den erfolgreichen Abschluss der Weiterbildung vorbereitet.",
  "Ich fühle mich gut auf die künftige Tätigkeit in der  Schule/ Unterrichtstätigkeit vorbereitet.",
  "Meine persönlichen Erwartungen an die Weiterbildungsmaßnahme werden erfüllt."
)

items_9 <- c(
  "Die Planung der Lehrveranstaltungen ist klar und übersichtlich",
  "Der Ablauf der Lehrveranstaltungen entspricht der Ankündigung",
  "Die Gestaltung der Lehrveranstaltungen ist zielführend",
  "Die Veranstaltungszeit der Lehrveranstaltungen wird effizient genutzt",
  "Die Gruppengröße ist für die Fachvorlesung angemessen",
  "Die Lehrveranstaltungen beginnen und enden pünktlich",
  "Die Teilnehmenden erscheinen pünktlich",
  "Das Ausbildungsmaterial ist aktuell und informativ",
  "Über Literatur und zusätzliche Materialien wird informier"
)

items_se_11 <- c(
  "Die Planung der Lehrveranstaltungen ist klar und übersichtlich.",
  "Der Ablauf der Lehrveranstaltungen entspricht der Ankündigung.",
  "Die Gestaltung der Lehrveranstaltungen ist zielführend.",
  "Die Inhalte des Seminars führen zu einem vertieften Einblick in bestimmte weiterführende Themen der Mathematik.",
  "Fragen, Erfahrungen und Anregungen der Teilnehmenden werden in den Veranstaltungen aufgegriffen.",
  "Es gibt genügend Zeit für den allgemeinen/ fachlichen Austausch.",
  "Die Gruppengröße ist für die Lehrveranstaltungen angemessen.",
  "Die Lehrveranstaltungen beginnen und enden pünktlich.",
  "Die Teilnehmenden erscheinen pünktlich.",
  "Das Ausbildungsmaterial ist aktuell und informativ.",
  "Über Literatur und zusätzliche Materialien wird informiert."
)
