
### allover plot options
colors.n8  <- brewer.pal(8, "Pastel2")
colors.n14 <- colorRampPalette(colors.n8)(14)
colors.n22 <- colorRampPalette(colors.n8)(22)
#colors.n8  <- qualitative_hcl(7, "Pastel1")
# colors.n14 <- rev(qualitative_hcl(14, "Pastel1"))
# colors.quali <- brewerplot("Pastel2")
colors.diver <- paste0(rev(brewer.pal(6, "RdYlGn")), "CC") # add transparency
colors.likert <- paste0(rev(brewer.pal(4, "RdYlGn")), "CC") # add transparency


bbst_1.recode <- function(data) {

	data %>% transmute(
			kurs				 = recode_factor(data$kurs, !!!lab.kurs),
	  	alter        = recode_factor(data$alter, !!!lab.alter),
	    aufmerk_ausb = recode_factor(data$aufmerk_ausb, !!!lab.aufmerk_ausb),
	    geschl       = recode_factor(data$geschl, !!!lab.geschl),
	    fach         = recode_factor(data$fach, !!!lab.fach),
	    erfahr       = recode_factor(data$erfahr, !!!lab.erfahr),
	    querein      = recode_factor(data$querein, !!!lab.querein),
	    schultyp     = recode_factor(data$schultyp, !!!lab.schultyp),
	    stunden      = recode_factor(data$stunden, !!!lab.stunden),
	    stunden_fach = recode_factor(data$stunden_fach,	!!!lab.stunden_fach),
	    stunden_stud = recode_factor(data$stunden_stud,	!!!lab.stunden_stud),
	    leitung      = recode_factor(data$leitung, !!!lab.leitung),
	    fehlt        = recode_factor(data$fehlt, !!!lab.fehlt),
	    belast       = recode_factor(data$belast, !!!lab.belast),
	    verein       = recode_factor(data$verein, !!!lab.verein),
			quali        = recode_factor(data$quali, !!!lab.quali),

			kurs				 = ordered(kurs, lev.kurs),
			alter			   = ordered(alter, lev.alter),
			aufmerk_ausb = ordered(aufmerk_ausb, lev.aufmerk_ausb),
			geschl       = ordered(geschl, lev.geschl),
			fach         = ordered(fach, lev.fach),
			erfahr       = ordered(erfahr, lev.erfahr),
			querein      = ordered(querein, lev.querein),
			schultyp     = ordered(schultyp, lev.schultyp),
			stunden      = ordered(stunden, lev.stunden),
			stunden_fach = ordered(stunden_fach,	lev.stunden_fach),
			stunden_stud = ordered(stunden_stud,	lev.stunden_stud),
			leitung      = ordered(leitung, lev.leitung),
			fehlt        = ordered(fehlt, lev.fehlt),
			belast       = ordered(belast, lev.belast),
	    verein       = ordered(verein, lev.verein),
	    quali        = ordered(quali, lev.quali))
}


es_1.recode <- function(data) {

	data %>% transmute(
			kurs				 = recode_factor(data$kurs, !!!lab.kurs),
			erstes			 = recode_factor(data$erstes, !!!lab.erstes),
			zufried 		 = recode_factor(data$zufried, !!!lab.zufried),
			stunden_weit = recode_factor(data$stunden_weit, !!!lab.stunden_weit),
			empfehl 		 = recode_factor(data$empfehl, !!!lab.empfehl),
			geschl       = recode_factor(data$geschl, !!!lab.geschl),
			alter        = recode_factor(data$alter, !!!lab.alter),
			schultyp     = recode_factor(data$schultyp, !!!lab.schultyp),

			kurs				 = ordered(kurs, lev.kurs),
			erstes			 = ordered(erstes, lev.erstes),
			zufried 		 = ordered(zufried, lev.zufried),
			stunden_weit = ordered(stunden_weit, lev.stunden_weit),
			empfehl 		 = ordered(empfehl, lev.empfehl),
			geschl       = ordered(geschl, lev.geschl),
			alter        = ordered(alter, lev.alter),
			schultyp     = ordered(schultyp, lev.schultyp))
}




batterie.recode <- function(data, i, vars) {

	data %>%
		mutate(kurs = recode_factor(data$kurs, !!!lab.kurs),
					 kurs	= ordered(kurs, lev.kurs)) %>%
		filter(str_detect(kurs, pattern = i)) %>%
		select(starts_with(vars)) %>%
		mutate_all(funs(recode_factor(.,  !!!lab.skala))) %>%
		mutate_all(funs(ordered(., levels = order)))
}


### PLOTs with LOOP-Function

plot.bar <- function(data, i, name, drop, title) {

  name = enquo(name)

	data %>% filter(!is.na((!!name)), str_detect(kurs, pattern = i)) %>%
	select((!!name)) %>%
	ggplot(aes(x = (!!name), fill = (!!name))) +
		geom_bar(width = 0.6) +
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    scale_fill_manual(values = colors.n8, drop = drop,
											name = element_blank()) +
	  theme_minimal() +
    labs(title = title, x = NULL, y = "Anzahl") +
		theme(legend.position = "right",
          plot.title = element_text(hjust = 0, size = 14, face = "bold"),
		      axis.text.x  = element_blank(),
		      axis.ticks.x = element_blank(),
		      panel.grid.major.x = element_blank(),
		      panel.grid.minor.y = element_blank())
}


plot.bar.div <- function(data, i, name, title, lab1, lab2) {

  name = enquo(name)
	data %>% filter(!is.na((!!name)), str_detect(kurs, pattern = i)) %>%
	select((!!name)) %>%
		ggplot(aes(x = (!!name), fill = (!!name))) +
			geom_bar(width = 0.6) +
    	scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
	    scale_fill_manual(values = colors.diver, drop = F,
				labels = c(lab1, " ", " ", " ", " ", lab2)) +
		  theme_minimal() +
	    labs(title = title, x = NULL, y = "Anzahl") +
			theme(legend.position = "right",
						legend.title = element_blank(),
	          plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
			      axis.text.x  = element_blank(),
			      axis.ticks.x = element_blank(),
			      panel.grid.major.x = element_blank(),
			      panel.grid.minor.y = element_blank())
}


plot.pie <- function(data, i, name, drop, title) {

  name = enquo(name)

  data %>% filter(!is.na((!!name)), str_detect(kurs, pattern = i)) %>%
		select((!!name)) %>%
    group_by((!!name)) %>%
    summarize(count = n()) %>%
    ggplot(., aes(x = "", y = count, fill = (!!name))) +
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y", start = 0, direction = -1) +
      scale_fill_manual(values = colors.n8, drop = drop)  +
      theme_minimal() +
      labs(title = title, fill = NULL, x = NULL, y = NULL) +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0, size = 14, face = "bold"),
            axis.line  = element_blank(),
            axis.text  = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
}


plot.likert <- function(batterie, title) {

  likert(batterie) %>%
  	likert.bar.plot(group.order = names(.$items),
  		plot.percent.low = F, plot.percent.high = F,
  		colors = colors.likert, wrap = 40, legend = NULL) +
  	labs(title = title) +
  	ylab("Zustimmung in Prozent") +
  	theme_light() +
  	theme(legend.position="bottom",
  				legend.text = element_text(size = 9),
  				legend.key.size = unit(.5, "cm"),
          plot.title = element_text(hjust = 0, size = 14, face = "bold")
				  )
}


plot.likert.sub <- function(batterie, title, sub) {

  likert(batterie) %>%
  	likert.bar.plot(group.order = names(.$items),
  		plot.percent.low = F, plot.percent.high = F,
  		colors = colors.likert, wrap = 40, legend = NULL) +
  	labs(title = title, subtitle = sub) +
  	ylab("Zustimmung in Prozent") +
  	theme_light() +
  	theme(legend.position="bottom",
  				legend.text = element_text(size = 9),
  				legend.key.size = unit(.5, "cm"),
          plot.title = element_text(hjust = 0, size = 14, face = "bold")
				  )
}

plot.kurs <- function(data) {

	data %>%
		filter(!is.na((kurs)), str_detect(kurs, pattern = i)) %>%
		select(kurs) %>%
		ggplot(aes(x = reorder(kurs, desc(kurs)), fill = kurs)) +
			geom_bar(width = 0.6) +
			coord_flip() +
    	scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
			scale_fill_manual(values = colors.n14) +
			theme_minimal() +
			labs(x = NULL, y = "Anzahl", title = "Kurse") +
			theme(legend.position = "none",
						plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
						axis.ticks.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.major.y = element_blank())

}


plot.bar.klassen <- function(data_c, data_1) {

	data_frame(klassen = lab.klassen,
						 anzahl  = as.numeric(c(
					 		length(grep("A", data_c$klassen[grep(i, data_1$kurs)])),
					 		length(grep("B", data_c$klassen[grep(i, data_1$kurs)])),
					 		length(grep("C", data_c$klassen[grep(i, data_1$kurs)])),
					 		length(grep("D", data_c$klassen[grep(i, data_1$kurs)]))))) %>%
		mutate(klassen = fct_relevel(klassen, "Klassenstufe 1 bis 3",
																					"Klassenstufe 4 bis 6",
																					"Klassenstufe 7 bis 10",
																					"Klassenstufe 11 bis 12/13"),
					 anzahl  = anzahl) %>%
		filter(anzahl > 0) %>%
		ggplot(aes(x = klassen, y = anzahl, fill = klassen)) +
			geom_bar(stat = "identity", width = 0.6) +
    	# scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
			scale_fill_manual(values = colors.n8) +
			theme_minimal() +
			ylim(0, NA) +
			labs(x = NULL, y = "Anzahl",
				title = "In welchen Klassenstufen sind Sie überwiegend eingesetzt? \n(Mehrfachantworten möglich)") +
			theme(legend.position = "right",
						legend.title = element_blank(),
						plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
						axis.text.x  = element_blank(),
						axis.ticks.x = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.y = element_blank())
}


plot.bar.einsatz <- function(data_c, data_1) {

	data_frame(einsatz = lab.einsatz,
						 anzahl  = as.numeric(c(
					     length(grep("A", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("B", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("C", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("D", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("E", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("F", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("G", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("H", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("I", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("J", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("K", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("L", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("M", data_c$einsatz[grep(i, data_1$kurs)])),
					     length(grep("N", data_c$einsatz[grep(i, data_1$kurs)]))))) %>%
  mutate(anzahl   = anzahl,
    		 einsatz  = reorder(einsatz, anzahl)) %>%
	filter(anzahl > 0) %>%
		ggplot(aes(x = reorder(einsatz, anzahl), y = anzahl, fill = einsatz)) +
    geom_bar(stat = "identity", width = 0.6) +
    coord_flip() +
	  # scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    scale_fill_manual(values = colors.n14) +
    theme_minimal() +
    labs(x = NULL, y = "Anzahl",
         title = "Welcher zusätzliche Einsatz wird von Ihnen seitens \nder Schule erwartet? (Mehrfachantworten möglich)") +
    theme(legend.position = "none",
          plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
          axis.ticks.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank())
}


plot.bar.fehlt <- function(data_c, data_1) {

  data_frame(fehlt  = lab.fehlt,
	           anzahl = as.numeric(c(
					     length(grep("A", data_c$fehlt[grep(i, data_1$kurs)])),
					     length(grep("B", data_c$fehlt[grep(i, data_1$kurs)])),
					     length(grep("C", data_c$fehlt[grep(i, data_1$kurs)])),
					     length(grep("D", data_c$fehlt[grep(i, data_1$kurs)])),
					     length(grep("E", data_c$fehlt[grep(i, data_1$kurs)])),
					     length(grep("F", data_c$fehlt[grep(i, data_1$kurs)]))))) %>%
	  mutate(anzahl   = anzahl,
	    		 fehlt    = reorder(fehlt, anzahl)) %>%
		filter(anzahl > 0) %>%
		ggplot(aes(x = reorder(fehlt, anzahl), y = anzahl, fill = fehlt)) +
	    geom_bar(stat = "identity", width = 0.6) +
	    coord_flip() +
    	# scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
	    scale_fill_manual(values = colors.n8) +
	    theme_minimal() +
			ylim(0, NA) +
	    labs(x = NULL, y = "Anzahl",
	         title = "Welche Aspekte der Beratung fehlten Ihrer Meinung nach? \n(Mehrfachantworten möglich)") +
	    theme(legend.position = "none",
	          plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
	          axis.ticks.x = element_blank(),
	          panel.grid.minor.x = element_blank(),
	          panel.grid.major.y = element_blank())
}


plot.bar.absolv <- function(data_c, data_1) {

	data_frame(absolv = lab.absolv,
             anzahl = as.numeric(c(
					     length(grep("A", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("B", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("C", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("D", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("E", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("F", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("G", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("H", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("I", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("J", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("K", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("L", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("M", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("N", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("O", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("P", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("Q", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("R", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("S", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("T", data_c$absolv[grep(i, data_1$kurs)])),
					     length(grep("U", data_c$absolv[grep(i, data_1$kurs)]))))) %>%
    mutate(anzahl  = anzahl,
           absolv = reorder(absolv, anzahl)) %>%
    filter(anzahl > 0) %>%
    ggplot(aes(x = reorder(absolv, anzahl), y = anzahl, fill = absolv)) +
      geom_bar(stat = "identity", width = 0.6) +
      coord_flip() +
    	# scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      scale_fill_manual(values = colors.n22) +
      theme_minimal() +
      labs(x = NULL, y = "Anzahl",
           title = "Welche Weiterbildung haben Sie bereits \nabsolviert? (Mehrfachantworten möglich)") +
      theme(legend.position = "none",
            plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
            axis.ticks.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank())
}


plot.bar.aufmerk_weit <- function(data_c, data_1) {

	data_frame(aufmerk_weit = lab.aufmerk_weit,
						 anzahl  = as.numeric(c(
						 	length(grep("A", data_c$aufmerk_weit[grep(i, data_1$kurs)])),
						 	length(grep("B", data_c$aufmerk_weit[grep(i, data_1$kurs)])),
						 	length(grep("C", data_c$aufmerk_weit[grep(i, data_1$kurs)])),
						 	length(grep("D", data_c$aufmerk_weit[grep(i, data_1$kurs)])),
						 	length(grep("E", data_c$aufmerk_weit[grep(i, data_1$kurs)])),
						 	length(grep("F", data_c$aufmerk_weit[grep(i, data_1$kurs)])),
						 	length(grep("G", data_c$aufmerk_weit[grep(i, data_1$kurs)]))))) %>%
		mutate(anzahl  = anzahl,
					 aufmerk_weit = reorder(aufmerk_weit, anzahl)) %>%
		filter(anzahl > 0) %>%
		ggplot(aes(x = reorder(aufmerk_weit, anzahl), y = anzahl, fill = aufmerk_weit)) +
			geom_bar(stat = "identity", width = 0.6) +
			coord_flip() +
    	# scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
			scale_fill_manual(values = colors.n8) +
			theme_minimal() +
			labs(x = NULL, y = "Anzahl",
					 title = "Wie wurden Sie auf diese Weiterbildungsmöglichkeit \naufmerksam? (Mehrfachantworten möglich)") +
			theme(legend.position = "none",
						plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
						axis.ticks.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.major.y = element_blank())
}


plot.bar.gruend <- function(data_c, data_1) {

	data_frame(gruend = lab.gruend,
						 anzahl = as.numeric(c(
						 	length(grep("A", data_c$gruend[grep(i, data_1$kurs)])),
						 	length(grep("B", data_c$gruend[grep(i, data_1$kurs)])),
						 	length(grep("C", data_c$gruend[grep(i, data_1$kurs)])),
						 	length(grep("D", data_c$gruend[grep(i, data_1$kurs)])),
						 	length(grep("E", data_c$gruend[grep(i, data_1$kurs)])),
						 	length(grep("F", data_c$gruend[grep(i, data_1$kurs)])),
						 	length(grep("G", data_c$gruend[grep(i, data_1$kurs)]))))) %>%
		mutate(anzahl  = anzahl,
					 gruend = reorder(gruend, anzahl)) %>%
		filter(anzahl > 0) %>%
		ggplot(aes(x = reorder(gruend, anzahl), y = anzahl, fill = gruend)) +
			geom_bar(stat = "identity", width = 0.6) +
			coord_flip() +
    	# scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
			scale_fill_manual(values = colors.n8) +
			theme_minimal() +
			labs(x = NULL, y = "Anzahl",
					 title = "Aus welchen Gründen haben Sie sich für diese Weiter \n bildung angemeldet? (Mehrfachantworten möglich)") +
			theme(legend.position = "none",
						plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
						axis.ticks.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.major.y = element_blank())
}

plot.bar.andere <- function(data_c, data_1) {

	data_frame(andere = lab.andere,
             anzahl = as.numeric(c(
				 	    length(grep("A", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("B", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("C", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("D", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("E", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("F", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("G", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("H", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("I", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("J", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("K", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("L", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("M", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("N", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("O", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("P", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("Q", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("R", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("S", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("T", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("U", data_c$andere[grep(i, data_1$kurs)])),
				 	    length(grep("V", data_c$andere[grep(i, data_1$kurs)]))))) %>%
		mutate(anzahl  = anzahl,
		       andere = reorder(andere, anzahl)) %>%
		filter(anzahl > 0) %>%
		ggplot(aes(x = reorder(andere, anzahl), y = anzahl, fill = andere)) +
		  geom_bar(stat = "identity", width = 0.6) +
		  coord_flip() +
    	# scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
		  scale_fill_manual(values = colors.n22) +
		  theme_minimal() +
		  labs(x = NULL, y = "Anzahl",
		       title = "Interessieren Sie sich außerdem für eine andere \nWeiterbildung? (Mehrfachantworten möglich)") +
		  theme(legend.position = "none",
		        plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
		        axis.ticks.x = element_blank(),
		        panel.grid.minor.x = element_blank(),
		        panel.grid.major.y = element_blank())
}
