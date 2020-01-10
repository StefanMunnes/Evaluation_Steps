
paten_csvs  <- csvs_pat_q[str_detect(csvs_pat_q, "/")]
paten_namen <- sapply(strsplit(paten_csvs, "/"), function(x) unlist(x)[1])

paten_daten <- lapply(paten_csvs, function(csv)
  read.csv2(paste0("3_Analyse/Scans/first_steps/", fs_jahr, "/pat_q/", csv)) %>%
  mutate(Note = as.character(Note)) %>%
  filter(Note != "-1") %>%
  select(starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", ""))) %>%
  gather(frg, note) %>%
  transmute(frage = recode_factor(frg, !!!varlab_pat_q, .ordered = T),
            note  = as.numeric(recode_factor(note, !!!notelab))) %>%
  filter(frage != "v0")
  )


names(paten_daten) <- paten_namen

paten_excel <- data.frame()

for (pate in names(paten_daten)) {

  teilnehmende <- nrow(paten_daten[[pate]])/10
  pate_note <- round(mean(paten_daten[[pate]][["note"]], na.rm = T), digits = 1)

  file <- paste0("4_Ergebnisse/Grafiken/first_steps/", fs_jahr, "/Patenberichte/", pate, ".pdf")

  if (!dir.exists(paste0("4_Ergebnisse/Grafiken/first_steps/", fs_jahr, "/Patenberichte"))) {
    dir.create(paste0("4_Ergebnisse/Grafiken/first_steps/", fs_jahr, "/Patenberichte"))
  }

  if (!dir.exists(paste0("4_Ergebnisse/Tabellen/first_steps/", fs_jahr))) {
    dir.create(paste0("4_Ergebnisse/Tabellen/first_steps/", fs_jahr))
  }

  paten_excel <- bind_rows(paten_excel,
                          data.frame("Name" = pate,
                                     "Quereinsteigende" = teilnehmende,
                                     "Note" = pate_note))


  # Margins für Plots
  par(oma = c(1, 1, 0, 1))
  par(mar = c(1, 5, 0, 1))


  # Plot für Title für p_pat
  plot(0:10, asp = 0.6, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

  text(5.5, 7, "Evaluation", cex = 2.2)
  text(5.5, 6, "FIRST STEPS", cex = 1.5)
  text(5.5, 4, paste0("Fragen zum Paten/zur Patin: ", pate), cex = 1.3)

  text(5.5, 2, paste0("Teilnehmende: ", teilnehmende), cex = 1)

  text(5.5, 0, paste0("Durchschnittliche Bewertung: ", pate_note), cex = 1)

  rasterImage(beberlin, 1, 9, 4.5, 10)
  rasterImage(arrow, 7.5, 9 , 10, 10)

  plot.title <- recordPlot()

  dev.off()


  # Plot für q_pat
  plot <- paten_daten[[pate]] %>% filter(between(note, 1, 6)) %>%
    ggplot(aes(y = note,
               x = reorder(frage, desc(frage)))) +
      geom_count(color = "mediumpurple3",
                 alpha = 0.6,
                 show.legend = F) +
      stat_summary(fun.y = mean, geom = "crossbar",
                   aes(ymax = ..y..,
                       ymin = ..y..),
                   color = "indianred2",
                   width = 0.5) +
      coord_flip() +
      scale_size_area(max_size = 6) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 60)) +
      scale_y_continuous(name = "",
                         limits = c(1, 6),
                         breaks = c(1, 2, 3, 4, 5, 6),
                         minor_breaks = NULL) +
      labs(x = "") +
      theme_light() +
      theme(axis.text = element_text(size = 10),
            plot.title = element_text(face = "bold", size = 12, colour = "black"))


  pdf(file, width = 8.3, height = 11.7, onefile = F)

  print(ggarrange(plot.title, plot, ncol = 1,  heights = c(1.6, 2), widths = c(2, 1.6)))

  dev.off()

}


write.xlsx(paten_excel, paste0("4_Ergebnisse/Tabellen/first_steps/", fs_jahr, "/Patenberichte.xlsx"))
