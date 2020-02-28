
for (x in unique(data.final$name)) {

  data.temp <- data.final[data.final$name == x,]

  file <- paste0("4_Ergebnisse/Grafiken/kick_off/Personen/", x, ".pdf")

  n_kurse <- length(unique(data.temp$group))

  teilnehmende <- nrow(data.temp)/11

  schnitt <- round(mean(data.temp[,"note"], na.rm = T), digits = 1)



  par(oma = c(1, 1, 0, 1))
  par(mar = c(1, 1, 0, 1))

  plot(0:10, asp = 0.6, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

  text(5.5, 7, "Feedback", cex = 2.2)
  text(5.5, 6, "zum KICK OFF", cex = 1.5)
  text(5.5, 5, x, cex = 1.2)

  text(5.5, 3, paste0("Lehrperson: ", x), cex = 1.2)
  text(5.5, 2, paste0("Kurse: ", n_kurse), cex = 1)
  text(5.5, 1, paste0("Teilnehmende: ", teilnehmende), cex = 1)
  text(5.5, 0, paste0("Durchschnittliche Bewertung: ", schnitt), cex = 1.2)

  rasterImage(beberlin, 1, 9, 4.5, 10)
  rasterImage(steps, 7.5, 9 , 10, 10)

  plot.title <- recordPlot()

  dev.off()


  plot.1 <- data.temp %>%
    filter(grepl("(^(1. )|2|3|4|5|6|7|8)", frage), between(note, 1, 6)) %>%
    ggplot(aes(y = note, x = reorder(frage, desc(frage)))) +
      coord_flip() +
      geom_count(color = alpha("mediumpurple3", 0.66), show.legend = F)  +
      scale_size_area(max_size = 6) +
      stat_summary(fun.y = mean, geom = "crossbar",
                   aes(ymax = ..y..,
                       ymin = ..y..),
                   color = "indianred2",
                   width = 0.5) +
      scale_y_continuous(name = "",
                         limits = c(1, 6),
                         breaks = c(1, 2, 3, 4, 5, 6)) +
      labs(title = "Zusammenfassende Einschätzung\nzur Veranstaltung", x = "") +
      theme_light() +
      theme(axis.text = element_text(size = 10),
            plot.title = element_text(face = "bold", size = 12, colour = "black"))


  plot.2 <- data.temp %>%
    filter(grepl("(9|10|11)", frage), between(note, 1, 6)) %>%
    ggplot(aes(y = note, x = reorder(frage, desc(frage)))) +
      coord_flip() +
      geom_count(color = alpha("mediumpurple3", 0.66), show.legend = F)  +
      scale_size_area(max_size = 6) +
      stat_summary(fun.y = mean, geom = "crossbar",
                   aes(ymax = ..y..,
                       ymin = ..y..),
                   color = "indianred2",
                   width = 0.5) +
      scale_y_continuous(name = "",
                         limits = c(1, 6),
                         breaks = c(1, 2, 3, 4, 5, 6)) +
      labs(title = "Der Dozent/ die Dozentin ...", x = "") +
      theme_light() +
      theme(axis.text = element_text(size = 10),
            plot.title = element_text(face = "bold", size = 12, colour = "black"))


  plot.graph <- ggarrange(plot.1, plot.2, ncol = 1, align = "v", heights = c(2, 1.1))

  pdf(file, width = 8.3, height = 11.7, onefile=F)

  print(ggarrange(plot.title, plot.graph, ncol = 1,  heights = c(1.6, 2), widths = c(2, 1.6)))

  dev.off()

}
