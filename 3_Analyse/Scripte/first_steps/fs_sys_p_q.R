# Variables für Titlepage
n_q <- nrow(data.final_sys[data.final_sys$part == "q",])/12
n_p <- nrow(data.final_sys[data.final_sys$part == "p",])/11

note_q <- round(mean(data.final_sys[data.final_sys$part == "q" & !grepl("Nein|v0", data.final_sys$frage),"note"], na.rm = T), digits = 1)
note_p <- round(mean(data.final_sys[data.final_sys$part == "p" & !grepl("Nein", data.final_sys$frage),"note"], na.rm = T), digits = 1)


# Ränder für Plots
par(oma = c(1, 1, 0, 1))
par(mar = c(1, 1, 0, 1))


# Plot für Title für p_q_sys
plot(0:10, asp = 0.6, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

text(5.5, 7, "Evaluation", cex = 2.2)
text(5.5, 6, "FIRST STEPS - Patensystem", cex = 1.5)

text(5.5, 4, "Teilnehmende:", cex = 1.2)
text(5.5, 3, paste0("Quereinsteiger/innen: ", n_q,
                    "     Paten/Patinnen: ", n_p), cex = 1)

text(5.5, 1, "Durchschnittliche Bewertung:", cex = 1.2)
text(5.5, 0, paste0("Quereinsteiger/innen: ", note_q,
                    "     Paten/Patinnen: ", note_p), cex = 1)

rasterImage(beberlin, 1, 9, 4.5, 10)
rasterImage(arrow, 7.5, 9 , 10, 10)

plot.title <- recordPlot()

dev.off()


# Plot für p_q_sys
plot <- data.final_sys %>% filter(!grepl("^Nein|v0", frage), between(note, 1, 6)) %>%
  ggplot(aes(y = note,
             x = reorder(frage, desc(frage)))) +
    geom_count(aes(color = factor(part, labels = c("Paten/Patinnen", "Quereinsteiger/innen"))),
               show.legend = T,
               position = position_dodge(0.8),
               alpha = 0.4) +
    stat_summary(fun.y = mean, geom = "crossbar",
                 aes(ymax = ..y..,
                     ymin = ..y..,
                     fill = part),
                 position = position_dodge(0.8),
                 color = "indianred2",
                 width = 0.6,
                 show.legend = F) +
    coord_flip() +
    scale_color_manual(values = c("#8968CD", "#ABCD68")) +
    scale_size_area(max_size = 5) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 60)) +
    scale_y_continuous(name = "",
                       limits = c(1, 6),
                       breaks = c(1, 2, 3, 4, 5, 6),
                       minor_breaks = NULL) +
    labs(x = "") +
    theme_light() +
    guides(color = guide_legend(reverse = T,
                                override.aes = list(size = 4)),
           size = F,
           fill = F) +
    theme(axis.text = element_text(size = 10),
          plot.title = element_text(face = "bold", size = 12, colour = "black"),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.position = "bottom")


file <- paste0("4_Ergebnisse/Grafiken/first_steps/", fs_jahr, "/fs_sys_p_q.pdf")

pdf(file, width = 8.3, height = 11.7, onefile = F)

print(ggarrange(plot.title, plot, ncol = 1,  heights = c(1.6, 2), widths = c(2, 1.6)))

dev.off()
