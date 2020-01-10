# Variables für Titlepage
n_p <- nrow(data.final_pat[data.final_pat$part == "p",])/10

note_p <- round(mean(data.final_pat[data.final_pat$part == "p" &
                                    !grepl("Nein", data.final_pat$frage_p),"note"], na.rm = T), digits = 1)


# Margins für Plots
par(oma = c(1, 1, 0, 1))
par(mar = c(1, 5, 0, 1))


# Plot für Title für p_pat
plot(0:10, asp = 0.6, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

text(5.5, 7, "Evaluation", cex = 2.2)
text(5.5, 6, "FIRST STEPS", cex = 1.5)
text(5.5, 4, "Fragen zur Patenschaft an die Patin/den Paten", cex = 1.3)

text(5.5, 2, paste0("Teilnehmende: ", n_p), cex = 1)

text(5.5, 0, paste0("Durchschnittliche Bewertung: ", note_p), cex = 1)

rasterImage(beberlin, 1, 9, 4.5, 10)
rasterImage(arrow, 7.5, 9 , 10, 10)

plot.title <- recordPlot()

dev.off()


# Plot für q_pat
plot <- data.final_pat %>% filter(part == "p", !grepl("^Nein|v0", frage_p), between(note, 1, 6)) %>%
  ggplot(aes(y = note,
             x = reorder(frage_p, desc(frage_p)))) +
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


file <- paste0("4_Ergebnisse/Grafiken/first_steps/", fs_jahr, "/fs_pat_p.pdf")

pdf(file, width = 8.3, height = 11.7, onefile = F)

print(ggarrange(plot.title, plot, ncol = 1,  heights = c(1.6, 2), widths = c(2, 1.6)))

dev.off()
