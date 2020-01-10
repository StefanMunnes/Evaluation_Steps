# Variables für Titlepage
n_p <- nrow(data.final_sys[data.final_sys$part == "p",])/11

note_p <- round(mean(data.final_sys[data.final_sys$part == "p" &
                                    !grepl("Nein", data.final_sys$frage_p),"note"], na.rm = T), digits = 1)

v7_no <- round(mean(data.final_sys[data.final_sys$part == "p" &
                                   grepl("v7", data.final_sys$frage_p),"note"], na.rm = T), digits = 2) * 100
v8_no <- round(mean(data.final_sys[data.final_sys$part == "p" &
                                   grepl("v8", data.final_sys$frage_p),"note"], na.rm = T), digits = 2) * 100

caption <- paste0("¹ Falls nein: ", v7_no, "% länger; ", 100-v7_no, "% kürzer.\n² Falls nein: ", v8_no, "% länger; ", 100-v8_no, "% kürzer.")
if (is.na(v7_no) & !is.na(v8_no)) {
 caption <- paste0("¹ Falls nein: keine weiteren Angaben.\n² Falls nein: ", v8_no, "% länger; ", 100-v8_no, "% kürzer.")
}
if (!is.na(v7_no) & is.na(v8_no)) {
 caption <- paste0("¹ Falls nein: ", v7_no, "% länger; ", 100-v7_no, "% kürzer.\n² Falls nein: keine weiteren Angaben.")
}
if (is.na(v7_no) & is.na(v8_no)) {
 caption <- "¹ Falls nein: keine weiteren Angaben.\n² Falls nein: keine weiteren Angaben."
}


# Margins für Plots
par(oma = c(1, 1, 0, 1))
par(mar = c(1, 5, 0, 1))


# Plot für Title für p_q_sys
plot(0:10, asp = 0.6, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

text(5.5, 7, "Evaluation", cex = 2.2)
text(5.5, 6, "FIRST STEPS", cex = 1.5)
text(5.5, 4, "Fragen zum Patensystem an die Patin/den Paten", cex = 1.3)

text(5.5, 1, paste0("Teilnehmende: ", n_p), cex = 1)

text(5.5, 0, paste0("Durchschnittliche Bewertung: ", note_p), cex = 1)

rasterImage(beberlin, 1, 9, 4.5, 10)
rasterImage(arrow, 7.5, 9 , 10, 10)

plot.title <- recordPlot()

dev.off()


# Plot für p_q_sys
plot <- data.final_sys %>% filter(part == "p", !grepl("^Nein|v0", frage_p), between(note, 1, 6)) %>%
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
    labs(x = "",
         caption = paste0("¹ Falls nein: ", v7_no, "% länger; ", 100-v7_no, "% kürzer.\n² Falls nein: ", v8_no, "% länger; ", 100-v8_no, "% kürzer.")) +
    theme_light() +
    theme(axis.text = element_text(size = 10),
          plot.title = element_text(face = "bold", size = 12, colour = "black"),
          plot.caption = element_text(size = 10, hjust = 0))


file <- paste0("4_Ergebnisse/Grafiken/first_steps/", fs_jahr, "/fs_sys_p.pdf")

pdf(file, width = 8.3, height = 11.7, onefile = F)

print(ggarrange(plot.title, plot, ncol = 1,  heights = c(1.6, 2), widths = c(2, 1.6)))

dev.off()
