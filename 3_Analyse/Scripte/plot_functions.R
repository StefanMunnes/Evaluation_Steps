plot1 <- function(data, name, title) {

		ggplot(data, aes(x = reorder(name, desc(name)), fill = name)) +
			geom_bar(aes(y = (..count..)/sum(..count..)),
	             width = 0.7) +
	    theme_minimal() +
      labs(title = title, x = NULL, y = "Prozent") +
			theme(legend.position = "none",
            plot.title = element_text(hjust = 0, size = 14, face = "bold")) +
	    scale_y_continuous(labels = scales::percent) +
      coord_flip()
}


plot.likert <- function(batterie, title) {

  likert(batterie) %>%
  	likert.bar.plot(group.order = names(.$items),
  		plot.percent.low = F, plot.percent.high = F,
  		colors = colors, wrap = 40, legend = NULL) +
  	labs(title = title) +
  	ylab("Zustimmung in Prozent") +
  	theme_light() +
  	theme(legend.position="bottom",
  				legend.text = element_text(size = 9),
  				legend.key.size = unit(.5, "cm"),
          plot.title = element_text(hjust = 0, size = 14, face = "bold")
				  )
}
