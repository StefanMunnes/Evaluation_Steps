raw <- sapply(raw_files, function(x) read.csv2(paste0("3_Analyse/Scans/set_up/", x),
                                               stringsAsFactor = T,
                                               colClasses = "character"))


rename <- raw %>% select(Prüfung, starts_with("TICKED.")) %>%
  rename_at(vars(starts_with("TICKED.")), funs(str_replace(., "TICKED.", "")))


part_1 <- rename[c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8")] %>%
	mutate_all(funs(recode_factor(.,  !!!skala))) %>%
	mutate_all(funs(ordered(., levels = order)))

names(part_1) <- items_1

final_1 <- gather(part_1, variable, mark)




part_2 <- rename[c("v9", "v10", "v11")] %>%
	mutate_all(funs(recode_factor(.,  !!!skala))) %>%
	mutate_all(funs(ordered(., levels = order)))

names(part_2) <- items_2

final_2 <- gather(part_2, variable, mark)


ggplot(final_1, aes(y= mark, x = variable)) +
  coord_flip() +
  geom_jitter(position = position_jitter(0.1), cex = 1.2) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "red")



ggplot(final_2, aes(x = mark, y = variable)) +
  geom_jitter(position = position_jitter(0.2), cex = 1.2) +
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, color = "red")
