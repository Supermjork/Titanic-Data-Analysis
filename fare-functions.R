# This will regard functions about Fare
library(ggplot2)
fare_grouped <- titanic_clean %>% group_by(Pclass)
fare_mean_class <- fare_grouped %>%
                    summarise_at(vars(Fare),
                                 list(mean = mean))
hist_pos <- "dodge"
leg_pos <- "right"
fare_group_juan <- fare_grouped %>% filter(Pclass == 1)
fare_group_dos <- fare_grouped %>% filter(Pclass == 2)
fare_group_tres <- fare_grouped %>% filter(Pclass == 3)
# This code sucks
fare_fancyplot <- fare_grouped %>%
                  ggplot(aes(x = Fare,
                             color = as.character(Pclass),
                             fill = as.character(Pclass))) +
                  geom_histogram(binwidth = 1,
                                 alpha = 0.25,
                                 position = hist_pos) +
                  labs(title = "Fare Price by Class",
                       y = "count") +
                  theme(legend.position = leg_pos)

fare_fancyplot
#Some sing bettar my friend )))
juan_plot <- ggplot(data = fare_group_juan,
                    aes(x = Fare, color = as.character(Pclass),
                    fill = as.character(Pclass))) +
            geom_histogram(binwidth = 1,
                           alpha = 0.25,
                          position = hist_pos) +
            labs(title = "Fare Price in Class 1",
          y = "count") +
          theme(legend.position = leg_pos)
dos_plot <- ggplot(data = fare_group_dos,
                    aes(x = Fare, color = as.character(Pclass),
                    fill = as.character(Pclass))) +
            geom_histogram(binwidth = 1,
                           alpha = 0.25,
                          position = hist_pos) +
            labs(title = "Fare Price in Class 2",
          y = "count") +
          theme(legend.position = leg_pos)
tres_plot <- ggplot(data = fare_group_tres,
                    aes(x = Fare, color = as.character(Pclass),
                    fill = as.character(Pclass))) +
            geom_histogram(binwidth = 1,
                           alpha = 0.25,
                          position = hist_pos) +
            labs(title = "Fare Price in Class 3",
          y = "count") +
          theme(legend.position = leg_pos)

class_mean_fare <- geom_vline(data = fare_mean_class,
                              aes(xintercept = mean,
                                  colour = as.character(Pclass)),
                              linetype = "dashed",
                              linewidth = 1)

fare_absmean <- geom_vline(aes(xintercept = mean(Fare)),
                             colour = "red",
                             linetype = "dashed",
                             linewidth = 1)

fare_plot_grid <- facet_rep_grid(as.character(Pclass) ~ ., scales = "free",
                                repeat.tick.labels = TRUE)

fare_fancyplot + fare_plot_grid

fare_plot_text <- geom_text(data = fare_mean_class,
                           aes(x = mean,
                               y = Inf,
                               vjust = 1,
                               label = paste0("Estimated Mean for class",
                                              Pclass,
                                              " Fare: ", signif(mean, 5))),
                           show.legend = FALSE,
                           colour = "black")

fare_fancyplot + list(fare_mean_class, fare_plot_grid, fare_plot_text)
