# This will regard functions about Fare
fare_grouped <- titanic_clean %>% group_by(Pclass)
fare_mean_class <- fare_grouped %>%
                    summarise_at(vars(Fare),
                                 list(mean = mean))

fare_group_juan <- fare_grouped %>% filter(Pclass == 1)
juan_mean <- mean(fare_group_juan$Fare)

fare_group_dos <- fare_grouped %>% filter(Pclass == 2)
dos_mean <- mean(fare_group_dos$Fare)

fare_group_tres <- fare_grouped %>% filter(Pclass == 3)
tres_mean <- mean(fare_group_tres$Fare)

# Put gradient colours for the plots (Preferably all different)
populus_colour_high <- "#213412"
populus_colour_low <- "#4412ff"

juan_colour_high <- "#331f21"
juan_colour_low <- "#aaf23e"

dos_colour_high <- "#312344"
dos_colour_low <- "#ffebc2"

tres_colour_high <- "#12334f"
tres_colour_low <- "#fcdf1a"

#Some sing bettar my friend )))
fare_populus <- function(pop_df) {
  fare_mean <- mean(pop_df$Fare)
  
  pop_df %>% ggplot(aes(x = Fare)) +
             labs(title = "Population's Fare Plot",
                  x = "Fare") +
             scale_fill_continuous(high = populus_colour_high,
                                   low = populus_colour_low) +
             geom_histogram(binwidth = 1,
                            aes(fill = after_stat(count))) +
             geom_vline(aes(xintercept = fare_mean),
                        colour = "red",
                        linetype = "dashed",
                        linewidth = 1) +
             geom_text(x = fare_mean,
                       y = Inf,
                       vjust = 1,
                       hjust = "inward",
                       aes(label = paste("Estimated Mean = ",
                                         signif(fare_mean,
                                                8))))
}

juan_plot <- fare_group_juan %>% ggplot(aes(x = Fare)) +
                                 geom_histogram(binwidth = 1,
                                                aes(fill = after_stat(count))) +
                                 labs(title = "Fare Price in Class 1") +
                                 scale_fill_continuous(high = juan_colour_high,
                                                       low = juan_colour_low) +
                                 theme(legend.position = "right") +
                                 geom_vline(aes(xintercept = juan_mean),
                                            colour = "red",
                                            linetype = "dashed",
                                            linewidth = 1) +
                                 geom_text(x = juan_mean,
                                           y = Inf,
                                           vjust = 1,
                                           hjust = "inward",
                                           aes(label = paste("Estimated Mean = ",
                                                             signif(juan_mean,
                                                                    8))))

dos_plot <- fare_group_dos %>% ggplot(aes(x = Fare)) +
                               geom_histogram(binwidth = 1,
                                              aes(fill = after_stat(count))) +
                               labs(title = "Fare Price in Class 2") +
                               scale_fill_continuous(high = dos_colour_high,
                                                     low = dos_colour_low) +
                               theme(legend.position = "right") +
                               geom_vline(aes(xintercept = dos_mean),
                                          colour = "red",
                                          linetype = "dashed",
                                          linewidth = 1) +
                               geom_text(x = dos_mean,
                                         y = Inf,
                                         vjust = 1,
                                         hjust = "inward",
                                         aes(label = paste("Estimated Mean = ",
                                                           signif(dos_mean,
                                                                  8))))

tres_plot <- fare_group_tres %>% ggplot(aes(x = Fare)) +
                                 geom_histogram(binwidth = 1,
                                                aes(fill = after_stat(count))) +
                                 labs(title = "Fare Price in Class 3") +
                                 scale_fill_continuous(high = tres_colour_high,
                                                       low = tres_colour_low) +
                                 theme(legend.position = "right") + 
                                 geom_vline(aes(xintercept = tres_mean),
                                            colour = "red",
                                            linetype = "dashed",
                                            linewidth = 1) +
                                 geom_text(x = tres_mean,
                                           y = Inf,
                                           vjust = 1,
                                           hjust = "inward",
                                           aes(label = paste("Estimated Mean = ",
                                                             signif(tres_mean,
                                                                    8))))

combining_plots <- ggarrange(juan_plot,
                             dos_plot,
                             tres_plot,
                             nrow = 3, ncol = 1)
combining_plots
