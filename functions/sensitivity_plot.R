# This function is to be used with sensitivity analysis. Makes a plot of the various corrected ages.
# This function last modified by Syafiqah on 9 March 2025

# Update 9 March: ran for individual samples for easier visualisation

sensitivity_plot <- function(x) {
  
  for (i in 1:length(unique(x$labID))) {
    
  df_subset <- x %>% dplyr::filter(labID == i)
  
  # data_ends <- df_subset %>%
  #   group_by(sampleID) %>%
  #   filter(value == max(value)) # Keeping only one max value per sample ID (the max) # this is for plotting
  
  sens_plot <- ggplot() +
    geom_ribbon(data = df_subset, aes(x = value, ymin = corr_mean - corr_2sd, ymax = corr_mean + corr_2sd, fill = sampleID), alpha = 0.2) +
    geom_line(data = df_subset, aes(y = corr_mean, x = value, colour = sampleID)) +
    # geom_text_repel(data = data_ends, aes(x = value, y = corr_mean, label = sampleID), colour = "black", size = 3, 
    #                 fontface = "bold", segment.colour = NA, nudge_x = 3e-6) +
    scale_x_continuous(breaks = seq(0, 10e-6, by = 2e-6), # Set x-axis ticks every 1e-6
                       labels = seq(0, 10, by = 2)) + # Convert scientific to whole numbers
    ylab("Corrected age (years BP)") + 
    xlab(expression("Initial " ~ {}^{230}*"Th/"*{}^{232}*"Th ratio (e-6)")) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size=12, face="bold", colour = "black"), # axis label
      axis.title.y = element_text(size=12, face="bold", colour = "black"),
      axis.text.x = element_text(size = 10, face = "bold"), # ticks
      axis.text.y = element_text(size = 10, face = "bold")) +
    labs(title = df_subset[1,1]) +
    guides(colour = "none", fill = "none")
  sens_plot
  
  # plot_name <- paste0('figures/sensitivity/sens_plot_', df_subset$labID, '.png')
  
  ggsave(paste0('figures/sensitivity/sens_plot_', df_subset$labID, '.png'), sens_plot, height = 5, width = 4)
  print(paste("Plot for", df_subset[1,1], "has been created and saved in /figures/sensitivity folder."))
  # print("Plot has been created and saved in /figures folder.")

  # print(sens_plot)
  }
}