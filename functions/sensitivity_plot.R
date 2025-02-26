# This function is to be used with sensitivity analysis. Makes a plot of the various corrected ages.
# This function last modified by Syafiqah on 26 February 2025

sensitivity_plot <- function(x) {
  
  data_ends <- x %>%
    group_by(sampleID) %>%
    filter(value == max(value)) # Keeping only one max value per sample ID (the max) # this is for plotting
  
  sens_plot <- ggplot() +
    geom_ribbon(data = x, aes(x = value, ymin = corr_mean - corr_2sd, ymax = corr_mean + corr_2sd, fill = sampleID), alpha = 0.2) +
    geom_line(data = x, aes(y = corr_mean, x = value, colour = sampleID)) +
    geom_text_repel(data = data_ends, aes(x = value, y = corr_mean, label = sampleID), colour = "black", size = 3, 
                    fontface = "bold", segment.colour = NA, nudge_x = 3e-6) +
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
    guides(colour = "none", fill = "none")
  sens_plot
  
  ggsave("figures/sensitivity_plot.png", sens_plot, height = 5, width = 6)
  print("Plot has been created and saved in /figures folder.")
  
  print(sens_plot)
}