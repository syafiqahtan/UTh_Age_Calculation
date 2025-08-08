# function to plot all the results from the sensitivity analysis
# function written by Syaf on 8August2025

plotAllSensResults <- function(infile, minBound, maxBound) { #min and maxBound also represents y axis limits
  
  # the infile is in .xlsx format
  
  data_ends <- inFile %>% group_by(sampleID) %>% filter(value == max(value))
  unique_sampleID <- unique(inFile$sampleID)
  num_uniquesampleID <- length(unique_sampleID)
  
  # choosing random colours
  colour_plot <- randomcoloR::randomColor(count = unique_files)
  
  # temp df for corresponding colours
  temp_colour_df <- data.frame(unique_id = unique_sampleID,
                               colour = colour_plot)
  
  # plotting now

all_results_plot <- ggplot(data = inFile) +
  geom_ribbon(aes(x = value, ymin = corr_mean - corr_2sd, ymax = corr_mean + corr_2sd, fill = sampleID), alpha = 0.5) +
  geom_line(aes(y = corr_mean, x = value, colour = sampleID)) +
  geom_text_repel(data = data_ends, aes(x = value, y = corr_mean, label = sampleID, , colour = sampleID), size = 3.5,
                  fontface = "bold", segment.colour = NA, nudge_x = 2e-6) +
  scale_x_continuous(breaks = seq(0, 10e-6, by = 2e-6), # Set x-axis ticks every 1e-6
                     labels = seq(0, 10, by = 2)) + # Convert scientific to whole numbers
  scale_y_continuous(breaks = seq(minBound, maxBound, by = 200),
                     limits = c(minBound,maxBound)) + 
  ylab("Corrected age (years BP)") + 
  xlab(expression("Initial " ~ {}^{230}*"Th/"*{}^{232}*"Th ratio (e-6)")) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size=12, face="bold", colour = "black"), # axis label
    axis.title.y = element_text(size=12, face="bold", colour = "black"),
    axis.text.x = element_text(size = 10, face = "bold"), # ticks
    axis.text.y = element_text(size = 10, face = "bold")) +
  guides(colour = "none", fill = "none") +
  scale_fill_manual(values = temp_colour_df$colour) +
  scale_colour_manual(values = temp_colour_df$colour) 
all_results_plot
  
print(all_results_plot)
}


  
