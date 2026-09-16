library(patchwork)

p1 <- p1 + labs(tag = "A)")
p2 <- p2+ labs(tag = "B)")

combined_plot <- (p1 / p3) | (p2 / p4)

# Add uppercase letter tags (A, B, C...)
combined_plot 

ggsave(paste0("Plots/jointPlot_diffid1_68_CCDC_LandTrendR.jpeg"),
       combined_plot,
       width = 32,
       height = 32,
       units = "cm",
       dpi = 300)

common_plot <- common_plot + scale_x_continuous(limits = c(-22,14)) + labs(tag = "A)") #+ 
  # theme(plot.margin = margin(5.5, 5.5, 5.5, 30),
  #       plot.tag.position = c(-0.05, 0.99))
common_plot_verif <- common_plot_verif+ scale_x_continuous(limits = c(-22,14))+ labs(tag = "B)") #+
  # theme(plot.margin = margin(5.5, 5.5, 5.5, 30),
  #       plot.tag.position = c(-0.05, 0.99))
  
combined_plot <- common_plot / common_plot_verif

combined_plot

ggsave(paste0("Plots/jointPlot_dateValidation_LandTrendR.jpeg"), 
       combined_plot, 
       width = 32, 
       height = 32,
       units = "cm",
       dpi = 300)
