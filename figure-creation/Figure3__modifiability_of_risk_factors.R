library(tidyverse)
library(survminer)

# Directory to read the data from
public_data_dir <- "~/repos/ukb-nightingale-omics-prediction/source-data"

# Directory where the plots will be saved
dir_figures <- "~/Documents"

output_dir <- "~/repos/ukb-genomics-prediction-paper/figures/"

#read in the survival data and set the factor order
df_plot_data <- read.csv(file.path(public_data_dir, "/Figure3_plotdata.csv")) %>%
    mutate(strata = factor(strata, c("recover_status=Stayed Top10%","recover_status=Left Top10%","recover_status=Joined Top10%","recover_status=Stayed Bottom90%")))

ls_plots_3 <- purrr::map(
  .x = unique(df_plot_data$endpoint_name),
  .f = function(endpoint) {
    
    #get the summary data for the plot
    km_summary_endpoint <- df_plot_data %>% 
      dplyr::filter(endpoint_name == !!endpoint) %>%
      mutate()
    
    splot <- survminer::ggsurvplot_df(
      km_summary_endpoint,
      fun = "event",
      censor = FALSE,
      size = 1.25,
      conf.int = TRUE,
      conf.int.style = "ribbon",
      title = paste0(endpoint, "\n"),
      legend.labs = c(
        "Stayed in top decile of metabolomic score",
        "Left top decile",
        "Joined top decile",
        "Everyone else"
      ),
      legend.title = "",
      color = c("strata"),
      surv.scale = "percent",
      fontsize = 3,
      newpage = FALSE,
      axes.offset = FALSE,
      ggtheme = theme_classic() + 
        theme(
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
          legend.text = element_text(size = 12, margin = margin(r=1, unit="cm")),
          legend.title = element_blank(),
          legend.margin = margin(0,-0.1,0,-0.1, unit="cm"),
          legend.position = "top"
        )
    )
    
    splot <- splot +
      guides(fill = "none") +
      scale_x_continuous(breaks = seq(2,10,2)) +
      scale_fill_manual(values = ( c( "maroon", "darkgreen","orange", "black")) ) +
      scale_color_manual(values = (c( "maroon", "darkgreen", "orange","black")) ) +
      guides(color=guide_legend(nrow = 1, byrow = TRUE))
    
    return(splot)
  }
)


# Output the plots -------------------------------------------------------
legend_3 <- 
  cowplot::plot_grid(
    plotlist = list(cowplot::get_plot_component(ls_plots_3[[1]],"top")),
    ncol = 1
  ) 

ls_plots_3 <- purrr::map(ls_plots_3, ~.x + theme(legend.position = "none"))

plot_main_3 <-
  gridExtra::grid.arrange(
    grobs = ls_plots_3 %>% lapply(egg::set_panel_size, width = unit(2.5, "in"), height = unit(2.1, "in")),
    ncol = 4, 
    left = grid::textGrob("Cumulative incidence (%)", hjust = 0.5, rot = 90, gp = grid::gpar(fontsize = 13)),
    bottom = grid::textGrob("Follow-up time after repeat sample (years)", hjust = 0.5, gp = grid::gpar(fontsize = 13))
  )

plot_3 <- cowplot::plot_grid(
  plotlist = list(legend_3, plot_main_3),
  ncol = 1,
  rel_heights = c(1, 10)
)

pdf(file.path(dir_figures, "Figure3__modifiability_of_risk_factors.pdf"), width = 12, height = 7)
plot_3
dev.off()

png(file.path(dir_figures, "Figure3__modifiability_of_risk_factors.png"), width = 12, height = 7, units = "in", res = 200)
plot_3
dev.off()
