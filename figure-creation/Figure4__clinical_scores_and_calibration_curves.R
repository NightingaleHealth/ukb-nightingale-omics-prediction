library(tidyverse)

## Set-up -------------------------------------------------------------------

# Directory to read the data from
public_data_dir <- "~/repos/ukb-nightingale-omics-prediction/source-data"

# Directory where the plots will be saved
dir_figures <- "~/Documents"

# Order that the endpoints should be plotted in
endpoint_order <- c(
  "Myocardial infarction", "Ischemic stroke", "Intracerebral hemorrhage", 
  "Lung cancer", "Type 2 diabetes", "Chronic obstructive pulmonary disease", 
  "Alzheimer's disease", "Vascular and other dementia", "Depressive disorders", 
  "Alcoholic liver disease", "Cirrhosis of the liver", "Colon and rectum cancers"
  )

# Colors that the HR points will be plotted with
hr_plot_colors <- c("#99837C", "#20C4B4", "#275DB0")

# Figure 4A - performance compared to clinical models --------------------------

# Read in public data
df_associations <- readr::read_csv(
  file = file.path(public_data_dir, "Figure4a_plotdata.csv"),
  show_col_types = FALSE
  )

# Plot
fig_4a <- 
  ggforestplot::forestplot(
    df_associations,
    name = row_names,
    estimate = estimate,
    se = se,
    pvalue = pvalue,
    colour = input_name,
    psignif = 0.05/12,
    logodds = TRUE,
    xlab = "Hazard ratio (95% CI), highest risk decile vs. remaining population"
  ) +
  scale_color_manual(values = hr_plot_colors) +
  theme(
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = c(0.80, 0.91),
    legend.key.height = unit(0.5, "cm"),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10, margin = margin(0.1, 0, 1, 0))
  ) +
  scale_size_manual(values = c(0.4, 0.4, 0.4), guide = "none") + 
  coord_cartesian( xlim = c(1, 35), ylim = c(1, 11), clip = "on") +
  scale_shape_manual(values = 21) +
  guides(
    colour = guide_legend(reverse = TRUE),
    shape = guide_legend(reverse = TRUE)
  )

# Figure 4B - calibration ------------------------------------------------------

# Read in public data and nest
df_plot_main <- readr::read_csv(
  file.path(public_data_dir, "Figure4b_plotdata.csv"),
  show_col_types = FALSE
  ) %>% 
  dplyr::mutate(
    cohort = factor(cohort, levels = c("UK Biobank", "Estonian Biobank", "THL Biobank, pooled")),
    endpoint_name = factor(endpoint_name, levels = endpoint_order)
  ) %>% 
  tidyr::nest(data = -endpoint_name)

# Plot
plot_list <- 
  purrr::map2(
    .x = df_plot_main$endpoint_name,
    .y = df_plot_main$data,
    .f = function(endpoint_name, data) {
      
      limits <- range(c(data$observed_risk_ci_lower, data$observed_risk_ci_upper))
      
      ggplot(
        data,
        aes(x = predicted_risk, y = observed_risk, color = cohort)
      ) +
        geom_abline(linetype = "dashed", color = "#7A7A7A") +
        geom_point() +
        geom_errorbar(aes(ymax = observed_risk_ci_upper, ymin = observed_risk_ci_lower)) +
        facet_wrap(~cohort, scales = "fixed") +
        scale_color_manual(values = c("#5F00DC", "#CB5F19", "#537C91")) +
        scale_x_continuous(limits = limits) +
        scale_y_continuous(limits = limits) +
        ggtitle(endpoint_name) +
        labs(
          x = "Predicted event rate (%)",
          y = "Observed event rate (%)"
        ) +
        ggh4x::force_panelsizes(rows = unit(1.5, "in"), cols = unit(1.5, "in")) +
        theme_classic() +
        theme(
          aspect.ratio = 1,
          strip.text = element_blank(),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 11),
          axis.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.text = element_text(size = 11)
        )
    }
  )

# Extract legend
legend <- 
  cowplot::plot_grid(
    plotlist = list(NULL, cowplot::get_legend(plot_list[[2]])),
    ncol = 10
    ) 

# Remove legend from original plots
plot_list <- purrr::map(.x = plot_list, .f = ~.x + theme(legend.position = "none"))

# Combine left and right hand side
plot_main <- 
  cowplot::plot_grid(
    plotlist = plot_list,
    ncol = 2,
    rel_widths = c(3, 2),
    byrow = FALSE
    )

fig_4b <- 
  cowplot::plot_grid(
    plotlist = list(legend, plot_main),
    ncol = 1,
    rel_heights = c(1, 8)
    )

# Combine panel A and B 
fig_4 <- cowplot::plot_grid(
  plotlist = list(
    fig_4a,
    fig_4b
    ),
  ncol = 1,
  rel_heights = c(0.4, 0.6),
  labels = c("A", "B"),
  label_x = 0.02
  )

ggsave(
  plot = fig_4,
  filename = paste0(dir_figures, "/Fig4__clinical-comparision_calibration.pdf"), 
  width = 10, 
  height = 12 
  )

ggsave(
  plot = fig_4,
  filename = paste0(dir_figures, "/Fig4__clinical-comparision_calibration.png"), 
  width = 10, 
  height = 12, 
  bg = "white"
  )

