library(tidyverse)

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

# Order that the names should be plotted in (for the edited ones)
name_order <- c(
  "Myocardial infarction\n", "Ischemic stroke\n", "Intracerebral\nhemorrhage", 
  "Lung cancer\n", "Type 2 diabetes\n", "Chronic obstructive\npulmonary disease", 
  "Alzheimer's disease\n", "Vascular and\nother dementia", "Depressive disorders\n", 
  "Alcoholic liver disease\n", "Cirrhosis of the liver\n", "Colon and\nrectum cancers"
)

# Order that the cohorts should be plotted in
cohort_order <- c(
  "Estonian Biobank",
  "THL Biobank",
  "UK Biobank",
  "",
  "Meta-analysis"
  )

# Plot 1A ----------------------------------------------------------------------

# Read in public data
df_plot_percentiles <- readr::read_csv(
  file.path(public_data_dir, "Figure1a_plotdata.csv"),
  show_col_types = FALSE
  ) %>% 
  dplyr::mutate(
    # Edit names so they fit in the plot
    two_line_name = dplyr::case_when(
      endpoint_name == "Intracerebral hemorrhage" ~ "Intracerebral\nhemorrhage",
      endpoint_name == "Chronic obstructive pulmonary disease" ~ "Chronic obstructive\npulmonary disease",
      endpoint_name == "Vascular and other dementia" ~ "Vascular and\nother dementia",
      endpoint_name == "Colon and rectum cancers" ~ "Colon and\nrectum cancers",
      TRUE ~ paste0(endpoint_name, "\n")
      ),
    two_line_name = factor(two_line_name, levels = name_order)
    ) %>% 
  dplyr::arrange(two_line_name) 

# Plot
plot_percentiles <- 
  ggplot(
    data = df_plot_percentiles,
    aes(x = percentile, y = estimate)
  ) +
  geom_rect(
    aes(xmin = 90, xmax = 100, ymin = -Inf, ymax = Inf), 
    fill =  "#F6DAE0", color = NA, alpha = 1
  ) +
  geom_point(size = 1, color = "gray20") +
  facet_wrap(~two_line_name, scales = "free") +
  geom_hline(
    aes(yintercept = total_incidence),
    color = "#B8B8B8",
    lty = "11", linewidth = 0.9
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Percentile of metabolomic score",
    y = "Incidence (%)"
  ) +
  ggh4x::force_panelsizes(rows = unit(1.5, "in"), cols = unit(1.5, "in")) +
  theme_classic() +
  theme(
    strip.background = element_rect(color = NA),
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

# Plot 1B ----------------------------------------------------------------------

# Read in public data
df_plot <- readr::read_csv(
  file.path(public_data_dir, "Figure1b_plotdata.csv"),
  show_col_types = FALSE
  ) %>% 
  dplyr::mutate(
    cohort = tidyr::replace_na(cohort, ""),
    cohort = factor(cohort, levels = cohort_order),
    endpoint_name = factor(endpoint_name, levels = endpoint_order)
    ) %>% 
  dplyr::arrange(cohort, endpoint_name)

# Plot
plot_assoc <- 
  ggforestplot::forestplot(
    df = df_plot,
    name = endpoint_name,
    estimate = estimate,
    colour = cohort,
    size = cohort,
    pvalue = pvalue,
    se = se,
    logodds = TRUE,
    shape = cohort,
    psignif = 0.05/12,
    stroke = 10
  ) +
  xlab(
    paste0(
      "Hazard ratio (95% CI), ", 
      "highest risk decile vs. remaining population\n"
    ) 
  ) +
  scale_colour_manual(values = c(
    adjustcolor("#CB5F19", alpha.f = 1), 
    adjustcolor("#537C91", alpha.f = 1),
    adjustcolor("#5F00DC", alpha.f = 1), 
    # Blank point to create space b/w meta-analysis and other cohorts
    adjustcolor( "white", alpha.f = 0), 
    "black")
  ) +
  scale_size_manual(values = c(0.4, 0.4, 0.4, 0.4, 0.8), guide = "none") +
  coord_cartesian(xlim = c(1, max(exp(df_plot$estimate + 1.96*df_plot$se)))) +
  scale_shape_manual(values = c(21,21,21,23,23)) +
  ggh4x::force_panelsizes(cols = unit(5, "in"), rows = unit(4.5, "in")) +
  guides(
    colour = guide_legend(ncol = 1, reverse = T)
  ) +
  theme(
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.835, 0.86),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.key.height = unit(0.5, "cm"),
    axis.title = element_text(size = 10),
    legend.title = element_blank(),
    strip.text = element_text(size = 10, margin = margin(0.1, 0, 1, 0))
  )

# Make the final 2-panel plot --------------------------------------------------

plot_final <- 
  cowplot::plot_grid(
    plotlist = list(
      plot_percentiles,
      plot_assoc
    ),
    ncol = 1,
    rel_heights = c(1, 0.8),
    labels = c("A", "B"),
    label_x = 0.02
  )

ggsave(
  plot = plot_final,
  filename = file.path(
    dir_figures, "Fig1AB__percentile_risk_and_hazard_ratios.pdf"
    ),
  w = 9,
  h = 12.5
)

ggsave(
  plot = plot_final,
  filename = file.path(
    dir_figures, "Fig1AB__percentile_risk_and_hazard_ratios.png"
    ),
  w = 9,
  h = 12.5,
  bg = "white",
  dpi = 200
)


