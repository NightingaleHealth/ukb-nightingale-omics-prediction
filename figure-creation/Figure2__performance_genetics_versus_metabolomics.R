library(tidyverse)

# Directory to read the data from
public_data_dir <- "~/repos/ukb-nightingale-omics-prediction/source-data"

# Directory where the plots will be saved
dir_figures <- "~/Documents"

#set some plotting parameters
hr_model_order = c("Metabolomics",
                   "PGS",
                   "Metabolomics + PGS")

endpoint_order <- c("Myocardial infarction", "Ischemic stroke", "Intracerebral hemorrhage", 
  "Lung cancer", "Type 2 diabetes", "Chronic obstructive pulmonary disease", 
  "Alzheimer's disease", "Vascular and other dementia", "Depressive disorders", 
  "Alcoholic liver disease", "Cirrhosis of the liver", "Colon and rectum cancers"
)

hr_plot_colors <- c("#5F00DC", "#CC79A7", "#009E73")

followup_plot_colors = c("#5F00DC", "#CC79A7")

##make Figure 2a ----------------------------------------------------
#read in data
df_associations <- read.csv(paste0(public_data_dir,"/Figure2a_plotdata.csv"))

#process the data ready for plotting
df_associations <- df_associations %>%
  dplyr::mutate(input_name = gsub( "Age + sex + ", "", input_name, fixed = T),
                input_name = gsub("met", "Met", input_name)) %>%
  dplyr::mutate(input_name = factor(input_name,
                                    levels = hr_model_order)) %>% 
  dplyr::mutate(endpoint_name = factor(endpoint_name, levels = endpoint_order)) %>%
  dplyr::arrange(endpoint_name) %>% 
  dplyr::group_by(endpoint_name) %>% 
  dplyr::arrange(input_name) %>% 
  dplyr::ungroup()

#make the first panel forestplot
figure_2a <- ggforestplot::forestplot(
  df_associations,
  name = endpoint_name,
  estimate = estimate,
  se = se,
  pvalue = pvalue,
  colour = input_name,
  psignif = 0.05/12,
  logodds = TRUE,
  xlab = "Hazard ratio (95% CI), highest risk decile vs. remaining population"
) +
  scale_color_manual(values = hr_plot_colors,
                     labels = c("Metabolomics", 
                                "PGS", 
                                "Metabolomics + PGS")) +
  theme( legend.background = element_rect(fill = "white"),
         legend.title = element_blank(),
         legend.position = c(0.83, 0.92),
         legend.key.height = unit(0.5, "cm"),
         axis.title = element_text(size = 10),
         strip.text = element_text(size = 10, margin = margin(0.1, 0, 1, 0))
  ) +
  scale_size_manual(values = c(0.4, 0.4, 0.4), guide = "none") + 
  coord_cartesian( xlim = c(1, 35), ylim = c(1, 12), clip = "on") +
  scale_shape_manual(values = 21) +
  guides(
    colour = guide_legend(reverse = TRUE),
    shape = guide_legend(reverse = TRUE)
  )


##make Figure 2b ----------------------------------------------------
#read in data
df_figure_2b <- read.csv(paste0(public_data_dir,"/Figure2b_plotdata.csv"))

#make the raw plots
figure_2b_raw <-
  purrr::map(
    .x = c("Myocardial infarction\n",
           "Ischemic stroke\n",
           "Intracerebral hemorrhage\n",
           "Lung cancer\n",
           "Type 2 diabetes\n",
           "Chronic obstructive\npulmonary disease",
           "Alzheimer's disease\n",
           "Depressive disorders\n",
           "Cirrhosis of the liver\n",
           "Colon and rectum cancers\n"),
    .f = function(endpoint) {
      
      # Fit the survival model  
      fit_summary <- df_figure_2b %>% dplyr::filter(endpoint_name == endpoint)
      
      # Kaplan-Meier plot
      plot <- 
        survminer::ggsurvplot_df(
          fit = fit_summary,
          fun = "event",
          censor = FALSE,
          size = 1,
          conf.int = TRUE,
          conf.int.style = "ribbon",
          title = endpoint,
          legend.title = "",
          legend.labs = c("Bottom 90% of PGS", "Top decile of PGS, bottom 90% of metabolomic score", 
                          "Top decile of PGS and metabolomic score"),
          color = c("strata"),
          newpage = FALSE,
          axes.offset = FALSE,
          surv.scale = "percent",
          fontsize = 3,
          ggtheme =  theme_classic() +
            theme(
              axis.title = element_blank(),
              axis.text = element_text(size = 7),
              plot.title = element_text(face = "bold", size = 8, hjust = 0.5),
              legend.text = element_text(size = 8.5, margin = margin(r=0.3, unit="cm")),
              legend.title = element_blank(),
              legend.margin = margin(0,-0.1,0,-0.1, unit="cm"),
            )
        )
      
      plot + 
        guides(fill = "none", color = guide_legend(reverse = TRUE)) +
        # scale_x_continuous(expand = c(0,0), n.breaks = 5) +
        scale_x_continuous(breaks = seq(2,10,2)) +
        scale_fill_manual(values = rev(c("#F2003D", "orange", "black"))) +
        scale_color_manual(values = rev(c("#F2003D", "orange", "black"))) 
      
    }
  )


# Extract legend
legend_2b <-
  cowplot::plot_grid(
    plotlist = list(cowplot::get_plot_component(figure_2b_raw[[1]],"top")),
    ncol = 1
  )

# Remove legend from original plot
figure_2b_no_legend <- purrr::map(figure_2b_raw, ~.x + theme(legend.position = "none"))

# Combine left and right hand side
plot_main_2b <-
  gridExtra::grid.arrange(
    grobs = figure_2b_no_legend %>% lapply(egg::set_panel_size, width = unit(1.35, "in"), height = unit(1.2, "in")),
    ncol = 5, 
    left = grid::textGrob("Cumulative incidence (%)", hjust = 0.5, rot = 90, gp = grid::gpar(fontsize = 10)),
    bottom = grid::textGrob("Follow-up time (years)", hjust = 0.2, gp = grid::gpar(fontsize = 10))
  )

figure_2b <- cowplot::plot_grid(
  plotlist = list(legend_2b, plot_main_2b),
  ncol = 1,
  rel_heights = c(0.1, 1)
)

##make Figure 2b
#read in data
tvc_results_combined <- read.csv(paste0(public_data_dir,"/Figure2c_tvc_plotdata.csv"), check.names = FALSE)
stratified_results_combined <- read.csv(paste0(public_data_dir,"/Figure2c_stratified_plotdata.csv"), check.names = FALSE)

endpoint_order[endpoint_order == "Chronic obstructive pulmonary disease"] <- "Chronic obstructive \npulmonary disease" 
stratified_results_combined$disease_endpoint <- factor(stratified_results_combined$disease_endpoint,levels = endpoint_order)
tvc_results_combined$disease_endpoint <- factor(tvc_results_combined$disease_endpoint,levels = endpoint_order)

figure_2c <- ggplot() +
  scale_color_manual(values = followup_plot_colors,
                     labels = c("Metabolomics", "PGS")) +
  theme(legend.title = element_blank(),
        legend.position="top",
        legend.text = element_text(size = 8.5),
        legend.key = element_rect(fill = "white"),
        strip.background = element_rect(color = "white",
                                        fill = "white"),
        strip.text.x = element_text(size = 8,
                                    face = "bold")) +
  xlab("Time to event (years)") +
  ylab("Hazard ratio (95% CI), highest risk decile vs. remaining population") +
  geom_ribbon(data = tvc_results_combined, alpha = 0.2, aes(fill = model,
                                                            x = event_agediff,
                                                            ymin = lower,
                                                            ymax = upper), colour = NA) +
  scale_fill_manual(values = followup_plot_colors) + 
  guides(fill = "none") + 
  facet_wrap(vars(disease_endpoint),
             nrow = 4,
             ncol = 3,
             scales = "free") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(2, 10, 2),
                     limits = c(0.8,10.2)
  ) + 
  ggh4x::facetted_pos_scales(y = list(
    disease_endpoint == "Myocardial infarction" ~ scale_y_continuous(breaks = c(1, 2, 4),
                                                                     limits = c(0.5, 4)),
    disease_endpoint == "Ischemic stroke" ~ scale_y_continuous(breaks = c(1, 2, 4),
                                                               limits = c(0.5, 4)),
    disease_endpoint == "Intracerebral hemorrhage" ~ scale_y_continuous(breaks = c(1, 2, 4),
                                                                        limits = c(0.5, 4)),
    disease_endpoint == "Lung cancer" ~ scale_y_continuous(breaks = c(1, 5, 10),
                                                           limits = c(0.5, 10)),
    disease_endpoint == "Type 2 diabetes" ~ scale_y_continuous(breaks = c(1, 10, 20),
                                                               limits = c(0.5, 20)),
    disease_endpoint == "Chronic obstructive \npulmonary disease" ~ scale_y_continuous(breaks = c(1, 5, 10),
                                                                                       limits = c(0.5, 10)),
    disease_endpoint == "Alzheimer's disease" ~ scale_y_continuous(breaks = c(1, 5, 10),
                                                                   limits = c(0.5, 10)),
    disease_endpoint == "Vascular and other dementia" ~ scale_y_continuous(breaks = c(1, 5, 10),
                                                                           limits = c(0.5, 10)),
    disease_endpoint == "Depressive disorders" ~ scale_y_continuous(breaks = c(1, 2, 4),
                                                                    limits = c(0.5, 4)),
    disease_endpoint == "Alcoholic liver disease" ~ scale_y_continuous(breaks = c(1, 25, 50),
                                                                       limits = c(0.5, 50)),
    disease_endpoint == "Cirrhosis of the liver" ~ scale_y_continuous(breaks = c(1, 10, 20),
                                                                      limits = c(0.5, 20)),
    disease_endpoint == "Colon and rectum cancers" ~ scale_y_continuous(breaks = c(1, 2, 4),
                                                                        limits = c(0.5, 4))
  )) + 
  geom_point(data = stratified_results_combined, aes(x = x_axis_points,
                                                     y = `HR top decile`,
                                                     color = model),
             shape = 19,
             size = 1.2) +
  geom_linerange(data = stratified_results_combined, aes(x = x_axis_points, 
                                                         ymin = `CI low`, 
                                                         ymax = `CI high`,
                                                         color = model),
                 show.legend = F) + 
  geom_hline(yintercept = 1, linetype = 2)




# Fig 2 Final --------------------------------------------------------

plot_final <- 
  cowplot::plot_grid(
    plotlist = list(
      figure_2a,
      figure_2b,
      figure_2c
    ),
    ncol = 1,
    rel_heights = c(1, 0.7, 1.5),
    labels = c("A", "B", "C"),
    label_x = 0.02
  )

# ggsave the PDF
ggsave(plot = plot_final,
       filename = paste0(dir_figures,"/Fig2__performance_genetics_versus_metabolomics.pdf"),
       device = "pdf",
       width = 9,
       height = 20,
       units = "in")

# ggsave the png
ggsave(plot = plot_final,
       filename = paste0(dir_figures,"/Fig2__performance_genetics_versus_metabolomics.png"),
       device = "png",
       width = 9,
       height = 20,
       units = "in",
       dpi = 200,
       bg = "white")

