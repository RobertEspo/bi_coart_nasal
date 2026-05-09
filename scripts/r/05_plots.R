### dominance distribution in sample ###

p_dominance_distribution <- dat_female_tidy %>%
  group_by(participant) %>%
  summarise(
    interviewee_final_blp_score = first(interviewee_final_blp_score)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = "", y = interviewee_final_blp_score, color = interviewee_final_blp_score)) +
  coord_cartesian(ylim = c(-160, 160)) +
  scale_y_continuous(breaks = seq(-160, 160, by = 20)) +
  geom_violin(fill = "black", alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "grey80", outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 2, alpha = 0.9) +
  scale_color_gradient2(
    low = "#648FFF",
    mid = "#FFB000",
    high = "#DC267F",
    midpoint = 0,
    breaks = c(-200, 0, 200),
    limits = c(-218, 218),
    labels = c("Spanish", "Balanced", "English")
  ) +
  labs(x = NULL, y = "Bilingual Language Profile score", color = "Bilingual Language \nProfile score") +
  ds4ling::ds4ling_bw_theme(base_size = 12)

ggsave(here("figs","p_dominance_distribution.png"), 
       plot = p_dominance_distribution, 
       width = 10, height = 10, dpi = 300)

###############################################################################
### RQ1 Plots ###

# Smooths for English & Spanish

if (TRUE) {
png(here("figs","p_rq1_smooths.png"), width = 15, height = 15, units = "in", res = 300)

  peaks_smooth <- as.data.frame(plot_smooth(
    gam_rq1_ar1,
    view="point_vwlpct",
    plot_all="language.ord",
    col = c("#0072B2","#E69F00"))$fv) %>%
    group_by(language.ord) %>%
    slice_max(fit, n = 1, with_ties = TRUE) %>%
    summarise(
      peak_vwlpct = mean(point_vwlpct),
      peak_fit = max(fit),
      .groups = "drop"
    )
  
  plot_smooth(
    gam_rq1_ar1,
    view="point_vwlpct",
    plot_all="language.ord",
    col = c("#0072B2","#E69F00"),
    v0=peaks_smooth$peak_vwlpct,
    h0=peaks_smooth$peak_fit,
    ylim=c(-8,-2)
  )

dev.off()

}

# Diff plot Spanish - English

if (TRUE) {
png(here("figs","p_rq1_diff.png"), width = 15, height = 15, units = "in", res = 300)

  valley_diff <- as.data.frame(
    plot_diff(
      gam_rq1_ar1,
      view = "point_vwlpct",
      comp = list(language.ord = c("English", "Spanish")),
      mark.diff = FALSE
    )
  ) %>%
    slice_min(est, n = 1, with_ties = TRUE) %>%
    summarise(
      valley_vwlpct = mean(point_vwlpct),
      valley_est = min(est),
      .groups = "drop"
    )
  
  plot_diff(
    gam_rq1_ar1,
    view="point_vwlpct",
    comp=list(language.ord=c("English","Spanish")),
    mark.diff = FALSE,
    v0=valley_diff[1,1],
    h0=valley_diff[1,2]
  )

dev.off()
}
  
###############################################################################
### RQ2 Plots ###
# What scores to plot at is annoying. The sample I have is skewed towards
# English-dominance, and the range is -144 to 155, so it is quite far
# from reaching the limits of -218 to 218. Going off z scores (e.g., -1, 0, 1)
# seems like a bad idea, because that would be relative to my sample, not
# relative to the actual BLP scale. So for this, I'll go with -75, 0, 75.
# This seems reasonable given that we have a decent amount of individuals
# who have values within this range, and they seem far enough apart count
# as "more dominant" in English vs Spanish.

### Spanish ###

if (TRUE) {
  png(here("figs","p_rq2_spanish_smooths.png"), width = 15, height = 15, units = "in", res = 300)
  
  plot_smooths_dominance(gam_rq2_es_ar1,
                         -75,
                         0,
                         75,
                         c(-6.5,-1.5))
  dev.off()
}

### English ###

if (TRUE) {
  
  png(here("figs","p_rq2_english_smooths.png"), width = 15, height = 15, units = "in", res = 300)
  
  plot_smooths_dominance(gam_rq2_en_ar1,
                         -75,
                         0,
                         75,
                         c(-9,-3))
  dev.off()
}




