# dominance distribution in sample

dominance_distribution <- sp_dat_tidy %>%
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

ggsave(here("figs","dominance_distribution.png"), 
       plot = dominance_distribution, 
       width = 10, height = 15, dpi = 300)

# get dominance scores
blp_summary <- sp_dat_tidy %>%
  distinct(participant, interviewee_final_blp_score) %>%
  mutate(
    group = ntile(interviewee_final_blp_score, 3)
  ) %>%
  group_by(group) %>%
  summarise(
    mean_blp = mean(interviewee_final_blp_score),
    median_blp = median(interviewee_final_blp_score),
    min_blp = min(interviewee_final_blp_score),
    max_blp = max(interviewee_final_blp_score),
    n = n()
  )

# plot smooths based on scores above
plot_smooths_dominance(m_gam_ar1,
                       blp_summary[1,3] %>% pull(),
                       blp_summary[2,3] %>% pull(),
                       blp_summary[3,3] %>% pull(),
                       0)

# or define dominance as -75, 0, 75
plot_smooths_dominance(m_gam_ar1,
                       -75,
                       0,
                       75,
                       0)


plot_diff(
  m_gam_ar1,
  view = "point_vwlpct",
  
  comp = list(
    interviewee_final_blp_score = c(-100, 100)
  ),
  
  cond = list(
    vwl_duration_z = 0
  ),
  
  rm.ranef = FALSE
)
