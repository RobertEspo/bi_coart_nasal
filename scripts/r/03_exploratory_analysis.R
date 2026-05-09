# What does A1-P0 look like in our sample across language?

dat_female_tidy %>%
  ggplot(aes(x = a1p0)) +
  geom_boxplot()

dat_female_tidy %>%
  ggplot(aes(x = a1p0)) +
  geom_histogram()

# What about A1-P0 within language?

dat_female_tidy %>%
  ggplot(aes(x = a1p0, color = language)) +
  geom_boxplot()

dat_female_tidy %>%
  ggplot(aes(x = a1p0, fill = language)) +
  geom_histogram()

# What about the timecourse over the vowel across language?

dat_female_tidy %>%
  ggplot(aes(
    x = point_vwlpct,
    y = a1p0
  )) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = TRUE
  ) +
  labs(
    x = "Vowel %",
    y = "A1-P0"
  ) +
  ds4ling::ds4ling_bw_theme(base_size = 12)

# Time course within language?

dat_female_tidy %>%
  ggplot(aes(
    x = point_vwlpct,
    y = a1p0,
    color = language
  )) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 7),
    se = TRUE
  ) +
  labs(
    x = "Vowel %",
    y = "A1-P0"
  ) +
  ds4ling::ds4ling_bw_theme(base_size = 12)

# And what if we distinguish the nasal consonant, /n/ vs /m/?

dat_female_tidy %>%
  ggplot(aes(
    x = point_vwlpct,
    y = a1p0,
    color = following_segment,
    linetype = language
  )) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 7),
    se = TRUE
  ) +
  labs(
    x = "Vowel %",
    y = "A1-P0"
  ) +
  ds4ling::ds4ling_bw_theme(base_size = 12)

# Now let's look at possible effects of dominance

# first, let's see the distribution of dominance scores
dat_female_tidy %>%
  distinct(participant, interviewee_final_blp_score) %>%
  ggplot(aes(x = "", y = interviewee_final_blp_score)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, alpha = .4) +
  scale_y_continuous(breaks = seq(-218, 218, by = 25))

### Does dominance predict average a1p0? ###
### First in English ###

dat_female_tidy %>% filter(language == "English") %>%
  group_by(participant) %>%
  summarize(
    mean_a1p0 = mean(a1p0),
    interviewee_final_blp_score = first(interviewee_final_blp_score)
  ) %>%
  ggplot(.,
         aes(x = interviewee_final_blp_score, y = mean_a1p0)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# Calculate pearson's coef

corr_dat <- dat_female_tidy %>% filter(language == "English") %>%
  group_by(participant) %>%
  summarize(
    mean_a1p0 = mean(a1p0),
    interviewee_final_blp_score = first(interviewee_final_blp_score)
  )

cor(corr_dat$mean_a1p0, corr_dat$interviewee_final_blp_score)

### now in Spanish ###

dat_female_tidy %>% filter(language == "Spanish") %>%
  group_by(participant) %>%
  summarize(
    mean_a1p0 = mean(a1p0),
    interviewee_final_blp_score = first(interviewee_final_blp_score)
  ) %>%
  ggplot(.,
         aes(x = interviewee_final_blp_score, y = mean_a1p0)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# Calculate pearson's coef

corr_dat <- dat_female_tidy %>% filter(language == "Spanish") %>%
  group_by(participant) %>%
  summarize(
    mean_a1p0 = mean(a1p0),
    interviewee_final_blp_score = first(interviewee_final_blp_score)
  )

cor(corr_dat$mean_a1p0, corr_dat$interviewee_final_blp_score)

# Let's take a look at the timecourse now
# here, we have to categorize scores
# so we'll z score dominance
# spanish-dom   > balanced  > english-dom
# -0.5          >           < 0.5

# First let's look at English

dat_female_tidy %>% filter(language == "English") %>%
  mutate(
    dominance = cut(
      interviewee_final_blp_score,
      breaks = c(-218, -72.6667, 72.6667, 218),
      labels = c("Spanish dominant", "Balanced", "English dominant"),
      include.lowest = TRUE
    )
  ) %>%
  ggplot(aes(
    x = point_vwlpct,
    y = a1p0,
    color = dominance,
    fill = dominance
  )) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = TRUE
  ) +
  facet_wrap(~ syllable) +
  labs(
    x = "Vowel %",
    y = "A1-P0"
  ) +
  ds4ling::ds4ling_bw_theme(base_size = 12)

# Now let's look at Spanish

dat_female_tidy %>% filter(language == "Spanish") %>%
  mutate(
    dominance = cut(
      interviewee_final_blp_score,
      breaks = c(-218, -72.6667, 72.6667, 218),
      labels = c("Spanish dominant", "Balanced", "English dominant"),
      include.lowest = TRUE
    )
  ) %>%
  ggplot(aes(
    x = point_vwlpct,
    y = a1p0,
    color = dominance,
    fill = dominance
  )) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = TRUE
  ) +
  facet_wrap(~ syllable) +
  labs(
    x = "Vowel %",
    y = "A1-P0"
  ) +
  ds4ling::ds4ling_bw_theme(base_size = 12)
