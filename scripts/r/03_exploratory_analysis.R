# What does A1-P0 look like in our sample?

sp_dat_tidy %>%
  ggplot(aes(x = a1p0)) +
  geom_boxplot()

sp_dat_tidy %>%
  ggplot(aes(x = a1p0)) +
  geom_histogram()

# Take a look at different syllable types

sp_dat_tidy %>%
  ggplot(aes(x = a1p0, color = syllable)) +
  geom_boxplot()

sp_dat_tidy %>%
  ggplot(aes(x = a1p0, fill = syllable)) +
  geom_histogram()

# Let's take a look at the time course now for each syllable type

sp_dat_tidy %>%
  ggplot(aes(
    x = point_vwlpct,
    y = a1p0
  )) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = TRUE
  ) +
  facet_grid( ~ syllable) +
  labs(
    x = "Vowel %",
    y = "A1-P0"
  ) +
  ds4ling::ds4ling_bw_theme(base_size = 12)

# Okay, now let's move onto dominance (i.e., the BLP)
# BLP possible range = -218 to 218
# Negative values = more Spanish-dominant
# What's our distribution of BLP scores look like?

sp_dat_tidy %>%
  distinct(participant, interviewee_final_blp_score) %>%
  ggplot(aes(x = "", y = interviewee_final_blp_score)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, alpha = .4) +
  scale_y_continuous(breaks = seq(-218, 218, by = 25))

# Does dominance predict average a1p0?

sp_dat_tidy %>%
  group_by(participant) %>%
  summarize(
    mean_a1p0 = mean(a1p0),
    interviewee_final_blp_score = first(interviewee_final_blp_score)
  ) %>%
  ggplot(.,
         aes(x = interviewee_final_blp_score, y = mean_a1p0)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# Let's calculate the pearson correlation

corr_dat <- sp_dat_tidy %>%
  group_by(participant) %>%
  summarize(
    mean_a1p0 = mean(a1p0),
    interviewee_final_blp_score = first(interviewee_final_blp_score)
  )

cor(corr_dat$mean_a1p0, corr_dat$interviewee_final_blp_score)

# Let's go a bit more fine-grain and look at averages for each syllable type

sp_dat_tidy %>%
  group_by(participant, syllable) %>%
  summarize(
    mean_a1p0 = mean(a1p0),
    interviewee_final_blp_score = first(interviewee_final_blp_score),
    syllable = first(syllable)
  ) %>%
  ggplot(.,
         aes(x = interviewee_final_blp_score, y = mean_a1p0,
             color = syllable)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# Let's take a look at correlations for CVC and CVN

## CVC
corr_cvc_dat <- sp_dat_tidy %>%
  filter(syllable == 'CVC') %>%
  group_by(participant, syllable) %>%
  summarize(
    mean_a1p0 = mean(a1p0),
    interviewee_final_blp_score = first(interviewee_final_blp_score),
    syllable = first(syllable)
  )

cor(corr_cvc_dat$mean_a1p0, corr_cvc_dat$interviewee_final_blp_score)

## CVN

corr_cvc_dat <- sp_dat_tidy %>%
  filter(syllable == 'CVN') %>%
  group_by(participant, syllable) %>%
  summarize(
    mean_a1p0 = mean(a1p0),
    interviewee_final_blp_score = first(interviewee_final_blp_score),
    syllable = first(syllable)
  )

cor(corr_cvc_dat$mean_a1p0, corr_cvc_dat$interviewee_final_blp_score)

# Let's move away from single point values
# Moving onto changes over time
# First, let's look at different syllable types
# This is just a prettier version an above plot

sp_dat_tidy %>%
  ggplot(aes(
    x = point_vwlpct,
    y = a1p0,
    color = syllable
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

# Does dominance matter?
# Remember that "dominance" is z-scored BLP scores & then categorized
# such that Spanish ~ Balanced ~ English is defined as:
#           -0.5    >          < 0.5

sp_dat_tidy %>%
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

# Going back to just a1-p0, I wonder if the previous segment matters?

sp_dat_tidy %>%
  ggplot(aes(
    x = point_vwlpct,
    y = a1p0,
    color = previous_segment,
    fill = previous_segment
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

# and if we divide it by sonorant or not?

sp_dat_tidy %>%
  mutate(
    previous_sonorant = as.factor(ifelse(
      previous_segment %in% c('l','r','w','ʎ','ɾ'),1,0)
  )) %>%
  ggplot(aes(
    x = point_vwlpct,
    y = a1p0,
    color = previous_sonorant,
    fill = previous_sonorant
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
