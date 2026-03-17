library(tidyverse)
library(here)

nasality <- readr::read_tsv(
  here::here("data", "_nasalitylogv5.txt")
) %>%
  na.omit()
  

no_errors_nas <- nasality %>%
  filter(
    !vowel == "R"
  ) %>%
  mutate(
    nasal_following = ifelse(following_segment %in% c("n","m","N","M"),1,0),
    language = if_else(str_detect(filename, "English"), "English", "Spanish")
  )

onsets <- no_errors_nas %>%
  arrange(filename, word, point_time) %>%
  group_by(filename, word, vowel, nasal_following, language) %>%
  summarise(
    onset_pct = point_vwlpct[which.min(diff(a1p0)) + 1],
    .groups = "drop"
  )

ggplot(onsets, aes(onset_pct, fill = factor(nasal_following))) +
  geom_density(alpha = .4) +
  facet_wrap(~language)

onsets %>%
  group_by(language, nasal_following) %>%
  summarise(
    mean_onset = mean(onset_pct, na.rm = TRUE),
    sd_onset = sd(onset_pct, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(onsets,
       aes(factor(nasal_following), onset_pct, fill = language)) +
  geom_boxplot() +
  labs(
    x = "Following nasal (0 = no, 1 = yes)",
    y = "Nasality onset (% of vowel)"
  )

no_errors_nas %>%
  group_by(language, nasal_following, point_vwlpct) %>%
  summarise(a1p0 = mean(a1p0, na.rm = TRUE), .groups="drop") %>%
  ggplot(aes(point_vwlpct, a1p0, color = language)) +
  geom_line(size = 1) +
  facet_wrap(~nasal_following)
