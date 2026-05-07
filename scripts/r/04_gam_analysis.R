# let's run the first model and see what's going on
m_gam <- bam(
  a1p0 ~ 
    interviewee_final_blp_score +
    vwl_duration_z +
    s(point_vwlpct) +
    s(point_vwlpct, by = interviewee_final_blp_score, bs = "cr"),

  data = sp_dat_tidy %>% filter(previous_sonorant == 1),
  method = "fREML",
  discrete = TRUE
)

m_gam_01 <- bam(
  a1p0 ~ 
    interviewee_final_blp_score +
    vwl_duration_z +
    s(point_vwlpct) +
    s(point_vwlpct, by = interviewee_final_blp_score, bs = "cr"),
  
  data = sp_dat_tidy %>% filter(previous_sonorant == 0),
  method = "fREML",
  discrete = TRUE
)

m_gam_02 <- bam(
  a1p0 ~ 
    interviewee_final_blp_score +
    vwl_duration_z +
    s(point_vwlpct),

  data = sp_dat_tidy %>% filter(previous_sonorant == 1),
  method = "fREML",
  discrete = TRUE
)

m_gam_03 <- bam(
  a1p0 ~ 
    vwl_duration_z +
    s(point_vwlpct),

  data = sp_dat_tidy %>% filter(previous_sonorant == 1),
  method = "fREML",
  discrete = TRUE
)

m_gam_04 <- bam(
  a1p0 ~ 
    s(point_vwlpct),
  
  data = sp_dat_tidy %>% filter(previous_sonorant == 1),
  method = "fREML",
  discrete = TRUE
)

AIC(m_gam, m_gam_01, m_gam_02, m_gam_03, m_gam_04)

# sonorant model has smaller AIC, so performs better
# going to the use the sonorant model

# summary of the model
summary(m_gam)

# let's look at the residuals

acf_plot(resid(m_gam), split_by=list(sp_dat_tidy %>% filter(previous_sonorant == 1) %>% pull(participant)))

# pretty high autocorrelation at lag 1, so let's try to fix that
# let's try an AR1 model

ar1_dat <- sp_dat_tidy %>%
  filter(previous_sonorant == 1) %>%
  mutate(
    start_event = ifelse(timepoint == 1, TRUE, FALSE)
  )

# get estimate of corr between adjacent errors

r1 <- start_value_rho(m_gam)

# fit the model

m_gam_ar1 <- bam(
  a1p0 ~ 
    interviewee_final_blp_score +
    vwl_duration_z +
    s(point_vwlpct) +
    s(point_vwlpct, by = interviewee_final_blp_score, bs = "cr"),

  data = ar1_dat,
  method = "fREML",
  discrete = TRUE,
  
  rho = r1,
  AR.start = ar1_dat$start_event
)

# first, let's compare the models

# model comparison

AIC(m_gam, m_gam_ar1)

# ok, m_gam_ar1 performs better, so let's go with that one.

summary(m_gam_ar1)

# let's look at the residuals first

acf_resid(m_gam_ar1, split_pred="AR.start")

# let's check if k is too low

gam.check(m_gam_ar1)

# let's plot using the ar1 model
# let's plot blp with the values from above

plot_smooths_dominance(m_gam_ar1,
                       blp_summary[1,3] %>% pull(),
                       blp_summary[2,3] %>% pull(),
                       blp_summary[3,3] %>% pull(),
                       0)

plot_smooths_dominance(m_gam_ar1,
                       -30,
                       0,
                       30,
                       0)
diff_smooths(
  m_gam_ar1,
  
)