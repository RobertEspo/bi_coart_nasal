# Relied heavily on Sóskuthy (2017) to write this code #

###############################################################################
### %%% RESEARCH QUESTION 1 %%% ###
### %%% Do we find language-specific patterns for A1-P0? %%% ###

### Should vowel duration be included in model? ###

# First, let's see if we should include vowel duration in the model.
# It seems likely that we should, because Tamanga & Zellou (2015) 
# found that it was significant as a linear predictor, but let's make sure.

gam_rq1_00 <- bam(
  a1p0 ~ 
    s(point_vwlpct, bs = "cr"),
    
  data = dat_female_tidy,
  method = "ML"
)

gam_rq1_01 <- bam(
  a1p0 ~ 
    vwl_duration_z +
    s(point_vwlpct, bs = "cr"),

  data = dat_female_tidy,
  method = "ML"
)

compareML(gam_rq1_00, gam_rq1_01)
summary(gam_rq1_01)

# There is evidence that vowel duration matters,
# so we will include it in the final model

### Should language be included in the model? ###

# now we fit the model with the difference smooth for language
# first, we have to set up the data

dat_female_tidy$language.ord <- as.ordered(dat_female_tidy$language)
contrasts(dat_female_tidy$language.ord) <- "contr.treatment"

gam_rq1_02 <- bam(
  a1p0 ~ 
    vwl_duration_z +
    language.ord +
    s(point_vwlpct, bs = "cr") +
    s(point_vwlpct, by = language.ord, bs = "cr"),
  
  data = dat_female_tidy,
  method = "ML"
)

# Now we can compare the models to see if language matters

compareML(gam_rq1_01, gam_rq1_02)
AIC(gam_rq1_00, gam_rq1_02)

# Now we can deal with autocorrelation by using an AR1 error model

rq1_ar1_dat <- dat_female_tidy %>%
  mutate(
    start_event = ifelse(timepoint == 1, TRUE, FALSE)
  )

# I'm paranoid that this doesn't stay contrast coded.
rq1_ar1_dat$language.ord <- as.ordered(rq1_ar1_dat$language)
contrasts(rq1_ar1_dat$language.ord) <- "contr.treatment"

# get estimate of corr between adjacent errors

rq1_r1 <- start_value_rho(gam_rq1_02)

# fit the model

gam_rq1_ar1 <- bam(
  a1p0 ~ 
    vwl_duration_z +
    language.ord +
    s(point_vwlpct, bs = "cr") +
    s(point_vwlpct, by = language.ord, bs = "cr"),
  
  data = rq1_ar1_dat,
  method = "ML",

  rho = rq1_r1,
  AR.start = rq1_ar1_dat$start_event
)

acf_resid(gam_rq1_ar1, split_pred="AR.start")
compareML(gam_rq1_02, gam_rq1_ar1)
summary(gam_rq1_ar1)

###############################################################################
### RESEARCH QUESTION 2 ###
# %%% PART 1: Do we find that BLP is significant for SPANISH? %%% #

# First, let's just check again about vowel duration

gam_rq2_es_00 <- bam(
  a1p0 ~ 
    s(point_vwlpct),
    
  data = dat_female_tidy %>% filter(language == "Spanish"),
  method = "ML"
)

gam_rq2_es_01 <- bam(
  a1p0 ~
    vwl_duration_z +
    s(point_vwlpct),
  
  data = dat_female_tidy %>% filter(language == "Spanish"),
  method = "ML"
)

compareML(gam_rq2_es_00, gam_rq2_es_01)
AIC(gam_rq2_es_00, gam_rq2_es_01)

# Ok, so when looking at just Spanish, duration doesn't matter.

gam_rq2_es_02 <- bam(
  a1p0 ~ 
    interviewee_final_blp_score +
    s(point_vwlpct) +
    ti(point_vwlpct, by = interviewee_final_blp_score, k=c(10,4)),
    
  data = dat_female_tidy %>% filter(language == "Spanish"),
  method = "ML"
)

# now we compare models with and without blp
compareML(gam_rq2_es_00, gam_rq2_es_02)
AIC(gam_rq2_es_00, gam_rq2_es_02)
summary(gam_rq2_es_02)

# okay, blp seems to matter.
# let's fit an ar1 error model for plotting it.

rq2_es_ar1_dat <- dat_female_tidy %>% filter(language == "Spanish") %>%
  mutate(
    start_event = ifelse(timepoint == 1, TRUE, FALSE)
  )

rq2_es_r1 <- start_value_rho(gam_rq2_es_02)

gam_rq2_es_ar1 <- bam(
  a1p0 ~ 
    interviewee_final_blp_score +
    s(point_vwlpct) +
    ti(point_vwlpct, by = interviewee_final_blp_score, k=c(10,4)),
  
  data = rq2_es_ar1_dat, # Spanish is already filtered
  method = "ML",
  
  rho = rq2_es_r1,
  AR.start = rq2_es_ar1_dat$start_event
)

acf_resid(gam_rq2_es_ar1, split_pred="AR.start")
compareML(gam_rq2_es_ar1, gam_rq2_es_02)
summary(gam_rq2_es_ar1)

# Okay, so when we use the AR1 model, BLP isn't significant.

# %%% PART 2: Do we find that BLP is significant for ENGLISH? %%% #

# Let's see if vowel duration matters for English

gam_rq2_en_00 <- bam(
  a1p0 ~ 
    s(point_vwlpct),
  
  data = dat_female_tidy %>% filter(language == "English"),
  method = "ML"
)

gam_rq2_en_01 <- bam(
  a1p0 ~
    vwl_duration_z +
    s(point_vwlpct),
  
  data = dat_female_tidy %>% filter(language == "English"),
  method = "ML"
)

compareML(gam_rq2_en_00, gam_rq2_en_01)
summary(gam_rq2_en_01)

# Duration matters for English, so we will include it.

gam_rq2_en_02 <- bam(
  a1p0 ~ 
    vwl_duration_z +
    interviewee_final_blp_score +
    s(point_vwlpct) +
    ti(point_vwlpct, by = interviewee_final_blp_score, bs = "cr", k=c(10,4)),
  
  data = dat_female_tidy %>% filter(language == "English"),
  method = "ML"
)

# now we compare models with and without blp
compareML(gam_rq2_en_01, gam_rq2_en_02)
summary(gam_rq2_en_02)

# blp doesn't seem to matter, just like spanish.
# let's fit an ar1 error model for plotting it.

rq2_en_ar1_dat <- dat_female_tidy %>% filter(language == "English") %>%
  mutate(
    start_event = ifelse(timepoint == 1, TRUE, FALSE)
  )

rq2_en_r1 <- start_value_rho(gam_rq2_en_02)

gam_rq2_en_ar1 <- bam(
  a1p0 ~ 
    vwl_duration_z +
    interviewee_final_blp_score +
    s(point_vwlpct) +
    ti(point_vwlpct, by = interviewee_final_blp_score, bs = "cr",k=c(10,4)),
  
  data = rq2_en_ar1_dat, # english is already filtered
  method = "ML",
  
  rho = rq2_en_r1,
  AR.start = rq2_en_ar1_dat$start_event
)

acf_resid(gam_rq2_en_ar1, split_pred="AR.start")
compareML(gam_rq2_en_02, gam_rq2_en_ar1)
summary(gam_rq2_en_ar1)

# Okay, now save all of the models that will be used for plotting

saveRDS(gam_rq1_ar1,
        file = here("models", "gam_rq1_ar1.rds"))

saveRDS(gam_rq2_es_ar1,
        file = here("models", "gam_rq2_es_ar1.rds"))

saveRDS(gam_rq2_en_ar1,
        file = here("models", "gam_rq2_en_ar1.rds"))




