# let's try a bayesian gam
# looking only at CVN syllables
# let's also use z scored BLP so I can actually make sense of the priors...

# what priors are we working with?

prior.list <- get_prior(a1p0 ~ 
                          interviewee_final_blp_score_z +
                          vwl_duration_z +
                          s(point_vwlpct) +
                          s(point_vwlpct, by = interviewee_final_blp_score_z, bs = "cr"),
                        data = sp_dat_tidy %>% filter(syllable == "CVN"))

# let's set some preliminary priors

priors_test_01 <- c(
  prior(normal(-4, 4), class = "Intercept"), # intercept; I expect values to be between 0 and -8.
  prior(normal(0, .3), class = b), # main effects; I expect the effect to be small
  prior(student_t(3, 0, 5.7), class = sds) # smoothing
  # per https://people.linguistics.mcgill.ca/~morgan/adv-quant-methods/bayesian_gamms.html
  # going to keep sds the same as given in get_prior()
)

# let's test those priors

b_test_01 <- brm(
  formula = a1p0 ~ 
    interviewee_final_blp_score_z +
    vwl_duration_z +
    s(point_vwlpct) +
    s(point_vwlpct, by = interviewee_final_blp_score_z, bs = "cr"),
  
  data = sp_dat_tidy %>% filter(syllable == "CVN"),
  prior = priors_test_01, 
  sample_prior = "only",
  file = here("models","b_test_01")
)

# visualize just the smooth
# remember that this gives two plots
# I think is the spline prior (ie sds)

conditional_smooths(b_test_01, spaghetti=T, ndraws=20)

# ok, so the first plot shows ~10 units of deviation
# that's a bit much, so gonna tighten sds prior
# the second plot is showing an insanely large y axis

# now let's visualize the predicted a1-p0 curves
# note that I'm varying data_grid for blp_z at the following values:
# -1, -0.5, 0, 0.5, 1

sp_dat_tidy %>% filter(syllable == "CVN") %>%
  data_grid(point_vwlpct = seq_range(point_vwlpct, n = 100),
            interviewee_final_blp_score_z = 0,
            vwl_duration_z = 0) %>%
  add_epred_draws(b_test_01, ndraws = 100) %>%
  ggplot(aes(x = point_vwlpct, y = interviewee_final_blp_score_z)) +
  geom_line(aes(y = .epred, group = .draw), alpha = .1)

# this is looking at the intercept, I think. The y axis range is a bit
# too large, but nothing insane.

# let's try again.

priors_test_02 <- c(
  prior(normal(-4, 2), class = "Intercept"), # intercept; let's tighten this.
  prior(normal(0, .15), class = b), # main effects. I'll tighten this as well.
  prior(student_t(3, 0, .05), class = sds) # smoothing. I think this is what is
  # causing the second plot of conditional_smooths() to be crazy, so we'll
  # tighten this by a lot...
)

# let's test priors 02! 

b_test_02 <- brm(
  formula = a1p0 ~ 
    interviewee_final_blp_score_z +
    vwl_duration_z +
    s(point_vwlpct) +
    s(point_vwlpct, by = interviewee_final_blp_score_z, bs = "cr"),
  
  data = sp_dat_tidy %>% filter(syllable == "CVN"),
  prior = priors_test_02, 
  sample_prior = "only",
  file = here("models","b_test_02")
)

# Let's plot those priors!

conditional_smooths(b_test_02, spaghetti=T, ndraws=20)

# Oh, that's way too restrictive!

sp_dat_tidy %>% filter(syllable == "CVN") %>%
  data_grid(point_vwlpct = seq_range(point_vwlpct, n = 100),
            interviewee_final_blp_score_z = 0,
            vwl_duration_z = 0) %>%
  add_epred_draws(b_test_02, ndraws = 100) %>%
  ggplot(aes(x = point_vwlpct, y = interviewee_final_blp_score_z)) +
  geom_line(aes(y = .epred, group = .draw), alpha = .1)

# These are not wiggly at all! Way too restrictive!

# Let's go for round 3 of priors

priors_test_03 <- c(
  prior(normal(-4, 2), class = "Intercept"), # intercept.
  prior(normal(0, .15), class = b), # main effects. 
  prior(student_t(3, 0, .5), class = sds) # smoothing. ok, let's widen it a bit again!
)

b_test_03 <- brm(
  formula = a1p0 ~ 
    interviewee_final_blp_score_z +
    vwl_duration_z +
    s(point_vwlpct) +
    s(point_vwlpct, by = interviewee_final_blp_score_z, bs = "cr"),
  
  data = sp_dat_tidy %>% filter(syllable == "CVN"),
  prior = priors_test_03, 
  sample_prior = "only",
  file = here("models","b_test_03")
)

conditional_smooths(b_test_03, spaghetti=T, ndraws=20)

# This still seems too restrictive.

sp_dat_tidy %>% filter(syllable == "CVN") %>%
  data_grid(point_vwlpct = seq_range(point_vwlpct, n = 100),
            interviewee_final_blp_score_z = -.5,
            vwl_duration_z = 0) %>%
  add_epred_draws(b_test_03, ndraws = 100) %>%
  ggplot(aes(x = point_vwlpct, y = interviewee_final_blp_score_z)) +
  geom_line(aes(y = .epred, group = .draw), alpha = .1)

# Round 4's the charm! 

priors_test_04 <- c(
  prior(normal(-4, 2), class = "Intercept"), # intercept.
  prior(normal(0, .15), class = b), # main effects. 
  prior(student_t(3, 0, 1), class = sds) # smoothing. let's widen it more.
)

# I'm not sure what a large linear effect would look like, but the current
# prior on 'b' allows for a change of 1.5 (vowel percent range = 0 to 100,
# so .15 * 100 = 1.5).

b_test_04 <- brm(
  formula = a1p0 ~ 
    interviewee_final_blp_score_z +
    vwl_duration_z +
    s(point_vwlpct) +
    s(point_vwlpct, by = interviewee_final_blp_score_z, bs = "cr"),
  
  data = sp_dat_tidy %>% filter(syllable == "CVN"),
  prior = priors_test_04, 
  sample_prior = "only",
  file = here("models","b_test_04")
)

conditional_smooths(b_test_04, spaghetti=T, ndraws=100)

# okay, i think this seems better...
# The predictions for blp = -0.95 & 1.02 still seem kind of large to me...
# but maybe that's just because they're at the tails and the tails are heavy?

sp_dat_tidy %>% filter(syllable == "CVN") %>%
  data_grid(point_vwlpct = seq_range(point_vwlpct, n = 50),
            interviewee_final_blp_score_z = .5,
            vwl_duration_z = 0) %>%
  add_epred_draws(b_test_04, ndraws = 50) %>%
  ggplot(aes(x = point_vwlpct, y = interviewee_final_blp_score_z)) +
  geom_line(aes(y = .epred, group = .draw), alpha = .1)

# this seems better to me, but
# at the tails (e.g., blp = 1, -1), it's allowing kind of crazy values still...
# but alas, we're going to take it and see how the model does. I've wasted
# too much time on these priors as it is.

# for the hell of it, let's fit a model with standardized a1p0.

priors_test_05 <- c(
  prior(normal(0, .3), class = "Intercept"), # intercept.
  prior(normal(0, .3), class = b), # main effects. 
  prior(student_t(3, 0, .25), class = sds) # smoothing. let's widen it more.
)

b_test_05 <- brm(
  formula = a1p0_z ~ 
    interviewee_final_blp_score_z +
    vwl_duration_z +
    s(point_vwlpct) +
    s(point_vwlpct, by = interviewee_final_blp_score_z, bs = "cr"),
  
  data = sp_dat_tidy %>% filter(syllable == "CVN"),
  prior = priors_test_04, 
  sample_prior = "only",
  file = here("models","b_test_05")
)

conditional_smooths(b_test_05, spaghetti=T, ndraws=100)

sp_dat_tidy %>% filter(syllable == "CVN") %>%
  data_grid(point_vwlpct = seq_range(point_vwlpct, n = 100),
            interviewee_final_blp_score_z = .5,
            vwl_duration_z = 0) %>%
  add_epred_draws(b_test_05, ndraws = 100) %>%
  ggplot(aes(x = point_vwlpct, y = interviewee_final_blp_score_z)) +
  geom_line(aes(y = .epred, group = .draw), alpha = .1)

# Ok, so let's start off with a null model.
# Let's just use standardized a1p0 and see how that goes.

priors_null <- c(
  prior(normal(0, 3), class = "Intercept")
)

b_gam_00 <- brm(
  formula = a1p0_z ~ 1,
  data = sp_dat_tidy %>% filter(syllable == "CVN"),
  prior = priors_null, 
  warmup = 1000, iter = 2000, chains = 4, 
  cores = 4, 
  control = list(adapt_delta = 0.99, max_treedepth = 20),
  file = here("models", "b_gam_00")
)

# now let's fit the time smooth
# here i'll repeat the earlier priors

priors_test_05 <- c(
  prior(normal(0, .3), class = "Intercept"), # intercept.
  prior(normal(0, .3), class = b), # main effects. 
  prior(student_t(3, 0, .25), class = sds) # smoothing. let's widen it more.
)

b_gam_01 <- brm(
  formula = a1p0_z ~
    s(point_vwlpct, k = 5),
  data = sp_dat_tidy %>% filter(syllable == "CVN"),
  prior = priors_test_05, 
  warmup = 500, iter = 1000, chains = 4,
  cores = 4, 
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  file = here("models", "b_gam_01"),
  backend = "cmdstanr"
)

# There was 1 divergent transition with the above specifications
# so for the second model, i'll add warmups/iters
# We can check for non-linearity, which we know should be positive.
# The bulk of the distribution should be different than 0:

mcmc_plot(b_gam_01, variable="^sds", regex=TRUE, type="dens") + xlim(-.5,2.5)

plot(conditional_smooths(b_gam_01))

# Now we can add blp.
# I'm going to add standardized vowel duration since I don't want to run
# a third model (no time) and tamminga & zellou 2015 did it & found it
# to be meaningful, so I have motivation there.

b_gam_02 <- brm(
  formula = a1p0_z ~ 
    interviewee_final_blp_score_z +
    vwl_duration_z +
    s(point_vwlpct) +
    s(point_vwlpct, by = interviewee_final_blp_score_z, bs = "cr"),
  
  data = sp_dat_tidy %>% filter(syllable == "CVN"),
  prior = priors_test_05, 
  warmup = 1000, iter = 2000, chains = 4, 
  cores = 4, 
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  file = here("models", "b_gam_02"),
  backend = "cmdstanr"
)
