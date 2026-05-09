source(here::here("scripts","r","00_libs.R"))
source(here::here("scripts","r","01_helpers.R"))
source(here::here("scripts","r","02_load_data.R"))
source(here::here("scripts","r","05_plots.R"))


# Bilingual language profile descriptive stats

dominance_distribution <- dat_female_tidy %>%
  group_by(participant) %>%
  summarise(
    interviewee_final_blp_score = first(interviewee_final_blp_score)
  ) %>%
  ungroup()

################################################################################

# load interviewer/interviewee metadata 
metadata <- read_csv(here("data","metadata.csv"))

# find who has mexican origins
birth_country_summary <- metadata %>%
  select(
    interviewee_sex,
    interviewee_participant_code,
    interviewee_birth_country,
    interviewee_mother_birth_country,
    interviewee_father_birth_country,
    interviewee_grandparent_birthplace_01,
    interviewee_grandparent_birthplace_02,
    interviewee_grandparent_birthplace_03,
    interviewee_grandparent_birthplace_04,
  ) %>%
  filter(interviewee_sex == "female") %>%
  summarize(
    n_mexican_origin = sum(
      if_any(
        c(
          interviewee_birth_country,
          interviewee_mother_birth_country,
          interviewee_father_birth_country,
          interviewee_grandparent_birthplace_01,
          interviewee_grandparent_birthplace_02,
          interviewee_grandparent_birthplace_03,
          interviewee_grandparent_birthplace_04,
        ),
        # include specific cities because some people did not put "mexico",
        # but instead a city/region in mexico
        ~ grepl("^mexico$|^méxico$|^sinaloa$|^jalisco$", ., ignore.case = TRUE)
      )
    )
  ) %>%
  as.data.frame()

# calculate raw count & percentage

mex_origin <- metadata %>% 
  filter(interviewee_sex == "female") %>% 
  summarise(
    n_mexican_origin = birth_country_summary[1,1],
    n_female = n(),
    percent_mexican_origin = n_mexican_origin / n_female
  ) %>%
  as.data.frame()

num_interviewees <- dat_female_tidy %>%
  summarize(
    n_participants = n_distinct(participant)
  ) %>% as.data.frame()

# GAMs outputs
# produces the following dfs:
## [model_name]_param_df
## [model_name]_smooth_df
## [model_name]_summary_df

model_summary_table(gam_rq1_ar1)

model_summary_table(gam_rq2_en_ar1)

model_summary_table(gam_rq2_es_ar1)

###############################################################################

### Find where estimates converge for gam_rq2_en/es_ar1
# You have to run 05_plots.R to get the necessary dfs

# English

find_dominance_convergence(gam_rq2_en_ar1_dominance_effect_df)

# Spanish

find_dominance_convergence(gam_rq2_es_ar1_dominance_effect_df)
