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

num_interviewees <- sp_dat_tidy %>%
  summarize(
    n_participants = n_distinct(participant)
  ) %>% as.data.frame()