source(here::here("scripts","r","00_libs.R"))

# Load metadata
metadata <- read_csv(here("data","metadata.csv")) %>%
  filter(!interviewee_generation_immigration == "L2") %>% # remove L2 speakers
  transmute(participant = interviewee_participant_code,
            interviewee_final_blp_score = as.numeric(interviewee_final_blp_score), # convert BLP to numeric
            # this step introduces NAs
  ) %>%
  filter(!is.na(interviewee_final_blp_score)) # remove NAs

# Load acoustic data

raw_dat_females_sp_01_raw <- read_delim(here("data","raw_dat_females_sp_01.txt"))

non_numeric_cols <- c(
  "filename", "word", "vowel", "p0_id",
  "attempted_fix", "status", "errorflag", 
  "previous_segment", "following_segment"
)

numeric_cols <- setdiff(names(raw_dat_females_sp_01_raw), non_numeric_cols)

raw_dat_females_sp_01_clean <- raw_dat_females_sp_01_raw %>%
  mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(if_all(all_of(numeric_cols), ~ !is.na(.)))

# Clean up the environment!
rm(raw_dat_females_sp_01_raw, non_numeric_cols, numeric_cols)
gc()

raw_dat_females_sp_02_raw <- (read_delim('D:/bi_coart_nasal_corpus_spanish_2/raw_dat_females_sp_02.txt'))

non_numeric_cols <- c(
  "filename", "word", "vowel", "p0_id",
  "attempted_fix", "status", "errorflag", 
  "previous_segment", "following_segment"
)

numeric_cols <- setdiff(names(raw_dat_females_sp_02_raw), non_numeric_cols)

raw_dat_females_sp_02_clean <- raw_dat_females_sp_02_raw %>%
  mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(if_all(all_of(numeric_cols), ~ !is.na(.)))

# Clean up the environment!
rm(raw_dat_females_sp_02_raw, non_numeric_cols, numeric_cols)
gc()

# raw_dat_females_sp_02 <- read_delim(here("data","raw_dat_females_sp_02.txt"))

raw_dat_females <- bind_rows(raw_dat_females_sp_01_clean, 
                             raw_dat_females_sp_02_clean)

# Clean up the environment!
rm(raw_dat_females_sp_01_clean, raw_dat_females_sp_02_clean)
gc()

# Tidy acoustic data
sp_dat_with_outliers <- raw_dat_females %>%
  select(
    filename:vowel, a1p0, timepoint, point_time, 
    point_vwlpct, attempted_fix, errorflag, 
    previous_segment, following_segment,
    vwl_duration
  ) %>%
  mutate(
    following_nasal = as.factor(if_else(following_segment %in% c("n","N","m","M", "NG","ñ", "ɲ", "ŋ"),1,0)),
    previous_nasal = as.factor(if_else(previous_segment %in% c("n","N","m","M", "NG","ñ", "ɲ", "ŋ"),1,0)),
    previous_sonorant = as.factor(if_else(previous_segment %in% c('l','r','w','ʎ','ɾ'),1,0)),
    participant = as.factor(str_remove(filename, "_[^_]+$")),
    language = as.factor(str_extract(filename, "[^_]+$"))
  ) %>%
  # define syllables
  mutate(
    # Only including /m n/ since those are the phonological nasal consonants
    # shared between Spanish & English
    # Maybe will look at velar separately since it's a phoneme in English,
    # but an allophone in Spanish
    
    syllable = case_when(
      !(previous_segment %in% c("i","e","a","o","u","boundary")) &
        previous_nasal == 0 &
        following_segment %in% c("n","m","N","M") ~ "CVN",
      
      previous_segment %in% c("n","m","N","M") &
        following_nasal == 0 &
        !(following_segment %in% c("i","e","a","o","u","boundary")) ~ "NVC",
      
      previous_segment %in% c("n","m","N","M") &
        following_segment %in% c("n","m","N","M") ~ "NVN",
      
      !(previous_segment %in% c("i","e","a","o","u","boundary")) &
        previous_nasal == 0 & following_nasal == 0 &
        !(following_segment %in% c("i","e","a","o","u","boundary","n","m","N","M")) ~ "CVC",
      
      TRUE ~ NA_character_
    )) %>%
  filter(syllable == "CVN",
         following_segment %in% c("m","n")) %>% # only CVN syllables with /n/ or /m/ consonants
  left_join(metadata) %>% # add the metadata
  filter(!is.na(interviewee_final_blp_score)) %>% # some people don't have BLP scores
  # which means that NAs must be removed again
  mutate(
    # standardize blp
    interviewee_final_blp_score_z = scale(interviewee_final_blp_score)[,1],
    interviewee_final_blp_score = interviewee_final_blp_score,
    # categorize using z scores
    # negative blp = more spanish-dominant
    dominance = factor(case_when(
      interviewee_final_blp_score_z <= -0.5 ~ "Spanish",
      interviewee_final_blp_score_z >= 0.5  ~ "English",
      TRUE ~ "Balanced"
    )),
    filename = as.factor(filename),
    word = as.factor(word),
    previous_segment = as.factor(previous_segment),
    following_segment = as.factor(following_segment),
    following_nasal = as.factor(following_nasal),
    previous_nasal = as.factor(previous_nasal),
    participant = as.factor(participant),
    language = as.factor(language),
    syllable = as.factor(syllable)
  ) %>%
  # now we remove any tokens that don't have 10 datapoints
  group_by(filename, word) %>% # create id col
  mutate(token_id = cur_group_id()) %>%
  ungroup() %>%
  group_by(token_id) %>% # count number of values per token
  mutate(n_timepoints = n()) %>%
  ungroup() %>%
  filter(n_timepoints == 10) %>%
  select(-n_timepoints)

# now we remove outliers
sp_dat_tidy <- sp_dat_with_outliers %>%
  # let's get rid of outliers
  # following tamminga & zellou 2015
  filter(vwl_duration > 50) %>% # vowels less than 50 ms will be removed
  mutate(a1p0_z = scale(a1p0)[,1]) %>%
  group_by(filename, word) %>%
  mutate(token_valid = all(abs(a1p0_z) <= 2.5)) %>%
  ungroup() %>%
  filter(token_valid) %>%
  select(-token_valid) %>%
  mutate(vwl_duration_z = scale(vwl_duration)[,1])
  
outlier_summary <- data.frame(
  n_with_outliers_values = nrow(sp_dat_with_outliers),
  n_with_outliers_tokens = nrow(sp_dat_with_outliers) / 10,
  
  n_without_outliers_values = nrow(sp_dat_tidy),
  n_without_outliers_tokens = nrow(sp_dat_tidy) / 10,
  
  proportion_retained = nrow(sp_dat_tidy) / nrow(sp_dat_with_outliers)
)

# Clean up the environment!
rm(metadata, raw_dat_females, sp_dat_with_outliers)
gc()
