###############################################################################

### Load libs ###

source(here::here("scripts","r","00_libs.R"))

# I unfortunately can't upload my raw data to github because the files are
# too large, so all of the following is wrapped in if(FALSE){} so that it
# doesn't run. 
# Only the very bottom of the script runs, which loads in the tidied data
# from a csv & models.

if (FALSE) {

###############################################################################
### Load metadata ###

metadata <- read_csv(here("data","metadata.csv")) %>%
  filter(!interviewee_generation_immigration == "L2") %>% # remove L2 speakers
  transmute(participant = interviewee_participant_code,
            interviewee_final_blp_score = as.numeric(interviewee_final_blp_score), # convert BLP to numeric
            # this step introduces NAs
  ) %>%
  filter(!is.na(interviewee_final_blp_score)) # remove NAs

###############################################################################
### Load Spanish acoustic data ###

### load first spanish acoustic data file

dat_es_females_01_raw <- read_delim("D:/bi_coart_nasal_corpus_data/raw_dat_females_sp_01.txt")

# change necessary cols to numeric and
# remove NA values that result from --undefined-- values in numeric cols

non_numeric_cols <- c(
  "filename", "word", "vowel", "p0_id",
  "attempted_fix", "status", "errorflag", 
  "previous_segment", "following_segment"
)

numeric_cols <- setdiff(names(dat_es_females_01_raw), non_numeric_cols)

dat_es_females_01_clean <- dat_es_females_01_raw %>%
  mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(if_all(all_of(numeric_cols), ~ !is.na(.)))

# Clean up the environment!
rm(dat_es_females_01_raw, non_numeric_cols, numeric_cols)
gc()

### load second spanish acoustic data file

dat_es_females_02_raw <- read_delim('D:/bi_coart_nasal_corpus_data/raw_dat_females_sp_02.txt')

# change necessary cols to numeric and
# remove NA values that result from --undefined-- values in numeric cols

non_numeric_cols <- c(
  "filename", "word", "vowel", "p0_id",
  "attempted_fix", "status", "errorflag", 
  "previous_segment", "following_segment"
)

numeric_cols <- setdiff(names(dat_es_females_02_raw), non_numeric_cols)

dat_es_females_02_clean <- dat_es_females_02_raw %>%
  mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(if_all(all_of(numeric_cols), ~ !is.na(.)))

# Clean up the environment!
rm(dat_es_females_02_raw, non_numeric_cols, numeric_cols)
gc()

### Now bind the two dfs

raw_dat_females_es <- bind_rows(dat_es_females_01_clean, 
                                dat_es_females_02_clean)

# Clean up the environment!
rm(dat_es_females_01_clean, dat_es_females_02_clean)
gc()

###############################################################################
### Load English acoustic data ###

### load first english acoustic data file
dat_en_females_01_raw <- (read_delim('D:/bi_coart_nasal_corpus_data/raw_dat_females_en_01.txt'))

non_numeric_cols <- c(
  "filename", "word", "vowel", "p0_id",
  "attempted_fix", "status", "errorflag", 
  "previous_segment", "following_segment"
)

numeric_cols <- setdiff(names(dat_en_females_01_raw), non_numeric_cols)

dat_en_females_01_clean <- dat_en_females_01_raw %>%
  mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(if_all(all_of(numeric_cols), ~ !is.na(.)))

rm(dat_en_females_01_raw, non_numeric_cols, numeric_cols)
gc()


### load second english acoustic data file
### load first english acoustic data file
dat_en_females_02_raw <- (read_delim('D:/bi_coart_nasal_corpus_data/raw_dat_females_en_02.txt'))

non_numeric_cols <- c(
  "filename", "word", "vowel", "p0_id",
  "attempted_fix", "status", "errorflag", 
  "previous_segment", "following_segment"
)

numeric_cols <- setdiff(names(dat_en_females_02_raw), non_numeric_cols)

dat_en_females_02_clean <- dat_en_females_02_raw %>%
  mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(if_all(all_of(numeric_cols), ~ !is.na(.)))

rm(dat_en_females_02_raw, non_numeric_cols, numeric_cols)
gc()

### Now bind the two dfs

raw_dat_females_en <- bind_rows(dat_en_females_01_clean,
                                dat_en_females_02_clean)

rm(dat_en_females_01_clean, dat_en_females_02_clean)
gc()

###############################################################################
### Combine Spanish & English acoustic data

raw_dat_females <- bind_rows(
  raw_dat_females_es,
  raw_dat_females_en
)

rm(raw_dat_females_es, raw_dat_females_en)
gc()

###############################################################################
### This is the first pass for cleaning
### The resulting DF will be all tokens, before outliers are removed

# First we'll make lists of all the previous & following segments
# and classify them based on phonological factors e.g., nasal, sonorant.
# as.factor(raw_dat_females$following_segment) %>% levels()
# as.factor(raw_dat_females$previous_segment) %>% levels()

nasals <- c("M", "N", "NG", # english
                     "m", "n", "ɲ", "ŋ" ) # spanish

non_nasal_sonorants <- c("Y", "L", "R", "W", # english
                         "l", "r", "ɾ", "j", "w", "ʎ", "ʝ") # spanish

unvoiced_stop <- c("P","T","K", # english
                   "p","t","k") # spanish

vowels <- c(
  "AA1", "AA2", "AE0", "AE1", "AE2", "AH0", "AH1", "AO0", "AO1", "AO2", #english
  "AW0", "AW1", "AY0", "AY1", "AY2", "EH0", "EH1", "ER0", "ER1",
  "EY0", "EY1", "EY2", "IH0", "IH1", "IH2", "IY0", "IY1", "IY2",
  "OW0", "OW1", "OW2", "OY1", "UH1", "UW0", "UW1",
  "a", "e", "i", "o", "u") # spanish

dat_females_w_outliers <- raw_dat_females %>%
  select( # get only relevant cols (and maybe some ones that we don't really need)
    filename:vowel, a1p0, timepoint, point_time, 
    point_vwlpct, attempted_fix, errorflag, 
    previous_segment, following_segment,
    vwl_duration
  ) %>%
  mutate( # make some new cols for previous/following segment
    following_nasal = as.factor(if_else(following_segment %in% nasals,1,0)),
    previous_nasal = as.factor(if_else(previous_segment %in% nasals,1,0)),
    previous_sonorant = as.factor(if_else(previous_segment %in% non_nasal_sonorants,1,0)),
    # add cols for participant id & language
    participant = as.factor(str_remove(filename, "_[^_]+$")),
    language = as.factor(str_extract(filename, "[^_]+$")),

  # define syllables

    syllable = case_when(
      !(previous_segment %in% c(vowels,"boundary")) &
        previous_nasal == 0 &
        following_segment %in% nasals ~ "CVN",
      
      previous_segment %in% nasals &
        following_nasal == 0 &
        !(following_segment %in% c(vowels,"boundary")) ~ "NVC",
      
      previous_segment %in% nasals &
        following_segment %in% nasals ~ "NVN",
      
      !(previous_segment %in% c(vowels,"boundary")) &
        previous_nasal == 0 & following_nasal == 0 &
        !(following_segment %in% c(vowels,"boundary",nasals)) ~ "CVC",
      
      TRUE ~ NA_character_
    )) %>%
  
  # for the current study, we're looking only at CVN syllables with /m/ or /n/
  # and only when the C is an unvoiced stop
  filter(syllable == "CVN",
         following_segment %in% c("M","N", # english
                                  "m","n"), # spanish
         previous_segment %in% unvoiced_stop) %>% 
  left_join(metadata) %>% # add the metadata
  # some people don't have BLP scores
  # which means that NAs must be removed again
  filter(!is.na(interviewee_final_blp_score)) %>% 
  # now make sure everything is a factor as needed
  mutate(
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

###############################################################################
### Outlier removal procedure
# Following tamminga & zellou 2015:
# Remove any vowels that have a duration less than 50 ms
# Removes any points +/- 2.5 sd from the grand mean
# within each language, assuming that English & Spanish
# have different distributions for A1-P0

dat_female_tidy <- dat_females_w_outliers %>%
  filter(vwl_duration > 50) %>%
  group_by(language) %>%
  mutate(a1p0_z = scale(a1p0)[,1]) %>%
  ungroup() %>%
  # remove entire token if ANY point is an outlier
  group_by(language, filename, word) %>%
  mutate(token_valid = all(abs(a1p0_z) <= 2.5)) %>%
  ungroup() %>%
  filter(token_valid) %>%
  select(-token_valid) %>%
  # standardize vowel duration
  mutate(vwl_duration_z = scale(vwl_duration)[,1])

###############################################################################
### Some descriptive stats about the trimming procedure

outlier_summary <- full_join( 
  dat_females_w_outliers %>% count(language, name = "n_values_with") %>% 
    mutate(n_tokens_with = n_values_with / 10), dat_female_tidy %>% 
    count(language, name = "n_values_without") %>% 
    mutate(n_tokens_without = n_values_without / 10), by = "language" 
  ) %>% 
  mutate(proportion_retained = n_values_without / n_values_with) %>% 
  bind_rows(
    summarise( ., language = "Total", 
               n_values_with = sum(n_values_with, na.rm = TRUE),
               n_tokens_with = sum(n_tokens_with, na.rm = TRUE),
               n_values_without = sum(n_values_without, na.rm = TRUE),
               n_tokens_without = sum(n_tokens_without, na.rm = TRUE),
               proportion_retained = n_values_without / n_values_with) 
    )

# Clean up the environment!
rm(raw_dat_females, dat_females_w_outliers, 
   nasals, non_nasal_sonorants, 
   vowels, unvoiced_stop, metadata)
gc()

###############################################################################
### Write .csv file of tidied data

write.csv(dat_female_tidy, here("data","dat_female_tidy.csv"), row.names = FALSE)
write.csv(outlier_summary, here("data","outlier_summary.csv"), row.names = FALSE)

}

###############################################################################
### Load tidied data

if (TRUE) {

  # load in the tidied data
  # remember this is CVN syllables, where the C
  # is an unvoiced stop
  
  dat_female_tidy <- read_csv(here("data","dat_female_tidy.csv")) %>%
    # clean up col types
    mutate(
      filename = as.factor(filename),
      word = as.factor(word),
      previous_segment = as.factor(previous_segment),
      following_segment = as.factor(following_segment),
      following_nasal = as.factor(following_nasal),
      previous_nasal = as.factor(previous_nasal),
      participant = as.factor(participant),
      language = as.factor(language),
      syllable = as.factor(syllable))

  # load in info about outliers
  
  outlier_summary <- read_csv(here("data","outlier_summary.csv"))

  # load in models
  
  files <- list.files(here::here("models"),
                      pattern = "\\.rds$",
                      full.names = TRUE)
  
  models <- setNames(
    lapply(files, readRDS),
    tools::file_path_sans_ext(basename(files))
  )
  
  list2env(models, envir = .GlobalEnv)
  
  # Clean up the environment!
  rm(models, files)
  gc()

}
