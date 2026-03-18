require(ggplot2)
require(brms)
require(parallel)
require(tidybayes)
require(dplyr)
require(extraDistr)
require(HDInterval)
require(mgcv)
require(itsadug)
require(ggpubr)
library(here)
library(tidyverse)

# Define directories
tg_dir <- "D:/muhsic_corpus/public-selected/textgrid"
wav_dir <- "D:/bi_coart_nasal_corpus"

# List all TextGrids and WAVs
tg_files <- list.files(tg_dir, pattern = "\\.TextGrid$", full.names = TRUE)
wav_files <- list.files(wav_dir, pattern = "\\.wav$", full.names = TRUE)

# Extract base names without extensions
tg_names <- tools::file_path_sans_ext(basename(tg_files))
wav_names <- tools::file_path_sans_ext(basename(wav_files))

# Find matching TextGrids
matching_tgs <- tg_files[tg_names %in% wav_names]

# Copy matching TextGrids to wav_dir
file.copy(matching_tgs, wav_dir, overwrite = TRUE)
