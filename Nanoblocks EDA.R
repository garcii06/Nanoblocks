# Libraries ---------------------------------------------------------------
# Descriptive statistics
library(psych)
# Working with dates if needed
library(lubridate)
# Data cleaning and preparation
library(tidyverse)

# Importing data set ------------------------------------------------------
nanoblocks <- read_csv("Nanoblocks_clean.csv")

nanoblocks %>% 
  view()

# Adding columns ----------------------------------------------------------
# Get the total pieces in a box by multiplying the products in the box by the pieces
# for a single item.
nanoblocks <- nanoblocks %>% 
  mutate(total_pieces = case_when(!is.na(pieces_box) ~ pieces_box * pieces_single,
         is.na(pieces_box) ~ pieces_single))

# Tool kit or no ----------------------------------------------------------
# Filter all the non building products.
# First check if all the kits are the ones with NA pieces.
# Looking into the data, not all the entries are tools. Few like ミニナノ ミッフィー
# are a set but it doesn't have pieces information. 
nanoblocks %>% 
  filter(is.na(total_pieces)) %>% 
  view()

nanoblocks_build <- nanoblocks %>% 
  filter(!is.na(total_pieces))

# Descriptive statistics --------------------------------------------------
# The only variables that can be "described" with statistics are the age, level,
# and the total of pieces.
# target_age: 313 observations, standard deviation of 0 can be interpreted as
# all the entries are 12, can be confirmed with the range.
# difficulty_level: 263 observations, around 70% of the entries include a level.
# The mean is 2.67, trimmed mean is 2.55 and the median is 3, that means that 
# the majority of sets have a difficulty lower than 3.
# total_pieces: the outliers makes the mean and standard deviation higher,
# best central measurements are median or trimmed mean as they reflect better
# the mean and robust from the outliers.
# The standard deviation loses the meaning as it is so high that subtracting it
# from the mean to get the distribution will make that the 68% of the sets have
# between -200 pieces and 1000 pieces, and we can not have less than one piece.

# Total rows in the build data set are 313.
nanoblocks_build %>% 
  nrow()

nanoblocks_build %>% 
  select(-c(release_date_japan, package_size_mm, built_up_size_mm, pieces_single,
            pieces_box, name, item_number)) %>% 
  describe(na.rm = TRUE)
