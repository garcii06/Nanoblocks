# Libraries ---------------------------------------------------------------
# Working with dates
library(lubridate)
# Data cleaning and preparation
library(tidyverse)

# Importing dataset -------------------------------------------------------
nanoblocks <- read_csv("Nanoblocks_information.csv")


# Cleaning columns --------------------------------------------------------
# Char columns: name, item_number, package_size_mm, built_up_size_mm
# Date columns: release_date_japan
# Numeric cols: target_age, difficulty_level, pieces_single, pieces_box

# Possible changes: split the size* columns into separate fields.
# Although name is in a untidy form there is no intention to clean it.
# For anyone that wants to clean it:
# locale the names, all of them contains the Japanese name with romaji or Japanese.
# clean the Korean part and the series name, between many other things.

## item_number -------------------------------------------------------------
# Change the inconsistency of - _ marks.
nanoblocks$item_number <- nanoblocks$item_number %>% 
  str_replace_all("(.*)_(.*)", "\\1-\\2")

## release_date_japan ------------------------------------------------------
# Remove non numeric characters from the date and set it to a date column.
nanoblocks$release_date_japan <- nanoblocks$release_date_japan %>%
  str_replace("予", "") %>% 
  parse_date("%Y/%m/%d")

## package_size_mm  --------------------------------------------------------
# Remove the (mm) character and spaces between numbers
nanoblocks$package_size_mm <- nanoblocks$package_size_mm %>% 
  str_replace_all(" ", "") %>% 
  str_replace("\\(mm\\)", "")

## built_up_size_mm --------------------------------------------------------
# Although is the same format as package_size_mm, it has more inconsistencies
# that make harder the cleaning.
# Need to double check this columns as there might unclean data, or trimmed numbers.
nanoblocks$built_up_size_mm <- nanoblocks$built_up_size_mm %>% 
  str_extract(".*×.*(×| ).*") %>% 
  str_replace_all("\\(mm\\).*", "") %>% 
  str_replace_all("mm", "") %>% 
  str_trim(side = "right") %>% 
  str_replace("× ", "×") %>% 
  str_replace(" ", "×")

## pieces_single -----------------------------------------------------------
# Remove any character that is not a number.
nanoblocks$pieces_single <- nanoblocks$pieces_single %>% 
  str_extract("\\d{0,10}") %>% 
  parse_integer()

## pieces_box --------------------------------------------------------------
# The format for box sets is [the number of boxes] BOX [number of items in the box] [counter character]
# Only need is the [number of items in the box]
# Each box contains 6 items so I can hard-code but cleaning it using Tidyverse
# functions will ensure that it continues working.
nanoblocks$pieces_box <- nanoblocks$pieces_box %>% 
  str_extract("\\d個") %>% 
  str_remove("個") %>% 
  parse_integer()

## Others ------------------------------------------------------------------
# Other columns are already clean
nanoblocks$target_age
nanoblocks$difficulty_level

# Clean file ---------------------------------------------------------------
nanoblocks %>% 
  view()

# Writing the file --------------------------------------------------------
# Again write the file in the cleanest possible version.
# Programs like Excel will have trouble to read all the characters.
write_csv(nanoblocks, "Nanoblocks_clean.csv")
