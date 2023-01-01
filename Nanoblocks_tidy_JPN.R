# Nanoblock cleaning.

# Libraries ---------------------------------------------------------------
# The following libraries where used to do the data cleaning easier.
# To install all the packages:
# install.packages(c("tidyverse"))
# Make tidy data. 
library(tidyverse)
# Work with dates.
library(lubridate)

# Nanoblock data cleaning -------------------------------------------------
# Open de dataset in tibble form.
nanoblock_raw <- read_csv("JPN Files/Nanoblock_raw_JPN.csv")

nanoblock_raw |> 
  glimpse()

nanoblock_raw |> 
  view()


## Parsing item number column ---------------------------------------------
# Standardize the item number column.
nanoblock_raw <- nanoblock_raw |>  
  mutate(item_number = str_trim(item_number, "right"),
         item_number = str_replace(item_number, "-", "_"))

## Parsing date column ----------------------------------------------------
# As the date column is a string, first trim and remove any additional character,
# then parse it as date.
nanoblock_raw <- nanoblock_raw |> 
  mutate(release_date_japan = str_trim(release_date_japan, "right"),
         release_date_japan = str_remove(release_date_japan, "予"),
         release_date_japan = ymd(release_date_japan))

## Parsing package size ---------------------------------------------------
# For this column, trim from both parts as observation tend to have special 
# characters at the start or end of the specification, the remove the "(mm)",
# finally, remove any white space between.
nanoblock_raw <- nanoblock_raw |> 
  mutate(package_size_mm = str_remove(package_size_mm, "\\(mm\\)"),
         package_size_mm = str_remove_all(package_size_mm, "mm"),
         package_size_mm = str_remove_all(package_size_mm, " "),
         package_size_mm = str_trim(package_size_mm, "both"))

## Parsing built size -----------------------------------------------------
# This column follow a similar process, but we need to extract the numbers in
# a special way.
nanoblock_raw <- nanoblock_raw |> 
  mutate(built_up_size_mm = str_remove(built_up_size_mm, "\\(mm\\)"),
         built_up_size_mm = str_remove_all(built_up_size_mm, "mm"),
         built_up_size_mm = str_remove(built_up_size_mm, "\\(.*"),
         built_up_size_mm = str_remove(built_up_size_mm, "\\（.*"),
         built_up_size_mm = str_remove_all(built_up_size_mm, " "),
         built_up_size_mm = str_extract(built_up_size_mm, "\\d*×\\d*×\\d*"),
         built_up_size_mm = str_trim(built_up_size_mm, "both"))

## Parsing pieces single --------------------------------------------------
nanoblock_raw <- nanoblock_raw |> 
  mutate(pieces_single = str_remove(pieces_single, "\\(.*"),
         pieces_single = str_remove(pieces_single, "\\（.*"),
         pieces_single = str_remove_all(pieces_single, " "),
         pieces_single = str_trim(pieces_single, "both"),
         pieces_single = parse_integer(pieces_single))

## Parsing pieces box -----------------------------------------------------
nanoblock_raw <- nanoblock_raw |> 
  mutate(pieces_box = str_remove(pieces_box, ".*BOX"),
         pieces_box = str_remove(pieces_box, "個"),
         pieces_box = str_trim(pieces_box, "both"),
         pieces_box = parse_integer(pieces_box))

## Parsing target age -----------------------------------------------------
# Seems that target age column is clean.

## Parsing level ----------------------------------------------------------
# TODO: Pass the column into a factor or character.

## Additional columns -----------------------------------------------------
# Get the total pieces assuming that all the items in a box set are of equal 
# number of pieces.
nanoblock_raw <- nanoblock_raw |> 
  mutate(total_pieces = if_else(!is.na(pieces_box), pieces_single * pieces_box, pieces_single))

# Writing the clean version -----------------------------------------------
nanoblock_clean <- nanoblock_raw |> 
  select(name, item_number, release_date_japan, package_size_mm, built_up_size_mm,
         pieces_single, pieces_box, total_pieces, target_age, difficulty_level,
         link_rakuten)

write_csv(nanoblock_clean, "JPN Files/Nanoblock_clean_JPN.csv")

# Rakuten data cleaning ---------------------------------------------------


