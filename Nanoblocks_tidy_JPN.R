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
#nanoblock_raw <- 
nanoblock_raw <- nanoblock_raw |> 
  mutate(difficulty_level = str_replace_na(difficulty_level, "None"),
         difficulty_level = str(difficulty_level))

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
# Open de dataset in tibble form.
rakuten_raw <- read_csv("JPN Files/rakuten_raw_JPN.csv")

rakuten_raw |> 
  glimpse()

rakuten_raw |> 
  view()

# Filter repeated rows.
rakuten_raw <- rakuten_raw |> 
  group_by(nanoblock_item_number) |> 
  filter(row_number() < 2)

## Parsing item number column ---------------------------------------------
# Standardize the item number column.
rakuten_raw <- rakuten_raw |>  
  mutate(nanoblock_item_number = str_trim(nanoblock_item_number, "right"),
         nanoblock_item_number = str_replace(nanoblock_item_number, "-", "_"))

## Parsing price ----------------------------------------------------------
rakuten_raw <- rakuten_raw |> 
  mutate(rakuten_price = str_remove(rakuten_price, "円"),
         rakuten_price = str_trim(rakuten_price, "both"),
         rakuten_price = parse_number(rakuten_price))

## Parsing points ---------------------------------------------------------
rakuten_raw <- rakuten_raw |> 
  mutate(rakuten_points = str_remove(rakuten_points, "ポイント"),
         rakuten_points = str_trim(rakuten_points, "both"),
         rakuten_points = parse_integer(rakuten_points))

## Parsing weight ---------------------------------------------------------
# Although weigh is not included as a field, it can be extracted from the
# description. ◆重量(g)：132 ◆
rakuten_raw <- rakuten_raw |> 
  mutate(rakuten_weight_g = str_extract(rakuten_item_description, "◆重量\\(g\\)：.*"),
         rakuten_weight_g = str_remove(rakuten_weight_g, "◆重量\\(g\\)："),
         rakuten_weight_g = parse_number(rakuten_weight_g))

# Writing the clean version -----------------------------------------------
rakuten_clean <- rakuten_raw |> 
  select(nanoblock_item_number, rakuten_price, rakuten_points, rakuten_weight_g)

write_csv(rakuten_clean, "JPN Files/rakuten_clean_JPN.csv")