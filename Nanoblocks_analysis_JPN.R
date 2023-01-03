# Libraries ---------------------------------------------------------------
library(skimr)
library(infer)
library(tidyverse)
library(lubridate)

# Business Questions ------------------------------------------------------
# As with most analysis, questions need to be prioritized.
# Without any question, the value of our analysis and insights is mostly zero.
# 1. Predict the price according the the number of pieces in a set and/or other
# variables.
# 2. Is there any significant difference in the median price between items with
# level and those without it?
# 3. Is there any significant difference in the median price between a set and
# individual items?

# Load data ---------------------------------------------------------------
nanoblock <- read_csv("JPN Files/nanoblock_clean_JPN.csv")
rakuten <- read_csv("JPN Files/rakuten_clean_JPN.csv")

# Joining the data
nano_raku <- nanoblock |> 
  left_join(rakuten, by = c("item_number" = "nanoblock_item_number"))

# The first two "additional" transformations will help in the analysis process
# as there is no current column that let us know if it is a set box or individual
# item, and if it has a level.
nano_raku <- nano_raku |> 
  mutate(is_set = case_when(is.na(pieces_box) ~ 0,
                            !is.na(pieces_box) ~ 1),
         has_level = case_when(difficulty_level == "None" ~ 0,
                               difficulty_level != "None" ~ 1),
         is_set = factor(is_set),
         has_level = factor(has_level))

# View the data -----------------------------------------------------------
# View the whole dataset
nano_raku |> 
  view()

# Another way to view, just a part of the dataset, is using glimpes that gives
# a clean view of the datatype columns. Using head or sample_n to view a random
# sample of entries.
nano_raku |> 
  glimpse()

nano_raku |> 
  sample_n(size = 6) |> 
  view()

# Summary statistics ------------------------------------------------------
## Univariate plots -------------------------------------------------------
nano_raku |> 
  ggplot(aes(x = total_pieces)) +
  geom_histogram(color = "white") +
  labs(x = "Total pieces in the set",
       y = "Total sets",
       title = "Distribution of pieces")

nano_raku |> 
  ggplot(aes(x = rakuten_price)) +
  geom_histogram(color = "white") +
  labs(x = "price (¥)",
       y = "Total sets",
       title = "Distribution of sets by price")

# Even if we transform the price to a logarithm scale, the shape is not in the
# form of a normal distribution.
nano_raku |> 
  ggplot(aes(x = log10(rakuten_price))) +
  geom_histogram(binwidth = 0.1, color = "white") + 
  labs(x = "price (¥) log10",
       y = "Total sets",
       title = "Distribution of sets by price")

nano_raku |> 
  ggplot(aes(x = difficulty_level)) +
  geom_bar() +
  labs(x = "Official difficulty level",
       y = "Total sets",
       title = "Distribution of sets by level")
  
nano_raku |> 
  ggplot(aes(x = rakuten_weight_g)) +
  geom_histogram() +
  labs(x = "Weight (g)",
       y = "Total sets",
       title = "Distribution of sets by weight")

nano_raku |> 
  group_by(year(release_date_japan)) |>
  filter(year(release_date_japan) < 2023) |> 
  summarise(total_sets = n(),
            year = year(release_date_japan)) |> 
  ggplot(aes(x = year, y = total_sets)) +
  geom_line() +
  labs(x = "Year",
       y = "Total sets",
       title = "Sets released in japan before 2023")

## Bivariate plots --------------------------------------------------------
nano_raku |> 
  ggplot(aes(x = difficulty_level, y = rakuten_price)) +
  geom_boxplot() +
  labs(x = "Official difficulty level",
       y = "Price (¥)")

nano_raku |> 
  ggplot(aes(x = has_level, y = rakuten_price)) +
  geom_boxplot(aes(group = has_level)) +
  labs(x = "Has difficulty level assigned?",
       y = "Price (¥)")

nano_raku |> 
  ggplot(aes(x = is_set, y = rakuten_price)) +
  geom_boxplot(aes(group = is_set)) +
  labs(x = "Set box or indiviual product?",
       y = "Price (¥)")

nano_raku |> 
  ggplot(aes(x = rakuten_weight_g, y = rakuten_price, color = difficulty_level)) +
  geom_jitter() +
  labs(x = "Weight (g)",
       y = "Price (¥)",
       title = "Relation between the total weight and price")

nano_raku |> 
  ggplot(aes(x = log10(total_pieces), y = log10(rakuten_price), color = difficulty_level)) +
  geom_jitter() +
  labs(x = "Pieces log10",
       y = "Price (¥) log10",
       title = "Relation between the total of pieces and price")

nano_raku |> 
  ggplot(aes(x = rakuten_points, y = rakuten_price, color = difficulty_level)) +
  geom_jitter() +
  labs(x = "Reward points",
       y = "Price (¥)",
       title = "Relation between the points and price")

# Analysis ----------------------------------------------------------------
# To do the analysis, first I reduce the dataset with only the variables of interest.
nano_raku_short <- nano_raku |> 
  select(total_pieces, difficulty_level, rakuten_price, rakuten_points, rakuten_weight_g, has_level, is_set) |> 
  mutate(total_pieces_log10 = log10(total_pieces),
         rakuten_price_log10 = log10(rakuten_price))

# The purpose is to identify and predict prices, so any item without a price
# or with a price of zero has no use.
nano_raku_short <-  nano_raku_short |> 
  filter(!is.na(total_pieces), rakuten_price != 0)

# Look again into the variables distributions.
nano_raku_short |> 
  skim()

# Matrix correlation between continuous variables.
# We can see in the matrix of correlation and using ggpairs that all of the 
# continuous variables have a positive relation with the price variable.
nano_raku_short |> 
  select(rakuten_price_log10, total_pieces_log10, rakuten_weight_g, rakuten_points) |> 
  cor()

nano_raku_short |> 
  GGally::ggpairs()


# 1. Question -------------------------------------------------------------
# Predict the price according the the number of pieces and/or other variables.
# To address this question, I will use the linear regression with to formulas
# to compare how they get affected by the explanatory variables.
# a. price = total_pieces + difficulty level.
# b. price = total_pieces + is_set.

## a. ---------------------------------------------------------------------
# With the linear regression, we can now see that only two difficulty levels
# have a negative relationship (None and difficulty 1).
nano_raku_short |> 
  ggplot(aes(x = total_pieces_log10, y = rakuten_price_log10, color = difficulty_level)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Nmuber of pieces log10",
       y = "Price (¥) log10",
       colour = "Difficulty", 
       title = "Relation between the total number of pieces and price")

model_interaction_difficulty <- lm(rakuten_price_log10 ~ difficulty_level * total_pieces_log10, data = nano_raku_short)

moderndive::get_regression_table(model_interaction_difficulty)

## b. ---------------------------------------------------------------------
nano_raku_short |> 
  ggplot(aes(x = total_pieces_log10, y = rakuten_price_log10, color = is_set)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Nmuber of pieces log10",
       y = "Price (¥) log10",
       colour = "Is set?",
       title = "Relation between the total number of pieces and price")

model_interaction_set <- lm(rakuten_price_log10 ~ is_set * total_pieces_log10, data = nano_raku_short)
moderndive::get_regression_table(model_interaction_set)

# 2. Question -------------------------------------------------------------
# 3. Question -------------------------------------------------------------
# I wanted to address both of these questions when I first looked into the catalog,
# but the population for both of this categories is to low that there is almost
# or completely no overlapping to compare.
# If it was only a sample then we could make some bootstrapping or permutations,
# but considering that we have the population then we got all the information
# available like the population proportion:
# 31 / 277 = 11.19% is a set
# 49 / 259 = 18.91% has a level
nano_raku_short |> 
  select(is_set, has_level) |> 
  skim()

nano_raku_short |> 
  select(is_set, rakuten_price) |> 
  group_by(is_set) |> 
  summarise(total_items = n(),
            avg_price = mean(rakuten_price))

nano_raku_short |> 
  select(has_level, rakuten_price) |> 
  group_by(has_level) |> 
  summarise(total_items = n(),
            avg_price = mean(rakuten_price))