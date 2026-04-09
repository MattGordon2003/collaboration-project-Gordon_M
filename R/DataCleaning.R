pacman::p_load(tidyverse, tidymodels, skimr, readxl, here, dplyr, janitor)

data <- readxl::read_excel(here("raw-data", "FITFR-final-data-v3.xlsx"))

# AF and PR = outcome variables
# age, sex, treatment and genetics = predictors

# CLEANING #

# correct row names
data <- data |>
  row_to_names(row_number = 1)

# standardise sex column
data <- data |>
  mutate(
    sex = case_when(
      str_detect(sex, "M") ~ "male",
      str_detect(sex, "Male") ~ "male",
      str_detect(sex, "Female") ~ "female",
      TRUE ~ sex)
  )

# remove last 4 useless columns
data <- data |>
  slice(1:(n() - 4))

# remove illogical ages
data <- data |>
  mutate(age = na_if(age, "-99"))

# standardise treatment column
data <- data |>
  mutate(
    treatment = case_when(
      str_detect(treatment, "a") ~ "A",
      str_detect(treatment, "b") ~ "B",
      str_detect(treatment, "c") ~ "C",
      TRUE ~ treatment)
  )

# make yes/no response binary
data <- data |>
  mutate(
    PR = case_when(
      PR == "yes" ~ 1,
      PR == "no" ~ 0,
      TRUE ~ NA_real_)
)

# convert to numeric variables
data <- data |>
  mutate(across(c(age, ID, AF), as.numeric))

# convert to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

