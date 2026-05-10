pacman::p_load(tidyverse, tidymodels, skimr, readxl, here, dplyr, janitor, ggplot2)

data <- read_csv(here("data", "clean_data.csv"))

glimpse(data)

fit_bivariate <- function(x, y, df) {
  df <- df |>
    select(all_of(c(x, y))) |>
    na.omit()
  model_form <- reformulate(x, y)
  null_form <- formula(str_glue("{y} ~ 1"))
  full <- lm(model_form, df)
  null <- lm(null_form, df)
  pv <- anova(full, null, test = "LRT") |>
    broom::tidy() |>
    slice(2) |>
    pull(p.value)
  tibble(term = x, pv = pv)
}
fit_bivariates <- function(y, preds, df) {
  bivariates <-
    preds |>
    map(safely(\(x) fit_bivariate(x, y, df))) |>
    transpose()
  bivariates$result |>
    list_rbind() |>
    mutate(adj_p = p.adjust(pv, method = "fdr")) |>
    dplyr::filter(!is.na(adj_p)) |>
    mutate(outcome = y)
}

preds <- names(data)[2:(ncol(data) - 2)]
fits_pr <- fit_bivariates("PR", preds, data)
fits_af <- fit_bivariates("AF", preds, data)

# Primary relationship plots #

# AGE vs AF
ggplot(data, aes(x = age, y = AF)) +
  geom_point() +
  labs(title = "Age vs Activating Factor (AF)", x = "Age", y = "AF") +
  theme_minimal()

# AGE vs PR
ggplot(data, aes(x = age, y = PR, colour = factor(PR))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, colour = "black") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Age",
    y = "Probability of PR = 1",
    title = "Age vs Positive Response (Scatter Plot with Logistic Fit)"
  ) +
  theme_minimal()

# SEX vs AF
ggplot(data, aes(x = sex, y = AF, fill = sex)) +
  geom_boxplot() +
  labs(title = "Activating Factor (AF) by Sex", x = "Sex", y = "AF") +
  theme_minimal()

# TREATMENT vs AF
ggplot(data, aes(x = treatment, y = AF, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Activating Factor (AF) by Treatment", x = "Treatment", y = "AF") +
  theme_minimal()

# SEX vs PR
ggplot(data, aes(x = sex, fill = factor(PR, labels = c("No", "Yes")))) +
  geom_bar(position = "fill") +
  labs(
    y = "Proportion",
    fill = "Positive Response"
  ) +
  theme_minimal()

# TREATMENT vs PR
ggplot(data, aes(x = treatment, fill = factor(PR, labels = c("No", "Yes")))) +
  geom_bar(position = "fill") +
  labs(
    y = "Proportion",
    fill = "Positive Response"
  ) +
  theme_minimal()

