library(tidyverse)
library(scales)
library(testthat)
library(lubridate)
library(broom)
library(arrow)
library(tidyr)

# Generate Data ----
set.seed(42)

n_practices <- 80
n_providers <- rpois(80, 3)

# medicare, private pay, 
d <- tibble(
  n_providers
) %>% 
  mutate(
    practice_id = row_number(),
    n_providers = ifelse(n_providers == 0, 1, n_providers)
  ) %>%
  select(practice_id, n_providers) %>%
  mutate(
    medicaid_mix = runif(nrow(.), 0, 0.2),
    medicare_mix = runif(nrow(.), 0, 0.8),
    private_mix = 1 - (medicaid_mix + medicare_mix)
  ) %>%
  mutate(
    n_medicare_pt = floor(medicare_mix * n_providers * rnorm(nrow(.), mean = 1000, sd = 100)),
    n_medicaid_pt = floor(medicaid_mix * n_providers * rnorm(nrow(.), mean = 1200, sd = 150)),
    n_private_pt = floor(private_mix * n_providers * rnorm(nrow(.), mean = 2100, sd = 200)),
    n_pt = n_medicare_pt + n_medicaid_pt + n_private_pt
  )
  
medicare_pt <- d %>%
  select(practice_id, n_medicare_pt) %>%
  group_by(practice_id) %>%
  tidyr::expand(pt = 1:n_medicare_pt) %>%
  mutate(insurance = "medicare") %>%
  ungroup()

medicaid_pt <- d %>%
  select(practice_id, n_medicaid_pt) %>%
  group_by(practice_id) %>%
  tidyr::expand(pt = 1:n_medicaid_pt) %>%
  mutate(insurance = "medicaid") %>%
  ungroup()

private_pt <- d %>%
  select(practice_id, n_private_pt) %>%
  group_by(practice_id) %>%
  tidyr::expand(pt = 1:n_private_pt) %>%
  mutate(insurance = "private") %>%
  ungroup()

pt_df <- bind_rows(medicare_pt, medicaid_pt, private_pt) %>%
  arrange(practice_id) %>%
  select(-pt) %>%
  group_by(practice_id) %>%
  mutate(patient_id = row_number()) %>%
  ungroup() %>%
  select(practice_id, patient_id, everything())

pt_df2 <- pt_df %>%
  mutate(
    # for age
    medicare_rv = runif(nrow(.)),
    # for ESRD
    medicare_rv2 = runif(nrow(.)),
    age = case_when(
      insurance == "medicaid" ~ ceiling(runif(nrow(.), min = 0, max = 64)),
      insurance == "private" ~ ceiling(runif(nrow(.), min = 0, max = 64)),
      # 85% of medicare recipients are 65+
      insurance == "medicare" & medicare_rv > 0.15 ~ ceiling(64 + rgamma(n = nrow(.), 3.24, scale = 3.3)),
      insurance == "medicare" ~ ceiling(runif(nrow(.), 17, 64))
    ),
    sex = sample(c("M", "F"), size = nrow(.), replace = TRUE),
    esrd = case_when(
      insurance == "medicare" & age < 65 & medicare_rv2 < 0.05 ~ 1L,
      insurance == "medicare" & age >= 65 & medicare_rv2 < 0.01 ~ 1L,
      TRUE ~ 0L
    ),
    ssdi = case_when(
      insurance == "medicare" & age < 65 & esrd == 0L ~ 1L,
      TRUE ~ 0L
    )
  ) %>% print

# medicare, age, random_var
expect_equal(pt_df2 %>% filter(age >= 65, ssdi == 1) %>% nrow, 0)


# intervention data
d_intervene <- d %>%
  select(-ends_with("_mix")) %>%
  mutate(
    has_intervention = ifelse(practice_id %% 4 == 0, 0L, 1L),
    intervention_start = case_when(
      has_intervention == 0 ~ NA,
      TRUE ~ as.Date("2023-01-01") + months(sample(1:35, nrow(.), TRUE))
    ),
    random_practice_effect = rnorm(nrow(.), mean = 0, sd = 0.1)
  )

# write parquet
d_intervene %>% select(-random_practice_effect) %>%
  write_parquet("practice-{Sys.time()}.parquet")

pt_df3 <- pt_df2 %>%
  left_join(d_intervene, by = "practice_id") %>%
  mutate(
    hosp_lp = -3 + .01*(age-60) +  2*(esrd == 1) + .7 * ssdi - .3 * (insurance == "private"),
    hosp_p = 1 / (1 + exp(-hosp_lp)),
    hosp = runif(nrow(.)) < hosp_p,
    hosp_date = case_when(
      hosp ~ as.Date("2022-01-01") + months(sample(1:48, nrow(.), TRUE)),
      !hosp ~ NA
    ),
    hosp_date_days = as.integer(hosp_date - as.Date("2022-01-01")),
    has_intervention_pt = case_when(
      has_intervention == 0 ~ 0,
      intervention_start > hosp_date ~ 0,
      TRUE ~ 1
    ),
    # to get a baseline rate of .14
    readmit_lp = -1.8 + random_practice_effect + 0.2 * (insurance == "medicare")- .0002 * hosp_date_days + .8 * esrd + .4 * ssdi + .01 * (age - 60) - .3 * has_intervention_pt,
    readmit_p = 1 / (1 + exp(-readmit_lp)),
    readmit = as.integer(runif(nrow(.)) < readmit_p)
  ) 

# patients
pt_df3 %>%
  select(practice_id, patient_id, insurance, age, sex, esrd, ssdi) %>%
  write_parquet("patients-{Sys.time()}.parquet")

# hospitalizations
pt_df3 %>%
  select(practice_id, patient_id, hosp_date, readmit) %>%
  filter(!is.na(hosp_date)) %>%
  write_parquet("hospitalizations-{Sys.time()}.parquet")

