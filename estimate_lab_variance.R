library(tidyverse)
library(lme4)
library(here)

# in which we try to use other MB projects to get a graph on what kind of lab-lab variance might be expected

# from MB1

dat_mb1 <- read_csv(url("https://raw.githubusercontent.com/manybabies/mb1-analysis-public/refs/heads/master/processed_data/03_data_trial_main.csv")) |>
  mutate(scaled_looking_time = scale(looking_time))

m1 <- lmer(scaled_looking_time ~ trial_type + trial_order + age_group + method + (trial_type + 1 | subid_unique) + (trial_type + 1 | lab), data = dat_mb1)

summary(m1)

# for scaled looking time
# intercept
# sd of subid = .4
# sd of labs = .3
# effect
# sd of subject = .06
# sd of labs = .08

# MB2 and MB3 don't have obviously easily available post-processed data :(
# But thanks to insider connections,  I got post-processed data! yay!

# MB2

dat_mb2 <- read_rds(here("mb2_clean_data.rds")) |>
  filter(age_cohort != "adults") |>
  select(condition, data_type, prop_exit, lab_id, participant_id) |>
  mutate(participant_id = str_c(lab_id, participant_id))

m2 <- lmer(prop_exit ~ condition + data_type + (1 | lab_id), data = dat_mb2)

summary(m2)

# after rescaling total variance to 1
# sd of labs = .08

# MB3

dat_mb3 <- read_csv(here("mb3_clean_data.csv")) |>
  rename(trial_type_char = trial_type) |>
  mutate(
    log_looking_time = log(looking_time_seconds),
    multilingual_exposure = as.vector(scale((100 - lang1_exposure) / 100)), # proportion of not L1
    age = as.vector(scale(participant_age_days / 30.42)), # convert age to months
    repetition_char = case_when(trial_type_char != "fam" & grepl("abb", stimulus_filename) ~ "ABB",
      trial_type_char != "fam" & grepl("aba", stimulus_filename) ~ "ABA",
      .default = NA_character_
    ),
    repetition = case_when(repetition_char == "ABB" ~ .5,
      repetition_char == "ABA" ~ -.5,
      .default = NA_integer_
    ),
    trial_type = case_when(trial_type_char == "consistent" ~ .5,
      trial_type_char == "inconsistent" ~ -.5,
      .default = NA_integer_
    ),
    trial_num_sc = scale(trial_num),
  ) |> # add repetition variable based on test trials (ABB and ABA)
  rename(experimental_method = method) |>
  filter(trial_type_char != "fam") |>
  filter(!is.infinite(log_looking_time))

m3 <- lmer(log_looking_time ~ trial_type + experimental_method + age + (1 | lab_id) + (1 | unique_participant_id),
  data = dat_mb3
)

summary(m3)

# scaled for a variance of 1
# per subject intercept = .5
# per subject condition = .016
# per lab intercept = .3
# per lab condition = .05

# MB4

dat_mb4 <- read_csv(here("mb4_clean_data.csv")) |>
  select(helper_hinderer_choice, lab_id, subj_id, condition, method, age_group) |>
  mutate(chose_helper = case_when(
    helper_hinderer_choice == "helper" ~ 1,
    helper_hinderer_choice == "hinderer" ~ 0,
    T ~ NA
  ), subj_id = str_c(lab_id, subj_id))

m4 <- lmer(chose_helper ~ age_group + method + condition + (condition + 1 | lab_id), data = dat_mb4)

summary(m4)
# scaling total variance up to 1
# can't do individual level since single trials
# but sd of lab = .18 for intercept
# sd of lab .14 for effect
