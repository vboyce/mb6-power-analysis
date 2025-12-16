library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(emmeans)
library(lme4)
library(multidplyr)
library(stringr)
# library(MASS)
unjuiced_data <- read.csv("raw.csv")

# first pass
options <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(0), # what to add (on average) to data
  dispersion = c(.3, 1, 3), # what dispersion to use for all adjustments
  younger_TP_match = c(0, .1, .2, .3, .4), # add to younger TP-TP
  younger_TP_preference = c(0), # add to all younger adult_TP
  older_TP_preference = c(0), # add to all older adult_TP
) |>
  mutate(
    older_TP_match = younger_TP_match,
    younger_MO_match = younger_TP_match,
    older_MO_match = younger_TP_match
  )

juice_data <- function(overall_multiplier, overall_add, dispersion,
                       younger_TP_match, older_TP_match,
                       younger_MO_match, older_MO_match,
                       younger_TP_preference, older_TP_preference) {
  unjuiced_data |>
    rowwise() |>
    mutate(
      # apply overall boost
      count = count * overall_multiplier + rnbinom(n = 1, mu = overall_add, size = dispersion)
    ) |>
    group_by(infant_behavior, age) |>
    mutate(std = sd(count)) |> # all our effects are in sd units, so need to scale
    rowwise() |>
    mutate(
      count_delta = case_when(
        age == "younger" & infant_behavior == "TP" & adult_demo == "TP" ~
          rnbinom(n = 1, mu = younger_TP_match * std, size = dispersion) +
          rnbinom(n = 1, mu = younger_TP_preference * std, size = dispersion),
        age == "younger" & infant_behavior == "TP" & adult_demo == "MO" ~ 0,
        age == "older" & infant_behavior == "TP" & adult_demo == "TP" ~
          rnbinom(n = 1, mu = older_TP_match * std, size = dispersion) +
          rnbinom(n = 1, mu = older_TP_preference * std, size = dispersion),
        age == "older" & infant_behavior == "TP" & adult_demo == "MO" ~ 0,
        age == "younger" & infant_behavior == "MO" & adult_demo == "TP" ~
          rnbinom(n = 1, mu = younger_TP_preference * std, size = dispersion),
        age == "younger" & infant_behavior == "MO" & adult_demo == "MO" ~
          rnbinom(n = 1, mu = younger_MO_match * std, size = dispersion),
        age == "older" & infant_behavior == "MO" & adult_demo == "TP" ~
          rnbinom(n = 1, mu = older_TP_preference * std, size = dispersion),
        age == "older" & infant_behavior == "MO" & adult_demo == "MO" ~
          rnbinom(n = 1, mu = older_MO_match * std, size = dispersion),
      ),
      count = count + count_delta,
      count = case_when(
        count < 0 ~ 0, # shouldn't happen, but just in case
        count > 15 ~ 15, # cap,
        T ~ round(count + rnorm(1, 0, .01)) # add random noise to prevent .5 rounding consistently to even
      )
    ) |>
    ungroup() |>
    dplyr::select(-std, -count_delta)
}

sample_data <- function(data, sample_size) {
  # sample size is per age group, in # of labs
  data |>
    group_by(age, infant_behavior, adult_demo) |>
    slice_sample(n = sample_size * 16) |>
    mutate(
      lab = rep(1:sample_size, 16),
      lab = str_c(age, "_", lab),
      infant = as.character(seq(1:n()))
    ) |>
    ungroup()
}

run_model <- function(data) {
  fix_fx_mod <- MASS::glm.nb(count ~ age * adult_demo * infant_behavior, data = data)

  ran_fx_mod <- glmer.nb(count ~ age * adult_demo * infant_behavior + (1 | infant) +
    (adult_demo * infant_behavior | lab), data = data)

  interaction <- emmeans(fix_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(interaction = T) |>
    tidy() |>
    mutate(sig = case_when(
      estimate > 0 & p.value < .05 ~ 1, # positive means aligned in higher I think
      T ~ 0
    )) |>
    mutate(model = "fixed", age = "all", type = "interaction") |>
    dplyr::select(age, type, sig, model)

  by_age <- emmeans(fix_fx_mod, ~ age * adult_demo * infant_behavior) |>
    pairs(by = "age", interaction = T) |>
    tidy() |>
    mutate(sig = case_when(
      estimate > 0 & p.value < .05 ~ 1, # positive means aligned in higher I think
      T ~ 0
    )) |>
    mutate(model = "fixed", type = "interaction") |>
    dplyr::select(age, type, sig, model)


  by_age_behavior <- emmeans(fix_fx_mod, ~ age * adult_demo * infant_behavior) |>
    pairs(by = c("infant_behavior", "age")) |>
    tidy() |>
    mutate(
      sig = case_when(
        infant_behavior == "MO" & estimate > 0 & p.value < .05 ~ 1, # MO-TP is positive when MO is higher
        infant_behavior == "TP" & estimate < 0 & p.value < .05 ~ 1,
        T ~ 0
      ),
      model = "fixed"
    ) |>
    dplyr::select(age, type = infant_behavior, sig, model)

  by_behavior <- emmeans(fix_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(by = c("infant_behavior")) |>
    tidy() |>
    mutate(sig = case_when(
      infant_behavior == "MO" & estimate > 0 & p.value < .05 ~ 1, # MO-TP is positive when MO is higher
      infant_behavior == "TP" & estimate < 0 & p.value < .05 ~ 1,
      T ~ 0
    )) |>
    mutate(model = "fixed", age = "all") |>
    dplyr::select(age, type = infant_behavior, sig, model)


  interaction_random <- emmeans(ran_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(interaction = T) |>
    tidy() |>
    mutate(sig = case_when(
      estimate > 0 & p.value < .05 ~ 1, # positive means aligned in higher I think
      T ~ 0
    )) |>
    mutate(model = "random", age = "all", type = "interaction") |>
    dplyr::select(age, type, sig, model)

  by_age_random <- emmeans(ran_fx_mod, ~ age * adult_demo * infant_behavior) |>
    pairs(by = "age", interaction = T) |>
    tidy() |>
    mutate(sig = case_when(
      estimate > 0 & p.value < .05 ~ 1, # positive means aligned in higher I think
      T ~ 0
    )) |>
    mutate(model = "random", type = "interaction") |>
    dplyr::select(age, type, sig, model)


  by_age_behavior_random <- emmeans(ran_fx_mod, ~ age * adult_demo * infant_behavior) |>
    pairs(by = c("infant_behavior", "age")) |>
    tidy() |>
    mutate(
      sig = case_when(
        infant_behavior == "MO" & estimate > 0 & p.value < .05 ~ 1, # MO-TP is positive when MO is higher
        infant_behavior == "TP" & estimate < 0 & p.value < .05 ~ 1,
        T ~ 0
      ),
      model = "random"
    ) |>
    dplyr::select(age, type = infant_behavior, sig, model)

  by_behavior_random <- emmeans(ran_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(by = c("infant_behavior")) |>
    tidy() |>
    mutate(sig = case_when(
      infant_behavior == "MO" & estimate > 0 & p.value < .05 ~ 1, # MO-TP is positive when MO is higher
      infant_behavior == "TP" & estimate < 0 & p.value < .05 ~ 1,
      T ~ 0
    )) |>
    mutate(model = "random", age = "all") |>
    dplyr::select(age, type = infant_behavior, sig, model)

  bind_rows(
    interaction, by_age, by_behavior, by_age_behavior,
    interaction_random, by_age_random, by_behavior_random, by_age_behavior_random
  )
}

get_power <- function(overall_multiplier, overall_add, dispersion,
                      younger_TP_match, older_TP_match,
                      younger_MO_match, older_MO_match,
                      younger_TP_preference, older_TP_preference, reps = 30) {
  tibble(reps = 1:reps) |>
    mutate(data = map(
      reps,
      \(r){
        juiced_data <- juice_data(
          overall_multiplier, overall_add, dispersion,
          younger_TP_match, older_TP_match,
          younger_MO_match, older_MO_match,
          younger_TP_preference, older_TP_preference
        )
        # View(juiced_data)
        expand_grid(samples = c(3, 6, 12)) |>
          rowwise() |>
          mutate(data = map(samples, \(s){
            sample_data(data = juiced_data, sample_size = samples) |>
              run_model() |>
              nest()
          }))
      }
    )) |>
    unnest(data) |>
    unnest(data) |>
    unnest(data) |>
    group_by(samples, age, type, model) |>
    summarize(sig = mean(sig)) |>
    ungroup() |>
    nest()
}


cluster <- new_cluster(16)
cluster_library(cluster, "dplyr")
cluster_library(cluster, "purrr")
cluster_library(cluster, "tidyr")
cluster_library(cluster, "tibble")
cluster_library(cluster, "broom")
cluster_library(cluster, "emmeans")
cluster_library(cluster, "lme4")
cluster_library(cluster, "stringr")

cluster_copy(cluster, "unjuiced_data")
cluster_copy(cluster, "juice_data")
cluster_copy(cluster, "sample_data")
cluster_copy(cluster, "run_model")
cluster_copy(cluster, "get_power")

# library(tictoc)
# tic()


stuff <- options |>
  partition(cluster) |>
  mutate(data = pmap_df(list(
    overall_multiplier, overall_add, dispersion,
    younger_TP_match, older_TP_match,
    younger_MO_match, older_MO_match,
    younger_TP_preference, older_TP_preference
  ), get_power)) |>
  collect() |>
  unnest(data) |>
  unnest(data) |>
  saveRDS("output_ranef_2_1.rds")
# toc()
