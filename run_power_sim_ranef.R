library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(broom)
library(emmeans)
library(lme4)
library(multidplyr)
# library(MASS)
unjuiced_data <- read.csv("raw_data_for_ranef.csv")

# equal effects: no age-group difference & equal for TP and MO
options <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(1), # what to add (on average) to data
  dispersion = c(1), # what dispersion to use for all adjustments
  match_effect = c(0,.1, .2, .3, .4), # add to younger TP-TP
) |> expand_grid(tribble(
  ~lab_intercept_sd, ~lab_match_sd,
  0, 0,
  .1, 0,
  .1, .1,
  .3, .15
))

juice_data <- function(overall_multiplier, overall_add, dispersion,
                       match_effect,
                       lab_intercept_sd, lab_match_sd) {
  # TODO need to add grouping into fake labs & sample the lab level factors and add in!!!

  # overall_multiplier=1.5
  # overall_add=1
  # dispersion=1
  # match_effect=0
  # lab_intercept_sd=.1
  # lab_match_sd=.1

  fake_labs <- expand_grid(age = c("younger", "older"), lab = 1:24) |>
    mutate(lab = str_c(age, lab)) |>
    rowwise() |>
    mutate(
      lab_intercept_effect = rnorm(n = 1, mean = 0, sd = lab_intercept_sd),
      lab_match_effect = rnorm(n = 1, mean = 0, sd = lab_match_sd)
    ) |>
    expand_grid(participant = 1:16) |>
    arrange(age) |>
    dplyr::select(-age)

  younger_sample <- unjuiced_data |>
    filter(age == "younger") |>
    slice_sample(n = 384, replace = T)
  older_sample <- unjuiced_data |>
    filter(age == "older") |>
    slice_sample(n = 384, replace = T)

  older_sample |>
    bind_rows(younger_sample) |>
    bind_cols(fake_labs) |>
    rowwise() |>
    mutate(
      boost = rnbinom(n = 1, mu = overall_add, size = dispersion),
      MO_MO = MO_MO * overall_multiplier + boost,
      MO_TP = MO_TP * overall_multiplier + boost,
      TP_MO = TP_MO * overall_multiplier + boost,
      TP_TP = TP_TP * overall_multiplier + boost
    ) |>
    group_by(age) |>
    mutate(
      MO_MO_std = sd(MO_MO),
      MO_TP_std = sd(MO_TP),
      TP_MO_std = sd(TP_MO),
      TP_TP_std = sd(TP_TP)
    ) |> # all our effects are in sd units, so need to scale
    rowwise() |>
    mutate(
      MO_MO = case_when(
        (lab_intercept_effect + lab_match_effect) < 0 ~
          MO_MO + rnbinom(n = 1, mu = match_effect * MO_MO_std, size = dispersion) -
          rnbinom(n = 1, mu = abs(lab_intercept_effect + lab_match_effect) * MO_MO_std, size = dispersion),
        T ~ MO_MO + rnbinom(n = 1, mu = (match_effect) * MO_MO_std, size = dispersion) +
          rnbinom(n = 1, mu = (lab_intercept_effect + lab_match_effect) * MO_MO_std, size = dispersion)
      ),
      TP_TP = case_when(
        (lab_intercept_effect + lab_match_effect) < 0 ~
          TP_TP + rnbinom(n = 1, mu = (match_effect) * TP_TP_std, size = dispersion) -
          rnbinom(n = 1, mu = abs(lab_intercept_effect + lab_match_effect) * TP_TP_std, size = dispersion),
        T ~ TP_TP + rnbinom(n = 1, mu = (match_effect) * TP_TP_std, size = dispersion) +
          rnbinom(n = 1, mu = (lab_intercept_effect + lab_match_effect) * TP_TP_std, size = dispersion)
      ),
      MO_TP = case_when(
        (lab_intercept_effect) < 0 ~
          MO_TP -
          rnbinom(n = 1, mu = abs(lab_intercept_effect) * MO_TP_std, size = dispersion),
        T ~ MO_TP +
          rnbinom(n = 1, mu = (lab_intercept_effect) * MO_TP_std, size = dispersion)
      ),
      TP_MO = case_when(
        (lab_intercept_effect) < 0 ~
          TP_MO -
          rnbinom(n = 1, mu = abs(lab_intercept_effect) * TP_MO_std, size = dispersion),
        T ~ TP_MO +
          rnbinom(n = 1, mu = (lab_intercept_effect) * TP_MO_std, size = dispersion)
      )
    ) |>
    mutate(across(c(MO_MO, TP_TP, TP_MO, MO_TP), \(c) {
      case_when(
        c < 0 ~ 0, # shouldn't happen, but just in case
        c > 15 ~ 15, # cap,
        T ~ round(c + rnorm(1, 0, .01)) # add random noise to prevent .5 rounding consistently to even
      )
    })) |>
    ungroup() |>
    dplyr::select(-starts_with("std"), -boost, -lab_intercept_effect, -lab_match_effect)
}

run_model <- function(data) {
  ran_fx_mod <- glmer.nb(count ~ age * adult_demo * infant_behavior + (1 | Subject) +
    (adult_demo * infant_behavior | lab), data = data)

  fix_fx_mod <- MASS::glm.nb(count ~ age * adult_demo * infant_behavior, data = data)

  interaction_random <- emmeans(ran_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(interaction = T) |>
    tidy() |>
    mutate(sig = case_when(
      estimate > 0 & p.value < .05 ~ 1, # positive means aligned in higher I think
      T ~ 0
    )) |>
    mutate(age = "all", type = "interaction", model="random") |>
    dplyr::select(age, type, sig, model)

  by_behavior_random <- emmeans(ran_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(by = c("infant_behavior")) |>
    tidy() |>
    mutate(sig = case_when(
      infant_behavior == "MO" & estimate > 0 & p.value < .05 ~ 1, # MO-TP is positive when MO is higher
      infant_behavior == "TP" & estimate < 0 & p.value < .05 ~ 1,
      T ~ 0
    )) |>
    mutate(age = "all", model="random") |>
    dplyr::select(age, type = infant_behavior, sig, model)

  interaction_fixed <- emmeans(fix_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(interaction = T) |>
    tidy() |>
    mutate(sig = case_when(
      estimate > 0 & p.value < .05 ~ 1, # positive means aligned in higher I think
      T ~ 0
    )) |>
    mutate(age = "all", type = "interaction", model="fixed") |>
    dplyr::select(age, type, sig, model)

  by_behavior_fixed <- emmeans(fix_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(by = c("infant_behavior")) |>
    tidy() |>
    mutate(sig = case_when(
      infant_behavior == "MO" & estimate > 0 & p.value < .05 ~ 1, # MO-TP is positive when MO is higher
      infant_behavior == "TP" & estimate < 0 & p.value < .05 ~ 1,
      T ~ 0
    )) |>
    mutate(age = "all", model="fixed") |>
    dplyr::select(age, type = infant_behavior, sig, model)


  bind_rows(interaction_random, by_behavior_random, interaction_fixed, by_behavior_fixed)
}

get_power <- function(overall_multiplier, overall_add, dispersion,
                      match_effect, lab_intercept_sd, lab_match_sd, reps = 30) {
  tibble(reps = 1:reps) |>
    mutate(data = map(
      reps,
      \(r){
        juiced_data <- juice_data(
          overall_multiplier, overall_add, dispersion,
          match_effect, lab_intercept_sd, lab_match_sd
        )
        # View(juiced_data)
        tibble(samples = c(3, 6, 12)) |>
          mutate(data = map(samples, \(s){
            juiced_data |>
              filter(lab %in% c(str_c("younger", 1:s), str_c("older", 1:s))) |>
              pivot_longer(cols = c("TP_TP", "TP_MO", "MO_MO", "MO_TP"), names_to = "type", values_to = "count") |>
              separate(type, into = c("infant_behavior", "adult_demo")) |>
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
cluster_library(cluster, "stringr")
cluster_library(cluster, "lme4")


cluster_copy(cluster, "unjuiced_data")
cluster_copy(cluster, "juice_data")
cluster_copy(cluster, "run_model")
cluster_copy(cluster, "get_power")

# library(tictoc)
# tic()
stuff <- options |>
  partition(cluster) |>
  mutate(data = pmap_df(list(
    overall_multiplier, overall_add, dispersion,
    match_effect, lab_intercept_sd, lab_match_sd
  ), get_power)) |>
  collect() |>
  unnest(data) |>
  unnest(data) |>
  saveRDS("output_ranef_5.rds")
# toc()
