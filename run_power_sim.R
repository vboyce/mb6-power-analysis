library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(emmeans)
library(multidplyr)
# library(MASS)
unjuiced_data <- read.csv("raw.csv")

# first pass
options <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(1), # what to add (on average) to data
  dispersion = c(1), # what dispersion to use for all adjustments
  younger_TP_match = c(0, .2, .4, .6), # add to younger TP-TP
  older_TP_match = c(0, .2, .4, .6), # add to older TP-TP
  younger_MO_match = c(0, .2, .4, .6), # add to younger MO-MO
  older_MO_match = c(0, .2, .4, .6), # add to older MO-MO
  younger_TP_preference = c(0), # add to all younger adult_TP
  older_TP_preference = c(0), # add to all older adult_TP
)

# equal effects: no age-group difference & equal for TP and MO
options_2 <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(1), # what to add (on average) to data
  dispersion = c(1), # what dispersion to use for all adjustments
  younger_TP_match = c(0, .1, .2, .3, .4, .5, .6, .7), # add to younger TP-TP
  # older_TP_match = c(0, .2, .4,.6), # add to older TP-TP
  # younger_MO_match = c(0, .2, .4,.6), # add to younger MO-MO
  # older_MO_match = c(0, .2, .4,.6), # add to older MO-MO
  # younger_TP_preference =c(0,1),  # add to all younger adult_TP
  older_TP_preference = c(0, 1), # add to all older adult_TP
) |> mutate(
  older_TP_match = younger_TP_match,
  younger_MO_match = younger_TP_match,
  older_MO_match = younger_TP_match,
  younger_TP_preference = older_TP_preference
)

# no age-group difference & TP effect (0, .1, .2, .3, .4, .5, .6, .7) with (0, .2, ) MO effect
options_3 <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(1), # what to add (on average) to data
  dispersion = c(1), # what dispersion to use for all adjustments
  younger_TP_match = c(0, .1, .2, .3, .4, .5, .6, .7), # add to younger TP-TP
  # older_TP_match = c(0, .2, .4,.6), # add to older TP-TP
  younger_MO_match = c(0, .2), # add to younger MO-MO
  # older_MO_match = c(0, .2, .4,.6), # add to older MO-MO
  # younger_TP_preference =c(0,1),  # add to all younger adult_TP
  older_TP_preference = c(0, 1), # add to all older adult_TP
) |> mutate(
  older_TP_match = younger_TP_match,
  older_MO_match = younger_MO_match,
  younger_TP_preference = older_TP_preference
)

# difference only in older babies & equal for TP and MO (0, .1, .2, .3, .4, .5, .6, .7)
options_4 <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(1), # what to add (on average) to data
  dispersion = c(1), # what dispersion to use for all adjustments
  younger_TP_match = c(0), # add to younger TP-TP
  older_TP_match = c(0, .1, .2, .3, .4, .5, .6, .7), # add to older TP-TP
  younger_MO_match = c(0), # add to younger MO-MO
  # older_MO_match = c(0, .2, .4,.6), # add to older MO-MO
  # younger_TP_preference =c(0,1),  # add to all younger adult_TP
  older_TP_preference = c(0, 1), # add to all older adult_TP
) |> mutate(
  older_MO_match = older_TP_match,
  younger_TP_preference = older_TP_preference
)

# difference only in older babies & TP effect (0, .1, .2, .3, .4, .5, .6, .7) with (0, .2,) MO effect
options_5 <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(1), # what to add (on average) to data
  dispersion = c(1), # what dispersion to use for all adjustments
  younger_TP_match = c(0), # add to younger TP-TP
  older_TP_match = c(0, .1, .2, .3, .4, .5, .6, .7), # add to older TP-TP
  younger_MO_match = c(0), # add to younger MO-MO
  older_MO_match = c(0, .2), # add to older MO-MO
  # younger_TP_preference =c(0,1),  # add to all younger adult_TP
  older_TP_preference = c(0, 1), # add to all older adult_TP
) |> mutate(
  younger_TP_preference = older_TP_preference
)

# equal effects: no age-group difference & equal for TP and MO
options_6 <- expand_grid(
  overall_multiplier = c(1.5, 2), # what to multiply data by
  overall_add = c(1, 0), # what to add (on average) to data
  dispersion = c(1, .3, 3), # what dispersion to use for all adjustments
  younger_TP_match = c(0, .1, .2, .3, .4, .5, .6, .7), # add to younger TP-TP
  # older_TP_match = c(0, .2, .4,.6), # add to older TP-TP
  # younger_MO_match = c(0, .2, .4,.6), # add to younger MO-MO
  # older_MO_match = c(0, .2, .4,.6), # add to older MO-MO
  # younger_TP_preference =c(0,1),  # add to all younger adult_TP
  older_TP_preference = c(0, .2, .6), # add to all older adult_TP
) |> mutate(
  older_TP_match = younger_TP_match,
  younger_MO_match = younger_TP_match,
  older_MO_match = younger_TP_match,
  younger_TP_preference = older_TP_preference
)

# no age-group difference & TP effect (0, .1, .2, .3, .4, .5, .6, .7) with (0, .2, ) MO effect
options_7 <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(1), # what to add (on average) to data
  dispersion = c(1), # what dispersion to use for all adjustments
  younger_TP_match = c(0, .1, .2, .3, .4, .5, .6, .7), # add to younger TP-TP
  # older_TP_match = c(0, .2, .4,.6), # add to older TP-TP
  younger_MO_match = c(0, .2), # add to younger MO-MO
  # older_MO_match = c(0, .2, .4,.6), # add to older MO-MO
  # younger_TP_preference =c(0,1),  # add to all younger adult_TP
  older_TP_preference = c(0, .2, .6), # add to all older adult_TP
) |> mutate(
  older_TP_match = younger_TP_match,
  older_MO_match = younger_MO_match,
  younger_TP_preference = older_TP_preference
)

# difference only in older babies & equal for TP and MO (0, .1, .2, .3, .4, .5, .6, .7)
options_8 <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(1), # what to add (on average) to data
  dispersion = c(1), # what dispersion to use for all adjustments
  younger_TP_match = c(0), # add to younger TP-TP
  older_TP_match = c(0, .1, .2, .3, .4, .5, .6, .7), # add to older TP-TP
  younger_MO_match = c(0), # add to younger MO-MO
  # older_MO_match = c(0, .2, .4,.6), # add to older MO-MO
  # younger_TP_preference =c(0,1),  # add to all younger adult_TP
  older_TP_preference = c(0, .2, .6), # add to all older adult_TP
) |> mutate(
  older_MO_match = older_TP_match,
  younger_TP_preference = older_TP_preference
)

# difference only in older babies & TP effect (0, .1, .2, .3, .4, .5, .6, .7) with (0, .2,) MO effect
options_9 <- expand_grid(
  overall_multiplier = c(1.5), # what to multiply data by
  overall_add = c(1), # what to add (on average) to data
  dispersion = c(1), # what dispersion to use for all adjustments
  younger_TP_match = c(0), # add to younger TP-TP
  older_TP_match = c(0, .1, .2, .3, .4, .5, .6, .7), # add to older TP-TP
  younger_MO_match = c(0), # add to younger MO-MO
  older_MO_match = c(0, .2), # add to older MO-MO
  # younger_TP_preference =c(0,1),  # add to all younger adult_TP
  older_TP_preference = c(0, .2, .6), # add to all older adult_TP
) |> mutate(
  younger_TP_preference = older_TP_preference
)

# options_all <- options_2 |>
#   rbind(options_3, options_4, options_5) |>
#   distinct() |>
#   anti_join(options)

options_take_3 <- rbind(options_6, options_7, options_8, options_9) |>
  distinct() |>
  anti_join(rbind(options, options_2, options_3, options_4, options_5) |> distinct())

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
  # sample size is per age group
  data |>
    group_by(age, infant_behavior, adult_demo) |>
    slice_sample(n = sample_size) |>
    ungroup()
}

run_model <- function(data) {
  fix_fx_mod <- MASS::glm.nb(count ~ age * adult_demo * infant_behavior, data = data)

  interaction <- emmeans(fix_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(interaction = T) |>
    tidy() |>
    mutate(sig = case_when(
      estimate > 0 & p.value < .05 ~ 1, # positive means aligned in higher I think
      T ~ 0
    )) |>
    mutate(age = "all", type = "interaction") |>
    dplyr::select(age, type, sig)

  by_age <- emmeans(fix_fx_mod, ~ age * adult_demo * infant_behavior) |>
    pairs(by = "age", interaction = T) |>
    tidy() |>
    mutate(sig = case_when(
      estimate > 0 & p.value < .05 ~ 1, # positive means aligned in higher I think
      T ~ 0
    )) |>
    mutate(, type = "interaction") |>
    dplyr::select(age, type, sig)


  by_age_behavior <- emmeans(fix_fx_mod, ~ age * adult_demo * infant_behavior) |>
    pairs(by = c("infant_behavior", "age")) |>
    tidy() |>
    mutate(sig = case_when(
      infant_behavior == "MO" & estimate > 0 & p.value < .05 ~ 1, # MO-TP is positive when MO is higher
      infant_behavior == "TP" & estimate < 0 & p.value < .05 ~ 1,
      T ~ 0
    )) |>
    dplyr::select(age, type = infant_behavior, sig)

  by_behavior <- emmeans(fix_fx_mod, ~ adult_demo * infant_behavior) |>
    pairs(by = c("infant_behavior")) |>
    tidy() |>
    mutate(sig = case_when(
      infant_behavior == "MO" & estimate > 0 & p.value < .05 ~ 1, # MO-TP is positive when MO is higher
      infant_behavior == "TP" & estimate < 0 & p.value < .05 ~ 1,
      T ~ 0
    )) |>
    mutate(age = "all") |>
    dplyr::select(age, type = infant_behavior, sig)


  bind_rows(interaction, by_age, by_behavior, by_age_behavior)
}

get_power <- function(overall_multiplier, overall_add, dispersion,
                      younger_TP_match, older_TP_match,
                      younger_MO_match, older_MO_match,
                      younger_TP_preference, older_TP_preference, reps = 100) {
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
        expand_grid(samples = c(50, 100, 200, 400)) |>
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
    group_by(samples, age, type) |>
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

cluster_copy(cluster, "unjuiced_data")
cluster_copy(cluster, "juice_data")
cluster_copy(cluster, "sample_data")
cluster_copy(cluster, "run_model")
cluster_copy(cluster, "get_power")
# library(tictoc)
# tic()

# stuff <- options |>
#   partition(cluster) |>
#   mutate(data = pmap_df(list(
#     overall_multiplier, overall_add, dispersion,
#     younger_TP_match, older_TP_match,
#     younger_MO_match, older_MO_match,
#     younger_TP_preference, older_TP_preference
#   ), get_power)) |>
#   collect() |>
#   unnest(data) |>
#   unnest(data) |>
#   saveRDS("output.rds")

# stuff_2 <- options_all |>
#   partition(cluster) |>
#   mutate(data = pmap_df(list(
#     overall_multiplier, overall_add, dispersion,
#     younger_TP_match, older_TP_match,
#     younger_MO_match, older_MO_match,
#     younger_TP_preference, older_TP_preference
#   ), get_power)) |>
#   collect() |>
#   unnest(data) |>
#   unnest(data) |>
#   saveRDS("output2.rds")

stuff_3 <- options_take_3 |>
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
  saveRDS("output3.rds")
# toc()
