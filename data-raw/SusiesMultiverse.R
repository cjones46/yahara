library(dplyr)
library(magrittr)
library(tidyr)
library(broom)
library(multiverse)

set.seed(09302026)
N = 200
df = tibble(
  age1 = round(runif(N, min = 21, max = 90)),
  age2 = age1 + sample(c(-10, -1, 0, 1, 10), N, prob = c(0.01, 0.03, 0.92, 0.03, 0.01), replace = TRUE),
  avg_age = (age1 + age2)/2,
  edu = sample(c("HS", "Bach", "Masters+"), N, prob = c(0.4, 0.3, 0.3), replace = TRUE),
  income = runif(N, 0, 50000) + 1000 * runif(N, min = 0, max = 1) * (avg_age - 18),
  perc_questions_answered = case_when(
    edu == "HS" ~ sample(c(0, 0.1, 0.2, 1), N, prob = c(0.1, 0.1, 0.1, 0.7), replace = TRUE),
    .default =sample(c(0, 0.1, 0.2, 1), N, prob = c(0.05, 0.05, 0.05, 0.85), replace = TRUE)
  ),
  mins_on_survey = rnorm(N, mean = 7, sd = 3) + 0.01 * (avg_age - 21),
  prob_response = 1/(1+exp(-(-5 + 0.05*avg_age + (edu == "Bach") + 2*(edu == "Masters+") + 0.000027*income)))
) %>%
  rowwise() %>%
  mutate(submitted = sample(c(T, F), 1, prob = c(prob_response, 1-prob_response))) %>%
  select(-prob_response)

SusiesMultiverse = multiverse()

inside(SusiesMultiverse, {
  df = df %>%
    filter(branch(age_mismatch,
                  "remove" ~ age1 == age2,
                  "recent" ~ TRUE,
                  "avg" ~ TRUE)) %>%
    mutate(age = branch(age_mismatch,
                        "remove" ~ age2,
                        "recent" ~ age2,
                        "avg" ~ avg_age)) %>%
    mutate(speeder = mins_on_survey < branch(speeding_threshold, "3" ~ 3, "5" ~ 5)) %>%
    branch(speeders,
           "remove" ~ filter(!speeder),
           "nonResponse" ~ mutate(submitted = ifelse(speeder, FALSE, submitted))) %>%

    mutate(submitted = ifelse(submitted & perc_questions_answered <= branch(skip_threshold,
                                                                            "0" ~ 0,
                                                                            "0.1" ~ 0.1,
                                                                            "0.2" ~ 0.2),
                              FALSE, submitted))

  fit = glm(submitted ~ age + edu + income + branch(interaction, "yes" ~ age:income, "no" ~ NULL), family = "binomial", data = df)
  summary_fit = fit %>% broom::tidy()
})
execute_multiverse(SusiesMultiverse)

usethis::use_data(SusiesMultiverse, overwrite = TRUE)
