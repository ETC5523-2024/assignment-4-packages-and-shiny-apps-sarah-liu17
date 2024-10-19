## code to prepare `grant_opp` dataset goes here

library(tidyverse)


grant_opp <- read_csv("data-raw/grant_opportunity_details.csv") |>
  pivot_longer(
    cols = "category_agriculture":"category_other",
    names_to = "category",
    values_to = "value"
  ) |>
  filter(value == TRUE) |> mutate(category = gsub("category_", "", category)) |>
  filter(current_closing_date_for_applications > 2024) |>
  select(funding_opportunity_title, expected_number_of_awards, current_closing_date_for_applications,
         award_ceiling, award_floor, category)


grant <- read_csv("data-raw/grants.csv")

usethis::use_data(grant, overwrite = TRUE)

usethis::use_data(grant_opp, overwrite = TRUE)

