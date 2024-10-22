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
         award_ceiling, award_floor, category, opportunity_id, estimated_total_program_funding,
         eligibility_individuals,
         eligibility_state_governments,
         eligibility_county_governments,
         eligibility_independent_school_districts,
         eligibility_city_or_township_governments,
         eligibility_special_district_governments,
         eligibility_native_american_tribal_governments_federally_recognized,
         eligibility_native_american_tribal_organizations_other,
         eligibility_nonprofits_501c3,
         eligibility_nonprofits_non_501c3,
         eligibility_for_profit,
         eligibility_small_businesses,
         eligibility_private_institutions_of_higher_education,
         eligibility_public_institutions_of_higher_education,
         eligibility_public_indian_housing_authorities,
         eligibility_others,
         eligibility_unrestricted)


grant <- read_csv("data-raw/grants.csv")

usethis::use_data(grant, overwrite = TRUE)

usethis::use_data(grant_opp, overwrite = TRUE)

