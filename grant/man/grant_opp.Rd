% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{grant_opp}
\alias{grant_opp}
\title{grant and grant opportunity datasets}
\format{
A data frame with 2843 rows and 25 variables.
\describe{
\item{funding_opportunity_title}{Title of the opportunity}
\item{expected_number_of_awards}{Expected count of awards}
\item{current_closing_date_for_applications}{When the opportunity is currently scheduled to close}
\item{award_ceiling}{Maximum individual award amount in dollars}
\item{award_floor}{Minimum individual award amount in dollars}
\item{category}{category or type of field that the grant opportunity belongs to}
\item{opportunity_id}{Integer ID for this opportunity}
\item{estimated_total_program_funding}{Estimated funding amount in dollars}
\item{eligibility_individuals}{Are individuals eligible?}
\item{eligibility_state_governments}{Are state governments eligible?}
\item{eligibility_county_governments}{Are county governments eligible?}
\item{eligibility_independent_school_districts}{Are independent school districts eligible?}
\item{eligibility_city_or_township_governments}{Are city or township governments eligible?}
\item{eligibility_special_district_governments}{Are special district governments eligible?}
\item{eligibility_native_american_tribal_governments_federally_recognized}{Are Native American tribal governments (Federally recognized) eligible?}
\item{eligibility_native_american_tribal_organizations_other}{Are Native American tribal organizations (other than Federally recognized tribal governments) eligible?}
\item{eligibility_nonprofits_501c3}{Are nonprofits having a 501(c)(3) status with the IRS, other than institutions of higher education eligible?}
\item{eligibility_nonprofits_non_501c3}{Are nonprofits that do not have a 501(c)(3) status with the IRS, other than institutions of higher education eligible?}
\item{eligibility_for_profit}{Are for profit organizations other than small businesses eligible?}
\item{eligibility_small_businesses}{Are small businesses eligible?}
\item{eligibility_private_institutions_of_higher_education}{Are private institutions of higher education eligible?}
\item{eligibility_public_institutions_of_higher_education}{Are public and State controlled institutions of higher education eligible?}
\item{eligibility_public_indian_housing_authorities}{Are public housing authorities and Indian housing authorities eligible?}
\item{eligibility_others}{Are other groups eligible?}
\item{eligibility_unrestricted}{Is eligibility unrestricted?}
}
}
\source{
\url{https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-10-03}
}
\usage{
grant_opp
}
\description{
A dataset that contains the information about grants opportunity details
}
\keyword{datasets}
