#' Summarize Grant Opportunities by Category
#'
#' This function summarizes the number of grant opportunities and total funding by category.
#'
#' @param grants A data frame containing grant information.
#'
#' @return A data frame summarizing the number of grants and total funding by category.
#' @export
#'
#' @examples
#' summarize_grants_by_category(grant_opp)

summarize_grants_by_category <- function(grant_opp) {
  grant_opp %>%
    group_by(category) %>%
    summarise(
      number_of_grants = n(),
      total_funding = sum(estimated_total_program_funding, na.rm = TRUE)
    ) %>%
    arrange(desc(total_funding))
}








