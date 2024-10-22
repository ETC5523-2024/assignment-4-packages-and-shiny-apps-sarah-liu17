#' Award Summary
#'
#' This function calculates the mean award ceiling, mean award floor,
#' and the difference between them for the filtered award data.
#'
#' @param filtered_awards A data frame containing filtered grant awards.
#' @return A data frame summarizing the mean award ceiling, mean award floor,
#'         and their difference by category.

#' @export
#' @import dplyr
#' @examples
#' random_number(12)
award_summary <- function(filtered_awards) {
  filtered_awards %>%
    group_by(category) %>%
    summarise(
      mean_ceiling = mean(award_ceiling, na.rm = TRUE),
      mean_floor = mean(award_floor, na.rm = TRUE)
    ) %>%
    mutate(difference = mean_ceiling - mean_floor)
}
