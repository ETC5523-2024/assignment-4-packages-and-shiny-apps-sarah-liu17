#' Top Funded Grant Opportunities Plot
#'
#' This function generates a bar plot of the top N most funded grant opportunities based on estimated program funding.
#'
#' @param grants_long A data frame containing grant information with `opportunity_id` and `estimated_total_program_funding`.
#' @param grants A data frame containing grant titles and categories, with `opportunity_id` and `opportunity_title` columns.
#' @param top_n An integer specifying the number of top funded grants to display (default is 7).
#'
#' @return A ggplot object showing a bar plot of the top funded grants.
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples
#' top_funded_grants(grants_long, grants, top_n = 10)  # Display top 10 grants
#' top_funded_grants(grants_long, grants)               # Display top 7 grants by default
top_funded_grants <- function(grant_opp, grant, top_n = 7) {
  # Validate that top_n is a positive integer
  if (!is.numeric(top_n) || top_n <= 0) {
    stop("top_n must be a positive integer.")
  }

  # Step 1: Group, calculate total funding, arrange by descending funding, and select top N grants
  top_est_fund <- grant_opp %>%
    group_by(opportunity_id) %>%
    mutate(total_funding = sum(estimated_total_program_funding, na.rm = TRUE)) %>%
    arrange(desc(total_funding)) %>%
    head(top_n)

  # Step 2: Join with the grants dataset to get the opportunity title
  top_est_fund <- top_est_fund %>%
    inner_join(grant, by = "opportunity_id") %>%
    select(opportunity_title, total_funding, category)

  # Step 3: Create the bar plot
  p <- ggplot(top_est_fund, aes(x = reorder(opportunity_title, total_funding),
                                y = total_funding, fill = category)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Top", top_n, "Most Funded Grant Opportunities"),
         x = "Grant Opportunity",
         y = "Total Funding",
         caption = "Top grants by total estimated program funding") +
    theme_minimal()

  # Return the plot
  return(p)
}
