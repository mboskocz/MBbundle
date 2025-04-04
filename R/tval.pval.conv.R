#' T-value to P-value Conversion Function
#'
#' This function takes a t-value and converts it to a p-value.
#'
#' @param t_value Number that represents a t-value.
#' @param df Number that represents degrees of freedom. Defaults to 100000.
#' @param tail String that represents the hypothesis. Either two.sided, greater or less. Defaults to two.sided.
#' @return Returns the p-value.
#' @export
tval.pval.conv <- function(t_value, df=100000, tail = "two.sided") { #this function converts t-value to p-value
  if (tail == "two.sided") {
    p_value <- 2 * (1 - stats::pt(abs(t_value), df))
  } else if (tail == "greater") {
    p_value <- 1 - stats::pt(t_value, df)
  } else if (tail == "less") {
    p_value <- stats::pt(t_value, df)
  } else {
    stop("Invalid tail argument. Use 'two.sided', 'greater', or 'less'.")
  }
  return(p_value)
}