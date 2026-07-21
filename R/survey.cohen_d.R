#' Calculate Weighted Cohen's d and T-test for Survey Data
#'
#' @description 
#' Computes the weighted Cohen's d effect size for two groups using complex survey design objects. 
#' The function calculates weighted means and variances, computes a weighted pooled standard deviation, 
#' and returns the effect size alongside the results of a weighted t-test (`survey::svyttest`).
#' Requires the survey pacakge.
#'
#' @param formula A formula object specifying the dependent and independent variables 
#'   (e.g., \code{dependent_var ~ grouping_var}). The grouping variable must have exactly two levels 
#'   to compute Cohen's d.
#' @param design A survey design object created with \code{survey::svydesign}.
#' @param data A data frame containing the variables specified in the formula and the weight variable.
#' @param weight_var A character string specifying the name of the weight variable in \code{data}. 
#'   Defaults to \code{"W_FSTUWT"} (standard PISA final student weight).
#'
#' @return A list containing the following components:
#' \item{means}{A numeric vector of weighted means for each group.}
#' \item{sd}{A numeric vector of weighted standard deviations for each group.}
#' \item{d}{A numeric value representing the weighted Cohen's d effect size. Returns a warning message if the grouping variable has more than two categories.}
#' \item{t_test}{An object of class \code{htest} containing the results of the \code{survey::svyttest}.}
#'
#' @importFrom survey svyby svymean svyvar svyttest
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'pisa_design' is a valid survey design object and 'df' is the raw data
#' result <- survey.cohen_d(ST268_TT ~ Gender, design = pisa_design, data = df)
#' print(result$d)
#' }
survey.cohen_d <- function(formula, design, data, weight_var = "W_FSTUWT") {
  dep_var <- all.vars(formula)[1]
  indep_var <- all.vars(formula)[2]

  means_obj <- survey::svyby(as.formula(paste("~", dep_var)), as.formula(paste("~", indep_var)), design, survey::svymean)
  vars_obj  <- survey::svyby(as.formula(paste("~", dep_var)), as.formula(paste("~", indep_var)), design, survey::svyvar)
  
  groups <- means_obj[[indep_var]]
  means <- means_obj[[dep_var]]
  vars  <- vars_obj[[dep_var]]
  
  w_sums <- sapply(groups, function(g) {
    sum(data[[weight_var]][data[[indep_var]] == g])
  })
  
  sd_pooled <- sqrt(sum(w_sums * vars) / sum(w_sums))
  
  if(length(means) == 2) {
    cohens_d <- (means[1] - means[2]) / sd_pooled
  } else {
    cohens_d <- "More than 2 groups - cannot calculate cohen D"
  }
  
  t_test <- survey::svyttest(formula, design = design)
  
  # Výstup
  cat("--- Group for analysis:", indep_var, "---\n")
  cat("Means:", paste(groups, "=", round(means, 2), collapse=", "), "\n")
  cat("SD:", paste(groups, "=", round(sqrt(vars),2), collapse = ", "), "\n")
  cat("Weighted Cohen d:", ifelse(is.numeric(cohens_d), round(cohens_d, 3), cohens_d), "\n\n")
  
  return(list(means = means, sd  = sqrt(vars), d = cohens_d, t_test = t_test))
}
