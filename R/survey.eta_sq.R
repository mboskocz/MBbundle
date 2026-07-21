#' Calculate Weighted ANOVA and Eta-squared for Survey Data
#'
#' @description 
#' Computes the weighted Eta-squared effect size and performs a weighted ANOVA 
#' (Wald test via \code{survey::regTermTest}) for a categorical grouping variable. 
#' It is designed to work with complex survey design objects and provides variance 
#' estimates and effect sizes that account for sampling weights.
#'
#' @param formula A formula object specifying the dependent and independent variables 
#'   (e.g., \code{dependent_var ~ grouping_var}). The grouping variable must be categorical 
#'   (can have two or more levels).
#' @param design A survey design object created with \code{survey::svydesign}.
#' @param data A data frame containing the variables specified in the formula and the weight variable.
#' @param weight_var A character string specifying the name of the weight variable in \code{data}. 
#'   Defaults to \code{"W_FSTUWT"}.
#'
#' @return A list containing the following components:
#' \item{means}{A numeric vector of weighted means for each group.}
#' \item{sd}{A numeric vector of weighted standard deviations for each group.}
#' \item{eta_squared}{A numeric value representing the weighted Eta-squared effect size.}
#' \item{anova_test}{An object of class \code{regTermTest} containing the results of the Wald test.}
#'
#' @importFrom survey svyby svymean svyvar svyglm regTermTest
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'pisa_design' is a valid survey design object and 'df' is the raw data
#' result <- survey.anova_eta(ST268_TT ~ Druh_skoly, design = pisa_design, data = df)
#' print(result$eta_squared)
#' }
#' 
survey.eta_sq <- function(formula, design, data, weight_var = "W_FSTUWT") {
  dep_var <- all.vars(formula)[1]
  indep_var <- all.vars(formula)[2]
  
  means_obj <- survey::svyby(as.formula(paste("~", dep_var)), as.formula(paste("~", indep_var)), design, survey::svymean, na.rm=TRUE)
  vars_obj  <- survey::svyby(as.formula(paste("~", dep_var)), as.formula(paste("~", indep_var)), design, survey::svyvar, na.rm=TRUE)
  
  groups <- means_obj[[indep_var]]
  means  <- means_obj[[dep_var]]
  vars   <- vars_obj[[dep_var]]
  
  grand_mean_obj <- survey::svymean(as.formula(paste("~", dep_var)), design, na.rm=TRUE)
  grand_mean <- as.numeric(grand_mean_obj)
  
  w_sums <- sapply(groups, function(g) {
    sum(data[[weight_var]][data[[indep_var]] == g])
  })
  
  SS_between <- sum(w_sums * (means - grand_mean)^2)
  SS_within <- sum(w_sums * vars, na.rm = TRUE)
  
  SS_total <- SS_between + SS_within
  eta_squared <- SS_between / SS_total
  
  mod_formula <- as.formula(paste(dep_var, "~ as.factor(", indep_var, ")"))
  svy_model <- survey::svyglm(mod_formula, design = design)
  
  test_term <- as.formula(paste("~ as.factor(", indep_var, ")"))
  anova_test <- survey::regTermTest(svy_model, test.terms = test_term)
  
  cat("--- Weighted ANOVA for group:", indep_var, "---\n")
  cat("Weighted means by group:\n")
  for(i in 1:length(groups)) {
    cat(paste0("  ", groups[i], ": ", round(means[i], 3), "\n")) 
  }
  cat("SD by group:\n")
  for(i in 1:length(groups)) {
    cat(paste0("  ", groups[i], ": ", round(sqrt(vars[i]), 3), "\n")) 
  }
  
  cat("\nEffect size:\n")
  cat("Eta-squared (η²):", round(eta_squared, 4), "\n")
  cat("Explained variance:", round(eta_squared * 100, 2), "%\n")
  
  cat("\nWald Test result (omnibus p-value):\n")
  cat("p-val:", anova_test$p[1], "\n\n")
  
  return(list(
    means = means,
    sd = sqrt(vars),
    eta_squared = eta_squared, 
    anova_test = anova_test
  ))
}