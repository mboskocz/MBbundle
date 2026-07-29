#' Table of Fit Indicators of lavaan Models
#'
#' This function evaluates the result of the `lavaan::fitMeasures()` function based on Papi (2010). 
#' Additionally, Awang (2015) presents a slightly less strict level of acceptance for RMSEA and GFI, 
#' which is reflected with "(A)" in the evaluation column. 
#' 
#' @details 
#' The function extracts common fit indices (Chi-square, GFI, AGFI, NFI, NNFI, CFI, RMSEA, SRMR, 
#' RFI, TLI, PNFI, IFI) and compares them against established thresholds. 
#' If a specific fit index is missing from the input (e.g., when a different estimator is used 
#' in lavaan), the function safely assigns an `NA` to both the value and the evaluation 
#' instead of throwing an error.
#' 
#' @param fitMeasures A named numeric vector or a list containing the outcome of the 
#'   \code{\link[lavaan]{fitMeasures}} function.
#' 
#' @return A `data.frame` with 4 columns:
#'   \item{Index}{The name of the fit index.}
#'   \item{Current level}{The extracted numeric value from the lavaan model.}
#'   \item{Accepted level}{The threshold used for evaluation.}
#'   \item{Evaluation}{String indicating if the fit is "Satisfactory", "Satisfactory (A)", "Poor", or `NA`.}
#' 
#' 
#' @seealso \code{\link[lavaan]{fitMeasures}}
#' 
#' @references Awang, Z. (2015). \emph{SEM Made Simple: A Gentle Approach to Learning Structural Equation Modeling}. MPWS Publisher.
#' @references Papi, M. (2010). The L2 motivational self system, L2 anxiety, and motivated behavior: A structural equation modeling approach. \emph{System, 38}(3), 467–479. \doi{10.1016/j.system.2010.06.011}
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # Load the lavaan package
#' library(lavaan)
#' 
#' # 1. Define a simple Confirmatory Factor Analysis (CFA) model
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' 
#' # 2. Fit the model using the built-in HolzingerSwineford1939 dataset
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#' 
#' # 3. Extract the fit measures
#' my_fit_measures <- fitMeasures(fit)
#' 
#' # 4. Run the evaluation function
#' eval_table <- lavaanFit.indicators.table(my_fit_measures)
#' 
#' # Print the resulting table
#' print(eval_table)
#' }
lavaanFit.indicators.table <- function(fitMeasures) {
  
  # Helper function to safely extract values. Returns NA if the measure is missing.
  safe_extract <- function(x, name) {
    if (name %in% names(x)) {
      return(as.numeric(x[[name]]))
    } else {
      return(NA_real_)
    }
  }
  
  # Safely calculate chiSq / df
  chisq_val <- safe_extract(fitMeasures, "chisq")
  df_val <- safe_extract(fitMeasures, "df")
  chisq_df <- if (!is.na(chisq_val) && !is.na(df_val) && df_val != 0) chisq_val / df_val else NA_real_
  
  # 1. Column: Names of indices
  indices <- c("chiSq pval", "chiSQ/df", "GFI", "AGFI", "NFI", "NNFI", 
               "CFI", "RMSEA", "SRMR", "RFI", "TLI", "PNFI", "IFI")
  
  # 2. Column: Accepted levels
  accepted <- c(">.05", "<3.0", ">.95 / >.90(A)", ">.90", ">.90", ">.90", 
                ">.90", "<.05 / <.08(A)", "<.08", ">.90", ">.90", ">.50", ">.90")
  
  # 3. Column: Extracted current levels
  current_level <- c(
    safe_extract(fitMeasures, "pvalue"),
    chisq_df,
    safe_extract(fitMeasures, "gfi"),
    safe_extract(fitMeasures, "agfi"),
    safe_extract(fitMeasures, "nfi"),
    safe_extract(fitMeasures, "nnfi"),
    safe_extract(fitMeasures, "cfi"),
    safe_extract(fitMeasures, "rmsea"),
    safe_extract(fitMeasures, "srmr"),
    safe_extract(fitMeasures, "rfi"),
    safe_extract(fitMeasures, "tli"),
    safe_extract(fitMeasures, "pnfi"),
    safe_extract(fitMeasures, "ifi")
  )
  
  # Helper function for evaluation logic that handles NAs
  evaluate_fit <- function(val, type) {
    if (is.na(val)) return(NA_character_) # If value is missing, return NA string
    
    if (type == "pval") return(ifelse(val > .05, "Satisfactory", "Poor"))
    if (type == "chisq_df") return(ifelse(val < 3, "Satisfactory", "Poor"))
    if (type == "gfi") return(ifelse(val > .95, "Satisfactory", ifelse(val > .90, "Satisfactory (A)", "Poor")))
    if (type == "rmsea") return(ifelse(val < .05, "Satisfactory", ifelse(val < .08, "Satisfactory (A)", "Poor")))
    if (type == "srmr") return(ifelse(val < .08, "Satisfactory", "Poor"))
    if (type == "pnfi") return(ifelse(val > .50, "Satisfactory", "Poor"))
    
    # Default for the rest (AGFI, NFI, NNFI, CFI, RFI, TLI, IFI) is > 0.90
    return(ifelse(val > .90, "Satisfactory", "Poor"))
  }
  
  # 4. Column: Evaluations
  evaluations <- c(
    evaluate_fit(current_level[1], "pval"),
    evaluate_fit(current_level[2], "chisq_df"),
    evaluate_fit(current_level[3], "gfi"),
    evaluate_fit(current_level[4], "default"),
    evaluate_fit(current_level[5], "default"),
    evaluate_fit(current_level[6], "default"),
    evaluate_fit(current_level[7], "default"),
    evaluate_fit(current_level[8], "rmsea"),
    evaluate_fit(current_level[9], "srmr"),
    evaluate_fit(current_level[10], "default"),
    evaluate_fit(current_level[11], "default"),
    evaluate_fit(current_level[12], "pnfi"),
    evaluate_fit(current_level[13], "default")
  )
  
  # Bind everything into a structured data.frame
  result <- data.frame(
    Index = indices,
    `Current level` = current_level,
    `Accepted level` = accepted,
    Evaluation = evaluations,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  return(result)
}