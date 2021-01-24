
#' Adjust Factor Case
#' @description Adjust the case of a factor variable.
#' For example make its categories title case instead of lower case while keeping the levels in the same order
#'
#' @param var variable of choice; a vector of class factor
#' @param case_fxn a function to be used to adjust the case of var; defaults to str_to_title
#'
#' @importFrom stringr str_to_title
#' @importFrom magrittr '%>%'
#'
#' @examples
#'
#' df <- data.frame(
#'   gender = as.factor(c("man", "woman", "non-binary")),
#'   blood_type = as.factor(c("ab", "o", "a"))
#' )
#'
#' #to make it title case
#' df$gender <- factor_case(df$gender)
#' #to make it all caps
#' df$blood_type <- factor_case(df$blood_type, toupper)
#'
#' @return vector of class factor
#' @export

factor_case <- function(var, case_fxn = stringr::str_to_title){
  if(!is.factor(var)){
    stop("var must already be a factor")
  }
  #adjust levels
  corrected_levels <- levels(var) %>% case_fxn
  #adjust var
  var <- as.character(var) %>% case_fxn %>% factor(levels = corrected_levels)

  return(var)
}
