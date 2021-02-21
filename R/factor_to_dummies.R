


#' Convert Factor To Dummy Variables
#' @description Convert factor to columns of dummy variables
#'
#' @param var variable of choice; a vector of class factor
#' @param dummy_levels vector of character; corresponds to the levels in the vector you would
#'  like to generate dummy columns for; defaults to the levels of var;
#'  If you want a dummy for just one or two of the levels specify that here
#' @param varname_prefix character to prefix the names of the columns returned by this function;
#'  defaults to NULL in which case variable levels alone are translated to column names
#' @param nas_as_zeros a logical for if NAs in the factor should be treated as 0s in the dummies
#'  or if they should be treated NAs; default is treating NAs as NAs
#'
#'
#' @importFrom dplyr setdiff
#' @importFrom purrr map_dfc set_names
#' @importFrom tibble tibble
#' @importFrom magrittr '%>%'
#'
#' @examples
#'
#' df <- data.frame(
#'   gender = as.factor(rep(c("Man", "Woman", "Non-binary"),3)),
#'   blood_type = as.factor(rep(c("AB", "O", "A"),3))
#' )
#'
#' df2 <- dplyr::bind_cols(
#'   df,
#'   #to make dummy variables for the gender categories
#'   factor_to_dummies(df$gender)
#'   )
#'
#' df3 <- dplyr::bind_cols(
#'   df,
#'   #to make dummy variables for the gender categories and specify gender in the varnames
#'   factor_to_dummies(df$gender, varname_prefix = "gender_")
#'   )
#'
#' #or maybe we just want a dummy for NB
#' df4 <- dplyr::bind_cols(
#'   df,
#'   #to make dummy variables for the gender categories and specify gender in the varnames
#'   factor_to_dummies(df$gender, dummy_levels = c("Non-binary"))
#'   )
#'
#' @return tibble of dummies
#' @export

factor_to_dummies <- function(var, dummy_levels = levels(var), varname_prefix = NULL, nas_as_zeros = F){
  #TO DO: have this work with characters based on unique values too
  #if not a factor, ERROR!
  if(!is.factor(var)){
    stop("var is not class factor. Please convert it to a factor")
  }
  #if they gave us a level not in factor, ERROR!
  if(length(dplyr::setdiff(dummy_levels, levels(var))) > 0){
    stop("one or more of the dummy_levels provided is not a level in var")
  }

  new_cols <- purrr::map_dfc(dummy_levels, function(dummy_level){
    #if nas should be nas in dummy
    if(nas_as_zeros == F){
      tmp <- ifelse(var == dummy_level, 1L, 0L)
    }
    #if nas should be 0s in dummy
    else{
      tmp <- ifelse(var %in% c(dummy_level), 1L, 0L)
    }

    #if they supplied a prefix, let's use it, else just use the level
    varname <- ifelse(
      is.null(varname_prefix),
      dummy_level,
      paste0(varname_prefix, dummy_level))
    #setting the variable name
    tmp <- tmp %>% tibble::tibble() %>%
      purrr::set_names(varname)
    return(tmp)
  })
  return(new_cols)
}
