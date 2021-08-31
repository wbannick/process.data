

#' As Clean Numeric
#' @description Read a data directory to a data frame or a list
#'
#' @param x object to be coerced
#' @param removal_pattern regular expression to be used in removal of special characters
#' @param show_nas a logical for whether or not to print a message of up to 5 characters that are converted to NAs
#'
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_remove_all
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#'
#' # this is perfect if you have a column in a dataframe which you would like to
#' # convert a poorly formated character vector of numbers to numeric
#' df <- df %>%
#'   mutate(
#'     # sometimes variables in dollars have $ and/or ,s
#'     gdp = as_clean_numeric(gdp),
#'     # sometimes these have percent signs
#'     percent_under30 = as_clean_numeric(percent_under_30)
#'   )
#'
#' }
#'
#' @return object of class numeric
#' @export
#'

as_clean_numeric <- function(
  x, removal_pattern = c("\\$|\\,|\\%|\\#"), show_nas = FALSE){
  # remove all predicted unwanted characters. defaults should do well with most cases
  x <- stringr::str_remove_all(x, removal_pattern)

  if(show_nas == TRUE){
    # find the ones that will trip the issue (can improve here)
    na_numerics <- suppressWarnings(x[is.na(as.numeric(x))])
    # print helpful warning message if there are na numerics
    if(length(na_numerics) != 0){
      na_numerics <- utils::head(na_numerics, 5) %>% paste(collapse = ", ")
      warning(
        paste0(
          "NAs introduced by coercion. Examples of strings coerced to NAs: ",
          na_numerics)
      )
    }
    # basically runs a second time so could take a while for big data. can improve
    x <- suppressWarnings(as.numeric(x))
    return(x)
  }

  return(as.numeric(x))

}
