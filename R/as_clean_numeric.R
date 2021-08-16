

as_clean_numeric <- function(
  x, removal_pattern = c("\\$|\\,|\\%|\\#"), show_nas = FALSE){
  # remove all predicted unwanted characters. defaults should do well with most cases
  x <- str_remove_all(x, removal_pattern)

  if(show_nas == TRUE){
    # find the ones that will trip the issue (can improve here)
    na_numerics <- suppressWarnings(x[is.na(as.numeric(x))])
    na_numerics <- head(na_numerics, 5) %>% paste(collapse = ", ")
    # print helpful warning message
    warning(
      paste0(
        "NAs introduced by coercion. Examples of strings coerced to NAs: ",
           na_numerics)
      )
    # basically runs a second time so could take a while for big data. can improve
    x <- suppressWarnings(as.numeric(x))
    return(x)
  }

  return(as.numeric(x))

}
