
#' Read Data Directory
#' @description Read a data directory to a data frame or a list
#'
#' @param directory_path path to the data directory
#' @param map_fxn the map_fxn to use; defaults to map_dfr which is ideal for creating a dataframe
#' @param read_fxn the function used to read in individual files; defaults to readr::read_csv
#' @param filename_as_var a logical for whether or not the filename should be stored as a variable; defaults to FALSE
#' this can be helpful if individual data files do not have variables that can be used to distinguish the files from each other
#' @param exclude_files_regex a regex that can be used indicate which files in a directory to exclude; defaults to NULL in which case all files are read
#'
#' @importFrom stringr str_detect str_remove
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv
#' @importFrom dplyr mutate
#' @importFrom magrittr '%>%'
#'
#' @examples
#' \dontrun{
#' directory_path <- "Data"
#' #to read the data into a data frame
#' df <- read_data_directory(directory_path, purrr::map_dfr, haven::read_csv)
#' #to read the data into list
#' data_list <- read_data_directory(directory_path, purrr::map, haven::read_csv)
#'
#' #with defaults you can also just read data from csvs to a dataframe
#' df <- read_data_directory(directory_path)
#' }
#'
#' @return data return depends on map_fxn
#' @export

read_data_directory <- function(directory_path, map_fxn = purrr::map_dfr, read_fxn = readr::read_csv,
                                filename_as_var = FALSE, exclude_files_regex = NULL){
  # if no trailing / let's add it
  if(!stringr::str_detect(directory_path, "/$")){
    directory_path <- paste0(directory_path, "/")
  }
  # all files
  data_files <- list.files(directory_path)
  # if they supplied some files to exclude, exclude em
  if(!(is.null(exclude_files_regex))){
    data_files <- data_files[!stringr::str_detect(data_files, exclude_files_regex)]
  }
  if(length(data_files) == 0){
    stop("There are no data files to read. Check your directory and exclude_files_regex.")
  }
  #paste path on
  data_files <- paste0(directory_path, data_files)

  #the bulk of the fxn
  return(
    map_fxn(data_files, function(f){
      # reads file
      tmp <- read_fxn(f)
      # adds the filename if it is desired
      # (helpful if no var to identify different portions of the stacked data)
      if(filename_as_var == T){
        tmp <- tmp %>%
          mutate(
            # grabbing just the file name
            filename = stringr::str_remove(f, directory_path)
            )
      }
      return(tmp)
    })
  )

}

