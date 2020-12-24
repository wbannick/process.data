
#' Read Data Directory
#' @description Read a data directory to a data frame or a list
#'
#' @param directory_path path to the data directory
#' @param map_fxn the map_fxn to use; defaults to map_dfr which is ideal for creating a dataframe
#' @param read_fxn the function used to read in individual files; defaults to read.csv
#'
#'
#' @importFrom stringr str_detect str_remove
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv
#' @importFrom dplyr mutate
#' @importFrom magrittr '%>%'
#'
#' @examples
#' \dontrun{
#' directory_path <- "Data/PPIC"
#' #to read the data into a data frame
#' df <- read_data_directory(directory_path, purrr::map_dfr, haven::read_sav)
#' #to read the data into list
#' data_list <- read_data_directory(directory_path, purrr::map, haven::read_sav)
#'
#' #with defaults you can also just read data from csvs to a dataframe
#' df <- read_data_directory(directory_path)
#' }
#'
#' @return data return depends on map_fxn
#' @export

read_data_directory <- function(directory_path, map_fxn = purrr::map_dfr, read_fxn = readr::read_csv){
  #if no trailing / let's add it
  if(!stringr::str_detect(directory_path, "/$")){
    directory_path <- paste0(directory_path, "/")
  }
  #all files
  data_files <- list.files(directory_path)
  #paste path on
  data_files <- paste0(directory_path, data_files)

  #the bulk of the fxn
  return(
    map_fxn(data_files, function(f){
      #reads file
      read_fxn(f) %>%
        #adds file identifier
        mutate(
          #grabbing just the file name
          filename = stringr::str_remove(f, directory_path)
        )
    })
  )

}

