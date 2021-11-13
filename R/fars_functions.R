#' Read FARS data
#'
#' Reads a .csv file containing FARS data into a tibble
#'
#' @details
#' This function takes the path to a file containing FARS data. The FARS data
#' is in .csv.bz2 format, which will be decompressed and read automatically into a
#' tibble.
#'
#' Requires the \code{readr} and \code{tibble} packages.
#'
#' @seealso [fars_read_years()], [fars_summarize_years()], [fars_map_state()]
#'
#' @param filename path to the file
#' @return a tibble
#'
#' @section Errors:
#' Will raise an error if the file is not found
#'
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' fars_data <- fars_read("path/to/fars_data.csv")
#' }
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}

#' Generate FARS filename
#'
#' Generates the properly formatted file name for FARS dataset based on the year.
#' Filenames are in the form "accident_<year>.csv.bz2".
#'
#' @seealso [fars_read_years()], [fars_map_state()]
#'
#' @param year year
#' @return a valid FARS data file name
#'
#' @examples
#' \dontrun{
#' filename <- make_filename(2013)
#' }
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Pre-format dataframe for selection
#'
#' @details
#' This function takes a list of years and attempts to read the FARS
#' data file into a data frame for each year. The year is then appended
#' as a column to each data frame, Then it along with the MONTH column
#' are extracted. The result is a 2-column data frame with the same number
#' of rows as the original FARS data for that year containing the MONTH column
#' from the FARS data and the year of the data file.
#'
#' This function must be called from the directory where the
#' data is located.
#'
#' This function requires \code{dplyr}
#'
#' @seealso [fars_summarize_years()]
#'
#' @param years a list of years
#' @return a list of tribbles with two columns, MONTH and year
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @section Errors:
#' This function will raise an error if a FARS-formatted file
#' for that year cannot be located in the current directory
#'
#' @examples
#' \dontrun{
#' years <- fars_read_years(list(2013, 2014))
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Returns monthly number of accidents by year
#'
#' @details
#' This function takes in a list of years and returns a data frame
#' containing the monthly number of accidents for each year. The first column
#' of the data frame contains the month, with subsequent columns each corresponding to
#' a year and each row the number of accidents that occurred in that month for that year.
#'
#' This function requires \code{dplyr} and \code{tidyr}.
#'
#' @seealso
#'
#' @param years list of years
#' @return data frame whose first column is the month and whose subsequent columns
#' contain monthy accident counts for each year passed to the function
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' annual_summary <- fars_summarize_years(list(2013, 2014))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plots the locations of accidents on a state map
#'
#' @details
#' This function takes number of a state in the FARS dataset along with a year
#' and uses the \code{maps} package to plot the locations of each accident on a
#' map of the state.
#'
#' This function requires \code{dplyr} and \code{maps}
#'
#' @param state.num number of the state in the FARS dataset
#' @param year the year
#' @return \code{NULL} if no data, else shows plot
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#'
#' @section Errors:
#' Raises an error of the state number does not match that
#' of any in the FARS dataset
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
