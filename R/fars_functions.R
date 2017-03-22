#' @title Read FARS file
#' @description
#' \code{fars_read} Returns a data frame from a file.
#' @param filename character vector of the name of a .csv file to load
#' @return Data frame containing the contents of the file named in \code{filename}
#' @details
#' This function calls read_csv from the readr library to read in a csv file
#' and uses tbl_df to convert the tibble to a data frame.
#' \code{frs_read()} will fail if the file does not exist.
#' @examples
#' \dontrun{
#' fars_read("./my_csv_file.csv")
#' }
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}
#' @title Make a filename
#' @description
#' Creates filenames for loading FARS files from a given year
#' @param year A character or numeric vector of the desired year(s)
#' @return A character vector containing FARS filenames for given years
#' @details
#' Takes a character or numeric vector of target years and returns a character
#' vector of FARS filenames for the target years in the format \code{"accident_year.csv.bz2"}
#' @examples
#' make_filename("2017")
#' make_filename(2017)
#' make_filename(c("2004", "2005", "2006"))
#' make_filename(2010:2016)
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
#' @title Read FARS data from given years
#' @description
#' Reads in FARS data files from a given vector of years.
#' @param years a character or numeric vector of target years from which to load data
#' @return a list of data frames containing data from each target year
#' @details
#' Loads a list of data frames of FARS data from the specified years.
#' @examples
#' \dontrun{
#' fars_read_years(2017)
#' fars_read_years(c("2016", "2017"))
#' fars_read_years(2010:2017)
#' }
#' @export
#' @import dplyr
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

#' @title Summarize FARS data by year
#' @description Summary data by year and month from a list of target years.
#' @param years A character or numeric vector of target \code{years}
#' @return A data frame
#' @details 
#' Loads the FARS files associated with each year, then summarizes by each
#' year and month in the data.
#' Data files must be in .bz format and must have the name format of
#' "accident_year.csv.bz2" for each year in the \code{years} vector.
#' @examples
#' \dontrun{
#' fars_summarize_years(c("2016", "2017"))
#' fars_summarize_years(1980:1990)
#' }
#' @export
#' @import dplyr
#' @importFrom tidyr spread
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}
#' @title Draw a map of accidents by State and Year
#' @description
#' Draw the location of each accident in a given State in a given year.
#' @param state.num A number corresponding to a State in the FARS data.
#' @param year A character or number representing the target year.
#' @return A plot of the accident locations in the State in the target year
#' @details
#' Loads the file with the name \code{"accidnet_year.csv.bz2"} where year
#' corresponds to the value of \code{year}.
#' Calls map::maps and graphics::points to plot the accidnets by latitude and longtude.
#' Will produce an error if the \code{state.num} is incorrect or the file for
#' the given year does not exist.
#' @examples
#' \dontrun{
#' fars_map_state(17, 2016)
#' fars_map_state(21, "2016")
#' }
#' @export
#' @import dplyr
#' @importFrom maps map
#' @importFrom graphics points
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
