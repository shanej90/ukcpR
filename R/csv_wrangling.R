#' read_observation_csv
#'
#' Read a UKCP observation data .csv file and handle some basic processing.
#' @param filepath Location of the file.
#' @param convert_coordinates Convert coordinates (if you are using them instead of named area) from northings/eastings to latitude/longitude.
#'
#' @return A dataframe with the processed data (and pertinent metadata) in tall format.
#'
#' @examples
#' \dontrun{
#' read_observation_csv(
#'   "filename.csv"
#')
#'}
#'
#' @export

read_observation_csv <- function(
  filepath,
  convert_coordinates = T
) {

  #error handling---------------------------
  if(!is.logical(convert_coordinates)) {
    stop("`convert_coordinates` must be T/TRUE or F/FLASE (unquoted).")
  }

  if(!is.character(filepath)) {
    stop("`fielpath` must be a string pointing to the desired by-month UKCP .csv file.")
  }

  #get pertinent metadata
  metric <- read.csv(
    filepath,
    stringsAsFactors = F,
    skip = 7,
    nrows = 1,
    header = F
  ) |>
    dplyr::pull(V2)

  units <- stringr::str_extract(metric, "(?<=\\()(.*)(?=\\))")

  time_period <- read.csv(filepath, stringsAsFactors = F, skip = 6, nrow = 1, header = F) |>
    dplyr::pull(V2)

  grid_size <- read.csv(filepath, stringsAsFactors = F, skip = 4, nrow = 1, header = F) |>
    dplyr::pull(V2)

  area <- read.csv(filepath, nrows = 1, skip = 1, stringsAsFactors = F, header = F) |>
    dplyr::pull(V2)

  temporal_avg <- read.csv(filepath, nrows = 1, skip = 5, stringsAsFactors = F, header = F) |>
    dplyr::pull(V2)

  #check area type - is it coordinate based or is it a named area?
  if(stringr::str_detect(grid_size, "grid")) {

    is_grid <- T

  } else {

    is_grid <- F

  }

  #check if data is 'bbox' (ie, there is a grid of readings rather than a table)
  if(stringr::str_count(area, " ") == 3) {

    bbox <- T

  } else {

    bbox <- F

  }

  if(bbox == T) {

    #create main dataframe
    df <- read.csv(
      filepath,
      stringsAsFactors = F,
      skip = 12,
      header = F
      ) |>
      #convert all fields to numeric for ease
      dplyr::mutate(dplyr::across(-V1, as.numeric)) |>
      #format date and fill blanks
      dplyr::mutate(date = strptime(V1, format = "%Y-%m-%d")) |>
      tidyr::fill(date)

    #extract eastings to use as column headings
    col_heads <- unlist(as.character(df[2,]))
    col_heads[1] <- "northings"
    col_heads[length(col_heads)] <- "date"
    colnames(df) <- col_heads

    #put into tall format and filter out redundant rows
    df <- df |>
      dplyr::mutate(
        year = lubridate::year(date),
        month = lubridate::month(date),
        season = dplyr::case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3:5) ~ "Spring",
          month %in% c(6:8) ~ "Summer",
          month %in% c(9:11) ~ "Autumn"
        )
        ) |>
      dplyr::select(-date) |>
      tidyr::pivot_longer(
        cols = c(-northings) & !dplyr::contains("year") & !dplyr::contains("month"),
        names_to = "eastings",
        values_to = "value"
        ) |>
      dplyr::filter(!is.na(value) & northings != "--") |>
      #finalise columns
      dplyr::select(-temp) |>
      dplyr::transmute(
        year,
        season = ifelse(temporal_avg != "Annual", season, NA_character_),
        month = ifelse(!temporal_avg %in% c("Annual", "Season"), month, NA_character_),
        measurement = metric,
        measurement_units = units,
        period = time_period,
        spatial_rep = grid_size,
        northings = as.numeric(northings),
        eastings = as.numeric(eastings),
        value
        )

    #convert coordinates if requested
    if(convert_coordinates == T) {

      #perform conversion
      converted_coords <- do_coord_conversion(df$northings, df$eastings)

      #add to dataframe
      df <- df |>
        dplyr::mutate(
          northings = converted_coords[["latitude"]],
          eastings = converted_coords[["longitude"]]
        ) |>
        dplyr::rename(
          latitude = northings,
          longitude = eastings
        )

    }

  } else {

    #sort area
    eastings <- stringr::word(area, 1)
    northings <- stringr::word(area, 2)

    #process data
    df <- read.csv(filepath, skip = 10, stringsAsFactors = F) |>
      janitor::clean_names() |>
      dplyr::rename(value = 2) |>
      #format date and fill blanks
      #dplyr::mutate(date = strptime(date, format = "%Y-%m-%d")) |>
      #tidyr::fill(date) |>
      dplyr::mutate(
        year = lubridate::year(date),
        month = lubridate::month(date),
        season = dplyr::case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3:5) ~ "Spring",
          month %in% c(6:8) ~ "Summer",
          month %in% c(9:11) ~ "Autumn"
        )
      ) |>
      dplyr::transmute(
        year,
        season = ifelse(temporal_avg != "Annual", season, NA_character_),
        month = ifelse(!temporal_avg %in% c("Annual", "Season"), month, NA_character_),
        date = ifelse(temporal_avg == "Daily", date, NA),
        spatial_rep = grid_size,
        measurement = metric,
        measurement_units = units,
        eastings = eastings,
        northings = northings,
        value
      )

    #if convert_coordinates = T, convert from northings/eastings to latitude/longitude
    if(convert_coordinates == T & is_grid == T) {

      converted <- do_coord_conversion(
        df$northings |> as.numeric(),
        df$eastings |> as.numeric()
        )

      df <- df |>
        dplyr::mutate(
          northings = converted[["latitude"]],
          eastings = converted[["longitude"]]
        ) |>
        dplyr::rename(
          latitude = northings,
          longitude = eastings
        )

    }

  }

  return(df)

}


#' read_projection_csv
#'
#' Read a UKCP projection data .csv file(s) and handle some basic processing.
#' @param filepath Location of the file.
#' @param convert_coordinates Convert coordinates from northings/eastings to latitude/longitude.
#'
#' @return A dataframe with the processed data (and pertinent metadata) in tall format.
#'
#' @examples
#' \dontrun{
#' read_projection_csv(
#'   "filename.csv"
#')
#'}
#'
#' @export

read_projection_csv <- function(filepath, convert_coordinates = T) {

  #error handling---------------------------
  if(!is.logical(convert_coordinates)) {
    stop("`convert_coordinates` must be T/TRUE or F/FLASE (unquoted).")
  }

  if(!is.character(filepath)) {
    stop("`fielpath` must be a string pointing to the desired by-month UKCP .csv file.")
  }

  #get pertinent metadata
  metric <- read.csv(
    filepath,
    stringsAsFactors = F,
    skip = 9,
    nrows = 1,
    header = F
  ) |>
    dplyr::pull(V2)

  units <- stringr::str_extract(metric, "(?<=\\()(.*)(?=\\))")

  min_yr <- read.csv(filepath, stringsAsFactors = F, skip = 10, nrow = 1, header = F) |>
    dplyr::pull(V2)

  max_yr <- read.csv(filepath, stringsAsFactors = F, skip = 11, nrow = 1, header = F) |>
    dplyr::pull(V2)

  year_range <- paste0(min_yr, " - ", max_yr)

  grid_size <- read.csv(filepath, stringsAsFactors = F, skip = 6, nrow = 1, header = F) |>
    dplyr::pull(V2)

  model_ids <- read.csv(filepath, stringsAsFactors = F, skip = 2, nrow = 1, header = F) |>
    dplyr::pull(V2)

  warming_scenario <- read.csv(filepath, stringsAsFactors = F, skip = 4, nrow = 1, header = F) |>
    dplyr::pull(V2)

  temporal_avg <- read.csv(filepath, stringsAsFactors = F, skip = 7, nrow = 1, header = F) |>
    dplyr::pull(V2)

    #create main dataframe
    df <- read.csv(
      filepath,
      stringsAsFactors = F,
      skip = 12,
      header = F
    ) |>
      #convert all fields to numeric for ease
      dplyr::mutate(dplyr::across(-V1, as.numeric)) |>
      #format date and fill blanks
      dplyr::mutate(date = strptime(V1, format = "%Y-%m-%d")) |>
      tidyr::fill(date)

    #extract eastings to use as column headings
    col_heads <- unlist(as.character(df[2,]))
    col_heads[1] <- "northings"
    col_heads[length(col_heads)] <- "date"
    colnames(df) <- col_heads

    #put into tall format and filter out redundant rows
    df <- df |>
      dplyr::mutate(
        year = lubridate::year(date),
        month = lubridate::month(date),
        season = dplyr::case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3:5) ~ "Spring",
          month %in% c(6:8) ~ "Summer",
          month %in% c(9:11) ~ "Autumn"
        )
      ) |>
      dplyr::select(-date) |>
      tidyr::pivot_longer(
        cols = c(-northings) & !dplyr::contains("year") & !dplyr::contains("month"),
        names_to = "eastings",
        values_to = "value"
      ) |>
      dplyr::filter(!is.na(value) & northings != "--") |>
      #finalise columns
      dplyr::select(-temp) |>
      dplyr::mutate(
        year,
        season = ifelse(temporal_avg != "Annual", season, NA_character_),
        month = ifelse(!temporal_avg %in% c("Annual", "Season"), month, NA_character_),
        measurement = metric,
        measurement_units = units,
        period = time_period,
        spatial_rep = grid_size,
        northings = as.numeric(northings),
        eastings = as.numeric(eastings),
        models = model_ids,
        climate_scenario = warming_scenario
      )

    #convert coordinates if requested
    if(convert_coordinates == T) {

      #perform conversion
      converted_coords <- do_coord_conversion(df$northings, df$eastings)

      #add to dataframe
      df <- df |>
        dplyr::mutate(
          northings = converted_coords[["latitude"]],
          eastings = converted_coords[["longitude"]]
        ) |>
        dplyr::rename(
          latitude = northings,
          longitude = eastings
        )

    }


  return(df)

}

#' do_coord_conversion
#'
#' Helper function to convert northings/eastings to latitude/longitude respectively.
#' @param northings Numeric vector of northings to be converted.
#' @param eastings Numeric vector of eastings to be converted.
#'
#' @return A list object containing two vectors, one for latitude and one for longitude.
#'
#' @examples
#' \dontrun{
#' do_coord_conversion(
#'  292500,
#'  72500
#' )
#' }
#'
#' @export


do_coord_conversion <- function(northings, eastings) {

  #define the UK coordinate projection
  uk_projection <- sf::st_crs(27700)

  #create spatial points object with coords
  north_east <- data.frame(eastings = eastings, northings = northings)
  points <- sf::st_as_sf(north_east, coords = c("eastings", "northings"), crs = uk_projection)

  #convert to latitude/longitude
  lat_long <- sf::st_transform(points, "+proj=longlat +datum=WGS84")

  #extract lat/long
  lat <- sf::st_coordinates(lat_long)[, "Y"]
  long <- sf::st_coordinates(lat_long)[, "X"]

  return(list("latitude" = lat, "longitude" = long))

}
