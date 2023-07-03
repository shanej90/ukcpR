#' start_ukcp_observation_csv_job
#'
#' Send a query to the UKCP API for observation data and get a job (that will produce .csv outputs) started.
#' @param label Job label for your request - this will appear in the UKCP API.
#' @param API_KEY Your UKCP API key. Defaults to the `UKCP_API_KEY` environment variable if created.
#' @param identifier Identifier for the dataset you would like to query.
#' @param area_type How area you would like to cover is set up, eg a 'point', 'admin_region' or a 'bbox' (these are not the full set of options) to cover multiple grid cells.
#' @param area_details The name or coordinates (northings/eastings) of the location you are interested in. If coordinates, the the point ID or edges of your boundary. In the latter case, enter as a vector with W, S, E, N boundaries in that order.
#' @param collection The collection the queried dataset is part of.
#' @param temp_avg The time period the data is broken down by, eg 'day' or 'may' (for data for that month, 'mall' for all months).
#' @param year_range Years covered by your call. Enter as a vector in the following format: `c(min_yr, max_yr)`.
#' @param variable The measurement you are trying to extract, eg 'rainfall' or 'sun'.
#'
#' @return A list of information about your job, crucially the URL to use to get the results (when ready).
#'
#' @examples
#' \dontrun{
#'  start_ukcp_observation_csv_job(
#'   label = "ukcpR-query",
#'   identifier = "LS6_Subset_02",
#'   area_type = "point",
#'   area_details = c(292500, 72500),
#'   collection = "land-obs_5",
#'   temp_avg = "day",
#'   year_range = c(1960, 2021),
#'   variable = "rainfall"
#'  )
#'  }
#'
#' @export

start_ukcp_observation_csv_job <- function(
    label,
    API_KEY = Sys.getenv("UKCP_API_KEY"),
    identifier,
    area_type,
    area_details, #for bbox, W, S E, N boundaries, for point, eastings, northings
    collection,
    temp_avg, #could be by month (specify month or all (mall)), day etc
    year_range,
    variable
) {

  #base url
  base <- "https://ukclimateprojections-ui.metoffice.gov.uk/wps"

  #parse vector parameters
  if(is.numeric(area_details)) {
    area_details <- paste(paste0(area_details, ".0"), collapse = "%7C")
  } else {
    area_details <- stringr::str_replace(area_details, " ", "%20")
  }
  year_range <- paste(year_range, collapse = "%7C")

  #parameters
  params <- list(
    service = "WPS",
    request = "Execute",
    version = "1.0.0",
    Identifier = identifier,
    Format = "text/xml",
    Inform = "false",
    Store = "false",
    Status = "false",
    DataInputs = paste0(
      'Area=', paste0(area_type, "%7C", area_details), ';',
      'Collection=', collection, ';',
      'DataFormat=csv;',
      'JobLabel=', label, ';',
      'TemporalAverage=', temp_avg, ';',
      'TimeSlice=', year_range, ';',
      'Variable=', variable, ';'
    ),
    ApiKey = API_KEY
  )

  #make the query
  result <- httr::GET(
    url = base,
    query = params,
    timeout = httr::timeout(15)
  )

  #display error message if required
  if(result$status_code >= 400) {
    err_msg = httr::http_status(result)
    stop(err_msg)
  }

  #parse results
  result_xml <- httr::content(result, type = "text/xml")

  #extract job info
  status_url <- xml2::xml_find_first(result_xml, "//*[local-name()='ExecuteResponse']") |> xml2::xml_attr("statusLocation")
  job_id <- xml2::xml_find_first(result_xml, "//*[local-name()='JobID']") |> xml2::xml_text()
  request_url <- xml2::xml_find_first(result_xml, "//*[local-name()='JobRequestURL']") |> xml2::xml_text()

  return(list("job_id" = job_id, "req_url" = request_url, "status_url" = status_url))

}

#' start_ukcp_projection_csv_job
#'
#' Send a query to the UKCP API for projection data and get a job (that will produce .csv outputs) started.
#' @param label Job label for your request - this will appear in the UKCP API.
#' @param API_KEY Your UKCP API key. Defaults to the `UKCP_API_KEY` environment variable if created.
#' @param identifier Identifier for the dataset you would like to query.
#' @param area_details The coordinates (northings/eastings) of the location you are interested in. Enter as a vector with W, S, E, N boundaries in that order.
#' @param collection The collection the queried dataset is part of.
#' @param temp_avg The time period the data is broken down by, eg 'day' or 'may' (for data for that month, 'mall' for all months).
#' @param year_range Years covered by your call. Enter as a vector in the following format: `c(min_yr, max_yr)`. Note there may be restrictions on which years can be sent depending on which data you are requesting.
#' @param variable The measurement you are trying to extract, eg 'rainfall' or 'sun'.
#' @param climate_change_type Whether you want 'absolute' or 'anomaly' values.
#' @param ensemble_member_set Set of models used to generate projections.
#' @param scenario RCP scenario (rcp26 or rcp85) to gather projections for. Note only variable for global projections.
#' @return A list of information about your job, crucially the URL to use to get the results (when ready).
#'
#' @examples
#' \dontrun{
#'  start_ukcp_projection_csv_job(
#'   label = "ukcpR-query",
#'   identifier = "LS2_Subset_01",
#'   area_details = c(292500, 72500, 293500, 73500),
#'   collection = "land-gcm",
#'   temp_avg = "mall",
#'   year_range = c(2061, 2080),
#'   variable = "tas"
#'  )
#'  }
#'
#' @export


start_ukcp_projection_csv_job <- function(
    label,
    API_KEY = Sys.getenv("UKCP_API_KEY"),
    identifier,
    area_details, #for bbox, W, S E, N boundaries
    collection,
    temp_avg, #could be by month (specify month or all (mall)), annual etc
    year_range,
    variable,
    climate_change_type,
    ensemble_member_set,
    scenario
) {

  #error checking

  #base url
  base <- "https://ukclimateprojections-ui.metoffice.gov.uk/wps"

  #parse vector parameters
  area_details <- paste(paste0(area_details, ".0"), collapse = "%7C")
  year_range <- paste(year_range, collapse = "%7C")

  #data inputs, based on user selection
  if(identifier == "LS2_Subset_01") {

    data_inputs <- paste0(
      'Area=', paste0("bbox%7C", area_details), ';',
      'ClimateChangeType=', climate_change_type, ';',
      'Collection=', collection, ';',
      'EnsembleMemberSet=', ensemble_member_set,
      'DataFormat=csv;',
      'JobLabel=', label, ';',
      'Scenario=', scenario, ';',
      'TemporalAverage=', temp_avg, ';',
      'TimeSlice=', year_range, ';',
      'Variable=', variable, ';'
    )

  } else {

    data_inputs <- paste0(
      'Area=', paste0("bbox%7C", area_details), ';',
      'ClimateChangeType=', climate_change_type, ';',
      'Collection=', collection, ';',
      'EnsembleMemberSet=', ensemble_member_set,
      'DataFormat=csv;',
      'JobLabel=', label, ';',
      'TemporalAverage=', temp_avg, ';',
      'TimeSlice=', year_range, ';',
      'Variable=', variable, ';'
    )

  }

  #parameters
  params <- list(
    service = "WPS",
    request = "Execute",
    version = "1.0.0",
    Identifier = identifier,
    Format = "text/xml",
    Inform = "false",
    Store = "false",
    Status = "false",
    DataInputs = paste0(data_inputs),
    ApiKey = API_KEY
  )

  #make the query
  result <- httr::GET(
    url = base,
    query = params,
    timeout = httr::timeout(15)
  )

  #display error message if required
  if(result$status_code >= 400) {
    err_msg = httr::http_status(result)
    stop(err_msg)
  }

  #parse results
  result_xml <- httr::content(result, type = "text/xml")

  #extract job info
  status_url <- xml2::xml_find_first(result_xml, "//*[local-name()='ExecuteResponse']") |> xml2::xml_attr("statusLocation")
  job_id <- xml2::xml_find_first(result_xml, "//*[local-name()='JobID']") |> xml2::xml_text()
  request_url <- xml2::xml_find_first(result_xml, "//*[local-name()='JobRequestURL']") |> xml2::xml_text()

  return(list("job_id" = job_id, "req_url" = request_url, "status_url" = status_url))

}


#' download_ukcp_csv_results
#'
#' Download the results of your API query, using the status_url returned in `start_ukcp_csv_job`. Polls server once every five seconds.
#' @param status_url URL used to retrieve results from the API.
#' @param output_folder Folder (in working directory) to save .csv files to.
#' @param keep_zip Keep the original zip file (in your working directory root folder). Note the initial name will always be `temp.zip`.
#' @param API_KEY UKCP API key. Defaults to using environment variable `UKCP_API_KEY`.
#'
#' @return Downloads the .zip file held at the `status_url` and extracts the csv(s) within (users can optionally keep the .zip file).
#'
#' @examples
#' \dontrun{
#'  download_ukcp_csv_results(
#'   status_url = "https://ukclimateprojections-ui.metoffice.gov.uk/status/your-id-here",
#'   output_folder = "outputs"
#' )
#' }
#'
#' @export

download_ukcp_csv_results <- function(
    status_url,
    output_folder = "outputs",
    keep_zip = F,
    API_KEY = Sys.getenv("UKCP_API_KEY")
    ) {

  #make sure keep_zip is boolean
  if(!is.logical(keep_zip)) {
    stop("`keep_zip` must be T/TRUE or F/FALSE (unquoted)")
  }

  print("Polling server...")

  #make initial query
  result <- httr::GET(
    url = status_url,
    timeout = httr::timeout(15)
  )

  #display error message if required
  if(result$status_code >= 400) {
    err_msg = httr::http_status(result)
    stop(err_msg)
  }

  #parse results
  result_xml <- httr::content(result, type = "text/xml")

  #status as per result xml and extract child node names
  status <- xml2::xml_find_first(result_xml, "//*[local-name()='Status']")
  status_child <- xml2::xml_name(xml2::xml_children(status))

  #run the above process through a while loop until completed
  while(status_child %in% c("ProcessAccepted", "ProcessStarted")) {

    #UI
    print(paste0("Polling server...last status: ", status_child))

    #make the query
    result <- httr::GET(
      url = status_url,
      timeout = httr::timeout(15)
    )

    #display error message if required
    if(result$status_code >= 400) {
      err_msg = httr::http_status(result)
      stop(err_msg)
    }

    #parse results
    result_xml <- httr::content(result, type = "text/xml")

    #status as per result xml and extract child node names
    status <- xml2::xml_find_first(result_xml, "//*[local-name()='Status']")
    status_child <- xml2::xml_name(xml2::xml_children(status))

    #sleep for 2 seconds to avoid overwhelming API
    Sys.sleep(5)

  }

  #if result completed successfully, grab file and download...
  if(status_child == "ProcessSucceeded") {

    print("Success! Now extracting csv(s)...")

    #get the download URL from the XML response
    download_url <- result_xml |> xml2::xml_find_first("//*[local-name()='FileURL']") |> xml2::xml_text()

    #now you need to append the api key...
    download_url_w_key <- paste0(download_url, "?ApiKey=", API_KEY)

    #create the output directory if needed
    if(!dir.exists(output_folder)) {
      dir.create(output_folder)
    }

    #in the highly unlikely case that a file call temp.zip exists in their outputs folder...
    if(file.exists(paste0(output_folder, "/temp.zip"))) {

      stop("A file called `temp.zip` exists in your chosen output folder.
           This package creates a file with this name.
           Please rename, move or delete the file and try again.")

    }

    #get the csv and save it
    destination <- paste0(output_folder, "/temp.zip")
    download.file(download_url_w_key, destination, mode = "wb")

    #identify the csv file(s)
    files <- unzip(destination, list = T)
    csvs <- files$Name[grepl("csv", files$Name)]

    #for each csv identified, save in the outputs folder
    for (f in csvs) {

      unzip(destination, exdir = output_folder, files = f)

    }

    #delete the zip, if requested
    if(!keep_zip) {
      file.remove(paste0(output_folder, "/temp.zip"))
    }

    print(paste0("Finished - extracted files saved to ", output_folder))


  } else {

    #if unsuccessful, stop and inform user
    stop("Process failed; review your request and try again.")

  }

}

