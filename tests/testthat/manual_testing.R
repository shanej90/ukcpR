#manual testing file due to difficulty of using testthat with API calls that can take minutes to run etc

# devtools::load_all()
#
# #observations csv job--------------------
#
# call_results <-  start_ukcp_observation_csv_job(
#   label = "testtthat-good-query",
#   identifier = "LS6_Subset_02",
#   area_type = "point",
#   area_details = c(292500, 72500),
#   collection = "land-obs_5",
#   temp_avg = "day",
#   year_range = c(1960, 2021),
#   variable = "rainfall"
# )
#
# get_files <- download_ukcp_csv_results(call_results$status_url, output_folder = "assets")
#
#  example_csv <- read_observation_csv("assets/subset_2023-07-03T09-25-13.csv")
