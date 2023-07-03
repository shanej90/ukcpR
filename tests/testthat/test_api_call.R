###test that a valid query to the API returns a list
testthat::expect_type(
  start_ukcp_observation_csv_job(
    label = "testtthat-good-query",
    identifier = "LS6_Subset_02",
    area_type = "point",
    area_details = c(292500, 72500),
    collection = "land-obs_5",
    temp_avg = "day",
    year_range = c(1960, 2021),
    variable = "rainfall"
  ),
  "list"
)
