# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble",
               'dplyr',
               'sf',
               'readxl',
               'stringr',
               'tidyr',
               'data.table',
               'readr',
               'igraph',
               'statnet',
               'ergm') # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(

# Read in data ------------------------------------------------------------
  tar_target(county_sf,
             readRDS('C:/R_Rstudio/Jake_Project/mortality_geostan/data/model_counties_filtered.rds') |>
               st_transform(crs = 4326) |>
               arrange(GEOID)),
  tar_target(
    county_sci_fp,
    here::here('data/county_county.parquet'),
    format = 'file'
  ),

  tar_target(county_distances,
             getCountyDistances(county_sf)
             ),

  tar_target(county_distance_edgelist,
             distanceMatrixtoEdgeList(county_distances)),
  tar_target(census_population_fp,
             '//tsclient/C/R_Rstudio/Projects/mortality_ratios/data/census_populations/2000-2022-pop.csv'),
  tar_target(census_population,
             read_csv(census_population_fp) |>
               filter(YEAR == '2021', AGEGRP == 0) |>
               select(STATE, COUNTY, YEAR, TOT_POP) |>
               mutate(fipscode = paste0(STATE, COUNTY)),
             format = 'parquet'),
  tar_target(
    county_sci,
    arrow::read_parquet(county_sci_fp) |>
      # filter out fips for territories
      filter(!str_starts(user_loc, '60|66|69|72|78'),
             !str_starts(fr_loc, '60|66|69|72|78')),
    format = 'parquet'
  ),

  tar_target(unique_sci_fips,
             unique(county_sci$user_loc)),
  tar_target(unique_consol_fips,
             unique(wider_consol_dat$fipscode)),

  tar_target(
    consol_dat_fp,
    "//cdc.gov/locker/CCHP_NCCD_DDT_Data1/epistat/Surveillance/Surveillance and Trends Reporting System/GRASP/NEW_Jan2018/Datasets/consol_dat_2023_0530_v3.sas7bdat",
    format = 'file'
  ),
  # Master GRASP data, but doens't have 2021
  tar_target(
    consol_dat,
     haven::read_sas(consol_dat_fp) |>
      filter(YearID == '2020'),
    format = 'parquet'
  ),

  tar_target(
    data_dict_fp,
    '//cdc.gov/locker/CCHP_NCCD_DDT_Data1/epistat/Surveillance/Surveillance and Trends Reporting System/GRASP/NEW_Jan2018/Lookup tables/2023_1010_master_lookup.xlsx',
    format = 'file'
  ),

  tar_target(
    data_dict,
    read_excel(data_dict_fp) |>
      left_join(x = _, read_excel(data_dict_fp, sheet = 2)),
    format = 'parquet'),

  tar_target(
    data_dict2,
    read_excel(data_dict_fp, sheet = 2),
    format = 'parquet'
  ),

  tar_target(consol_dat_dict_combined,
    joinDataWithDictionary(consol_dat, data_dict) |>
      bind_rows(inc_2020) |>
      select(fipscode, Geolevel, YearID, ends_with('Name'), contains('Estimate'),
             contains('Limit'), everything()) |>
      filter(RaceName == 'Total',
             GenderName == 'Total',
             AgeName == 'Age-Adjusted',
             EducationName == 'Total',
             (IndicatorName == 'Newly Diagnosed Diabetes' & EstimateName == 'Number') |
             (EstimateName == 'Percentage' & IndicatorName == 'Diagnosed Diabetes'),
             Geolevel == 'County'),
    format = 'parquet',

  ),

  tar_target(inc_2020_fp, '//cdc.gov/locker/CCHP_NCCD_DDT_Data1/epistat/USDSS/GRASP Datasets/County/countyincidence.sas7bdat',
             format = 'file'),
  tar_target(inc_2020,
             haven::read_sas(inc_2020_fp) %>%
               mutate(across(contains('ID'), ~as.character(.x))) |>
               filter(EstimateID == 72, GenderID == 0, AgeID == 100) %>%
               rename(IndicatorID = indicatorID,
                      fipscode = FIPSCode,
                      DataSourceID = DatasourceID,
                      Geolevel = GeoLevel) |>
               mutate(IndicatorName = 'Newly Diagnosed Diabetes'),
             format = 'parquet'),

  tar_target(wider_consol_dat,
             pivot_wider(
               consol_dat_dict_combined,
               names_from = IndicatorName,
               values_from = Estimate,
               id_cols = fipscode
             ) |>
               filter(fipscode %in% unique_sci_fips) |>
               left_join(dsmes_programs, by = c('fipscode' = 'County_FIPS')) |>
               left_join(select(census_population, fipscode, population = TOT_POP),
                         by = 'fipscode') |>
               mutate(log_pop = log(population)) |>
               arrange(fipscode),
             format = 'parquet'),

  # Have to remove loops in network to make the graph later. Don't want to analyze loops anyway
  tar_target(edge_dat,
             joinDiabetesData(county_sci, wider_consol_dat) |>
               arrange(user_loc, fr_loc) |>
               filter(user_loc != fr_loc) |>
               mutate(log_scaled_sci = log(scaled_sci)) |>
               arrange(user_loc, fr_loc),
             format = 'parquet'
             ),

  tar_target(complete_wider_consol_dat,
             wider_consol_dat |>
               mutate(fips_fact = factor(fipscode) |>
                        forcats::fct_expand(unique(edge_dat$user_loc))) |>
               complete(fips_fact) |>
               mutate(fipscode = coalesce(fipscode, fips_fact)) |>
               arrange(fipscode),
             format = 'parquet'
             ),

  tar_target(dsmes_programs_fp,
             '//tsclient/C/R_Rstudio/Projects/UrbanRuralPrevalence/output/tallied_programs.csv',
             format = 'file'),
  tar_target(dsmes_programs,
             read_csv(dsmes_programs_fp)),

  tar_target(adj_matrix_diab,
             makeAdjMatrix(edge_dat, 'diab_relationship')),

  tar_target(adj_matrix_dsmes,
             makeAdjMatrix(edge_dat, 'dsmes_relationship')),

  tar_target(adj_matrix_distances,
             makeAdjMatrix(county_distance_edgelist, 'distance_km')),

  tar_target(adj_matrix_sci,
             makeAdjMatrix(edge_dat, 'log_scaled_sci')),

  tar_target(adj_matrix_prev_sum,
             makeAdjMatrix(edge_dat, 'prev_sum')),

# This is the network where the edges are binary and reflect the presence of high tertile diab prev.
# SCI is added as a network attribute
  tar_target(
    diab_prev_binary_network,
    setupNetwork(
      adj_matrix_diab,
      adj_matrix_sci,
      adj_matrix_distances,
      complete_wider_consol_dat
    ),
    deployment = 'main'
  ),
  tar_target(
    dsmes_binary_network,
    setupNetwork(
      adj_matrix_dsmes,
      adj_matrix_sci,
      adj_matrix_distances,
      complete_wider_consol_dat
    )
  ),
  # Prevalence diabetes sum of diabetes as a continuous edge
  # tar_target(
  #   prev_sum_network,
  #   setupNetwork(
  #     adj_matrix_prev_sum,
  #     adj_matrix_sci,
  #     adj_matrix_distances,
  #     complete_wider_consol_dat
  #   )
  # ),

  # Test the model where diabetes prevalence is binary and sci as edgecov

  tar_target(diab_sci_only_model,
             ergm(diab_prev_binary_network ~ edges +
                    edgecov('log_scaled_sci'))
             ),
  tar_target(dsmes_sci_only_model,
             ergm(dsmes_binary_network ~ edges +
                    edgecov('log_scaled_sci'))
             ),

  tar_target(diab_sci_distance_model,
             ergm(
               diab_prev_binary_network ~ edges +
                 edgecov('log_scaled_sci') +
                 edgecov('distance_km')
             )
             ),
  tar_target(dsmes_sci_distance_model,
             ergm(
               dsmes_binary_network ~ edges +
                 edgecov('log_scaled_sci') +
                 edgecov('distance_km')
             )
             ),
  tar_target(
    diab_sci_distance_pop_model,
    ergm(
      diab_prev_binary_network ~ edges +
        edgecov('log_scaled_sci') +
        edgecov('distance_km') +
        nodecov('log_pop')
    )
  ),
  tar_target(
    dsmes_sci_distance_pop_model,
    ergm(
      dsmes_binary_network ~ edges +
        edgecov('log_scaled_sci') +
        edgecov('distance_km') +
        nodecov('log_pop')
    )
  ),

  tar_target(test_model,
             ergm(diab_prev_binary_network ~ edges + edgecov('log_scaled_sci'))
             ),
  # Test model with diabetes prevalence continuous
  # tar_target(test_model2,
  #            ergm(prev_sum_network ~ edges + nodecov('obesity') + edgecov('log_scaled_sci'))
  #            ),

  tar_target(test_glm_logistic,
             {
               edge_dat |>
                 group_by(user_loc) |>
                 summarise(scaled_sci = mean(scaled_sci)) |>
                 left_join(complete_wider_consol_dat, by = c('user_loc' = 'fipscode')) |>
                 glm(`Diagnosed Diabetes` ~ scaled_sci + Obesity + `Physical Inactivity`,
                     data = _,
                     family = Gamma('identity'))
             })
)
