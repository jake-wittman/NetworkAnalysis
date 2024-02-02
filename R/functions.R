tar_remake <- function(names) {
  tar_make( {{ names }}, reporter = 'timestamp_positives' )
  tar_load( {{ names }}, envir = .GlobalEnv )
}

`%!in%` <- Negate(`%in%`)

#' Combine dashboard data with dictionary
#'
#' @param consol_dat
#' @param data_dict
#'
#' @return
#' @export
#'
#' @examples
joinDataWithDictionary <- function(consol_dat, data_dict) {
  consol_dat |>
    mutate(across(ends_with('ID'), ~as.character(.x))) |>
    # Get actual names for the IndicatorID
    inner_join(data_dict |>
                 filter(VariableTypeID == 1) |>
                 select(Name, vid),
               by = c('IndicatorID' = 'vid')) |>
    rename(IndicatorName = Name) |>
    left_join(data_dict |>  # Suppress ID join
                filter(VariableTypeID == 4) |>
                select(Name, vid),
              by = c('SuppressID' = 'vid')) |>
    rename(SuppressName = Name) |>
    left_join(data_dict |>  # Race ID join
                filter(VariableTypeID == 5) |>
                select(Name, vid),
              by = c('RaceID' = 'vid')) |>
    rename(RaceName = Name) |>
    left_join(data_dict |>  # Gender ID join
                filter(VariableTypeID == 6) |>
                select(Name, vid),
              by = c('GenderID' = 'vid')) |>
    rename(GenderName = Name) |>
    left_join(data_dict |>  # Age ID join
                filter(VariableTypeID == 7) |>
                select(Name, vid),
              by = c('AgeID' = 'vid')) |>
    rename(AgeName = Name)|>
    left_join(data_dict |>  # Data source ID join
                filter(VariableTypeID == 8) |>
                select(Name, vid),
              by = c('DataSourceID' = 'vid')) |>
    rename(DatasourceName = Name) |>
    left_join(data_dict |>  # Estimate ID join
                filter(VariableTypeID == 9) |>
                select(Name, vid),
              by = c('EstimateID' = 'vid')) |>
    rename(EstimateName = Name) |>
    left_join(data_dict |>  # EducationID join
                filter(VariableTypeID == 10) |>
                select(Name, vid),
              by = c('EducationID' = 'vid')) |>
    rename(EducationName = Name) |>
    left_join(data_dict |>  # MiscID join
                filter(VariableTypeID == 11) |>
                select(Name, vid),
              by = c('MiscID' = 'vid')) |>
    rename(MiscName = Name) |>
    left_join(data_dict |>  # Dataset ID join
                filter(VariableTypeID == 20) |>
                select(Name, vid),
              by = c('DatasetID' = 'vid')) |>
    rename(DatasetName = Name)
}

#' Classify diabetes and join with edge data.
#'
#' Classify diabetes data into categories and use categories to define "relationships"
#' among counties.
#'
#' @param county_sci
#' @param wider_consol_dat
#'
#' @return
#' @export
#'
#' @examples
joinDiabetesData <- function(county_sci, wider_consol_dat) {
  # Calculate tertiles for binary edge connections
  tertiles <- quantile(wider_consol_dat$`Diagnosed Diabetes`, c(0, 0.33, 0.66, 1), na.rm = T)
  wider_consol_dat$tertile <- cut(wider_consol_dat$`Diagnosed Diabetes`,
      breaks = tertiles)
  tertile_levels <- unique(wider_consol_dat$tertile)
  temp <- full_join(county_sci, wider_consol_dat, by = c('user_loc' = 'fipscode'))
  temp <- select(temp, user_loc, fr_loc, scaled_sci, diab_county_1 = tertile)
  temp <- full_join(temp, wider_consol_dat, by = c('fr_loc' = 'fipscode'))
  temp <- select(temp, user_loc, fr_loc, scaled_sci, diab_county_1, diab_county_2 = tertile)
  temp$diab_relationship <-
    ifelse(
      temp$diab_county_1 == levels(tertile_levels)[length(levels(tertile_levels))] &
        temp$diab_county_2 == levels(tertile_levels)[length(levels(tertile_levels))],
      1,
      0
    )

  # Add diabetes prevalence to do a sum for continusous edges
  temp <- full_join(temp, wider_consol_dat, by = c('user_loc' = 'fipscode'))
  temp <- select(temp, user_loc, fr_loc, scaled_sci, diab_relationship,
                 diab_prev_county_1 = `Diagnosed Diabetes`)
  temp <- full_join(temp, wider_consol_dat, by = c('fr_loc' = 'fipscode'))
  temp <- select(temp, user_loc, fr_loc, scaled_sci, diab_relationship,
                 diab_prev_county_1, diab_prev_county_2 = `Diagnosed Diabetes`)
  temp <- mutate(temp, prev_sum = diab_prev_county_1 + diab_prev_county_2)
  select(temp, user_loc, fr_loc, scaled_sci, diab_relationship, prev_sum)
}

#' Setup network
#'
#' Setup network from adjacency matrix and add node attributes
#'
#' @param adj.matrix
#' @param vertex.data
#'
#' @return
#' @export
#'
#' @examples
setupNetwork <-
  function(adj.matrix.diab,
           adj.matrix.sci,
           vertex.data) {
    vertex.data <-
      vertex.data |> mutate(
        Obesity = case_when(is.na(Obesity) ~ mean(Obesity, na.rm = T),
                            TRUE ~ Obesity),
        `Physical Inactivity` = case_when(
          is.na(`Physical Inactivity`) ~ mean(`Physical Inactivity`, na.rm = T),
          TRUE ~ `Physical Inactivity`
        )
      )
    net <- as.network(adj.matrix.diab)
    net <- set.vertex.attribute(net, 'obesity', vertex.data$Obesity)
    net <-
      set.vertex.attribute(net,
                           'physical_inactivity',
                           vertex.data$`Physical Inactivity`)
    # Since SCI is an edge value, need to add it as a network attribute
    net <- set.network.attribute(net, 'log_scaled_sci', adj.matrix.sci)
  }


#' Make an adjacency matrix for a given variable as the values in the matrix
#'
#' @param edge_dat
#' @param value
#'
#' @return
#' @export
#'
#' @examples
makeAdjMatrix <- function(edge_dat, value) {
  # Cast edge list or pairwise list to matrix
  mat_out <-
    dcast(as.data.table(edge_dat),
          user_loc ~ fr_loc,
          value.var = value,
          fill = 0) |>
    as.matrix()
  loc_names <- mat_out[, 1]
  mat_out <- mat_out[, 2:ncol(mat_out)]
  mat_out <- apply(mat_out, 2, as.numeric) # convert character to #
  colnames(mat_out) <- rownames(mat_out) <- loc_names
  return(mat_out)
}
