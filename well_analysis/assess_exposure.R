##============================================================================##
## takes geocoded oil/gas wells data and births data and,
## for each birth, determines which wells is closest

## setup ---------------------------------------------------------------------

# packages and global variables ............................................
# install.packages(c("sf", "tidyverse", "lubridate")) # uncomment & run if needed
library("sf")
library("tidyverse")
library("lubridate")
library("here")

# defines coordinate reference systems (CRS) for the project
crs_nad83  <- st_crs(4269)  # NAD83 coordinate reference system
crs_albers <- st_crs(5070)  # Albers Equal-Area Conic projection, contiguous US


# paths to data -----------------------------------------------------------

data_births_path <- "R:\\CDPH birth data (2000-2020)\\Clean data\\"
data_wells_path <- here("data", "data_wells.csv")

# read birth data csvs ----------------------------------------------------

start_year <- 2007
end_year <- 2015

birth_data_raw <- read_csv(paste0(data_births_path,
                                  "birthdata_", 
                                  start_year,
                                  ".csv"), col_types = paste0(rep("c",110), collapse = ""))


for(year in (start_year+1):end_year){
  birth_data_raw <- birth_data_raw %>% 
    bind_rows(read_csv(paste0(data_births_path,
                              "birthdata_", 
                              year,
                              ".csv"), 
                       col_types = paste0(rep("c",110), collapse = ""))
    )
}



# import and prepare well data --------------------------------------------


data_wells  <- read_csv(data_wells_path) %>%
  # restricts to well types of interest
  filter(well_type %in% c("GAS", "INJECTION", "OIL & GAS", "CYCLIC STEAM",
                          "OIL")) %>%
  mutate(    date_spudded     = ymd(date_spudded),
             date_completed   = ymd(date_completed),
             prod_start       = ymd(prod_start),
             prod_end         = ymd(prod_end),
             prod_exp_begin   = ymd(prod_exp_begin),
             prod_exp_end     = ymd(prod_exp_end)) %>%
  # adds var with earliest and latest observed dates across all date columns
  mutate(date_earliest = pmin(date_spudded, date_completed, prod_start,
                              prod_end, na.rm = TRUE),
         date_latest   = pmax(date_spudded, date_completed, prod_start,
                              prod_end, na.rm = TRUE)) %>%
  # converts intervals variables to from character vector type to interval type.
  mutate(prod_exp_interval   = interval(prod_exp_begin, prod_exp_end)) %>%
  # drops wells drilled after the study period, keeping wells without any
  # operation dates, which we assume to have been drilled prior to the 1990s
  # @Kaitlyn - Adjust this date if our births data go past 2015
  filter(date_latest <= "2015-12-31" | is.na(date_latest)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83) %>%
  st_transform(crs_albers) %>%
  st_make_valid()


## defines exposure assessment function --------------------------------------

# generalized function to assess exposure by counting the number of
# wells in 1-km radius annuli within 10 km of the site;
# takes residence coordinates ('birth') as an sf object, generates 1-km annuli
# around the birth out to 10 km, and counts the number of well sites, both
# in the preproduction and production stages, within each annulus

assessExposureAnnuli <- function(birth, 
                                 wells,
                                 exposure_var_name) {
  
  # prepares the birth datasets ............................................
  
  # captures the birth interval start and end dates in their own variables
  interval_start <- birth$date_conception
  interval_end   <- birth$date_delivery
  
  birth <- birth %>% select("birth_id")
  
  # generates 10 km buffer as a mask around birth coordinates
  birth_mask <- birth %>% 
    st_transform(crs_albers) %>%
    st_buffer(dist = 10000)
  
  # subsets to wells that intersect with 'birth_mask'  i.e., within 10 km of 
  # the maternal residence, and that have production period that overlaps with
  # the gestation interval
  wells_within_10km <- wells %>%
    # restricts to wells within 10 km of the input birth
    st_intersection(birth_mask) %>%
    # adds birth interval (i.e., trimester) start and end dates
    mutate(interval_start = interval_start, interval_end = interval_end) %>%
    # adds birth interval interval to this dataset
    mutate(interval = interval(interval_start, interval_end)) %>%
    # adds indicator for whether well was in production stage (i.e., active)
    # during the gestation interval
    mutate(exposed = int_overlaps(interval, prod_exp_interval)) %>%
    filter(exposed == 1)
  
  # if there are wells that have dates that intersect with the birth interval,
  # determines the Euclidean distance to the nearest well
  if (nrow(wells_within_10km) > 0) {
    distances <- st_distance(birth, wells)
    min_distance <- min(distances) %>% as.numeric()
    birth <- birth %>% 
      mutate(!!as.name(paste(exposure_var_name)) := min_distance) %>% 
      as_tibble() %>% 
      select(-geometry)
  } else if (nrow(wells_within_10km) == 0) {
    birth <- birth %>% 
      mutate(!!as.name(exposure_var_name)  := NA) %>%
      as_tibble() %>% 
      select(-geometry)
  }
  return(birth) # returns the processed exposure data as output
}

nearest_active_well_distance <- function(births_near, 
                                         wells_active, 
                                         exposure_var_name = "dist_nearest_well_m") {
  # Project once
  births_pts <- st_transform(births_near, crs_albers) %>%
    mutate(birth_interval = interval(date_conception, date_delivery))
  
  wells_proj <- st_transform(wells_active, crs_albers)  # rename for clarity if needed
  
  # Spatial pairs: wells within 10 km of each birth (vectorized spatial index)
  idx_list <- st_is_within_distance(births_pts, wells_proj, dist = 10000)
  pairs <- tibble(birth_row = seq_len(nrow(births_pts)), well_row = idx_list) %>%
    unnest(well_row)
  
  if (nrow(pairs) == 0) {
    return(births_pts %>%
             st_drop_geometry() %>%
             transmute(birth_id, !!rlang::sym(exposure_var_name) := NA_real_))
  }
  
  # Attach attributes and filter by temporal overlap
  pairs_filtered <- pairs %>%
    mutate(
      birth_id       = births_pts$birth_id[birth_row],
      birth_interval = births_pts$birth_interval[birth_row],
      well_interval  = wells_proj$prod_exp_interval[well_row]
    ) %>%
    filter(int_overlaps(birth_interval, well_interval))
  
  if (nrow(pairs_filtered) == 0) {
    return(births_pts %>%
             st_drop_geometry() %>%
             transmute(birth_id, !!rlang::sym(exposure_var_name) := NA_real_))
  }
  
  # Pairwise distances 
  pairs_filtered$dist_m <- as.numeric(
    st_distance(births_pts[pairs_filtered$birth_row, ], 
                wells_proj[pairs_filtered$well_row, ], 
                by_element = TRUE)
  )
  
  # Min distance per birth, joined back to all births-near
  mins <- pairs_filtered %>%
    group_by(birth_id) %>%
    summarise(!!rlang::sym(exposure_var_name) := min(dist_m), .groups = "drop")
  
  births_pts %>%
    st_drop_geometry() %>%
    select(birth_id) %>%
    left_join(mins, by = "birth_id")
}


## calls exposure assessment for *active* wells ------------------------------

# preps data ................................................................
# dataset with wells in production (i.e., active) during the study period  
data_wells_active <- data_wells %>% filter(!is.na(prod_interval))
# makes 10 km buffer around active wells
wells_active_buffer <- data_wells_active %>% 
  st_transform(crs_albers) %>% # transforms into projected CRS for buffering
  st_buffer(dist = 10000) %>% # makes 10,000 m (10 km) buffer
  st_union() # merges polygons into one

# identifies births within 10 km of at least one active well; improves efficiency,
# since there's no need to assess exposure to births > 10 km from wells
data_births_near_active_wells <- birth_data_raw %>% 
  select(id,
         lmp,
         child_dob,
         X,
         Y) %>% 
  filter(!is.na(lmp) & !is.na(child_dob) & ! is.na(X) & !is.na(Y)) %>% 
  mutate(birth_id         = as.factor(id),
         date_conception  = ymd(lmp),
         date_delivery    = ymd(child_dob),
         X = as.numeric(X),
         Y = as.numeric(Y)) %>% 
  # converts to geospatial 'sf' object
  st_as_sf(coords = c("X", "Y"), crs = crs_nad83) %>%
  st_transform(crs_albers) %>%
  st_make_valid() %>% 
  st_intersection(wells_active_buffer)

rm(birth_data_raw)
# conducts assessment  .....................................................

data_births_exposure <- nearest_active_well_distance(
  data_births_near_active_wells,
  data_wells_active,
  exposure_var_name = "dist_nearest_well_m"
)

#data_births_exposure <- list() # initiates tibble to capture exposure data
# for each births row, calls exposure assessment function and captures output
#for (i in c(1:nrow(data_births_near_active_wells))) {
#  # data_births_exposure[i, ] <- 
# assessExposureAnnuli(data_births_near_active_wells[i, ],
#                      data_wells_active,
#                      "dist_nearest_well_m")

#}
# converts output list object to tabular data
#data_births_exposure <- do.call("rbind", data_births_exposure)
# joins exposure data for births < 10 km from active wells to full births dataset,
# filter out NA 
data_births_exposure_active_wells <- data_births_near_active_wells %>%
  select(birth_id) %>% # keeps only the identifiers
  left_join(data_births_exposure, by = c("birth_id")) %>%
  select(-geometry) %>% 
  filter(!is.na(dist_nearest_well_m)) # if no distance, assume no well

# exports interim dataset ..................................................
write_csv(data_births_exposure_active_wells,
          paste0(here(),
                 "\\data\\data_births_exposure_active_wells_2007_2015.csv"))

write_csv(data_births_exposure,
          paste0(here(),
                 "\\data\\data_births_exposure_2007_2015.csv"))

##============================================================================##