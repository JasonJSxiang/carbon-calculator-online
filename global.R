## Initialise database ####
con <- dbConnect(SQLite(), "database.sqlite")

# building asset
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS asset_building
          (
          id INTEGER PRIMARY KEY,
          country TEXT,
          city TEXT,
          asset_type TEXT,
          asset_name TEXT,
          office_floor_area REAL,
          area_unit TEXT,
          is_subleased INTEGER,
          applicable_emission_source TEXT,
          creation_time INTEGER)")

# consumption record: building
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS consumption_record_building
          (
          id INTEGER PRIMARY KEY,
          country TEXT,
          city TEXT,
          asset_name TEXT,
          reporting_year INTEGER,
          fuel_type TEXT,
          consumption REAL,
          unit TEXT,
          is_renewable TEXT,
          renewable_energy_consumption REAL,
          renewable_energy_type TEXT,
          start_date INTEGER,
          end_date INTEGER,
          additional_comment TEXT,
          creation_time INTEGER)")

# emission factor 
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS emission_factor_grid
    (
    id INTEGER PRIMARY KEY,
    country TEXT,
    emission_factor REAL,
    creation_time INTEGER
    )"
)

# emission record: building
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS emission_record_building
    (
    id INTEGER PRIMARY KEY,
    consumption_record_id INTEGER,
    asset_name TEXT,
    fuel_type TEXT,
    LB_emission REAL,
    MB_emission REAL,
    start_date INTEGER,
    end_date INTEGER,
    creation_time INTEGER)")

dbDisconnect(con)

## Globals static ####
#(code only run once when the app starts)

# fuel list for building
fuel_building <- c("Electricity",
                   "Heating",
                   "Steam",
                   "Cooling",
                   "Diesel",
                   "Petrol",
                   "Kerosene")

# vehicle type list for vehicle
vehicle_type <- c("Petrol",
                  "Diesel",
                  "Hybrid",
                  "Electric")

# building fuel unit
building_fuel_unit <- c("kWh", "BTU")

# reporting year
reporting_year <- c(2025:2015)

# renewable energy type
renewable_energy_type <- c("Solar", "Wind", "Hydro", "Biomass")

# country and city list (from maps package)
country_list <- world.cities |> 
    rename("country" = "country.etc") |> # rename the country col
    mutate(name = str_replace(name, "^'", "")) |> # remove the leading ' in the col
    dplyr::distinct(country, .keep_all = TRUE) |>  # keep unique country
    dplyr::arrange(country) |> 
    pull(country)

city_df <- world.cities |> 
    rename("country" = "country.etc") |> # rename the country col
    mutate(name = str_replace(name, "^'", "")) |> # remove the leading ' in the col
    dplyr::distinct(name, .keep_all = TRUE) |>  # keep unique country
    dplyr::arrange(name)