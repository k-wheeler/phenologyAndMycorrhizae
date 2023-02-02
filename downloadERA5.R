library('devtools')
library('ncdf4')
library("reticulate")

source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

cdsapi <- reticulate::import_from_path("cdsapi",path="~/.conda/envs/myR_new2/lib/python3.11/site-packages")
cclient <- cdsapi$Client()

start_date <- as.Date("2014-01-01")
end_date <- as.Date("2022-01-31")

variables <- tibble::tribble(
  ~cf_name, ~units, ~api_name, ~ncdf_name,
  "air_temperature", "Kelvin", "2m_temperature", "t2m",
  "air_pressure", "Pa", "surface_pressure", NA_character_,
  NA_character_, "Kelvin", "2m_dewpoint_temperature", NA_character_,
  "precipitation_flux", "kg/m2/s", "total_precipitation", NA_character_,
  "eastward_wind", "m/s", "10m_u_component_of_wind", NA_character_,
  "northward_wind", "m/s", "10m_v_component_of_wind", NA_character_,
  "surface_downwelling_shortwave_flux_in_air", "W/m2", "surface_solar_radiation_downwards", NA_character_,
  "surface_downwelling_longwave_flux_in_air", "W/m2", "surface_thermal_radiation_downwards", NA_character_,
  "soil_temperature_level_1","Kelvin","soil_temperature_level_1",NA_character_,
  "soil_temperature_level_2","Kelvin","soil_temperature_level_2",NA_character_,
  "soil_temperature_level_3","Kelvin","soil_temperature_level_3",NA_character_,
  "soil_temperature_level_4","Kelvin","soil_temperature_level_4",NA_character_,
  'volumetric_soil_water_layer_1','m3/m3','volumetric_soil_water_layer_1',NA_character_,
  'volumetric_soil_water_layer_2','m3/m3','volumetric_soil_water_layer_2',NA_character_,
  'volumetric_soil_water_layer_3','m3/m3','volumetric_soil_water_layer_3',NA_character_,
  'volumetric_soil_water_layer_4','m3/m3','volumetric_soil_water_layer_4',NA_character_
) #Some examples (see more at: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview)

var <- variables[["api_name"]]

for(s in seq_along(NEON_siteNames)){
  siteID <- NEON_siteNames[s]
  print(siteID)
  lat <- siteData$field_latitude[siteData$field_site_id==siteID]
  long <- siteData$field_longitude[siteData$field_site_id==siteID]
  area <- rep(round(c(lat, long) * 4) / 4, 2)
  
  fileName <- paste0(dataPath,'ERA5/',siteID,"_",start_date,"_",end_date,"_era5Members.nc")
  print(fileName)
  do_next <- tryCatch({
    cclient$retrieve(
      "reanalysis-era5-single-levels",
      list(
        variable = var,
        product_type = 'ensemble_members',
        date = paste(start_date, end_date, sep = "/"),
        time = "00/to/23/by/1",
        area = area,
        grid = c(0.25, 0.25),
        format = "netcdf"
      ),
      fileName
    )
    FALSE
  }, error = function(e) {
    print("Failed to download")
    TRUE
  })
}
