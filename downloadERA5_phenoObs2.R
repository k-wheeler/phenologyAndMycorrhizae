library('devtools')
library('ncdf4')
library("reticulate")

source('sharedVariables.R')
#includedStations <- read.csv("PEPlongtermStations.csv") #Created in PEPdataChangeFigure.R file
includedStations <- read.csv('allPhenoSites.csv')
includedStations <- includedStations %>% filter(!is.na(latitude) & !is.na(longitude))

cdsapi <- reticulate::import_from_path("cdsapi",path="~/.conda/envs/myR_new2/lib/python3.11/site-packages")
cclient <- cdsapi$Client()

variables <- tibble::tribble(
  ~cf_name, ~units, ~api_name, ~ncdf_name,
  "air_temperature", "Kelvin", "2m_temperature", "t2m",
  "precipitation_flux", "kg/m2/s", "total_precipitation", NA_character_,
)
var <- variables[["api_name"]]
maxLat <- 70#max(includedStations$latitude) + 0.5
minLat <- 35#min(includedStations$latitude) - 0.5
maxLon <- 42#max(includedStations$longitude) + 0.5
minLon <- -15#min(includedStations$longitude) - 0.5

area <- c(maxLat,minLon,minLat,maxLon)
#area <- c(52,8,48,12)

#for(s in (1:nrow(includedStations))){
lapply((2013:2022),function(X){
#X <- 1980
  yr <- X
  print(yr)
  start_date <- as.Date(paste0(yr,"-01-01"))
  end_date <- as.Date(paste0(yr,"-12-31"))
  
  fileName <- paste0(dataPath,'ERA5_phenoObs/',"phenoObs_europe",start_date,"_",end_date,"_era5Members.nc")
  print(fileName)
  if(!file.exists(fileName)){
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
})
