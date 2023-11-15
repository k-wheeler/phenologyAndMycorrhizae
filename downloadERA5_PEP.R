library('devtools')
library('ncdf4')
library("reticulate")

source('sharedVariables.R')
includedStations <- read.csv("PEPlongtermStations.csv") #Created in PEPdataChangeFigure.R file
includedStations <- includedStations %>% filter(!is.na(LAT) & !is.na(LON))

cdsapi <- reticulate::import_from_path("cdsapi",path="~/.conda/envs/myR_new2/lib/python3.11/site-packages")
cclient <- cdsapi$Client()

variables <- tibble::tribble(
  ~cf_name, ~units, ~api_name, ~ncdf_name,
  "air_temperature", "Kelvin", "2m_temperature", "t2m",
  "precipitation_flux", "kg/m2/s", "total_precipitation", NA_character_,
)
var <- variables[["api_name"]]
maxLat <- max(includedStations$LAT) + 0.5
minLat <- min(includedStations$LAT) - 0.5
maxLon <- max(includedStations$LON) + 0.5
minLon <- min(includedStations$LON) - 0.5

area <- c(maxLat,minLon,minLat,maxLon)
#area <- c(52,8,48,12)

#for(s in (1:nrow(includedStations))){
lapply((2022:1950),function(X){
#X <- 1980
  yr <- X
  print(yr)
  start_date <- as.Date(paste0(yr,"-01-01"))
  end_date <- as.Date(paste0(yr,"-12-31"))
  
  fileName <- paste0(dataPath,'ERA5_PEP/',"PEPsites_",start_date,"_",end_date,"_era5Members.nc")
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
