library(sf)
library(tidyverse)
dataPath <- "Data/"

siteData <- read_csv(paste0(dataPath,'NEON_Field_Site_Metadata_20220412.csv'))
siteData <- siteData %>% filter(field_site_type%in%c("Gradient Terrestrial","Core Terrestrial"))

divs <- read_sf('CONUS_CLIMATE_DIVISIONS.shp/GIS.OFFICIAL_CLIM_DIVISIONS.shp')
siteData <- siteData %>% filter(field_site_type%in%c("Gradient Terrestrial","Core Terrestrial"))

pnts <- data.frame(
  "x" = siteData$field_longitude,
  "y" = siteData$field_latitude)

# create a points collection
pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(pnts), 
                                     function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 

pnts_trans <- st_transform(pnts_sf, 2163) # apply transformation to pnts sf
tt1_trans <- st_transform(divs, 2163)      # apply transformation to polygons sf

# intersect and extract state name
pnts$region <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                     function(col) { 
                       tt1_trans[which(col), ]$CLIMDIV
                     })
pnts <- cbind(pnts,siteData$field_site_id)
#pnts$region[[c(2,5,9,13,15,20,30,41)]] <- NA
for(i in c(2,5,9,13,15,20,30,41)){
  pnts$region[[i]] <- NA
}

pnts <- pnts %>% mutate(region=as.character(unlist(region)),siteID=siteData$field_site_id) %>%
  dplyr::select(siteID,region)

droughtData <- read.table('climdiv-pdsidv-v1.0.0-20230206.txt')
colnames(droughtData) <- c("ID",seq(1,12))
droughtData$ID[nchar(droughtData$ID)<10] <- paste0("0",droughtData$ID[nchar(droughtData$ID)<10])
droughtData <- droughtData %>% mutate(year=substr(ID,7,10),region=substr(ID,1,4)) %>%
  dplyr::select(-ID)
combinedDrought <- left_join(pnts,droughtData,by="region")

write.csv(combinedDrought,file="droughtsAtNEON.csv",row.names = FALSE,quote=FALSE)



