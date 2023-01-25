#Shared Variables
library(tidyverse)
library(scales)
library("devtools") #
library("neonUtilities") #
library('suncalc') ##
library(daymetr) #
library("geoNEON") #
library("rhdf5") 
library("raster")
#library(inborutils)

options(stringsAsFactors=FALSE)
NEON_siteNames <- c("ABBY","BARR","BART","BLAN","BONA","CLBJ","CPER","DCFS","DEJU","DELA","DSNY","GRSM","GUAN","HARV",
                    "HEAL","JERC","JORN","KONA","KONZ","LAJA","LENO","MLBS","MOAB","NIWO","NOGP","OAES","ONAQ",
                    "ORNL","OSBS","PUUM","RMNP","SCBI","SERC","SJER","SOAP","SRER","STEI","STER","TALL","TEAK",
                    "TOOL","TREE","UKFS","UNDE","WOOD","WREF","YELL")
length_NEON_siteNames <- length(NEON_siteNames)
fungalRootDat <- read.csv('Data/fungalRootAssociations.csv',header=TRUE)
mycorrhizeTypes <- unique(fungalRootDat$Mycorrhizal.type)
dataPath <- "Data/"

siteData <- read_csv(paste0(dataPath,'NEON_Field_Site_Metadata_20220412.csv'))
siteData <- siteData %>% filter(field_site_type%in%c("Gradient Terrestrial","Core Terrestrial"))

NEON_phenophase_names <- c("Young leaves","Breaking leaf buds","Colored leaves","Increasing leaf size","Leaves","Falling leaves","Young needles","Breaking needle buds","Initial growth","Emerging needles","Colored needles","Needles","Falling needles")
mediumIntensity_phenophases <- c("11 to 100","11 to 100","25-49%","25-49%","25-49%","50-74%","11 to 100","11 to 100",NA,"11 to 100","25-49%","25-49%",NA)
NEON_PFTs <- c("Deciduous broadleaf","Semi-evergreen broadleaf","Evergreen broadleaf","Drought deciduous broadleaf","Graminoid","Forb","Evergreen conifer","Semi-evergreen forb","Pine","Deciduous conifer")
