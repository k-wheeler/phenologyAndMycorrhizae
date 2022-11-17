#Based off of phenophases() function in phenocamr package 
library('phenocamr')
#https://github.com/bluegreen-labs/phenocamr/blob/master/R/phenophases.r

phenoCamURLs <- read.csv(paste0(dataPath,'phenocamURLS.csv'))
for(i in 1:nrow(phenoCamURLs)){ #issue: 16,20 
  print(i)
  skip_to_next <- FALSE
  tryCatch(download_phenocam(site = phenoCamURLs$phenocam_id[i],
                    veg_type = (strsplit(phenoCamURLs$ROI[i],"_")[[1]][1]),
                    roi_id = (strsplit(phenoCamURLs$ROI[i],"_")[[1]][2]),
                    frequency = "1"),
           error = function(e) {skip_to_next <<- TRUE})
  if(skip_to_next){
    next
  }
  df <- read_phenocam(file.path(tempdir(),
                                paste0(phenoCamURLs$phenocam_id[i],"_",
                                       phenoCamURLs$ROI[i],"_1day.csv")))
  phenophases(df,out_dir = paste0(dataPath,'phenoCamData'),internal=FALSE,
              lower_thresh=0.15,middle_thresh=0.50,upper_thresh=0.85)
}





load(paste0(dataPath,"all_PhenoCam_gcc_data.RData")) #allPhenoCamData object
percentiles <- c(90,75,50,25,"mean")

for(s in seq_along(NEON_siteNames)){
  
  
  
  subDat <- allPhenoCamData %>% filter(site_id==NEON_siteNames[s])
  
  # split out data from read in or provided data
  df <- data$data
  
  rising <- do.call("rbind", lapply(percentiles, function(i){
    
    # calculate the transition dates
    tmp <- suppressWarnings(transition_dates(data,
                                             reverse = FALSE,
                                             percentile = i,
                                             ...))
    
    # screen for false rising parts
    loc <- strptime(as.Date(tmp$transition_10, origin = "1970-01-01"),
                    "%Y-%m-%d")$yday
    l <- which(loc < 30 | loc > 250)
    if (data$veg_type %in% c("DB","SH","GR","EN") & !length(l)==0L ){
      tmp <- tmp[-l,]
    }
    
    # formatting output
    gcc_value <- rep(sprintf("gcc_%s",i),dim(tmp)[1])
    tmp <- cbind(gcc_value, tmp)
    return(tmp)
  }))
  
  # calculate falling greenness transition dates
  # all percentiles
  falling <- do.call("rbind", lapply(percentiles, function(i){
    
    tmp <- suppressWarnings(transition_dates(data,
                                             reverse = TRUE,
                                             percentile = i,
                                             ...))
    
    # screen for false falling curves
    loc <- strptime(as.Date(tmp$transition_10, origin = "1970-01-01"),
                    "%Y-%m-%d")$yday
    
    
    l = which(loc > 30 & loc < 240)
    if ( mat < mat_threshold & data$veg_type %in% c("DB","SH","GR","EN") & !length(l)==0L ){
      tmp <- tmp[-l,]
    }
    
    # formatting output
    gcc_value <- rep(sprintf("gcc_%s",i),dim(tmp)[1])
    tmp <- cbind(gcc_value, tmp)
    return(tmp)
  }))
  
  # calculate the RMSE for all spline fits
  smooth_data <- df[,grep("smooth_gcc",colnames(df))]
  original_data <- df[,grep("^gcc",colnames(df))]
  
  # remove the gcc_std column
  original_data <- original_data[,-2]
  
  # calculate the RMSE
  RMSE <- round(sqrt(apply((smooth_data - original_data)^2,2,mean,na.rm=TRUE)),5)
  
    # return dates as a list if no output file is required
    return(list("rising" = stats::na.omit(rising),
                "falling" = stats::na.omit(falling)))
  }
}

}
