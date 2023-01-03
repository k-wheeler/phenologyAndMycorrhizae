library('phenocamr')

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
  test <- phenophases(df,out_dir = paste0(dataPath,'phenoCamData'),internal=FALSE,
              lower_thresh=0.15,middle_thresh=0.50,upper_thresh=0.85)
}

