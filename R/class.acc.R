#' Classify accelormeter profiles based on the random forest classifier by Shamoun-Baranes et al. 2016 (https://doi.org/10.1098/rstb.2015.0395) 
#' and addition of classification to a GPS dataset
#' 
#' GPS and meta data can be dowloaded directly from https://www.movebank.org/panel_embedded_movebank_webapp?gwt_fragment=page=studies,path=study985143423 (Lesser Black-backed Gulls)
#' and https://www.movebank.org/panel_embedded_movebank_webapp?gwt_fragment=page=studies,path=study986040562 (Herring Gulls) 

#' @param GPS.data data.frame, data.table or matrix. Movebank GPS data 
#' @param acc.data data.frame, data.table or matrix. Movebank accelerometerdata
#' @param RF.MOD Model. Supplied with the package.
#'
#'
#' @return A data.table with the GPS data appended with behaviour
#' based on the random forest model
#' 
#' @export
#' 
#' @importFrom data.table data.table
#' @importFrom randomForest RandomForest
#' @importFrom assertthat assert_that
#'
#' Default use
#' class.acc(GPS.data, acc.data, RF.mod)


class.acc  <- function(GPS.data,acc.data,RF.mod){
  
  
  # gps and acc_data are of the right class
  assert_that(
    any(c("matrix", "data.frame", "data.table") %in% class(GPS.data)),
    msg = "`gps` must be of class data.table, data.frame or matrix."
  )
  assert_that(
    any(c("matrix", "data.frame", "data.table") %in% class(acc.data)),
    msg = "`ref_data` must be of class data.table, data.frame or matrix."
  )
  
  # assing unique IDs to match events
  acc.data$unique.ID  <- paste(acc.data$`tag-local-identifier`,as.character(acc.data$`start-timestamp`))
  GPS.data$unique.ID  <- paste(GPS.data$`tag-local-identifier`,as.character(GPS.data$timestamp))
  
  # remove readings shorter than 1s
  reading.sel         <- sort(unique(acc.data$unique.ID))[table(acc.data$unique.ID)>=20]
  acc.data            <- acc.data[unique.ID %in% reading.sel,]      
  
  # convert to g
  acc.data$g.x        <- acc.data$`acceleration-raw-x`/981
  acc.data$g.y        <- acc.data$`acceleration-raw-y`/981
  acc.data$g.z        <- acc.data$`acceleration-raw-z`/981
  
  # calculate the 14 metrics used by the random forest classifier
  metrics                     <- data.table(unique.ID           = sort(unique(acc.data$unique.ID)))
  metrics$tag_ID              <- by(acc.data$`tag-local-identifier`,acc.data$unique.ID,function(x) x[1])
  metrics$timestamp           <- by(acc.data$`start-timestamp`,acc.data$unique.ID,function(x) as.character(x[1]))
  
  metrics$mean_x              <- as.numeric(by(acc.data$g.x,acc.data$unique.ID,function(x) mean(x[1:20])))
  metrics$sd_x                <- as.numeric(by(acc.data$g.x,acc.data$unique.ID,function(x) sd(x[1:20])))
  metrics$sd_z                <- as.numeric(by(acc.data$g.z,acc.data$unique.ID,function(x) sd(x[1:20])))
  metrics$mean_pitch          <- as.numeric(by(acc.data,acc.data$unique.ID,function(x) mean((180/pi)*atan(x$g.x[1:20])/sqrt(x$g.y[1:20]^20+x$g.z[1:20]^20))))
  metrics$gps_speed           <- as.numeric(GPS.data$`ground-speed`[match(metrics$unique.ID,GPS.data$unique.ID)])
  metrics$meanabsder_x        <- as.numeric(by(acc.data$g.x,acc.data$unique.ID,function(x) mean(abs(diff(x)))))
  metrics$meanabsder_y        <- as.numeric(by(acc.data$g.y,acc.data$unique.ID,function(x) mean(abs(diff(x)))))
  metrics$meanabsder_z        <- as.numeric(by(acc.data$g.z,acc.data$unique.ID,function(x) mean(abs(diff(x)))))
  metrics$noise_x             <- as.numeric(by(acc.data$g.x,acc.data$unique.ID,function(x) mean(abs(x-.5*(c(0,x)[1:20]-c(x[2:20],0))))))
  metrics$noise_y             <- as.numeric(by(acc.data$g.y,acc.data$unique.ID,function(x) mean(abs(x-.5*(c(0,x)[1:20]-c(x[2:20],0))))))
  metrics$noise_z             <- as.numeric(by(acc.data$g.z,acc.data$unique.ID,function(x) mean(abs(x-.5*(c(0,x)[1:20]-c(x[2:20],0))))))
  metrics$noise_absder_y      <- as.numeric(metrics$noise_y/metrics$meanabsder_y)
  metrics$noise_absder_z      <- as.numeric(metrics$noise_z/metrics$meanabsder_z)
  metrics$fundfreq_z          <- as.numeric(by(acc.data$g.z,acc.data$unique.ID,function(x) seq(0,2*pi - 2*pi/20,2*pi/20)[which.max(abs(fft(x)/20))[1]]))
  metrics$odba                <- as.numeric(by(acc.data,acc.data$unique.ID,function(x) mean(abs(x$g.x[1:20]-mean(x$g.x[1:20]))+abs(x$g.y[1:20]-mean(x$g.y[1:20]))+abs(x$g.z[1:20]-mean(x$g.z[1:20])))))
  metrics$fundfreqmagnitude_x <- as.numeric(by(acc.data$g.x,acc.data$unique.ID,function(x) max(abs(fft(x[1:20])/20))))
  metrics$fundfreqmagnitude_z <- as.numeric(by(acc.data$g.z,acc.data$unique.ID,function(x) max(abs(fft(x[1:20])/20))))
  
  # classify segments
  pred   <- predict(RF.mod,metrics)
  
  # append prediction to GPS data
  ind    <- match(metrics$unique.ID,GPS.data$unique.ID)
  GPS.data[ind[!is.na(ind)],"accelero_class"] <- pred[!is.na(ind)]
  GPS.data[ind[!is.na(ind)],"ODBA"]           <- metrics$odba[!is.na(ind)]
  
  # remove unique.ID
  GPS.data$unique.ID <- NULL
  
  return(GPS.data)
}
