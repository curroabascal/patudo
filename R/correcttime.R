#'There is a problem when running the classifications, getting the time at depth...
#'tags have time recorded in GMT. Local sunrise and sunsets can happen in different dates, if expressed
#'in GMT.
#'Better to correct and work in local time... a way is using the average longitude of a track
#'to convert to local assuming 1h per 15º of difference with Greenwich.
#' @param datetime vector of datetime as POSIXct
#' @param geolon Longitude (can use the average of a track, as an example)
#' @return Returns a vector with datetime corrected from GMT to local time
#' @export
#' @examples
#' data(archives)
#' archivaldata=archives[archives$tagno==990289,]
#' archivaldata=archivaldata[complete.cases(archivaldata),]
#' data(tracks)
#' trackdata=tracks[tracks$tagid==990289,]
#' archivaldata$datetimecor=correcttime(archivaldata$datetime,mean(trackdata$mptlon))
#' archivaldata$date=trunc(archivaldata$datetime,unit='days')
#' archivaldata$datecor=trunc(archivaldata$datetimecor,unit='days')
#' seldate=unique(archivaldata$date)[32]
#' x11();layout(matrix(1:2,ncol=1))
#' plot(archivaldata$datetime[archivaldata$date==seldate],archivaldata$depth[archivaldata$date==seldate],'p',pch=16,xlab='',ylab='',ylim=c(500,0))
#' plot(archivaldata$datetimecor[archivaldata$datecor==seldate],archivaldata$depth[archivaldata$datecor==seldate],'p',pch=16,xlab='',ylab='',ylim=c(500,0))
correcttime<-function(datetime,geolon){
  if (geolon>180) geolon=geolon-360 #To change it as little as possible.
  datetime+geolon/15*60*60
}  
