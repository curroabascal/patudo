#' This is a function to estimate the depth of different isoclines for each day from an archival tagging dataset
#' @param datetime vector of datetime as POSIXct
#' @param depth vector of depths (numeric)
#' @param exttemp vector of temperatures (numeric)
#' @param isoclines vector of isoclines (default c(18,20))
#' @return Returns a data.frame with the depth of the different isoclines by day
#' @export
#' @examples
#' data(archives)
#' archivaldata=archives[archives$tagno==990289,]
#' archivaldata=archivaldata[complete.cases(archivaldata),]
#' archivaldata$date=trunc(archivaldata$datetime,unit='days')
#' archivaldata=archivaldata[archivaldata$date<unique(archivaldata$date)[10],]
#' res=get.thermalstruct(archivaldata$datetime,archivaldata$depth,archivaldata$exttemp, isoclines=c(18,20,22))
#' res

get.thermalstruct<-function(datetime,depth,exttemp, isoclines=c(18,20)){
  require(zoo)
  data=data.frame(datetime=datetime,depth=depth,exttemp=exttemp)
  data=data[complete.cases(data),]
  data$date=trunc(data$datetime,unit='days')
  isodepths=NULL
  uniquedates=unique(data$date)

  for (k in 1:length(uniquedates)){
    print(paste(k,'out of',length(uniquedates)))
    data2=data[data$date==uniquedates[k],]

    if (nrow(data2)>3 & length(unique(data2$depth))>3){
    smoothtemp=smooth.spline(data2$depth,data2$exttemp,spar=.6)
    smoothtemp=predict(smoothtemp,seq(0,600,1))$y
    
    smoothtemp[-(max(round(min(data2$depth)),0):(max(round(max(data2$depth)),0)+2))]=NaN
    tempfun=approxfun(smoothtemp,seq(0,600,1))
    
    depthlag=30
    gradtemp=c(rep(NaN,times=depthlag/2),diff(smoothtemp,lag=depthlag),rep(NaN,times=depthlag/2))
    # depth=ifelse(which.min(gradtemp)==rev(which(!is.na(gradtemp)))[1],NA,which.min(gradtemp)-1)
    # if (length(depth)==0) depth=NA
    isodepths=rbind(isodepths,sapply(isoclines,tempfun))
    }
  }
  #Some days, it cannot be estimated because the fish stays in the surface. Do some interpolation.
  isodepths=na.approx(isodepths)
  colnames(isodepths)=paste0('depth',isoclines)
  return(isodepths)
}
