#' This is a function to split archival data between dawn, day, dusk and night.
#' based on the geolocations, estimating sunrise and sunset times and defining 
#' periods around sunrise and sunset.
#' datetime,depth,geodates,geolons,geolats
#' @param datetime vector of datetime as POSIXct
#' @param depth vector of depths (numeric)
#' @param geodates vector of dates from the geolocation file
#' @param geolons vector of longitudes from the geolocation file
#' @param geolats vector of latitudes from the geolocation file
#' @param plotit plot example (default to F)
#' @param timearoundsunrise numeric vector with two values defining the number of seconds before and after sunrise the define the dawn
#' @param timearoundsunset numeric vector with two values defining the number of seconds before and after sunrise the define the dawn
#' @return Returns a character vector with phases of the day for each record.
#' @export
#' @examples
#' data(archives)
#' archivaldata=archives[archives$tagno==990289,]
#' archivaldata=archivaldata[complete.cases(archivaldata),]
#' data(tracks)
#' trackdata=tracks[tracks$tagid==990289,]
#' archivaldata$phase=get.phasefromtrack(archivaldata$datetime,archivaldata$depth,trackdata$date,trackdata$mptlon,trackdata$mptlat,plotit=T,timearoundsunrise=c((60*60),60*60),timearoundsunset=c((60*60),60*60))
#' archivaldata$date=trunc(archivaldata$datetime,unit='days')
#' seldate=unique(archivaldata$date)[31]
#' plot(archivaldata$datetime[archivaldata$date==seldate],archivaldata$depth[archivaldata$date==seldate],'p',pch=16,xlab='',ylab='',ylim=c(300,0))
#' points(archivaldata$datetime[archivaldata$date==seldate & archivaldata$phase=='day'],archivaldata$depth[archivaldata$date==seldate & archivaldata$phase=='day'],pch=16,col='red')

get.phasefromtrack<-function(datetime,depth,geodates,geolons,geolats,plotit=F,timearoundsunrise=c((30*60),0),timearoundsunset=c((30*60),0)){   #period around sunrise and sunset to define dawn and dusk
  
  arch=data.frame(rec=1:length(datetime),datetime=datetime,depth=depth)
  arch$date=trunc(arch$datetime,unit='days')
  
  geo=data.frame(date=geodates,mptlon=geolons,mptlat=geolats)
  #There can be more than one position per day. Get the average
  geo2=aggregate(cbind(mptlon,mptlat)~date,data=geo,FUN=mean)
  geo2$date=as.POSIXct(geo2$date,format='%d/%m/%Y',tz='UTC')
  
  # arch$lon=geo2$mptlon[match(as.character(arch$date),as.character(geo2$date))]
  # arch$lat=geo2$mptlat[match(as.character(arch$date),as.character(geo2$date))]
  # 
  # #Only days with position estimates
  # arch=arch[!is.na(arch$lon),]
  if (!'maptools' %in% installed.packages()[,1]) devtools::install_version("maptools", version = "1.1-8")
  require(maptools) 
  
  sunrise<-crepuscule(crds=cbind(geo2$mptlon,geo2$mptlat),dateTime=geo2$date,direction='dawn',solarDep=3,POSIXct.out=T)
  sunset<-crepuscule(crds=cbind(geo2$mptlon,geo2$mptlat),dateTime=geo2$date,direction='dusk',solarDep=3,POSIXct.out=T)
  
  #Change TZ to make compatible.
  attr(sunrise$time,'tzone')<-'UTC'
  attr(sunset$time,'tzone')='UTC'
  
  phaseofday=rbind(sunrise,sunset)
  phaseofday$phase=rep(c("sunrise","sunset"),each=c(nrow(sunrise)))
  
  #Now, take the archival data and assign the closest sunrise/sunset event 
  #(assigning both sunrise and sunset when working in different time zones can be problematic,
  #because sunrise can be after sunset in UTC time... one option is working in local time,
  #but let us try by using the previous and next solar events to tell between day and night.
  
  index1=sapply(as.numeric(arch$datetime),function(x){which.min(ifelse((x-as.numeric(phaseofday$time))>0,(x-as.numeric(phaseofday$time)),1e6))})
  index2=sapply(as.numeric(arch$datetime),function(x){which.min(ifelse((as.numeric(phaseofday$time)-x)>0,(as.numeric(phaseofday$time)-x),1e6))})
  
  if (class(index1)=='list'){
    jnk=lapply(index1,length)
    jnk=unlist(jnk)
    which(jnk==0)
  }
  arch$previous_solarevent_time=phaseofday$time[index1]
  arch$previous_solarevent=phaseofday$phase[index1]
  arch$next_solarevent_time=phaseofday$time[index2]
  arch$next_solarevent=phaseofday$phase[index2]
  
  arch$phase='dawn'
  arch$phase[arch$previous_solarevent=='sunrise' & arch$datetime>(arch$previous_solarevent_time+timearoundsunrise[2]) & arch$datetime<(arch$next_solarevent_time-timearoundsunset[1])]='day'
  arch$phase[arch$previous_solarevent=='sunset' & arch$datetime>(arch$previous_solarevent_time+timearoundsunset[2]) & arch$datetime<(arch$next_solarevent_time-timearoundsunrise[1])]='night'
  # arch$phase[arch$previous_solarevent=='sunset' & arch$datetime<(arch$previous_solarevent_time+3600)]='dusk'
  arch$phase[arch$next_solarevent=='sunset' & arch$datetime>(arch$next_solarevent_time-timearoundsunset[1])]='dusk'
  arch$phase[arch$previous_solarevent=='sunset' & arch$datetime<(arch$previous_solarevent_time+timearoundsunset[2])]='dusk'
  if (plotit){
    datab=arch[arch$date %in% unique(arch$date)[7:12],]
    plot(datab$datetime,datab$depth,main=datab$tagno[1])
    points(datab$datetime[datab$phase=='day'],datab$depth[datab$phase=='day'],pch=16,col='green')
    points(datab$datetime[datab$phase=='night'],datab$depth[datab$phase=='night'],pch=16,col='blue')
    points(datab$datetime[datab$phase=='dawn'],datab$depth[datab$phase=='dawn'],pch=16,col='orange')
    points(datab$datetime[datab$phase=='dusk'],datab$depth[datab$phase=='dusk'],pch=16,col='orange')
    
  }
  arch$phase
}