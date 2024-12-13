#' This is a function to split archival data between dawn, day, dusk and night.
#' based on the same archival data by converting light at depth into light at surface,
#' estiming sunrise and sunset times and defining periods around sunrise and sunset.
#' @param datetime vector of datetime as POSIXct
#' @param depth vector of depths (numeric)
#' @param light vector of light levels
#' @param plotit plot example (default to F)
#' @param timearoundsunrise numeric vector with two values defining the number of seconds before and after sunrise the define the dawn
#' @param timearoundsunset numeric vector with two values defining the number of seconds before and after sunrise the define the dawn
#' @return Returns a character vector with phases of the day for each record.
#' @export
#' @examples
#' data(archives)
#' archivaldata=archives[archives$tagno==990289,]
#' archivaldata=archivaldata[complete.cases(archivaldata),]
#' archivaldata$phase=get.phasefromarchival(archivaldata$datetime,archivaldata$depth,archivaldata$light,plotit=T,timearoundsunrise=c((60*60),60*60),timearoundsunset=c((60*60),60*60))
#' archivaldata$date=trunc(archivaldata$datetime,unit='days')
#' seldate=unique(archivaldata$date)[31]
#' plot(archivaldata$datetime[archivaldata$date==seldate],archivaldata$depth[archivaldata$date==seldate],'p',pch=16,xlab='',ylab='',ylim=c(300,0))
#' points(archivaldata$datetime[archivaldata$date==seldate & archivaldata$phase=='day'],archivaldata$depth[archivaldata$date==seldate & archivaldata$phase=='day'],pch=16,col='red')

get.phasefromarchival<-function(datetime,depth,light,plotit=F,timearoundsunrise=c((30*60),0),timearoundsunset=c((30*60),0)){   
  if (plotit) x11();par(mfcol=c(2,2))
  archivaldata=data.frame(nrec=1:length(datetime),datetime,depth,light)
  archivaldata=archivaldata[complete.cases(archivaldata),]
  archivaldata=archivaldata[order(archivaldata$datetime),]
  archivaldata$date=trunc(archivaldata$datetime,"day")
  
  #See light in the surface using trackit method (two layer). It seems that the day by day correction is worst.
  
  
  #require(trackit)
  
  #Exploratory plot
  if (plotit){
    datab=archivaldata[archivaldata$date %in% unique(archivaldata$date)[1:5],]
    lightcorrected1=two.layer.depth.corr.Curro(datab$datetime,datab$depth,datab$light,weekbyweek=FALSE, D0=50)
    lightcorrected2=two.layer.depth.corr.Curro(datab$datetime,datab$depth,datab$light,weekbyweek=TRUE, D0=50)
    
    index=which(datab$date %in% unique(archivaldata$date)[1:5])
    plot(datab$datetime[index],datab$light[index],'o',col='black',pch=16,cex=.5,ylim=c(0,max(lightcorrected1,lightcorrected2)))
    lines(datab$datetime[index],lightcorrected1[index],'o',col='red',pch=16,cex=.5)
    lines(datab$datetime[index],lightcorrected2[index],'o',col='green',pch=16,cex=.5)
    rm(datab)
  }
  #### Not sure if it is better doing the correction day by day. At least for the 8th tag, it is much better.
  
  #tryerror=length(try(two.layer.depth.corr.Curro2(data,daybyday=T, D0=50),silent=T))==1  #I do not know why, but in few instances (I guess
  #depending on fish movement, I get an error if I want to day it
  #day by day.
  
  #I have modified the trackit script to keep the estimates of K.
  #if (tryerror) data=two.layer.depth.corr.Curro2(data,daybyday=F, D0=50) else {
  
  archivaldata$correctedlight=two.layer.depth.corr.Curro(archivaldata$datetime,archivaldata$light,archivaldata$depth,weekbyweek=T, D0=50)
  
  #      data=two.layer.depth.corr.Curro3(data,weekbyweek=F, D0=50)
  #}
  archivaldata$no=1:nrow(archivaldata)
  nlag=120/median(as.numeric(diff(archivaldata$datetime),unit='mins'))
  #data$light[data$light>quantile(data$light,.95)]=quantile(data$light,.95)
  archivaldata$deriv=c(rep(0,trunc(nlag/2)),diff(archivaldata$correctedlight,lag=nlag),rep(0,ceiling(nlag/2)))
  
  alpha=.1
  archivaldata$smoothed=loess(archivaldata$deriv ~ archivaldata$no,span=alpha*350/(nlag*length(unique(archivaldata$date))))$fitted
  
  require(plyr)
  archivaldata$date2=as.character(archivaldata$date)
  sunrises=ddply(archivaldata[,c("date2","datetime","smoothed")],~date2,function(x){x[which.max(x$smoothed),]})
  sunsets=ddply(archivaldata[,c("date2","datetime","smoothed")],~date2,function(x){x[which.min(x$smoothed),]})
  
  
  #Check it works
  if (plotit){
    datab=archivaldata[archivaldata$date %in% unique(archivaldata$date)[7:12],]
    plot(datab$datetime,datab$deriv,main=nlag)
    lines(datab$datetime,datab$smoothed,col='purple')
    abline(v=sunrises$datetime,col='green')
    abline(v=sunsets$datetime,col='red')
  }
  #
  sunrises$date=as.POSIXct(sunrises$date2)
  sunrises=sunrises[order(sunrises$date),]
  sunrises$no=1:nrow(sunrises)
  sunrises$sunrise2=as.numeric(format(sunrises$datetime,"%H"))+as.numeric(format(sunrises$datetime,"%M"))/60
  difsunrise=apply(abs(cbind(c(0,diff(sunrises$sunrise2)),c(diff(sunrises$sunrise2),0))),1,min)
  sunrises$sunrise2[difsunrise>2]=NA
  sunrisefun=loess(sunrises$sunrise2~sunrises$no,weights=c(0,rep(1,nrow(sunrises)-2),0),span=20/nrow(sunrises),family='symmetric')
  sunrises$smoothed=sunrises$date+3600*predict(sunrisefun,1:nrow(sunrises))
  
  sunsets$date=as.POSIXct(sunsets$date2)
  sunsets=sunsets[order(sunsets$date),]
  sunsets$no=1:nrow(sunsets)
  sunsets$sunset2=as.numeric(format(sunsets$datetime,"%H"))+as.numeric(format(sunsets$datetime,"%M"))/60
  difsunset=apply(abs(cbind(c(0,diff(sunsets$sunset2)),c(diff(sunsets$sunset2),0))),1,min)
  sunsets$sunset2[difsunset>2]=NA
  sunsetfun=loess(sunsets$sunset2~sunsets$no,weights=c(0,rep(1,nrow(sunsets)-2),0),span=20/nrow(sunsets),family='symmetric')
  sunsets$smoothed=sunsets$date+3600*predict(sunsetfun,1:nrow(sunsets))
  
  #See how sunrise time varies with date and smoothing.
  #If a fish moves around 1 degree in longitude in a day (around 100 KM, it would mean a variation of 4 min...).
  if (plotit){
    plot(sunrises$date,sunrises$sunrise2,ylim=c(0,24))
    lines(sunrises$date,as.numeric(format(sunrises$smoothed,"%H"))+as.numeric(format(sunrises$smoothed,"%M"))/60,col='red')
    points(sunsets$date,sunsets$sunset2)
    lines(sunsets$date,as.numeric(format(sunsets$smoothed,"%H"))+as.numeric(format(sunsets$smoothed,"%M"))/60,col='green')
    legend('bottomleft',legend=c('sunrise','sunset'),col=c('red','green'),lty=c(1,1))
  }
  #
  
  phaseofday=data.frame(time=c(sunrises$smoothed,sunsets$smoothed))
  phaseofday$phase=rep(c("sunrise","sunset"),each=c(nrow(sunrises)))
  
  index1=sapply(as.numeric(archivaldata$datetime),function(x){which.min(ifelse((x-as.numeric(phaseofday$time))>0,(x-as.numeric(phaseofday$time)),1e6))})
  index2=sapply(as.numeric(archivaldata$datetime),function(x){which.min(ifelse((as.numeric(phaseofday$time)-x)>0,(as.numeric(phaseofday$time)-x),1e6))})
  
  archivaldata$previous_solarevent_time=phaseofday$time[index1]
  archivaldata$previous_solarevent=phaseofday$phase[index1]
  archivaldata$next_solarevent_time=phaseofday$time[index2]
  archivaldata$next_solarevent=phaseofday$phase[index2]
  #Quick check...
  # with(arch[1:5000,],plot(1:5000,lightlevel,'p',pch=16,col=ifelse(previous_solarevent=='sunrise','green','black')))
  
  #Now split time series between daytime, nighttime, dawn (one hour around sunrise) and dusk (one hour around sunset)
  archivaldata$phase='dawn'
  archivaldata$phase[archivaldata$previous_solarevent=='sunrise' & archivaldata$datetime>(archivaldata$previous_solarevent_time+timearoundsunrise[2]) & archivaldata$datetime<(archivaldata$next_solarevent_time-timearoundsunset[1])]='day'
  archivaldata$phase[archivaldata$previous_solarevent=='sunset' & archivaldata$datetime>(archivaldata$previous_solarevent_time+timearoundsunset[2]) & archivaldata$datetime<(archivaldata$next_solarevent_time-timearoundsunrise[1])]='night'
  # archivaldata$phase[archivaldata$previous_solarevent=='sunset' & archivaldata$datetime<(archivaldata$previous_solarevent_time+3600)]='dusk'
  archivaldata$phase[archivaldata$next_solarevent=='sunset' & archivaldata$datetime>(archivaldata$next_solarevent_time-timearoundsunset[1])]='dusk'
  archivaldata$phase[archivaldata$previous_solarevent=='sunset' & archivaldata$datetime<(archivaldata$previous_solarevent_time+timearoundsunset[2])]='dusk'
  
  #Example of classification
  
  if (plotit){
    datab=archivaldata[archivaldata$date %in% unique(archivaldata$date)[7:12],]
    plot(datab$datetime,datab$light,main=datab$tagno[1])
    points(datab$datetime[datab$phase=='day'],datab$light[datab$phase=='day'],pch=16,col='green')
    points(datab$datetime[datab$phase=='night'],datab$light[datab$phase=='night'],pch=16,col='blue')
    points(datab$datetime[datab$phase=='dawn'],datab$light[datab$phase=='dawn'],pch=16,col='orange')
    points(datab$datetime[datab$phase=='dusk'],datab$light[datab$phase=='dusk'],pch=16,col='orange')
    
  }
  archivaldata=archivaldata[order(archivaldata$nrec),]
  archivaldata$phase
}