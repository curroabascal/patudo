#'This is a function to assign an behavioural type (associated, unassociated, other)
#'based on the criteria from Fuller et al., 2015
#'https://www.sciencedirect.com/science/article/pii/S0165783615300047
#' Possibly many things to fine tune (also I guess the classification needs of a manual component)
#' @param datetime vector of datetime as POSIXct
#' @param depth vector of depths
#' @param exttemp vector of external temperatures
#' @param phase phase of day (as obtained from get.phasefromarchival or get.phasefromtrack)
#' @return Returns a data.frame with the dates and behavioural classifications.
#' @export
#' @examples
#' data(archives)
#' archivaldata=archives[archives$tagno==990289,]
#' archivaldata=archivaldata[complete.cases(archivaldata),]
#' #Only a few days to make it shorter
#' archivaldata$date=trunc(archivaldata$datetime,unit='days')
#' archivaldata=archivaldata[archivaldata$date>=unique(archivaldata$date)[26] & archivaldata$date<=unique(archivaldata$date)[35],]
#' data(tracks)
#' trackdata=tracks[tracks$tagid==990289,]

#' archivaldata$phase=get.phasefromtrack(archivaldata$datetime,depth=archivaldata$depth,trackdata$date,trackdata$mptlon,trackdata$mptlat,plotit=T,timearoundsunrise=c((60*60),60*60),timearoundsunset=c((60*60),60*60))
#' archivaldata$datetimecor=correcttime(archivaldata$datetime,mean(trackdata$mptlon))
#' behaviours=get.behav(archivaldata$datetimecor,archivaldata$depth,archivaldata$exttemp,archivaldata$phase)
#' behaviours
#' plot(archivaldata$datetimecor,archivaldata$depth,'p',cex=.5,ylim=c(500,0),xlab='datetime',ylab='')
#' for (i in 1:nrow(behaviours)){
#'   behavcolor=ifelse(behaviours$behav[i]=='Associated','red',
#'                     ifelse(behaviours$behav[i]=='Unassociated','blue','grey'))
#'   rect(as.numeric(behaviours$date[i]),500,as.numeric(behaviours$date[i])+86400,480,col=behavcolor)
#' }
#' legend('bottomright',legend = c("Associated", "Unassociated","Other"), 
#'        col = c("red", "blue","grey"), pch = 15, pt.cex = 2, 
#'        cex = 1.2, title = "Behavioural type")


get.behav<-function(datetime,depth,exttemp,phase)
  {
  print('adding behaviour')
  data=data.frame(datetime=datetime,depth=depth,exttemp=exttemp,phase=phase)
  data$date=as.POSIXct(trunc(data$datetime,unit='days'))
  #I will add the classification based on Fuller et al., 2015. Ask Dan about doubts
  #I will do it in two steps: First, select the days with Unassociated behavior, and then classify the
  #remaining days as a function of the percentage of time above 20 degrees
  data$isinsurface=ifelse(data$exttemp>20,1,0)
  uniquedates=unique(data$date)
  behav=c('Associated',rep('Other',times=length(uniquedates)-1))
  # if (data$tagno[1] %in% c('00-112','98-347','98-353','98-363','98-372','98-463'
  #                          ,'98-479','99-190','99-213','99-216','99-224','99-237',
  #                          '99-243','99-247','99-262')){behav[1]="Unassociated"}
  
  sunrises=aggregate(datetime~as.character(date),data=data[data$phase=='dawn',],mean)
  names(sunrises)=c('date','timesunrise')
  
  sunsets=aggregate(datetime~as.character(date),data=data[data$phase=='dusk',],mean)
  names(sunsets)=c('date','timesunset')
  
  data$sunrise=sunrises$timesunrise[match(as.character(data$date),sunrises$date)]
  data$sunset=sunsets$timesunset[match(as.character(data$date),sunsets$date)]
  data=data[complete.cases(data),]
  for (i in 2:length(uniquedates)){
    # print(i)
    
    #Use the last point within one hour of sunrise in which the fish goes beyond 150 m (if it is the case)
    timedivetodepth=rev(data$datetime[data$date==uniquedates[i] & data$datetime>(data$sunrise-60*60) & data$datetime<(data$sunrise+60*60) & data$depth>150])[1]
    #forages to the surface between deepdive & one hour before sunset
    if (!is.na(timedivetodepth)){
      daydata=data[data$datetime>=timedivetodepth & data$date==uniquedates[i] & data$datetime<data$sunset-60*60,c("datetime","isinsurface","exttemp")]
      if (nrow(daydata)>0){
        daydata$foragetosurface=c(0,diff(daydata$isinsurface))
        daydata$surfaceeventno=ifelse(daydata$isinsurface==1,cumsum(ifelse(daydata$foragetosurface<1,0,1)),0)
        numberofforages=max(daydata$surfaceeventno)
        maxdurationforage=max(tapply(as.numeric(daydata$datetime[daydata$isinsurface==1]),daydata$surfaceeventno[daydata$isinsurface==1],function(x){(max(x)-min(x))/60}))
        if (numberofforages<24 & maxdurationforage<=30){behav[i]="Unassociated"}
      }
    }
    # plot(data$datetime[data$date==uniquedates[i]],data$depth[data$date==uniquedates[i]],ylim=c(500,0))
    # abline(v=daydata$datetime[!duplicated(daydata$surfaceeventno)],col='red')
    #         abline(v=data$sunset[data$date==uniquedates[i]],col='grey')
    #         if (!is.na(timedivetodepth)){
    #           abline(v=timedivetodepth,col='red',lwd=2)
    #           legend('bottomright',c(paste('Forages=',numberofforages,sep=''),paste('max.duration=',maxdurationforage)))
    #         }
    
  }
  require(data.table)
  data=data.table(data)
  data$date=as.POSIXct(data$date)
  percAbove20=data[phase=="day",list(percAbove20=length(exttemp[exttemp>20])/length(exttemp)), by = list(date,phase)]
  percAbove20=data.frame(percAbove20)
  if (length(percAbove20$date)<length(uniquedates)){
    percAbove20=rbind(percAbove20,data.frame(date=uniquedates[!uniquedates %in% percAbove20$date],phase="day",percAbove20=.6))
  }
  percAbove20=percAbove20[order(as.numeric(percAbove20$date)),]
  behav2=data.frame(date=uniquedates,behav1=behav,percAbove20=percAbove20$percAbove20,stringsAsFactors=F)
  behav2$behav2=behav2$behav1
  for (i in 2:nrow(behav2)){
    if (behav2$behav2[i]!="Unassociated"){
      if ((behav2$behav2[i-1]=="Associated" & behav2$percAbove20[i]>=.74) | behav2$percAbove20[i]>=.83){
        behav2$behav2[i]="Associated"
      } else behav2$behav2[i]="Other"
    }
  }
  behav2=behav2[,c(date="date",behav="behav2")]
  names(behav2)=c('date','behav')
  behav2
}
