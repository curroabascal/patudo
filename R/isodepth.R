#' Function to read files of potential temperatures from godas, estimate the depth of an isocline and save it as an RDS object
#' in the same folder
#' @param product Product to download. Mixed layer depth (mld) or Potential temperature (temp)
#' @param foldername folder with temperature data
#' @param year year (numeric)
#' @param isocline isocline depth to be estimated (Celsius)
#' @return Saves the download as an RDS file in the output folder
#' @export
#' @examples
#' dir.create('oceanodata')
#' downloadgodas(product='temp',year=1999,minlon=120,maxlon=240,minlat=-40,maxlat=40,maxdepth=500,outputfolder = 'oceanodata')
#' isodepth('oceanodata',year=1999,isocline=20)
#' dat=readRDS('./oceanodata/depth_20_isocline_1999.rds')
#' require(raster)
#' dat2=raster(t(dat$depth[,dim(dat$depth)[2]:1,1]))
#' crs(dat2) <- "+proj=longlat +datum=WGS84"
#' extent(dat2) <- c(range(dat$lons),range(dat$lats))
#' x11(10,10)
#' plot(dat2,main='depth 20 degs isocline in jan 1999')

isodepth<-function(foldername,year,isocline){
  #It can be a bit tricky if, for whatever reason, the same temperature is achieved at two different depths...
  #Instead of doing a function of depth~temp, to overcome this, I make a function of temp~depth, estimate temp at different
  #depths and select the shallowest one
  if(substr(foldername,nchar(foldername),nchar(foldername))!='/'){foldername=paste0(foldername,'/')}
  dat=readRDS(paste0(foldername,'pottmp_',year,'.rds'))
  # plot(dat$pottmp[1,240,,1],dat$depth,ylim=c(400,1))
  isodepth2<-function(temps){
    if (any(!is.na(temps))){
      tempatdepth<-splinefun(x=dat$depth,y=temps)
      tempatdepth2=tempatdepth(1:max(dat$depth))
      res=which.min(abs(tempatdepth2-isocline))
      res=ifelse(isocline>min(tempatdepth2,na.rm=T) & isocline<max(tempatdepth2,na.rm=T),res,NA)
      res
    } else NA
  }
  res=apply(dat$pottmp,c(1,2,4),isodepth2)
  res=list(lons=dat$lons,lats=dat$lats,months=dat$months,depth=res)
  saveRDS(res,paste0(foldername,'depth_',isocline,'_isocline_',year,'.rds'))
}