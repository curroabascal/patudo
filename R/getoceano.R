#' Function to interpolate values of oceanographic data (mixed layer depth or depth of an isocline)
#' @param product Product to interpolate Mixed layer depth (mld) or Potential temperature (temp)
#' @param foldername folder with data
#' @param lons longitudes in decimal degrees
#' @param lats latitudes in decimal degrees
#' @param years vector with years
#' @param months vector with months
#' @param isocline isocline to interpolate (Celsius)
#' @return vector with interpolated depth values
#' @export
#' @examples
#' dir.create('oceanodata')
#' downloadgodas(product='temp',year=1999,minlon=120,maxlon=240,minlat=-40,maxlat=40,maxdepth=500,outputfolder = 'oceanodata')
#' isodepth('oceanodata',year=1999,isocline=20)
#' getoceano(product = 'isoclinedepth',foldername='oceanodata',lons=c(145,164,192),lats=c(-15,-3.5,24),years=c(1999,1999,1999),months=c(1,3,3),isocline=20)


getoceano<-function(product=c('mld','isoclinedepth'),foldername,lons,lats,years,months,isocline=NULL){
  if(!"fields" %in% installed.packages()) {install.packages("fields")} 
  require(fields)
  
  locdat=data.frame(years,months,lons,lats)
  res=rep(NA,length(years))
    for (yy in 1:length(unique(years))){
      prod_name=c('dbss_obml','isocline')[match(product,c('mld','isoclinedepth'))]
      if(substr(foldername,nchar(foldername),nchar(foldername))!='/'){foldername=paste0(foldername,'/')}
      if (prod_name=='dbss_obml'){filename=paste0(foldername,prod_name,'_',unique(years)[yy],'.rds')} else {
        if (prod_name=='isocline') {filename=paste0(foldername,'depth_',isocline,'_isocline_',unique(years)[yy],'.rds')}
      }
      if (file.exists(filename)){
        oceanodat=readRDS(filename)
        locdat2 <- locdat[years == unique(years)[yy],]
       
        jnk = matrix(rep(NA, nrow(locdat2)), ncol = 1)
        dummy = tapply(1:nrow(locdat2), INDEX = locdat2$months, 
                       FUN = function(k) {
                         jnk[k] <<- interp.surface(list(x = oceanodat$lons, 
                                                        y = oceanodat$lats, z = oceanodat[[4]][, , unique(locdat2$months[k])]), 
                                                   cbind(locdat2$lon[k], locdat2$lat[k]))
                       })
        res[locdat$years == unique(years)[yy]] <- jnk
      } else {print(paste('File for year',unique(years)[[y]],'not found'))}
    }
  res
}

