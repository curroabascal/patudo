#' This is a function to download data from GODAS ncep using opendap.
#' @param product Product to download. Mixed layer depth (mld) or Potential temperature (temp)
#' @param year year (numeric)
#' @param minlon Longitude in decimal degrees
#' @param maxlon Latitude in decimal degrees
#' @param minlat Longitude in decimal degrees
#' @param maxlat Latitude in decimal degrees
#' @param maxdepth Maximum depth to download
#' @param outputfolder folder to store downloads
#' @return Saves the download as an RDS file in the output folder
#' @export
#' @examples
#' dir.create('oceanodata')
#' downloadgodas(product='temp',year=1999,minlon=120,maxlon=240,minlat=-40,maxlat=40,maxdepth=500,outputfolder = 'oceanodata')
#' downloadgodas(product='mld',year=2000,minlon=120,maxlon=240,minlat=-40,maxlat=40,outputfolder = 'oceanodata')


downloadgodas<-function(product=c('mld','temp'),year,minlon,maxlon,minlat,maxlat,maxdepth,outputfolder){
  prod_name=c('dbss_obml','pottmp')[match(product,c('mld','temp'))]
  url=paste0('http://psl.noaa.gov/thredds/dodsC/Datasets/godas/',prod_name,'.',year,'.nc')
  if(!"RNetCDF" %in% installed.packages()) {install.packages("RNetCDF")} 
  require(RNetCDF)
  ds<-open.nc(url)
  lons <- var.get.nc(ds, 'lon')
  lats <- var.get.nc(ds, 'lat')
  
  btw <- function(data, numrange){
    c(min(which(data>=numrange[1])), max(which(data<=numrange[2])))
  }
  
  
  lon_indices <- btw(data = lons, num = c(minlon,maxlon))
  lat_indices <- btw(data = lats, num = c(minlat,maxlat))
  
  # Count number of indices to extract along each dimension
  lon_range <- lon_indices[2] - lon_indices[1]+1
  lat_range <- lat_indices[2] - lat_indices[1]+1
  
  
  # Start and Count vectors
  offset <- c(lon_indices[1], lat_indices[1], 1)    #lon,lat,depth,time
  count <- c(lon_range, lat_range, 12)
  
  if (prod_name!="dbss_obml"){
    depths <- var.get.nc(ds, 'level')
    depth_indices <- btw(data = depths, num = c(0,maxdepth))
    depth_range <- depth_indices[2] - depth_indices[1]+1
    offset <- c(lon_indices[1], lat_indices[1], depth_indices[1],1)    #lon,lat,depth,time
    count <- c(lon_range, lat_range, depth_range,12)
  }
  
  # Get subsetted variable   
  prod=list(lons=lons[lon_indices[1]:lon_indices[2]],lats=lats[lat_indices[1]:lat_indices[2]],months=1:12)
  if (prod_name!="dbss_obml"){
    prod[["depth"]]=depths[depth_indices[1]:depth_indices[2]]
  }
  prod[[length(prod)+1]] <- var.get.nc(ds,prod_name, start = offset, count = count)
  close.nc(ds)
  
  if (product=='temp'){prod[[length(prod)]]=prod[[length(prod)]]-273} #better in degrees celsius.
  names(prod)[length(prod)]<-prod_name
  if(substr(outputfolder,nchar(outputfolder),nchar(outputfolder))!='/'){outputfolder=paste0(outputfolder,'/')}
  saveRDS(prod,paste0(outputfolder,prod_name,'_',year,'.rds'))
}
