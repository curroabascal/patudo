#' This is a function to check the rdata with the archival data has been downloaded.
#' Git LFS is tricky

#' @export
checkfiles<-function(){
  eezfilename=system.file("data", "archives.RData", package = "patudo")
  if (file.info(eezfilename)$size<10000000){
    file_url="https://github.com/curroabascal/myareas/raw/refs/heads/master/data/archives.RData?download="
    download.file(file_url, destfile = eezfilename,mode='wb')
  }
}