#' This is a function to check the rdata with the archival data has been downloaded.
#' After some issues with LFS, I put it in saco...

#' @export
checkfiles<-function(){
  archivesfilename = system.file("data", "archives.RData", package = "patudo")
  if (file.info(archivesfilename)$size < 1e+07 | is.na(file.info(archivesfilename)$size)) {
    options(timeout = max(600, getOption("timeout")))
    file_url = "https://saco.csic.es/s/7BjiQBtxbAQToRm/download?path=%2F&files=archives.RData"
    download.file(file_url, destfile = paste0(system.file("data", package = "patudo"),'/archives.RData'), mode = "wb")
  }
}

  