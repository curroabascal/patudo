#' This is a function to estimate length given the length at release and the time at liberty. VB Linf and K can 
#' be provided, but default parameters for BET, based on the most recent assessment, are provided (email JH, 25/09/2024).
#' @param Lrel Lengths at release
#' @param tal Times at liberty (days)
#' @param Linf L infinity in VB growth equation
#' @param k k in VB growth equation
#' @return vector with lengths
#' @export
#' @examples
#' #Some dummy data
#' dummy=data.frame(indiv=c(1,1,1,2,2,2),Lrel=c(30,30,30,80,80,80),tal=c(200,700,1500,200,700,1500))
#' dummy$L_rec=estlength(dummy$Lrel,dummy$tal)
#' dummy
estlength<-function(Lrel,tal,Linf=150.314,k=0.45104){
  res=Lrel+(Linf-Lrel)*(1-exp(-k*tal/365))
  return(res)
}

