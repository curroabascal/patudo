#' This is a function to estimate length given the length at release and the time at liberty, assigns 
#lengths, based on a par file (not used in the current study.
#' @param Lrel Lengths at release
#' @param tal Times at liberty (days)
#' @param repfilepath Path to par file (plot-OUT.par.rep)
#' @return vector with lengths
#' @export
#' @examples
#' #Some dummy data to check the functions are working as expected and to compare both
#' #2015 assessment data
#' repfilepath="plot-OUT.par.rep"
#' x11(20,15)
#' par <- readLines(repfilepath)
#' pos1 <- grep("Mean lengths at age", par)
#' MeanLatAge<- as.numeric(unlist(strsplit(par[pos1 + 1], split = "[[:blank:]]+"))[-1])
#' MeanLatAge<-data.frame(age=seq(1:length(MeanLatAge)),L=MeanLatAge)
#' '
#' plot(MeanLatAge$age,MeanLatAge$L,'l',pch=16,xlab='Age',ylab='length',lwd=2,ylim=c(0,180))
#' lines((1:40),estlength(MeanLatAge$L[1],(0:39)*365.25/4),col='green',lwd=2) #Assume L0 is the same as in 2015 assessment.
#' 
#' #Some dummy data
#' dummy=data.frame(indiv=c(1,1,1,2,2,2),Lrel=c(30,30,30,80,80,80),tal=c(200,700,1500,200,700,1500))
#' dummy$reclen_new=estlength(dummy$Lrel,dummy$tal)
#' dummy$reclen_old=estlengthold(dummy$Lrel,dummy$tal,repfilepath = './plot-OUT.par.rep')
#' 
#' #Make functions to anchor Lrel
#' ageatlengthold=splinefun(MeanLatAge$age~MeanLatAge$L)
#' dummy$agerel_old=ageatlengthold(dummy$Lrel)
#' 
#' ageatlengthnew=splinefun((1:40)~estlength(MeanLatAge$L[1],(0:39)*365.25/4))
#' dummy$agerel_new=ageatlengthnew(dummy$Lrel)
#' 
#' apply(dummy,1,function(x){arrows(x[6],x[2],x[6]+x[3]/365.25*4,x[5],col='red',angle=45,length=.05)})
#' apply(dummy,1,function(x){arrows(x[7],x[2],x[7]+x[3]/365.25*4,x[4],col='green',angle=45,length=.05)})
#' legend('bottomright',legend=c('2015 assessment','2023 assessment'),lwd=2,col=c('black','green'))
#' savePlot('ComparissonofgrowthFunctions.jpg',type='jpeg')

