#' Function to convert light at depth to light at surface
#' using a slight modification of the function in Rlibrary trackit. 
#' @param datetime vector of datetime as POSIXct
#' @param light vector with light levels
#' @param depth vector with depths
#' @param weekbyweek calculate attenuation coefficients per week or for whole time series
#' @return Returns a vector with corrected light levels
#' @export

two.layer.depth.corr.Curro=function (datetime,light,depth, weekbyweek = FALSE, D0 = 50)
{
  #I also add an if clause, for the cases where the depth is not greater than D0 (it gaved an error)
  archivaldata=data.frame(datetime=datetime,light=light,depth=depth)
  archivaldata$year=as.numeric(format(archivaldata$datetime,'%Y'))
  archivaldata$month=as.numeric(format(archivaldata$datetime,'%m'))
  archivaldata$day=as.numeric(format(archivaldata$datetime,'%d'))
  archivaldata$oldlight=as.numeric(archivaldata$light)
  
  archivaldata$K1=rep(NA,nrow(archivaldata))
  archivaldata$K2=rep(NA,nrow(archivaldata))
  archivaldata$K100=rep(NA,nrow(archivaldata))
  if (!weekbyweek) {
    p <- ifelse(archivaldata$depth > 50, archivaldata$depth - D0, 0)
    n <- ifelse(archivaldata$depth <= 50, archivaldata$depth, D0)
    dl <- diff(archivaldata$oldlight)
    dp <- diff(p)
    dn <- diff(n)
    ok <- complete.cases(cbind(dl, dp, dn))
    y <- rep(NA, 2)
    y[1] <- -sum(dl[ok] * dn[ok])
    y[2] <- -sum(dl[ok] * dp[ok])
    A <- matrix(NA, 2, 2)
    A[1, 1] <- sum(dn[ok]^2)
    A[1, 2] <- sum(dn[ok] * dp[ok])
    A[2, 1] <- A[1, 2]
    A[2, 2] <- sum(dp[ok]^2)
    if (y[1]>0 & y[2]>0){
      K <- solve(A, y)
      archivaldata$light <- archivaldata$oldlight + K[1] * n + K[2] * p
      archivaldata$K1=K[1]
      archivaldata$K2=K[2]
    } else {
      if (y[1]==0){
        K=c(NA,y[2]/A[2,2])
        archivaldata$light <- archivaldata$oldlight + K[2] * p
        archivaldata$K1=NA
        archivaldata$K2=K[2]
      } else {
        K=c(y[1]/A[1,1],NA)
        archivaldata$light <- archivaldata$oldlight + K[1] * n
        archivaldata$K1=K[1]
        archivaldata$K2=NA
      }
    }
    
    
    q1 <- ifelse(archivaldata$depth <= 100, archivaldata$depth, 100)
    q2 <- ifelse(archivaldata$depth > 100, archivaldata$depth-100, 0)
    dl <- diff(archivaldata$oldlight)
    dq1=diff(q1)
    dq2=diff(q2)
    ok <- complete.cases(cbind(dl, dq1, dq2))
    y <- rep(NA, 2)
    y[1] <- -sum(dl[ok] * dq1[ok])
    y[2] <- -sum(dl[ok] * dq2[ok])
    A <- matrix(NA, 2, 2)
    A[1, 1] <- sum(dq1[ok]^2)
    A[1, 2] <- sum(dq1[ok] * dq2[ok])
    A[2, 1] <- A[1, 2]
    A[2, 2] <- sum(dq2[ok]^2)
    archivaldata$K100=ifelse(y[1]>0,y[1]/A[1,1],NA)
  }
  else {
    #        f <- as.factor(paste(archivaldata$year, "x", archivaldata$month, "x",
    #            archivaldata$day, sep = ""))
    
    f<-as.factor(paste(archivaldata$year,"x",trunc(as.numeric(difftime(ISOdate(archivaldata$year, archivaldata$month,archivaldata$day),ISOdate(archivaldata$year-1,12,31)))/7),sep=''))
    dummy <- tapply(1:nrow(archivaldata), INDEX = f, FUN = function(i) {
      archivaldata$light[i] <<- two.layer.depth.corr.Curro(archivaldata$datetime[i],archivaldata$light[i],archivaldata$depth[i], weekbyweek = FALSE,
                                                           D0 = D0)
      NULL
    })
  }
  return(archivaldata$light)
}

