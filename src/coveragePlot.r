## I'm Rtist
## Goal: coverage plot flanking certain sites, i.e. splicing site or TSS, with 
##	95% confidential internval. 


## Prequested usr-defined function
## yield 95% confidential interval
## assuming it fits normal distribution
colConfiLow <- function (m)
{
  colConfiLowValue = apply(m, 2, function (cl) {
    clSd = sqrt(var(cl))
    clN = length (cl)
    clMean = mean(cl)
    clLow = clMean - 1.96 * clSd / sqrt(clN)
  })
  return (colConfiLowValue)
  
}

colConfiUpper <- function (m)
{
  colConfiUpperValue = apply(m, 2, function (cl) {
    clSd = sqrt(var(cl))
    clN = length (cl)
    clMean = mean(cl)
    clLow = clMean + 1.96 * clSd / sqrt(clN)
  })
  return (colConfiUpperValue)
  
}

## Bootstrap to esitmate the CI
library(boot)
getBoot <- function (data, indices){
  d <- data[indices]
  return (mean(d, na.rm=T))
}

## Dummy data 
## Coverage Table
left <- matrix(rnorm(20000, mean=10, sd=10), 200, 100)
right <- matrix(rnorm(20000, mean=15, sd=20), 200, 100)
covMx <- cbind (left, rep(0, 200), right)

## Data processing
## get mean, uppeer, lower coverage value for each site
covMn <- colMeans(covMx, na.rm=T)
covLow <- colConfiLow(covMx)
covUpper <- colConfiUpper(covMx)

## Coverage Plots
library(RColorBrewer)
myColor = brewer.pal(9, "Blues")
myOrg = brewer.pal(9, "OrRd")
par(mfrow=c(2,1))

## 95% CI estimated with normal distribtution
plot(1:201, covUpper * 1.02, type = "n",xaxt="n",xlab = "",ylab="Coverage", 
  ylim=c(0,23),
  main="CI estimated by Normal distribution")
## polygon x and y positions
x <- c(1, 1:201, 201)
yU <- c(0, covUpper,0)
yL <- c(0, covLow, 0)
polygon(x, yU, border="white", col=myColor[3])
polygon(x, yL, border="white", col="white")
lines(1:201, covMn, col=myColor[9])
axis(side=1, at=c(1,50,101,151,201),labels=c("-100","-50", "Center","+50","+100"))

## 95% CI by bootstrap
covMn <- colMeans(covMx, na.rm=T)
positionBootCIrpt <- apply(covMx, 2, function(cl) {
  clBoot <- boot(data=cl, statistic=getBoot, R=length(cl))
  clBootCI <- boot.ci(clBoot, conf=0.95, type="perc")
  return(clBootCI)
})
positionBootCI <- sapply(positionBootCIrpt, function(x) {
  y <- list()
  x_CI <- x$percent
  y$covLower <- x_CI[length(x_CI)-1]
  y$covUpper <- x_CI[length(x_CI)]
  return(y)
})
covLow <- unlist(positionBootCI[row.names(positionBootCI) == "covLower"])
covUpper <- unlist(positionBootCI[row.names(positionBootCI) == "covUpper"])
plot(1:201, covUpper * 1.02, type = "n",xaxt="n",xlab = "",ylab="Coverage", 
  ylim=c(0,23), 
  main="CI estimated by Bootstrap")
## polygon x and y positions
x <- c(1, 1:201, 201)
yU <- c(0, covUpper,0)
yL <- c(0, covLow, 0)
polygon(x, yU, border="white", col=myOrg[3])
polygon(x, yL, border="white", col="white")
lines(1:201, covMn, col=myOrg[9])
axis(side=1, at=c(1,50,101,151,201),labels=c("-100","-50", "Center","+50","+100"))

