# author: karl h. schlag
# date: september 15, 2015
# see 2015 document:  Who gives Direction to Statistical Testing? Best Practice meets Mathematically Correct Tests

# contains two routines for independent samples, 
# one for comparing medains, npMedianUnpaired, one for looking at median difference, npMedianDiffUnpaired

# **********************************************************************************************************
# tests whether median X = median Y based on two independent samples (beta version, waiting for comments)
# if sample sizes too large, use alternative software

npMedianUnpaired <- function (X,Y,alpha=0.05){
  
  
  nx <- length(X)
  ny <- length(Y)
  
  # split test, using z test, two sided
  
  L <- 1.5
  w <- 1/10
  
  # finding r such that only consider medians >=r and <=nx+ny-r
  
  r <- qbinom(w*alpha/4,nx+ny,0.5)
  
  if (r==0) {
    print("need to increase w or alpha or both *****************************************************")
    rm(list=ls(all=TRUE))
  }
  
  bin<-0
  i <- 0
  while (i<r) {
    bin <- bin + 2*choose(nx+ny,i)/ 2^{nx+ny}
    i <- i+1
  }
  
  
  mstatG <- function(k,nx,j,ny,L,r,s){
    if ((k+j<r) | (k+j>nx+ny-r)) mstatG <- Inf
    else {
      mstatG <- (k/nx - j/ny) / ( (k+j)/(nx+ny) * (1 - (k+j)/(nx+ny)) )^L
    } 
    mstatG
  }
  
  mstatL <- function(k,nx,j,ny,L,r,s){
    if ((k+j<r) | (k+j>nx+ny-r)) mstatL <- -Inf
    else {
      mstatL <- (k/nx - j/ny) / ( (k+j)/(nx+ny) * (1 - (k+j)/(nx+ny)) )^L
    } 
    mstatL
  }
  
  
  C <- sort(c(min(X,Y)-1,X,Y,max(X,Y)+1))
  D <- aggregate(C,by=list(C),FUN=mean)
  C <- D[,2]
  
  
  # determining cutoff that leads to rejecting med(X) <= med(Y), so reject if sx-sy is large
  
  rejL <- matrix(0,ncol=length(C),nrow=length(C))
  mL <- matrix(0,ncol=length(C),nrow=length(C))
  
  mcutsmaller <- -Inf
  i<- 1
  while (i<=length(C)) {
    
    mx <- C[i]
    j <- 1
    while (j<=i) {
      my <- C[j]       # so mx <= my
      
      sx <- sum(X>=mx)        
      sy <- sum(Y>my)       # reduce Y, as if using Y-eps
      
      if (mstatL(sx,nx,sy,ny,L,r,s)> mcutsmaller) mcutsmaller <- mstatL(sx,nx,sy,ny,L,r,s)
      
      rejL[i,j] <- mstatL(sx,nx,sy,ny,L,r,s)
      if (sx!=sy) mL[i,j] <- (sx/nx - sy/ny) / ( (sx+sy)/(nx+ny) * (1 - (sx+sy)/(nx+ny)) )^L else mL[i,j] <- 0
      
      j <- j+1
    }  
    i <- i+1
  }
  
  rejG <- matrix(0,ncol=length(C),nrow=length(C))
  mG <- matrix(0,ncol=length(C),nrow=length(C))
  
  mcutgreater <- Inf
  rej <- C
  i<- 1
  while (i<=length(C)) {
    
    mx <- C[i]
    j <- i
    while (j<=length(C)) {
      my <- C[j]
      
      sx <- sum(X>mx)   # reduce X, as if using X-eps
      sy <- sum(Y>=my)
      
      if (mstatG(sx,nx,sy,ny,L,r,s)< mcutgreater) mcutgreater <- mstatG(sx,nx,sy,ny,L,r,s)
      
      rejG[i,j] <- mstatG(sx,nx,sy,ny,L,r,s)
      if (sx!=sy) mG[i,j] <- (sx/nx - sy/ny) / ( (sx+sy)/(nx+ny) * (1 - (sx+sy)/(nx+ny)) )^L else mG[i,j] <- 0
      
      j <- j+1
    }  
    i <- i+1
  }
  
  
  # determining size of test given r for the two different cutoffs
  
  hg <- 0
  k <-0
  while (k<=nx) {
    j <-0
    while (j <= ny) {
      if (mstatG(k,nx,j,ny,L,r,s) >= mcutgreater) {hg <- hg + choose(nx,k) * choose(ny,j) / 2^{nx+ny}}
      j <- j+1
    }
    k <- k+1
  }
  
  hg <- 2*hg # adapt p value for two sided test
  
  hs <- 0
  k <-0
  while (k<=nx) {
    j <-0
    while (j <= ny) {
      if (mstatL(k,nx,j,ny,L,r,s) <= mcutsmaller) {hs <- hs + choose(nx,k) * choose(ny,j) / 2^{nx+ny}}
      j <- j+1
    }
    k <- k+1
  }
  
  hs <- 2*hs     # adapt p value for two sided test
  
  
  # computing alternative test based on intersection of confidence intervals
  
  sX<-sort(X)
  sY<-sort(Y)
  alphaconf <- 1-sqrt(1-alpha)
  cx<-qbinom(alphaconf/2,nx,1/2)-1
  cy<-qbinom(alphaconf/2,ny,1/2)-1
  
  if ((cx<0)|(cy<0)) rc <- 0 else if ((sX[cx]<=sY[ny-cy+1]) & (sX[nx-cx+1]>=sY[cy]))  rc <-0 else rc<-1
  
  dnameX <- deparse(substitute(X))
  dnameY <- deparse(substitute(Y))
  
  cat(">>> Two sided test for comparing medians <<<","\n")
  cat(paste("medians",round(median(X),2),round(median(Y),2),", lengths",length(X),length(Y),", m hat=",round(sum(X>=median(c(X,Y)))/length(X)-sum(Y>=median(c(X,Y)))/length(Y),3)),"\n")
  cat(paste("alternative test, intersect of CI, rej=1, not=0: ",rc),"\n")
  if (hg<=alpha) cat(paste("REJECT med(",dnameX,")=med(",dnameY,") in favor of med(",dnameX,")>med(",dnameY,") at level",alpha),"\n") 
  if (hs<=alpha) cat(paste("REJECT med(",dnameX,")=med(",dnameY,") in favor of med(",dnameX,")<med(",dnameY,") at level",alpha),"\n") 
  if ((hg>alpha)&(hs>alpha)) cat(paste("do not reject med(",dnameX,")=med(",dnameY,") at level",alpha),"\n")
  
}

# *******************************************************************************************************

# tests whether median (X-Y)=d based on two independent samples (beta version, waiting for comments), 
# uses npStochinUnpaired from package npExact
# if sample sizes too large, use alternative software

npMedianDiffUnpaired <- function (X,Y,d=0,alpha=0.05,estimate=FALSE){
  
  library(npExact)
  
  nx <- length(X)
  ny <- length(Y)
  nm <- min(nx,ny)
  
  
  # calculating estimate of median difference by averaging all possible pairings
  
  if (estimate) {
    iterations <- 100000
    i <- 1
    estmeddiff <- 0
    while (i <= iterations) {
      
      Xp <- sample(X)[1:nm]
      Yp <- sample(Y)[1:nm]
      Z <- Xp - Yp
      estmeddiff <- estmeddiff + median(Z)
      i <- i+1
    }
    estmeddiff <- estmeddiff/iterations
  }
  
  # finding smallest difference between values of X and Y
  
  mindiff <- Inf
  i <- 1
  while (i <= nx) {
    j <- 1
    while (j <= ny) {
      if (X[i]!=Y[j]) mindiff <- min(mindiff,abs(X[i]-Y[j]))
      j <- j+1
    }
    i <- i+1
  }
  
  
  dnameX <- deparse(substitute(X))
  dnameY <- deparse(substitute(Y))
  
  G <- npStochinUnpaired(X-mindiff/2,Y+d,alpha=alpha/2,alternative="greater")$rejection
  L <- npStochinUnpaired(Y+d,X+mindiff/2,alpha=alpha/2,alternative="greater")$rejection
  
  cat(">>> Two sided test of median difference <<<","\n")
  cat(paste("sample sizes=",length(X),length(Y),", medians",round(median(X),2),round(median(Y),2),", m hat=",round(sum(X>=median(c(X,Y)))/length(X)-sum(Y>=median(c(X,Y)))/length(Y),3)),"\n")
  if (estimate) cat(paste("estimate of med(",dnameX,"-",dnameY,") is ",signif(estmeddiff,3))) else cat("(to also get an  estimate choose estimate=TRUE)")
  cat("\n")
  if (G) cat(paste("REJECT med(",dnameX,"-",dnameY,")=",d," in favor of med(",dnameX,"-",dnameY,") >",d," at level",alpha),"\n") 
  if (L) cat(paste("REJECT med(",dnameX,"-",dnameY,")=",d," in favor of med(",dnameX,"-",dnameY,") <",d," at level",alpha),"\n") 
  if ((!G)&(!L)) cat(paste("do NOT reject med(",dnameX,"-",dnameY,")=",d," at level",alpha),"\n")
  
}