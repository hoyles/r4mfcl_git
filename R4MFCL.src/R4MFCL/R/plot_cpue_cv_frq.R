 plot_cpue_cv_frq <-
function(frq,parf,fisheries) {
  # Simon Hoyle 18 July 2010
  # Takes a version 6 frq file and par file and plots the CPUE and CVs for the chosen fisheries. Currently the fisheries need to have effort wts.
  wts1 <- read.par(parf)$ffl[,13]
  m <- as.data.frame(frq$mat)
  colnames(m)[4:7] <- c("f","catch","effort","wt")
  m$yq <- m[,1] + m[,2]/12
  m$cpue <- m$catch/m$effort
  m$cv <- sqrt(1/(2 * abs(wts1[m$f]) * m$wt))
  for(f in fisheries) {
    a <- m[m$f == f & m$effort > -1,]
    upper <- a$cpue + 1.96* a$cv
    lower <- a$cpue - 1.96* a$cv
    plot(a$yq,a$cpue,ylim=range(c(upper,lower,a$cpue)),main=paste("Fishery",f))
    segments(a$yq, upper,  a$yq, lower, lty=1, col="slate grey")
    }
  }
