 read.fit <-
function(fit.file) {
  # Simon Hoyle March 2010   
  # loads the observed and expected LF from the length.fit file by fishery and time period

  a <- readLines(fit.file)
  nbins <- as.numeric(unlist(strsplit(a[2],split="[[:blank:]]+")))[1]
  binfirst <- as.numeric(unlist(strsplit(a[2],split="[[:blank:]]+")))[2]
  binwidth <- as.numeric(unlist(strsplit(a[2],split="[[:blank:]]+")))[3]
  #number of fisheries
  nfish <- as.numeric(a[3]) - 1
  #records per fishery
  recsperfish <- as.numeric(unlist(strsplit(a[4],split="[[:blank:]]+"))[-1])
  # number of age classes
  nages <- as.numeric(a[5])
  # fishery locations
  fishlocs <- grep("fishery",a)   # f <- 1
  dates <- list(); obslf <- list(); predlf <- list()
  for (f in 1:nfish) {
    if (recsperfish[f] > 0) {
      dloc <- seq(fishlocs[f]+1,by=7+nages,length.out=recsperfish[f])
      dates[[f]] <- data.frame(t(matrix(as.numeric(unlist(strsplit(a[dloc],split="[[:blank:]]+"))),nrow=3)))
      obsloc <- seq(fishlocs[f]+4,by=7+nages,length.out=recsperfish[f])
      obslf[[f]] <- data.frame(t(matrix(as.numeric(unlist(strsplit(a[obsloc],split="[[:blank:]]+"))),nrow=nbins+1)))[,-1]
      predlf[[f]] <- data.frame(t(matrix(as.numeric(unlist(strsplit(a[obsloc+1],split="[[:blank:]]+"))),nrow=nbins+1)))[,-1]
      }
    }
  return(list(dates=dates,obslf=obslf,predlf=predlf))
  }
