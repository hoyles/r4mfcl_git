compare_par_flags <- function(par1,par2,flaglist=T) {
  # SDH July 2010
  # please update flaglist.csv for new flags
  if(flaglist) flags <- read.csv("I:/assessments/Pop dy modeling/MFCL/flaglist.csv",stringsAsFactors=F)
  nf1 <- dim(par1$ffl)[1]
  nf2 <- dim(par2$ffl)[1]
  if(nf1 != nf2) {
    print(paste("Par1 has ",nf1," fisheries and par2 has ",nf2,".",sep=""))
    if(nf1 > nf2) par1$ffl <- par1$ffl[1:nf2,]
    if(nf1 < nf2) par2$ffl <- par2$ffl[1:nf1,]
    }
  matchpfl <- which(par1$pfl != par2$pfl)
  matchafl <- which(par1$afl != par2$afl)
  matchffl <- which(par1$ffl != par2$ffl, arr.ind = T)

  pfldiffs <- as.data.frame(cbind(matchpfl,par1$pfl[matchpfl],par2$pfl[matchpfl]))
  afldiffs <- as.data.frame(cbind(matchafl,par1$afl[matchafl],par2$afl[matchafl]))
  ffldiffs <- as.data.frame(cbind(matchffl,par1$ffl[matchffl],par2$ffl[matchffl]))

  colnames(pfldiffs) <- c("flag","par1","par2");rownames(pfldiffs) <- NULL
  colnames(afldiffs) <- c("flag","par1","par2");rownames(afldiffs) <- NULL
  colnames(ffldiffs) <- c("fishery","flag","par1","par2");rownames(ffldiffs) <- NULL

  if(flaglist) {
    pfldiffs$flagid <- flags$parest_flags[pfldiffs[,1]]
    afldiffs$flagid  <- flags$age_flags[afldiffs[,1]]
    ffldiffs$flagid  <- flags$fish_flags[ffldiffs[,2]]
    }
  return(list(pfl=pfldiffs,afl=afldiffs,ffl=ffldiffs))
 }
