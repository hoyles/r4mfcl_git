 region_single_frq <-
function(frq,region) {
  # Simon D Hoyle March 2010
  # Change to a single region, removing all fisheries outside that region
  frq$struct$fishreg <- 1
  frq$struct$nf <- sum(frq$fish$fishreg == region)
  keep <- frq$fish$fishreg==region
  frq$struct$nreg <- 1
  frq$mpy <- 1
  frq$mweeks <- 1
  frq$fish <- frq$fish[keep,]
  frq$fish$fishreg <- 1
  frq$reg$relreg <- 1
  frq$reg$incidence <- 0
  frq$mat <- frq$mat[keep[frq$mat[,4]],]
  oldfish <- frq$fish$fishery
  newfish <- seq(1:frq$struct$nf)
  frq <- rename.fisheries.frq(frq,oldfish,newfish)
  return(frq)
  }
