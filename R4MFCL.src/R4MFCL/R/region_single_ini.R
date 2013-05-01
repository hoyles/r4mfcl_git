 region_single_ini <-
function(ini) {
  # Simon D Hoyle March 2010
  # Change to a single region, removing all fisheries outside that region
  ini$movemap <- 0
  ini$diffcoffs <- 0
  ini$recbyreg <- 1
  return(ini)
  }
