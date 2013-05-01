 fix_growth <-
function(a) {
# SDH 10/7/2009
  a <- a[grep("1 12 1",a,invert=T)]  # Lmin
  a <- a[grep("1 13 1",a,invert=T)]  # Lmax
  a <- a[grep("1 14 1",a,invert=T)]  # K
  a <- a[grep("1 15 1",a,invert=T)]  # generic sigma
  a <- a[grep("1 16 1",a,invert=T)]  # size-dependent sigma
  a <- a[grep("1 184 1",a,invert=T)] # growth deviates
  a <- a[grep("1 227 1",a,invert=T)] # growth deviates
  a <- add.flag(a,1,12,0,phase=1)
  a <- add.flag(a,1,13,0,phase=1)
  a <- add.flag(a,1,14,0,phase=1)
  a <- add.flag(a,1,15,0,phase=1)
  a <- add.flag(a,1,16,0,phase=1)
  a <- add.flag(a,1,184,0,phase=1)   # Estimate offsets
  a <- add.flag(a,1,227,0,phase=1)   # Estimate offsets
}
