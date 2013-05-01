 check_flag_value <-
function(parname,flagtype,flagnums,fishery=NA,flaglist=T) {
  # Simon Hoyle July 2010
  if(flaglist) flags <- read.csv("I:/assessments/Pop dy modeling/MFCL/flaglist.csv",stringsAsFactors=F)
  a <- parname
  if(is.na(fishery)==T) fishery <- 1:(dim(a$ffl)[1])
  if(flagtype==1) res <- data.frame(fl_type="parest",num=flagnums,val=a$pfl[flagnums],flag_desc=flags$parest_flags[flagnums])
  if(flagtype==2) res <- data.frame(fl_type="age",num=flagnums,val=a$afl[flagnums],flag_desc=flags$age_flags[flagnums])
  if(flagtype==3) res <- data.frame(fl_type="fish_flag",num=flagnums,fsh=fishery,val=a$ffl[fishery,flagnums],flag_desc=flags$fish_flags[flagnums])
  return(res)
  }
