create.rr.file <- function(parfile=basepar,fshcode=fdesc,gear=spp_fleets$gear,
                           inres=read.ini(baseini),prognames=c("RTTP","PTTP","CS"),regname="regions") # prognames must be in order that they appear in ini file
{
# Need to create data.frame "params"
grpmat <- inres$grpflags
grpmat <- grpmat[match(unique(apply(grpmat,1,sum)),apply(grpmat,1,sum)),]
params <- data.frame(fishery=rep(1:dim(grpmat)[2],length(prognames)), par_grp = as.vector(t(grpmat)), prog=c(rep(prognames,rep(dim(grpmat)[2],length(prognames)))))

# Read in fishery grouping files
nprog <- length(unique(params$prog))
fish_grp <- tapply(params$fishery,params$par_grp,paste,collapse=" ")        #Pointer to fisheries within the fishery reporting groups
fshryRRgp <- data.frame(Fshry=params$fishery,Fsh_repgrp=params$par_grp,
                  Meth=rep(gear,times=nprog),Flag=rep(fshcode$flag,times=nprog),
                  Reg = rep(fshcode[,regname],times=nprog),Prog = params$prog)
fshryRRgp$fsh_rel_ptr <- paste(fshryRRgp$Fshry,"_",fshryRRgp$Fsh_repgrp,sep="")
fshryRRgp$fshgp_meth <- rep(NA,length=length(fshryRRgp$Fshry))
fshryRRgp$fshgp_reg <- rep(NA,length=length(fshryRRgp$Fshry))
fshryRRgp$fshgp_flg <- rep(NA,length=length(fshryRRgp$Fshry))
fshryRRgp$fshgp_prg <- rep(NA,length=length(fshryRRgp$Fshry))
# Unique method, region and program codes for fish_gps
for(i in 1:length(fish_grp)){
  fshryptr <- as.numeric(unlist(strsplit(as.vector(fish_grp[i]),"[[:blank:]]+")))
  meths <- unique(fshryRRgp[fshryRRgp$Fshry %in% fshryptr,"Meth"])
  regs <- unique(fshryRRgp[fshryRRgp$Fshry %in% fshryptr,"Reg"])
  flgs <- unique(fshryRRgp[fshryRRgp$Fshry %in% fshryptr,"Flag"])
  prgs <- unique(fshryRRgp[fshryRRgp$Fsh_repgrp == i,"Prog"])
  #meths
  if(length(meths) > 1){
    meths <- paste(as.character(meths),collapse="_")
  } else {
    meths <- as.character(meths)
  }
  fshryRRgp[fshryRRgp$Fshry %in% fshryptr,"fshgp_meth"] <- meths
  #regs
  if(length(regs) > 1){
    regs <- paste("R",gsub(", ","",toString(regs), fixed=TRUE),sep="")
  } else {
    regs <- paste("R",regs,sep="")
  }
  fshryRRgp[fshryRRgp$Fshry %in% fshryptr,"fshgp_reg"] <- regs
  #flags
  if(length(flgs) > 1){
    flgs <- gsub(" ","",toString(flgs), fixed=TRUE)
  } else {
    flgs <- as.character(flgs)
  }
  fshryRRgp[fshryRRgp$Fshry %in% fshryptr,"fshgp_flg"] <- flgs
  #progs
  if(length(prgs) == length(unique(fshryRRgp$Prog))){
    prgs <- c("ALL")
  } else if(length(prgs) > 1 && length(prgs) < length(unique(fshryRRgp$Prog)))  {
    prgs <- paste(as.character(progs),collapse="_")
  } else {
    prgs <- as.character(prgs)
  }
  fshryRRgp[fshryRRgp$Fsh_repgrp == i,"fshgp_prg"] <- prgs
}

fshryRRgp$rel_fsh_grp_lab <- paste(fshryRRgp$fshgp_meth,fshryRRgp$fshgp_reg,fshryRRgp$fshgp_flg,fshryRRgp$fshgp_prg,sep="_")
fshryRRgp$fsh_grp_lab <- paste(fshryRRgp$fshgp_meth,fshryRRgp$fshgp_reg,fshryRRgp$fshgp_flg,sep="_")

#
# Dframe unique to rel_fsh_grp_lab
if(length(unique(fshryRRgp$Fsh_repgrp)) != length(unique(fshryRRgp$rel_fsh_grp_lab))) print("Unique fishery- and release-group specific RR groups don't match")

RR_grps <- data.frame(rel_fsh_grp = c(1:length(fish_grp)), fshry = rep(NA,length=length(fish_grp)),
           release = rep(NA,length=length(fish_grp)), flag = rep(NA,length=length(fish_grp)), labl = rep(NA,length=length(fish_grp)))
for(i in 1:length(fish_grp)){
  tmp <- sort(unique(as.numeric(unlist(strsplit(as.vector(fish_grp[i]),"[[:blank:]]+")))))
  RR_grps[i,"fshry"] <- paste(as.character(tmp),collapse=" ")
  ptr <- match(i,fshryRRgp$Fsh_repgrp,nomatch=0)
  if(length(ptr) > 1){print("Error in pointer to Fsh_repgrp"); break()}
  RR_grps[i,"release"] <- fshryRRgp[ptr,"fshgp_prg"]
  RR_grps[i,"flag"] <- fshryRRgp[ptr,"fshgp_flg"]
  RR_grps[i,"labl"] <- fshryRRgp[ptr,"rel_fsh_grp_lab"]
  RR_grps[i,"fshgp_labl"] <- fshryRRgp[ptr,"fsh_grp_lab"]
}
#
tagrep1 <- data.frame(grp=RR_grps$rel_fsh_grp,release=RR_grps$release,flag=RR_grps$flag,label=RR_grps$fshgp_labl)
return(tagrep1)
}
