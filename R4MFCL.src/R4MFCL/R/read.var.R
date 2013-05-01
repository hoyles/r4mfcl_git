 read.var <-
function(var.file) {
# By Simon D Hoyle 2009
   a <- readLines(var.file)
   a <- sub("^[[:blank:]]+","",a)
   a <- a[-c(1:3)]
    pos <- grep(" rbio\\([[:digit:]]+\\)$",a) ; rbio <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    nTimes <- dim(rbio)[[1]]
    pos <- grep(" adult_rbio\\([[:digit:]]+\\)$",a) ; adult_rbio <- t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" ln_reg_bio",a) ; ln_reg_bio <- t(sapply(a[pos],varfromstr,USE.NAMES =F)) ;
    nReg<- dim(ln_reg_bio)[[1]]/nTimes
    ln_reg_bio <- array(ln_reg_bio,c(nTimes,nReg,2))
    pos <- grep(" ln_adult_reg_bio",a) ; ln_adult_reg_bio <- t(sapply(a[pos],varfromstr,USE.NAMES =F)) ; ln_adult_reg_bio <- array(ln_adult_reg_bio,c(nTimes,nReg,2))
    pos <- grep(" msum",a) ; msum <- t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" nat_mort",a) ; nat_mort <-  as.vector(t(sapply(a[pos],varfromstr,USE.NAMES =F)))
    pos <- grep(" sv",a) ; sv <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" ln_rbio_maxr",a) ; ln_rbio_maxr <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" log\\(rbio\\(",a) ; ln_rbio_depletion <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" ln_ln_rbio_mean",a) ; ln_ln_rbio_mean <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" ln_rel_bio\\(",a) ; ln_rel_bio <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" ln_abs_bio",a) ; ln_abs_bio <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" relative_spawning_biomass",a) ; relative_spawning_biomass <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" ln_relative_recr",a) ; ln_relative_recr <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" ln_abs_recr",a) ; ln_abs_recr <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" lcatch\\-diff",a) ; lcatch_diff <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    nFisheries <- dim(lcatch_diff)[[1]]
    unseasoned_lcatch_diff <- array(dim=c(nFisheries,2))
    for (i in 1:nFisheries) {
      pos <- grep(paste("unseasoned_lcatch\\-diff\\(",i,sep=""),a)
      if(length(pos)>0) {unseasoned_lcatch_diff[i,] <-  varfromstr(a[pos])} #t(sapply(a[pos],varfromstr,USE.NAMES =F))
      }
    pos <- grep(" pred_yield_for_F_mult",a) ; pred_yield_for_F_mult <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" pred_equilib_SB_for_F_mult",a) ; pred_equilib_SB_for_F_mult <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" pred_equilib_totbio_for_F_mult",a) ; pred_equilib_totbio_for_F_mult <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" AdultB\\/AdultBmsy",a) ; AdultB_AdultBmsy <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" TotalB\\/TotalBmsy",a) ; TotalB_TotalBmsy <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" F\\/Fmsy",a) ; F_Fmsy <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    pos <- grep(" pred_rec_for_bio_level",a) ; pred_rec_for_bio_level <-  t(sapply(a[pos],cols=c(2,3,5),varfromstr,USE.NAMES =F))
    pos <- grep(" diffusion_rate_for_age_class",a) ; diffusion_rate_for_age_class <-  t(sapply(a[pos],varfromstr,USE.NAMES =F))
    var.obj <- list(nTimes=nTimes,nReg=nReg,nFisheries=nFisheries,
    rbio=rbio,adult_rbio=adult_rbio,ln_reg_bio=ln_reg_bio,ln_adult_reg_bio=ln_adult_reg_bio,
    msum=msum, nat_mort=nat_mort,sv=sv,
    ln_rbio_maxr=ln_rbio_maxr, ln_rbio_depletion=ln_rbio_depletion,ln_ln_rbio_mean=ln_ln_rbio_mean, ln_rel_bio=ln_rel_bio, ln_abs_bio=ln_abs_bio,
    relative_spawning_biomass=relative_spawning_biomass,
    ln_relative_recr=ln_relative_recr, ln_abs_recr=ln_abs_recr,
    lcatch_diff=lcatch_diff, unseasoned_lcatch_diff=unseasoned_lcatch_diff,
    pred_yield_for_F_mult=pred_yield_for_F_mult, pred_equilib_SB_for_F_mult=pred_equilib_SB_for_F_mult,
    pred_equilib_totbio_for_F_mult=pred_equilib_totbio_for_F_mult, AdultB_AdultBmsy=AdultB_AdultBmsy,
    TotalB_TotalBmsy=TotalB_TotalBmsy, F_Fmsy=F_Fmsy, pred_rec_for_bio_level=pred_rec_for_bio_level,
    diffusion_rate_for_age_class=diffusion_rate_for_age_class )
    return(var.obj)
}
