
# author: rob scott
# 2/03/2015 11:20:01 AM

subCtrl <- function(universe='vanilla', executable='file path', fileTrans='YES',
                    exeTrans='true', outputdir='results\\ ',
                    requirements='WINDOWS', transInputFiles=NULL, mfclFiles=NULL,
                    Rzip='R-3.1.1.zip', RScript=NULL, args=NULL){

     if(!is.element(requirements, c('WINDOWS','LINUX')))
       stop("Error - requirements must be either WINDOWS or LINUX")  
     
     if(is.null(RScript))
       stop("You need to specify an RScript")
     
     if(is.null(mfclFiles))
       stop("You need to specify the MFCL input files eg. skj.frq etc.")
     
     if(is.null(transInputFiles) & requirements=='WINDOWS')
       transInputFiles <- c("UNZIP.exe, mfclo64.exe, msvcp100.dll, msvcr100.dll, mfcl.cfg,")
     
     if(is.null(transInputFiles) & requirements=='LINUX')
       transInputFiles <- c("mfclo64, mfcl.cfg,")
     
     if(requirements=='WINDOWS')
       requirements <- "(OpSys == 'WINDOWS') && \\
                        (Machine != 'SPC112083.noumea.spc.local') && \\
                        (Machine != 'nouofpscalc01.corp.spc.local') && \\
                        (Memory > 15000)"
     
     if(requirements=="LINUX")
       requirements <- "(OpSys == 'LINUX') && \\
                        (Memory > 15000)"
                              
     
     ctrl <- list(universe=universe, executable=executable, fileTrans=fileTrans, 
                  exeTrans=exeTrans, outputdir=outputdir, requirements=requirements,
                  transInputFiles=transInputFiles, mfclFiles=mfclFiles, 
                  RScript=RScript, Rzip=Rzip, args=args)
     return(ctrl)
}


write.sub <- function(name='RSub.sub', dirPath=NULL, subCtrl=NULL){

  subfile <- paste(dirPath, name, sep="")
  
  cat(paste("universe = ", subCtrl$universe, "\n \n", sep=" "), file=subfile)
  
  cat(paste("executable = ", subCtrl$executable, "\n", sep=" "), file=subfile, append = TRUE)
  cat("getenv = true \n \n", file=subfile, append = TRUE)
  
  cat(paste("should_transfer_files =", subCtrl$fileTrans, "\n", sep=" "), file=subfile, append = TRUE)
  
  cat("error  = $(Cluster).$(Process).err \n", file=subfile, append = TRUE)
  cat("log    = $(Cluster).$(Process).log \n", file=subfile, append = TRUE)
  cat("output = $(Cluster).$(Process).out \n \n", file=subfile, append = TRUE)

  cat(paste("Requirements =", subCtrl$requirements, "\n \n", sep=" "), file=subfile, append = TRUE)
  
  cat(paste("transfer_input_files =", subCtrl$transInputFiles, "\\ \n", sep=" "), file=subfile, append=TRUE)
  
  cat('\t \t', paste(subCtrl$mfclFiles, ','), '\\', file=subfile, append=TRUE, fill=80)
  cat('\t \t', subCtrl$Rzip, ',', subCtrl$RScript,  file=subfile, append=TRUE)
  cat("\n \n", file=subfile, append=TRUE)
  
  cat(paste("should_transfer_files =", subCtrl$fileTrans, "\n", sep=" "), file=subfile, append = TRUE)
  cat(paste("transfer_executable =", subCtrl$exeTrans, "\n \n", sep=" "), file=subfile, append = TRUE)
  
  cat(paste("transfer_output_files =", subCtrl$outputdir, "\n \n", sep=" "), file=subfile, append = TRUE)
  
  cat("when_to_transfer_output = ON_EXIT_OR_EVICT \n \n", file=subfile, append=TRUE)
  
  if(!is.null(subCtrl$args))
    argslist <- paste("arguments =", subCtrl$RScript, 
                      apply(sapply(subCtrl$args,c),1, paste, collapse=" "), "\n queue \n \n")
  if(is.null(subCtrl$args))
    argslist <- paste("arguments =", subCtrl$RScript, "\n queue \n \n")
  
  cat(argslist, file=subfile, append=TRUE)
  
}


#subControl <- subCtrl(executable='C:\\temp\\R5bat.bat', RScript='dummy.r', 
#                      mfclFiles=c('skj.frq', '11.par', 'skj.tag'), requirements='WINDOWS',
#                      args=list(seq(0.6,0.8,by=0.1), seq(1,3,by=1)))

#write.sub("test.sub", "C:\\temp\\", subCtrl=subControl)


