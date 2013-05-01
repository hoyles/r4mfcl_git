copy.condor.files <-
function (rundir, condor.files = "./condor.files/") 
{
    dir.create(rundir, recursive = T, showWarnings = T)
    Sys.chmod(rundir, mode = "0777")
    mycopy <- function(fn, dir1, dir2) {
        file.copy(paste(dir1, fn, sep = "/"), paste(dir2, fn, 
            sep = "/"),overwrite=TRUE)
    }
    if (Sys.info()[1] != "Linux") 
        exeloc <- "L:/assessments/MFCL/2011_07_10/"
    else exeloc <- "/home/shares/assessments/MFCL/2011_07_10/"
    mycopy("mfclo32.exe", exeloc, rundir)
    mycopy("mfclo64.exe", exeloc, rundir)
    mycopy("mfclo64", exeloc, rundir)
    exx <- paste(exeloc, "DLLs_64/", sep = "/")
    mycopy("msvcp100_64.dll", exx, rundir)
    mycopy("msvcr100_64.dll", exx, rundir)
    exx <- paste(exeloc, "DLLs_32/", sep = "/")
    mycopy("msvcp100_32.dll", exx, rundir)
    mycopy("msvcr100_32.dll", exx, rundir)
    mycopy("mfcl.X86_64.LINUX.bat", condor.files, rundir)
    mycopy("mfcl.X86_64.WINNT61.bat", condor.files, rundir)
    mycopy("mfcl.INTEL.WINNT61.bat", condor.files, rundir)
    mycopy("mfcl.INTEL.WINNT60.bat", condor.files, rundir)
    mycopy("mfcl.INTEL.WINNT51.bat", condor.files, rundir)
    mycopy("mfcl.X86_64.WINNT52.bat", condor.files, rundir)
    mycopy("condor.sub", condor.files, rundir)
    mycopy("mfcl.cfg", condor.files, rundir)
    curdir <- getwd()
    setwd(rundir)
    Sys.chmod(list.files(),mode = "0777")
    setwd(curdir)
}
