get.logdata <- function(sqlcall) {
    #library(RODBC)
    channellog <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;
SourceDB=g:\\Tuna_dbs\\Log_dbs\\DBF\\logsheet.dbc;SourceType=DBC;Exclusive=No;
BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")

    # get table a_yb_ez which has:
    # catch/month/year/species/eez/flag
    ayr <- sqlQuery(channellog, sqlcall, max=0,
                    stringsAsFactors=FALSE)
    odbcCloseAll()
    return(ayr)
}
