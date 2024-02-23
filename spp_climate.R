#' Get Climate Summary Data
#' @param con A DBI connection object.
#' @param BGC A character vector of BGC.
#' @param Scn A character vector of length 1 matching a scenario.
#' @importFrom RPostgres dbGetQuery
#' @export

#' dbGetClimSum <- function(con,BGC,Scn){
#'   
#'   # Declare binding for checks
#'   if (FALSE) {
#'     period <- NULL
#'   }
#'   
#'   cat("in get clim sum")
#'   climVarFut <- RPostgres::dbGetQuery(con, paste0("select bgc,period,stat,climvar,value from szsum_fut where bgc in ('"
#'                                                   ,BGC,"') and period in ('2021-2040','2041-2060','2061-2080') 
#'                                   and stat = 'mean'
#'                                   and climvar in ('CMD','Tmin_sp','Tmax_sm') and scenario = '",Scn,"'"))
#'   climVarFut <- setDT(climVarFut)
#'   
#'   climVarCurr <- setDT(RPostgres::dbGetQuery(con, paste0("select bgc,period,stat,climvar,value from szsum_curr where bgc in ('"
#'                                                          ,BGC,"') and period in ('1991 - 2020') 
#'                                   and stat in ('st.dev.Ann','mean') 
#'                                   and climvar in ('CMD','Tmin_sp','Tmax_sm')")))
#'   climVarCurr[stat == 'st.dev.Ann',stat := "stdev"]
#'   climVarSD <- climVarCurr[stat == "stdev",]
#'   climVarCurr <- climVarCurr[stat != "stdev",]
#'   climVar <- rbind(climVarCurr,climVarFut)
#'   climVar[,period := as.numeric(substr(period,1,4))]
#'   return(list(Mean = climVar,SD = climVarSD))
#' }
#' 
#' #' Simulate Climate for Portfolio
#' #' @param climInfo A parameter.
#' #' @importFrom stats approx rnorm
#' #' @export
#' 
#' simulateClimate <- function(climInfo){ ##package function
#'   
#'   # Declare binding for checks
#'   if (FALSE) {
#'     climvar <-
#'       value <-
#'       period <-
#'       Var <-
#'       NULL
#'   }
#'   
#'   climParams <- list()
#'   simResults <- data.table()
#'   climVar <- climInfo$Mean
#'   climVarSD <- climInfo$SD
#'   for(cvar in c("CMD","Tmin_sp","Tmax_sm")){
#'     climSub <- climVar[climvar == cvar,list(value = mean(value)), by = list(period)]
#'     climSD <- climVarSD[climvar == cvar,value]
#'     ##table of means by period
#'     dat <- data.table(Year = c(2000,2025,2055,2085),Mean = climSub$value)
#'     s <- stats::approx(dat$Year, dat$Mean, n = 101) ##smooth
#'     
#'     ##simulate using mean and variance
#'     res <- numeric()
#'     for(i in 1:101){
#'       res[i] <- stats::rnorm(1,mean = s$y[i],sd = climSD)
#'     }
#'     temp <- data.table(Year = 2000:2100, Value = res)
#'     temp[,Var := cvar]
#'     simResults <- rbind(simResults,temp,fill = T)
#'   }
#'   simResults <- dcast(simResults,Year ~ Var, value.var = "Value")
#'   return(simResults)
#' }
#' 
#' #' Species Limits
#' #' @param con A DBI connection object.
#' #' @param SuitTable A suitability data.frame.
#' #' @param Trees A list of tree species.
#' #' @import foreach
#' #' @importFrom RPostgres dbGetQuery
#' #' @export
#' 
#' dbGetSppLimits <- function(con,SuitTable,Trees){
#'   
#'   # Declare binding for checks
#'   if (FALSE) {
#'     Suitability <-
#'       Spp <-
#'       spp <-
#'       value <-
#'       climvar <-
#'       Min <-
#'       Max <-
#'       NULL
#'   }
#'   
#'   cat("in get limits")
#'   sppLimits <- foreach(spp = Trees, .combine = rbind) %do% {##package function
#'     temp <- SuitTable[Suitability == 1 & Spp == spp,] ##what units is Fd 1?
#'     sppUnits <- unique(temp$BGC)
#'     
#'     climSum <- RPostgres::dbGetQuery(con, paste0("select bgc,period,stat,climvar,value from szsum_curr where bgc in ('"
#'                                                  ,paste(sppUnits,collapse = "','"),"') and period = '1991 - 2020' 
#'                                     and climvar in ('CMD','Tmin_sp','Tmax_sm')"))
#'     climSum <- setDT(climSum)
#'     climSum2 <- climSum[,list(Min = min(value),Max = max(value)),
#'                         by = list(stat,climvar)]
#'     climSum3 <- data.table(CMDMin = climSum2[stat == "mean" & climvar == "CMD",Min],
#'                            CMDMax = climSum2[stat == "mean" & climvar == "CMD",Max],
#'                            Tlow = climSum2[stat == "mean" & climvar == "Tmin_sp",Min],
#'                            Thigh = climSum2[stat == "mean" & climvar == "Tmax_sm",Max])
#'     climSum3[,Spp := spp]
#'     climSum3
#'   }
#' }
