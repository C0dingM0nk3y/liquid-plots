#### @ script_plotPool_v1 @ ####

#> Scripts take "id" and "dir.REF.single" "dir.TABLES.single" variables from GlobalEnv and use them to
#> 1. Rebuild all paths and upload data
#> 2. Perform calculations required for plotting
#> 3. Produce different plots
#> 4. Return a collage of plots, as a single 'fig'

# PART 1: IMPORT data to plot (snap_H/ops/claim) ####

# Rebuild PATHS
ops.table.path <- paste0(dir.TABLES.single,id,"_ops.csv")
claim.table.path <- paste0(dir.TABLES.single,id,"_claim.csv")
snap.history.path <- paste0(dir.TABLES.single,id,"_snapshots.History.csv")
refData.path <- paste0(dir.REF.single,id,"_refData.csv")

# IMPORT and Convert all dates to POSIX
ops_H <- read.csv2(ops.table.path) #REPLACE WITH HIST FILE
ops_H[,"Date_UTC"] %<>% as.POSIXct(tz="UTC")

claim_H <- read.csv2(claim.table.path) #REPLACE WITH HIST FILE
claim_H[,"Date_UTC"] %<>% as.POSIXct(tz="UTC")

snap_H <- read.csv2(snap.history.path)
snap_H[,"Date_UTC"] %<>% as.POSIXct(tz="UTC")

# PART 2: POOL CALCULATIONS ####

#> 2.a Pool INFO ####
poolName <- snap_H[1,"poolName", drop=T]
coin1 <- snap_H[1,"Coin1", drop=T]
coin2 <- snap_H[1,"Coin2", drop=T]
coin3 <- claim_H[1,"Coin3", drop=T] # coin 3 is recovered from claim_CALC

#> 2.b MARKET data ####
price1 <- priceMatrix[coin1,refCoin]
price2 <- priceMatrix[coin2,refCoin]
price3 <- priceMatrix[coin3,refCoin]

#> 2.c STARTING point data ####
#> All calc related to current (final) state of Pool
#>> NOTE: in current implementation, it is expected for the LAST operation to be a ADD. 
#>> Future implementation will be able to interpret more complex (multi ADD/RMOVE) operation, 
#>> for now it is only plotting results from LAST operation

ops.last <- ops_H %>% tail(1)# required to recover Claimed data (ops.last.date)
#sanity check
if (ops.last[1,"operation"]=="REMOVE"){ 
  print(ops_H)
  stop("Error: pools with multiple ADD/REMOVE operations are currently NOT supported (In progress)")}

#extract data
start_date <- ops.last[1,"Date_UTC", drop=T]
start_qnt1 <- ops.last[1,"Qnt1", drop=T] %>% as.numeric()
start_qnt2 <- ops.last[1,"Qnt2", drop=T] %>% as.numeric()

start_value <- (start_qnt1*price1) + (start_qnt2*price2)

#this is the most important param, as it define the center point for the IL plots.
entryRatio <- start_qnt1/start_qnt2
entryPrice <- 1/entryRatio

#> 2.d SNAPSHOTS History ####
snap_H[,"PoolRatio"] <- with(snap_H, Qnt1/Qnt2) 
snap_H[,"PoolPrice"] <- 1/snap_H$PoolRatio
snap_H[,"PriceChange"] <- with(snap_H, PoolPrice/entryPrice) 

#> 2.e CLAIM Calculations ####
#> scan the full history of pool claims, and FILTER for those that happened AFTER 'poolStart' date

claim_CALC <- subset(claim_H, Date_UTC > start_date)

#calculate cumulData
claim_CALC %<>% replace_na(replace = list(claimed1=0, claimed2=0, claimed3=0)) #replace NA with 0
claim_CALC[,"Cum_Qnt1"] <- cumsum(claim_CALC[,"claimed1"])
claim_CALC[,"Cum_Qnt2"] <- cumsum(claim_CALC[,"claimed2"])
claim_CALC[,"Cum_Qnt3"] <- cumsum(claim_CALC[,"claimed3"])

#calculate Cumul%, expressed as %of new coin earned copared to start_qnt
#> this is only calculated on coin1/coin2)
claim_CALC[,"Cum_%1"] <- claim_CALC[,"Cum_Qnt1"]/start_qnt1
claim_CALC[,"Cum_%2"] <- claim_CALC[,"Cum_Qnt2"]/start_qnt2

#Converts to Value
claim_CALC[,"Cum_Val1"] <- claim_CALC[,"Cum_Qnt1"]*price1
claim_CALC[,"Cum_Val2"] <- claim_CALC[,"Cum_Qnt2"]*price2
claim_CALC[,"Cum_Val3"] <- claim_CALC[,"Cum_Qnt3"]*price3

#Total Earn: Value and %
claim_CALC[,"Cum_ValTOT"] <- with(claim_CALC, (Cum_Val1+Cum_Val2+Cum_Val3))
claim_CALC[,"Cum_%ValTOT"] <- with(claim_CALC, Cum_ValTOT/start_value)

#> 2.f ENDPOINT Data ####
#> All calc related to current (final) state of Pool

end_date <- max(claim_CALC$Date_UTC, snap_H$Date_UTC)

#> [2.x] Recovers REFDATA [if.available] ####

if (file.exists(refData.path)){
  has.REFDATA = TRUE
  pool_REF <- read.csv2(refData.path) %>%           #import
    subset(Date_UTC > start_date)                   #filter
  pool_REF[order(pool_REF$updateTime), ]            #re-order
  pool_REF[,"Date_UTC"] %<>% as.POSIXct(tz = "UTC") #convert
  
}else{
  has.REFDATA <- FALSE
  pool_REF <- data.frame() #empty df
}

# PART 3: PLOT ASSEMBLY ####

