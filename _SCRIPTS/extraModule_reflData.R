pacman::p_load('magrittr')

#### Extra module: converts snapshots historical data from file ####

#INPUT: one of my 'LiqPool.snapshot_fullhist.csv' files
#OUTPUTS: 1. a DF called 'snapshots.HIST_wide.csv', with a collection of all snapshot prices recovered from my own data
#         2. a series of files that can be read directly from the plotting function to add historical data to plots


make_refData = TRUE #different output formats
make_poolHist = TRUE 

history_path <- "D:/Clouds/Dropbox/Everywhere/PROJECTS/PiggyBank/Binance/PROCESSING/LiqPool.snapshot_fullhist.csv"

dir.Export <- "REFDATA/" %T>% dir.create(showWarnings = F)
dir.Exp.Single <- "REFDATA/SinglePools/" %T>% dir.create(showWarnings = F)
dir.TABLES.Single <- "TABLES/SinglePools/" %T>% dir.create(showWarnings = F)
path.HIST.wide <- paste0(dir.Export, "snapshots.HIST_wide.csv") #this format is more suitable for historical data to provide AS A SERVICE

### PART 1: Pivot Wide "FULL DATABASE" ####

HIST <- read.csv2(history_path)
HIST <- HIST[order(HIST$updateTime),]
HIST2 <- HIST[(!duplicated(HIST[,c("updateTime","poolId","Coin")])),] #removes duplicates based on UNIX time + CoinName. WHY?*
                                                                 #> *this happens in some old file where to entry have the same time stamp, but different 
                                                                 #> number of decimal digits,so they create duplicated entries
HIST2[,"Date_UTC"] <- HIST2$updateTime %>% msec_to_datetime() %>% as.POSIXct(tz="UTC")

# UNPIVOT table
#1. assign "label" Coin1/Coin2
HIST2[, "Coin1"] <- str_split_i(HIST2$poolName,pattern = "/", 1) #split and return first coin
HIST2[, "Coin2"] <- str_split_i(HIST2$poolName,pattern = "/", 2) #split and return second coin

HIST2[,"coinID"] <- ifelse(HIST2$Coin==HIST2$Coin1, "1", "2")

#2. pivot wider
HIST_wide <- pivot_wider(HIST2, id_cols = c("poolName", "poolId", "updateTime", "share.Amount", "Date_UTC", "Coin1", "Coin2"), #columns to keep 
                       names_from = "coinID",  names_prefix =  "Qnt", values_from = "share.asset", names_sort = T)

#3. add calculations
HIST_wide[, "PoolRatio"] <- with(HIST_wide, Qnt1/Qnt2)                                    
HIST_wide[, "PoolPrice"] <- with(HIST_wide, Qnt2/Qnt1)                                    

#4. export
write.csv2(HIST_wide, path.HIST.wide, row.names = F)


### PART 2: SINGLE POOLS ####

allPoolId <- unique(HIST_wide$poolId)

if(make_refData){
  for (id in allPoolId){
    path.Exp.refData <- paste0(dir.Exp.Single, id, "_refData.csv") #this format is more suitable for historical data to provide AS A SERVICE
    
    ss <- subset(HIST_wide, poolId == id) %>% unique()
    
    write.csv2(ss, path.Exp.refData, row.names = F)}}
  
if(make_poolHist){
    for (id in allPoolId){
      path.Exp.refData <- paste0(dir.TABLES.Single, id, "_pool.History.csv") #this allows me to recover old data from previous scripts
      
      #Reformat according to export
      ss <- subset(HIST_wide, poolId == id) %>% unique()
      
      poolH <- data.frame(
        Date_UTC = ss$Date_UTC,
        Date_Unix = ss$updateTime,
        operation = "SNAPSHOT",
        poolId = ss$poolId,
        poolName = ss$poolName,
        shareAmount = ss$share.Amount %>% as.numeric(),
        Coin1 = ss$Coin1,
        Coin2 = ss$Coin2,
        Qnt1 = ss$Qnt1,
        Qnt2 = ss$Qnt2
      )
      
      write.csv2(poolH, path.Exp.refData, row.names = F)}
}

