# For each pool that is currently active:
# - Import JASON from "/DOWNLOAD/SinglePools/"
# - Export data as _unpivoted.csv into "/TABLES/SinglePools/"
# - Finally, check if previous data are available and, if so, append new data (HISTORY)
# - Clean Variables

max_iter = 10 #so far, only applies to Claimed. operations does not requires that

# PATHS #####
ops.j.path <- paste0(dir.IN.single,id,"_ops.json")
ops.table.path <- paste0(dir.TABLES.single,id,"_ops.csv") #REMOVE after making sure there is conflict
claim.j.path <- paste0(dir.IN.single,id,"_claim.json")
claim.table.path <- paste0(dir.TABLES.single,id,"_claim.csv")
pool.history.path <- paste0(dir.TABLES.single,id,"_pool.History.csv")

cat(sprintf("> Exporting (poolId=%s) to /TABLES/SinglePools/: <br> \n ", id))

# OPS #####
#cat("##### GET: Pool Operations (ADD/REMOVE)\ \n")
#cat("<div> > \n") #needed to encapsulate plot element so not to distrupt the TAB function

# DOWNLOAD ops table
ops.j <- BINANCE.GET(API_root, "/sapi/v1/bswap/liquidityOps", 
                     API_param = paste0("limit=100&poolId=",id),
                     timestamp = TRUE, sign = TRUE) 

write_json(ops.j, ops.j.path, auto_unbox=TRUE) # export JSON file to /DOWNLOADS/SinglePools/

# INTEPRET ops table
ops_DF <- ops.tableInterpreter(ops.j.path)
write.csv2(ops_DF, ops.table.path, row.names = F) # EXPORT 
cat(sprintf("\t _ops.csv (%s rows)<br> \n", nrow(ops_DF)))


# REWARDS #####
#> Download rewards, starting from date of pool start (from ops_DF)
#> if export reach limit (100 entries) it automatically download next page and join

# start dowloading data from pool start (see ops module)
startTime <- ops_DF %>% 
  subset(operation=="ADD", select = Date_Unix, drop = T) %>% 
  tail(1)# find last ADD

claim.j_list <- list() #for data collection

# first ITER
claim.j <- BINANCE.GET(API_root, "/sapi/v1/bswap/claimedHistory", 
                       API_param = sprintf("type=1&limit=100&startTime=%s&poolId=%s", startTime, id), #type 0/1 = pending/successful
                       timestamp = TRUE, sign = TRUE) 
claim.j_list[1] <- list(claim.j) #claim.j is already a list (1:100). This step creates a list of list.

# more ITER, if required
for (n in 1:max_iter){
  if (max_iter==0){break} #allow to break loop in case this is desirable (e.g. quick update)
  
  last_claim <- claim.j_list[[n]]
  if (length(last_claim)==100){ #that means that data reached limit (100) and need a separate API call to prev. one
    #output has earlier dates on top: first entry == higher date. 
    latestTimestamp <- last_claim[[1]]$claimedTime  #First entry [[1]] from last list [[n-1]]
    
    cat(paste0("<br>--> iter=",n+1,"\t")) #add an indentation and iter number to export      
    claim.j <- BINANCE.GET(API_root, "/sapi/v1/bswap/claimedHistory", timestamp = TRUE, sign = TRUE,
                           API_param = paste0("type=1&limit=100&poolId=",id, "&startTime=", latestTimestamp)) 
    #Why lastTimestamp and NOT lastTimestamp-1
    # > each time stamps has 2-3 rows (one for each coin + extra rewards)
    # > there is 2/3 chance that last row of table is incomplete, and miss at least one of the coin. 
    # > therefore, it is better to download that again, and the remove duplicate entries in later steps
    
    claim.j_list[n+1] <- list(claim.j) #list of list (JSON is 1-100 list)
  }
  else{break} #break loop: last claim had less than 100 entries
}

# List JOIN and EXPORT (JSON)
claim.joined <- unlist(claim.j_list, recursive = F)

claim.j.path <- paste0(dir.IN.single, id, "_claim.json")
write_json(claim.joined, claim.j.path, auto_unbox=TRUE) # export JSON file to /DOWNLOADS/SinglePools/

if (length(claim.joined)>100){
  cat(sprintf("<br><br> joined %s entries into single file: %s\n", length(claim.joined), claim.j.path))}

# Unpacking and EXPORT (CSV)
claim_DF <- claim.tableInterpreter(claim.j.path)
write.csv2(claim_DF, claim.table.path, row.names = F) # EXPORT
cat(sprintf("\t _claim.csv (%s rows)", nrow(claim_DF)))

# HISTORY ####
# Joined file: collects OPS and data from last pool balance (SNAPSHOTS)
# Snapshot and OPS have similar format, so they are merged together and exported as poolHistory.csv

snap_DF <-  subset(active.DF, poolId==id)
#add missing columns before joining to ops table
snap_DF[,"shareAmount"] <- snap_DF[,"share.Amount"] #name change
snap_DF[,"operation"] <- "SNAPSHOT"

# Update/Crete HISTORY file (HISTORY)
if (file.exists(pool.history.path)){
  pool_prev <- read.csv2(pool.history.path) #import prev history
  #recalculate from Unix, that is always the safest option 
  #> reason: if the file was opened with Excel, it changes all data formats)
  pool_prev[, "Date_UTC"] <- msec_to_datetime(pool_prev$Date_Unix, tz = "UTC")
}else{pool_prev <- snap_DF} #...or replace it with latest data

# JOIN and removes duplicates
common_cols <- intersect(colnames(ops_DF), colnames(pool_prev)) #why? because snap_DF has some extra cols with Values, and I want to drop them
pool_HIST <- bind_rows(list(ops_DF, #operations (OLD and NEW) 
                            pool_prev[,common_cols], # OLD DATA (including both OPS and SNAPS)
                            snap_DF[,common_cols])) # LAST SNAPSHOT (always NEW)
pool_HIST %<>% unique() #removes duplicates
pool_HIST <- pool_HIST[order(pool_HIST$Date_Unix), ] #order chronologically

write.csv2(pool_HIST, pool.history.path, row.names = F) # EXPORT HIST file
cat(sprintf("\t _pool.History.csv (read:%s, write:%s rows)\n", nrow(pool_prev), nrow(pool_HIST)))
