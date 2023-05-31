binanceTimestamp <- function(binanceURL = "https://api.binance.com"){
  #> Get current Binance timestamp for API request signature
  ts <- GET(paste0(binanceURL,'/api/v3/time'))
  ts <- rawToChar(ts$content) %>% fromJSON() %>% .[[1]]
  return(ts)}

BINANCE.GET <- function(API_root,
                        API_query,
                        API_param = "",
                        timestamp = FALSE,
                        sign= FALSE){
  #> wrapper to call API queries from Binance server
  #> Print user feedback if ERROR
  
  cat(sprintf("Request: '%s' [%s] to %s", API_query, API_param, API_root))
  
  #defines query parameters
  totalParams <- API_param #default
  
  if (timestamp==TRUE){
    timestamp_string <- paste0("timestamp=", binanceTimestamp())
    
    if(totalParams==""){ #no other param
      totalParams <- timestamp_string} #OVERWIRTE (no other param)
    else{
      totalParams %<>% paste(sep="&", timestamp_string)} #APPEND
  }
  
  if(sign==TRUE){
    #add encrypted signature
    signature <- hmac(key_private, totalParams, algo = 'sha256')
    
    signedQuery <- paste0(API_query, "?", totalParams, '&signature=',signature)
    
    request <- GET(config = add_headers("X-MBX-APIKEY" = key_public), 
                   API_root, 
                   path=signedQuery)}
  
  else{ #no signature required
    request <- GET(API_root, path=API_query)}
  
  # ERROR CAPTURE
  if(request$status_code==200){ #error capture
    cat("\t>> DOWNLOADED\n")}
  else{
    message(paste0("ERROR on API CALL:", API_query))
    print(request)}
  
  #PARSE as JSON
  json <- content(request, as = "parsed") #json interpreter
  
  return(json)
}

liquidity.tableInterpreter <- function(jsonPath){
  #> Import json fine and recover data from nested lists (pivot_longer)
  #> Returns df
  
  if(file.exists(jsonPath)){
    j <- fromJSON(jsonPath, 
                  flatten = TRUE, #automatically UN-Nest nested columns
                  simplifyVector = TRUE) 
    
    #this is a HUGE table with one col for each COIN 
    # eg. share.asset.GRT share.asset.FET share.asset.RPL share.asset.STG share.asset.MKR share.asset.SNX share.asset.LQTY)
    
    # Unpivot liquidity
    unpivotPrefix <- 'liquidity'
    
    colsToPivot <- colnames(j) %>% grep(unpivotPrefix, .)
    p1 <- pivot_longer(j, cols = all_of(colsToPivot), names_to = "Coin", names_prefix=paste0(unpivotPrefix,"."),
                       values_to = unpivotPrefix, values_drop_na = TRUE, values_transform = list(liquidity = as.numeric))  
    
    # Unpivot share.asset
    unpivotPrefix2 <- 'share.asset'
    
    colsToPivot <- colnames(p1) %>% grep(unpivotPrefix2, .)
    p2 <- pivot_longer(p1, cols = all_of(colsToPivot), names_to = "Coin2", names_prefix=paste0(unpivotPrefix2,"."),
                       values_to = unpivotPrefix2, values_drop_na = TRUE, values_transform = list(share.asset = as.numeric))
    
    # Remove duplicate ROWS
    p3 <- subset(p2, Coin == Coin2)  #removes duplicated entries created during the second pivot step.
    
    # Removes duplicate COLS (Coin1 and Coin2 are arbitrary names, and tehy are identical. They are only used for pivoting)
    p4 <- p3[, -match("Coin2", colnames(p3))]
    
    # Renames columns
    colnames(p4) %<>% str_replace_all(pattern="share.share", replacement = "share.") #rename 
    
    #convert to numeric
    #numColumns <- grep(pattern = "share.", x = colnames(p4))
    #p4[,numColumns] %<>% sapply(as.numeric)
    
    # Remove empty rows (coming from all the pool I have no liquidity into)
    p5 <- subset(p4, share.Amount >0)
    
    cat(sprintf("\tSHRINKING TABLE (removes pools w/o coins): from (%s) to (%s) rows\n", nrow(p4), nrow(p5)))
    
    return(p5)}
  
  else{
    cat(jsonPath, ": not FOUND\n")}
}

ops.tableInterpreter <- function(jsonPath){
  # Read LiquidPool Operations exports from Binance API and unpivot them to a readable table
  # Returns data.frame
  
  #input file
  ops.j <- fromJSON(jsonPath, simplifyVector = T, flatten = T)
  
  #liquidityAmount is a 2x2 df and need "unpacking"
  ops.df <-  ops.j %>% 
    unnest_longer(col = "liquidityAmount", simplify = T) %>% #duplicate rows (for for each col)
    unnest_wider(col = "liquidityAmount", simplify = T) %>% # unnest col 'liquidityAmount' into 'asset' and 'amount'
    subset(status==1) #filters out UNCOMPLETED transaction ("processing")
  
  #converts to numeric
  ops.df[,"shareAmount"] %<>% unlist() %>% as.numeric() 
  ops.df[,"amount"] %<>% unlist() %>% as.numeric() 
  
  ops.df[,"Date_Unix"] <- ops.df$updateTime #rename
  ops.df[,"Date_UTC"] <- ops.df$updateTime %>% msec_to_datetime()
  
  # pivot df so to have one 1-entry for each operation
  ops.df[,"Coin1"] <- str_split_i(ops.df$poolName,pattern = "/", 1) #split and return first coin
  ops.df[,"Coin2"] <- str_split_i(ops.df$poolName,pattern = "/", 2) #split and return first coin
  
  ops.df[,"coinId"] <- ifelse(ops.df$asset==ops.df$Coin1,"1","2")#either 1/2 depending on order in pool
  
  ops.wide <- pivot_wider(ops.df, id_cols=c("Date_UTC", "Date_Unix", "operation", "poolId", "poolName", "shareAmount",  "Coin1", "Coin2"), 
                          names_from = "coinId", values_from = "amount", names_prefix = "Qnt")
  
  #Reorder by date
  ops.wide <- ops.wide[order(ops.wide$Date_UTC), ]

  
  return(ops.wide)
}

claim.tableInterpreter <- function(jsonPath){
  # Read LiquidPool Claims exports from Binance API and unpivot them to a readable table
  # Returns data.frame
  
  #input file
  claim.j <- fromJSON(jsonPath, simplifyVector = T, flatten = T) %>%
    subset(status==1) %>%
    unique() #this is important in case of joined JSON from multiple API calls: 
             #> they ALWAYS have some duplicated entry at the interface between file 1 and 2 
             #> this is because of the reason data is downloaded)

  claim.j[,"claimAmount"] %<>% as.numeric()
  claim.j[,"Date_UTC"] <- claim.j$claimedTime %>% msec_to_datetime()
  
  # pivot the dataframe so to have one 1-entry for each operation 
  #> instead than 3: Coin1/Coin2/BonusReward
  claim.j[,"Coin1"] <- str_split_i(claim.j$poolName,pattern = "/", 1) #split and return first coin
  claim.j[,"Coin2"] <- str_split_i(claim.j$poolName,pattern = "/", 2) #split and return first coin
  
  # COIN 3: some pools also have extra rewards collected as Coin3 (usually, BNB)
  #> steps below allow to recover the name of coin3
  poolCoins <- claim.j[1,"poolName"] %>% str_split(pattern = "/") %>% unlist()
  
  Coin3 <- unique(claim.j$assetRewards) %>% 
    setdiff(c(poolCoins)) #remove known coins
  
  claim.j[,"Coin3"] <- ifelse(length(Coin3)==0, NA, Coin3) #if no Coin3, set to NA
  
  # Assign Coin number [1/2/3]
  claim.j[claim.j$assetRewards==claim.j$Coin1, "coinId"] <- "1"
  claim.j[claim.j$assetRewards==claim.j$Coin2, "coinId"] <- "2"
  claim.j[claim.j$assetRewards==claim.j$Coin3, "coinId"] <- "3"
  
  # pivot_wider
  claim.wide <- pivot_wider(claim.j, id_cols=c("Date_UTC", "poolId", "poolName", "status", "Coin1", "Coin2", "Coin3"), 
                            names_from = "coinId", values_from = "claimAmount", names_prefix = "claimed",
                            values_fn = sum) #define what to do in case of multiple value for the same name (e.g. BNB rewards in a COIN/BNB pool will have 2x BNB entries). SUM. )
  
  claim.wide <- claim.wide[order(claim.wide$Date_UTC), ]

  return(claim.wide)
}


activePools.Calc <- function(liq.df){
  #> wrapper to pivot and perform calculations on liq.df dataframe
  #> liq.df is the native format of Binance API export file [liquidity.json]
  
  # RENAME and CALC Coin1/Coin2 var
  active.calc <- data.frame(
    poolId = liq.df$poolId,
    poolName = liq.df$poolName,
    Date_Unix = liq.df$updateTime,
    Date_UTC = liq.df$updateTime %>% msec_to_datetime(),
    share.Amount = liq.df$share.Amount %>% as.numeric(),
    Coin1 = str_split_i(liq.df$poolName,pattern = "/", 1), #split and return first coin
    Coin2 = str_split_i(liq.df$poolName,pattern = "/", 2), #split and return second coin
    Qnt = liq.df$share.asset,
    coinName = liq.df$Coin,
    Value = liq.df$Value,
    Currency = liq.df$Currency
  )
  
  # UNPIVOT table
  #1. assign "label" Coin1/Coin2
  active.calc[,"coinID"] <- ifelse(active.calc$coinName==active.calc$Coin1, "1", "2")
  
  #2. pivot wider
  active.calc2 <- pivot_wider(active.calc,
                              id_cols = c("Date_Unix", "Date_UTC", "poolId", "poolName", "share.Amount", "Coin1", "Coin2", "Currency"), #columns to keep 
                              names_from = "coinID",  names_sep = "", values_from = c("Qnt", "Value") )
  
  #3. Value TOT
  active.calc2[,"Value_TOT"] <-  with(active.calc2, Value1+Value2)
  
  # REODER and DROP intermediate calc. columns
  active.DF <- active.calc2[order(active.calc2$Value_TOT, decreasing = T),#order by value_TOT (HIGH)
                            c("poolId","poolName","Date_Unix","Date_UTC","share.Amount", #re-order columns
                              "Coin1","Qnt1","Value1",
                              "Coin2","Qnt2","Value2",
                              "Value_TOT", "Currency")]
}





getPrice <- function(price_df, coin, refCoin="USDT"){
  #> scan the price_df dataframe for symbol COINUSDT or USDTCOIN respectively.
  #> Returns price expressed in 'refCoin'
  
  errorMessage <-function(ss){
    message("ERROR in getPrice(), selected coin has more than one symbol. This should not be possible. :'(")
    print(ss)
    stop()}
  
  if (coin==refCoin){return(1)} # SANITY-CHECK
  
  # Possible SYMBOLS to search for
  symbol_for <- paste0(coin,refCoin)
  symbol_rev <- paste0(refCoin,coin) #reverse symbol
  
  
  # ITERATION 1: symbol_for
  ss <- subset(price_df, symbol==symbol_for)
  if (nrow(ss)>1){errorMessage(ss)} #Quality check: this should be impossible
  
  else if (nrow(ss)==1){ 
    #that's the desired result: symbol should be unique
    price <- ss[1,"price", drop=T]
    return(price)}
  
  # ITERATION 2: symbol_rev 
  #> this part is only read if symbol_for was not found
  rev <- subset(price_df, symbol==symbol_rev)
  
  if (nrow(rev)>1){errorMessage(ss)} #error check
  else if (nrow(ss)==1){ 
    #that's the desired result: symbol should be unique
    price_rev <- ss[1,"price", drop=T]
    price <- 1/price_rev
    return(price)}
  else{
    # USER FEEDBACK
    message(sprintf("'%s' [%s/%s] price was not found into provided 'price_df'", coin, symbol_for, symbol_rev))
    return(NA)}
}

# TABLES and DATA ####

datetime_to_sec <- function(datetime, format="sec", tz="UTC"){
  # tz = "UTC", same as "GMT". Local: "CEST" same as "Europe/Belin"
  numDate <- datetime %>% as_datetime(tz = tz) %>% as.numeric()
  if (format =="sec"){return(numDate)}
  else if (format =="msec"){return(numDate*1000)}
  else{message(sprintf("ERROR processing %s: format allowed are 'sec' and 'msec'. Please select one."))}}

datetime_to_msec <- function(datetime, format="msec", tz="UTC"){
  datetime_to_sec(datetime, format, tz)} #as the above, default to msec

sec_to_datetime <- function(time_s, format="sec", tz="UTC"){
  if (format =="msec"){time_s <- time_s /1000}
  else if (format =="sec"){time_s <- time_s}
  else{message(sprintf("ERROR processing %s: format allowed are 'sec' and 'msec'. Please select one."))}
  return(as_datetime(time_s, tz = tz))}

msec_to_datetime <- function(datetime, format="msec", tz="UTC"){
  sec_to_datetime(datetime, format, tz)} #as the above, default to 

# MATH ####
ILtoRatio <- function(IL, trend="both"){
  # solve the Impermanent loss function for p -> IL = 2*sqrt(p)/(p+1)
  # p is the ratio of the price at the time of offering liquidity (r1) compared to a different price-ratio (r2)  -> p = r1/r2 
  
  # Input: IP percent (as a negative fraction: -0,01 = 1% or IP). 
  # Output: target price that would cause that % of IL. Expressed as fraction of starting price (e.g. start price)
  # trend ["up"/"down"/"both"]: select if the returned ratio refers to the a positive price change, or negative one.
  
  ## CREDITS to Piers for solving the algebra.
  
  IL <- -abs(IL) #convert to a neg percent, if not already
  
  a = (IL+1)^2
  b = (2*IL^2) +(4*IL) - 2
  c = (IL+1)^2
  delta = (b^2)-(4*a*c)
  
  # calculate price changes to cause given IL
  r_up = (-b +sqrt(delta))/(2*a)
  r_down = (-b -sqrt(delta))/(2*a)
  
  if(trend %in% c("both", "BOTH", 0)){return(c(r_down,r_up))}
  else if(trend %in% c("up", "UP", +1)){return(r_up)}
  else if(trend %in% c("up", "DOWN", -1)){return(r_down)}
}

RatioToIL <- function(p, PriceChange, trend="auto"){
  # solve the Impermanent loss function for p -> IL = (2*sqrt(p)/(p+1))-1
  # p (from formula) is the RatioChange (ratio of the price at the time of offering liquidity (r1) compared to a different price-ratio (r2))
  #   -> p = r1/r2 
  # trend: pos/neg/auto (preferred sign to return. Both give pos % if PriceRatio >1, else neg %)
  
  # Input: PriceChange (current price, r2) / priceBuy (r1). This it the inverse of the ratio required for formula.
  #       alternatively, p can be provided directly
  # Output: percent of Value loss
  
  if(missing(p)){p <- 1/PriceChange} #convert price ratio (r2/r1) into r-ratio (r1/r2)
  if(missing(PriceChange)){PriceChange <- 1/p} #convert price ratio (r2/r1) into r-ratio (r1/r2)
  
  IL <- (2*sqrt(p)/(p+1))-1 #always a NEG number
  
  if (trend=="pos" | PriceChange>=1){return(abs(IL))}
  if (trend=="neg" | PriceChange<1){return(IL)}
}