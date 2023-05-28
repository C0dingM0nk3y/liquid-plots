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
  
  cat(sprintf("\nRequest: '%s' [%s] to %s", API_query, API_param, API_root))
  
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
    cat("\tDOWNLOADED")}
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