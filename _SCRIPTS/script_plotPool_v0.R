#### @ script_plotPool_v1 @ ####

#> Scripts take "id" and "dir.REF.single" "dir.TABLES.single" variables from GlobalEnv and use them to
#> 1. Rebuild all paths and upload data
#> 2. Perform calculations required for plotting
#> 3. Produce different plots
#> 4. Return a collage of plots, as a single 'fig'

# PART 1: IMPORT data to plot (snap_H/ops/claim) ####

# Rebuild PATHS
claim.table.path <- paste0(dir.TABLES.single,id,"_claim.csv")
pool.history.path <- paste0(dir.TABLES.single,id,"_pool.History.csv") #includes OPS and SNAPS
refData.path <- paste0(dir.REF.single,id,"_refData.csv")

# IMPORT and Convert column data to the appropriate type (double, POSIX...)
pool_H <- read.csv2(pool.history.path) #%>% as.tibble()
pool_H[,"Date_UTC"] %<>% as.POSIXct(tz="UTC")

#pool_H[,"Qnt1"] %<>% as.numeric()
#pool_H[,"Qnt2"] %<>% as.numeric()


claim_H <- read.csv2(claim.table.path) #REPLACE WITH HIST FILE
claim_H[,"Date_UTC"] %<>% as.POSIXct(tz="UTC")

# PART 2: POOL CALCULATIONS ####

## 2.A Pool INFO ####
poolName <- pool_H[1,"poolName", drop=T]
coin1 <- pool_H[1,"Coin1", drop=T]
coin2 <- pool_H[1,"Coin2", drop=T]
coin3 <- claim_H[1,"Coin3", drop=T] # coin 3 is recovered from claim_CALC

## 2.B POOL RATIO/PRICE ####
# Add useful calculations for later ref
pool_H[,"PoolRatio"] <- with(pool_H, Qnt1/Qnt2) 
pool_H[,"PoolPrice"] <- 1/pool_H$PoolRatio

## 2.C MARKET data ####
price1 <- priceMatrix[coin1,refCoin]
price2 <- priceMatrix[coin2,refCoin]
price3 <- ifelse(is.na(coin3), 0, priceMatrix[coin3,refCoin])

# this is the Value that the pool would have AT CURRENT PRICE if user exited at that time point
pool_H[,"Value1"] <- with(pool_H, Qnt1*price1)
pool_H[,"Value2"] <- with(pool_H, Qnt2*price2)
pool_H[,"ValueTOT"] <- with(pool_H, Value1+Value2)

## 2.D STARTING point data ####
#> All calc related to current (final) state of Pool
#>> NOTE: in current implementation, it is expected for the LAST operation to be a ADD. 
#>> Future implementation will be able to interpret more complex (multi ADD/RMOVE) operation, 
#>> for now it is only plotting results from LAST operation

# !!! In the future, start_DF will be caulculated using the segment analsysis tool.
start_DF <- pool_H %>% 
  subset(operation=="ADD") %>% 
  tail(1)# find last ADD

#extract data
start_date <- start_DF[1,"Date_UTC", drop=T]
start_qnt1 <- start_DF[1,"Qnt1", drop=T] 
start_qnt2 <- start_DF[1,"Qnt2", drop=T] 

start_value <- start_DF[1,"ValueTOT", drop=T] #for Cum_ValTOTx100 calc

#this is the most important param, as it define the center point for the IL plots.
start_ratio <- start_DF[1,"PoolRatio", drop=T]
start_price <- start_DF[1,"PoolPrice", drop=T]


## 2.E MERGE to RefData, FILTER, CALC (IL/ROI) ####

## [optional] MERGE to REFDATA if optional ####
if (allow_refData & file.exists(refData.path)){
  #has.REFDATA = TRUE
  pool_REF <- read.csv2(refData.path)
  pool_REF[,"operation"] <- "REFDATA"
  pool_REF[,"Qnt1"] <- NA #this data does not belong to this pool. It can even be removed from REFDATA file
  pool_REF[,"Qnt2"] <- NA
  pool_REF[,"Date_UTC"] %<>% as.POSIXct(tz = "UTC") #convert
  colnames(pool_REF) %<>% str_replace("updateTime", "Date_Unix") #rename col
  
  #merge to single DF
  pool_H <- bind_rows(pool_H, pool_REF)
  pool_H <- pool_H[order(pool_H$Date_Unix), ]
  }

# subset and add calc.
pool_LAST <- pool_H %>% subset(Date_UTC >= start_date)

pool_LAST[,"PriceChange"] <- with(pool_LAST, PoolPrice/start_price) 
pool_LAST[,"RatioChance"] <- with(pool_LAST, PoolRatio/start_ratio)  # for IF calculation
pool_LAST[,"Swap1_X100"] <- with(pool_LAST, (Qnt1/start_qnt1)-1) #amount of coin swapped with the other
pool_LAST[,"Swap2_X100"] <- with(pool_LAST, (Qnt2/start_qnt2)-1) 

for (r in 1:nrow(pool_LAST)){
  p <- pool_LAST[r, "RatioChance"] 
  pool_LAST[r, "IL"] <- RatioToIL(p=p)}


## 2.F CLAIM Calculations ####
#> scan the full history of pool claims, and FILTER for those that happened AFTER 'poolStart' date

claim_CALC <- subset(claim_H, Date_UTC >= start_date)

#calculate cumulData
if(is.na(coin3)){claim_CALC[,"claimed3"] <- NA} #sanity check (some pools have no extra Rewards)

claim_CALC %<>% replace_na(replace = list(claimed1=0, claimed2=0, claimed3=0)) #replace NA with 0
claim_CALC[,"Cum_Qnt1"] <- cumsum(claim_CALC[,"claimed1"])
claim_CALC[,"Cum_Qnt2"] <- cumsum(claim_CALC[,"claimed2"])
claim_CALC[,"Cum_Qnt3"] <- cumsum(claim_CALC[,"claimed3"])

#calculate Cumul%, expressed as %of new coin earned compared to start_qnt

#> this is only calculated on coin1/coin2)
claim_CALC[,"Cum_%1"] <- claim_CALC[,"Cum_Qnt1"]/start_qnt1
claim_CALC[,"Cum_%2"] <- claim_CALC[,"Cum_Qnt2"]/start_qnt2

#Converts to Value
claim_CALC[,"Cum_Val1"] <- claim_CALC[,"Cum_Qnt1"]*price1
claim_CALC[,"Cum_Val2"] <- claim_CALC[,"Cum_Qnt2"]*price2
claim_CALC[,"Cum_Val3"] <- claim_CALC[,"Cum_Qnt3"]*price3

#Total Earn: Value and %
claim_CALC[,"Cum_ValTOT"] <- with(claim_CALC, (Cum_Val1+Cum_Val2+Cum_Val3))
claim_CALC[,"Cum_ValTOTx100"] <- with(claim_CALC, Cum_ValTOT/start_value)

## 2.G ENDPOINT Data ####
#> All calc related to current (final) state of Pool

# from pool_LAST
end_DF <- pool_LAST %>% 
  subset(Date_UTC==max(Date_UTC))
end_price <- end_DF[1,"PoolPrice"]

# from last between claim and pool_LAST
end_date <- max(claim_CALC$Date_UTC, pool_LAST$Date_UTC) #whichever is higher
end_maxEarn <- claim_CALC$Cum_ValTOT %>% tail(1) #last entry
end_maxEarnX100 <- claim_CALC$Cum_ValTOTx100 %>% tail(1) #last entry

# HODL calculation. Value if 
end_hodl1 <- start_qnt1*price1
end_hodl2 <- start_qnt2*price2
end_hodl_TOT <- end_hodl1+end_hodl2

# ROI and background color
end_valChange <- (end_DF[,"ValueTOT"]/start_value)-1
end_ROInet <- end_DF[,"ValueTOT"]-start_value+end_maxEarn 
end_ROInet_X100 <- end_maxEarnX100-abs(end_DF[1,"IL"]) 

# MOVE into PLOT part?
limits_DF <- plotLimits.Calc(stopLossTolerance= setStopLoss) 
                            #> returns useful cols like: 
                            #> LimitX100, Limit_Price, 
                            #> TextColor, Color,
                            #> Label_Extended, nudge_x, nudge_y


# PART 3: PLOT ASSEMBLY ####

##> COMMON plot aesthetics ####

#Change BACKGROUND color if pool is "at risk"
bg_color <- "white"
if(end_ROInet_X100<0){bg_color <- "orange"}
if(end_ROInet_X100<(setStopLoss*-1)){bg_color <- "red"}
panel_color <- bg_color

###> X-RANGE options
# calculate xLim (min and max dates)
xLim_left <- start_date #%>% as.POSIXct(tz = "UTC")
xLim_right <- end_date #%>% as.POSIXct(tz = "UTC")
plot_lim <- c(xLim_left, xLim_right)

#nudge param
nudge_x <- (datetime_to_sec(xLim_right)- datetime_to_sec(xLim_left)) %>% #distance (in UNIX time!) from label and datapoint.
  as.numeric()*0.4 #multiply by 0.4 (size of 2/5 of the plot area)) #v3.5 changed from 0.2
plot_nudged <- c(xLim_left, xLim_right+nudge_x) %>% as.POSIXct(tz="UTC")

#### WORK IN PROGRESS ####

# PLOT
cat(sprintf("###### <span style='background-color:%s;'> %s </span><br>\n",bg_color, poolName))
# cat(sprintf("###### %s <br>\n", poolName))
cat("<div>\n") #needed to encapsulate plot element so not to distrupt the TAB function
p1 <- LiqPlots_Trends() #plot1
p2 <- LiqPlots_ILChanges(stopLossTolerance = setStopLoss) #plot2
p3 <- LiqPlots_Swaps()
p4abs <- LiqPlots_PNL(type = "abs")
p4rel <- LiqPlots_PNL(type = "rel")
p5 <- LiqPlots_plotAPY()


# Arrange plots (pathwork)

#Plot L
pw_left <- p1/p2

pw_left[[1]] %<>% + theme(axis.title.x = element_blank(), axis.text.x=element_blank()) # Remove x.axis title and tick names from subplot
pw_left[[1]] %<>% + theme(plot.background = element_rect(fill = bg_color))
pw_left[[2]] %<>% + theme(plot.background = element_rect(fill = bg_color))
#pw[[2]] = pw[[2]] + theme(plot.title = element_blank()) # Remove title from second subplot

pw_left <- pw_left + plot_layout(heights = c(6,6), nrow = 2)

#Plot R
#pw_right <- ggplot(p4bis)/p3/p4/p5
#pw_right[[1]] %<>% + theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = panel_color))
pw_right <- p3/p4abs/p4rel/p5
pw_right[[1]] %<>% + theme(axis.title.x = element_blank(), axis.text.x=element_blank()) # Remove x.axis title and tick names from subplot
pw_right[[2]] %<>% + theme(axis.title.x = element_blank(), axis.text.x=element_blank()) 
pw_right[[3]] %<>% + theme(plot.title = element_blank(), #suppress title
                           axis.title.x = element_blank(), axis.text.x=element_blank(), legend.position = "none") 
pw_right <- pw_right + plot_layout(heights = c(6,4,3,4), nrow = 4)

pw <- pw_left|pw_right
pw <- pw + plot_layout(widths = c(3,2), ncol = 2)
print(pw)

cat("</div>\n")

# Link to Binance pool
cat(sprintf('<p>  Direct Link to <a href="https://www.binance.com/en/swap/liquidity?poolId=%s">**Binance Pool (%s)</a>** <- click to operate on pool</p>\n', id, poolName)) 

# Link to full data
cat("<div> \n") 
cat("####### DATA-TABLES, sources:<br>\n") # Tabs header
cat(sprintf('Pool History: <a href="%s">%s</a><br>', pool.history.path, pool.history.path))
cat(sprintf('Pool Claims: <a href="%s">%s</a> <br>', claim.table.path, claim.table.path))
cat("</div> \n") 
