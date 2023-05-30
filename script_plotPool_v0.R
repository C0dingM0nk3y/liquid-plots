# @ script_plotPool_v1 @ ####

#> Scripts take "id" and "dir.REF.single" "dir.TABLES.single" variables from GlobalEnv and use them to
#> 1. Rebuild all paths and upload data
#> 2. Perform calculations required for plotting
#> 3. Produce different plots
#> 4. Return a collage of plots, as a single 'fig'

# PART 1: IMPORT data to plot (snap_H/ops/claim) ####

# Rebuild PATHS
#ops.table.path <- paste0(dir.TABLES.single,id,"_ops.csv")
claim.table.path <- paste0(dir.TABLES.single,id,"_claim.csv")
#snap.history.path <- paste0(dir.TABLES.single,id,"_snapshots.History.csv")
pool.history.path <- paste0(dir.TABLES.single,id,"_pool.History.csv") #includes OPS and SNAPS
refData.path <- paste0(dir.REF.single,id,"_refData.csv")

# IMPORT and Convert column data to the appropriate type (double, POSIX...)
#ops_H <- read.csv2(ops.table.path) #REPLACE WITH HIST FILE
#ops_H[,"Date_UTC"] %<>% as.POSIXct(tz="UTC")
#ops_H[,"Qnt1"] %<>% as.numeric()
#ops_H[,"Qnt2"] %<>% as.numeric()

#snap_H <- read.csv2(snap.history.path)
#snap_H[,"Date_UTC"] %<>% as.POSIXct(tz="UTC")
#snap_H[,"Qnt1"] %<>% as.numeric()
#snap_H[,"Qnt2"] %<>% as.numeric()

pool_H <- read.csv2(pool.history.path) #%>% as.tibble()
pool_H[,"Date_UTC"] %<>% as.POSIXct(tz="UTC")

#pool_H[,"Qnt1"] %<>% as.numeric()
#pool_H[,"Qnt2"] %<>% as.numeric()

claim_H <- read.csv2(claim.table.path) #REPLACE WITH HIST FILE
claim_H[,"Date_UTC"] %<>% as.POSIXct(tz="UTC")


# PART 2: POOL CALCULATIONS ####

#> 2.a Pool INFO ####
poolName <- pool_H[1,"poolName", drop=T]
coin1 <- pool_H[1,"Coin1", drop=T]
coin2 <- pool_H[1,"Coin2", drop=T]
coin3 <- claim_H[1,"Coin3", drop=T] # coin 3 is recovered from claim_CALC

#> 2.b POOL RATIO/PRICE ####
# Add useful calculations for later ref
pool_H[,"PoolRatio"] <- with(pool_H, Qnt1/Qnt2) 
pool_H[,"PoolPrice"] <- 1/pool_H$PoolRatio

#> 2.c MARKET data ####
price1 <- priceMatrix[coin1,refCoin]
price2 <- priceMatrix[coin2,refCoin]
price3 <- priceMatrix[coin3,refCoin]

pool_H[,"Value1"] <- with(pool_H, Qnt1*price1)
pool_H[,"Value2"] <- with(pool_H, Qnt2*price2)
pool_H[,"ValueTOT"] <- with(pool_H, Value1+Value2)

#> 2.d STARTING point data ####
#> All calc related to current (final) state of Pool
#>> NOTE: in current implementation, it is expected for the LAST operation to be a ADD. 
#>> Future implementation will be able to interpret more complex (multi ADD/RMOVE) operation, 
#>> for now it is only plotting results from LAST operation

# !!! In the future, start_DF will be caulculated using the segment analsysis tool.
# If so, then all ops_df data can be merged to snapshot and used to simply have additional time points to plot

start_DF <- pool_H %>% 
  subset(operation=="ADD") %>% 
  tail(1)# find last ADD

#extract data
start_date <- start_DF[1,"Date_UTC", drop=T]
start_qnt1 <- start_DF[1,"Qnt1", drop=T] 
start_qnt2 <- start_DF[1,"Qnt2", drop=T] 

start_value <- start_DF[1,"ValueTOT", drop=T] 

#this is the most important param, as it define the center point for the IL plots.
start_ratio <- start_DF[1,"PoolRatio", drop=T]
start_price <- start_DF[1,"PoolPrice", drop=T]

#> 2.e FILTER for LATEST POOL ####
#> TRENDS/IL and other calc ####

pool_LAST <- pool_H %>% subset(Date_UTC >= start_date)

pool_LAST[,"PriceChange"] <- with(pool_LAST, PoolPrice/start_price) 
pool_LAST[,"RatioChance"] <- with(pool_LAST, PoolRatio/start_ratio) 

#> 2.f CLAIM Calculations ####
#> scan the full history of pool claims, and FILTER for those that happened AFTER 'poolStart' date

claim_CALC <- subset(claim_H, Date_UTC >= start_date)

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

#> 2.g ENDPOINT Data ####
#> All calc related to current (final) state of Pool
end_DF <- pool_LAST %>% subset(Date_UTC==max(Date_UTC))

end_date <- max(claim_CALC$Date_UTC, pool_LAST$Date_UTC) #whichever is higher

# TO BE FINISHED

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

##> COMMON plot aesthetics ####

###> X-RANGE options
# calculate xLim (min and max dates)
xLim_left <- start_date #%>% as.POSIXct(tz = "UTC")
xLim_right <- end_date #%>% as.POSIXct(tz = "UTC")
plot_lim <- c(xLim_left, xLim_right)


#### WORK IN PROGRESS ####
if(F){
LPlot_ILChanges <- function(ss_snaps, ss_claim, ss_end, seg_meta,
                            plot_lim, #if selected, value are used as man/max. c(NA,NA) = full data
                            stopLossTolerance = 0.01, # expressed as max IL% that is tolerate for this pool
                            bg_color = "white",
                            panel_color = "white"){}





#valStart <- seg_meta[1,"ValueSTART"] %>% round(2)
#valNow <- seg_meta[1,"ValueLAST"] %>% round(2)


## PLOT AESTETICS
#> Color Code DataPoints
ss_snaps[,"Label"] <- ifelse(ss_snaps[["IL"]]>0, "1", "2") #1= UP, 2= DOWN
legNames <- c(paste0("Get ",coin2),
              paste0("Get ",coin1))

# colorMatrix

#Change background color if pool is "at risk"
if(seg_meta[1,"ROI_Net"]<0){bg_color <- "orange"}
if(seg_meta[1,"ROI_Net"]<(stopLossTolerance*-1)){bg_color <- "red"}

## 0. PLOT INIT
fullTitle <- sprintf("%s: %se [value HODL: %se]", segName, valNow, valStart) # PLOT TITLE

plot0 <- ss_snaps %>%
  ggplot(aes(x=as.POSIXct(Date_UTC))) +
  theme_classic() +
  theme(panel.grid.major.y=element_line(), 
        panel.grid.minor.y=element_line(linetype="dashed"))+
  ggtitle(fullTitle)+
  scale_x_datetime(timezone = "UTC") +
  #scale_x_datetime(limits = plot_lim) + #v3.5: removed to allow stop-loss label to be plotted outside of area.
  xlab("Datetime (UTC)")

## 1. ADD: Price/IL
plot1 <- plot0 +
  geom_line(data=ss_snaps, aes(x=as.POSIXct(Date_UTC), y=IL), na.rm = TRUE) +
  geom_point(aes(y=IL, fill=Label),shape=21 , color="black", size=2, na.rm = TRUE) +
  scale_fill_manual(name = paste0("FavTrend: ",favTrend), values = labelColors, labels=legNames) +
  scale_y_continuous(labels = scales::percent_format(accuracy = NULL),
                     #minor_breaks = seq(-5, 5, 0.001), 
                     breaks=seq(-5, 5, 0.005)
  ) +
  geom_hline(yintercept = 0) 

## 2. ADD: CumulEARN

plot2 <- plot1 +
  geom_line(data= ss_claim, aes(x=as.POSIXct(Date_UTC), y=CumulX100_AVG), color="blue", na.rm = TRUE, linetype="dashed") +
  geom_line(data= ss_claim, aes(x=as.POSIXct(Date_UTC), y=-CumulX100_AVG), color="blue", na.rm = TRUE, linetype="dashed") +
  geom_line(data= ss_claim, aes(x=as.POSIXct(Date_UTC), y=CumulX100_TOT), color="blue", na.rm = TRUE) +
  geom_line(data= ss_claim, aes(x=as.POSIXct(Date_UTC), y=-CumulX100_TOT), color="blue", na.rm = TRUE) +
  geom_line(data= ss_claim, aes(x=as.POSIXct(Date_UTC), y=CumulX100_TOT+stopLossTolerance), color="red", na.rm = TRUE) +
  geom_line(data= ss_claim, aes(x=as.POSIXct(Date_UTC), y=-CumulX100_TOT-stopLossTolerance), color="red", na.rm = TRUE)

## 3. ADD: Endpoints
plot3 <- plot2 +
  #coord_cartesian(xlim = c(xLim_left, xLim_right + 1000000)) + 
  #> idea for the future: allow to expand grid size, so to be able to plot lables at the RIGHT of the plot.
  #> example: https://mran.microsoft.com/snapshot/2017-08-20/web/packages/ggrepel/vignettes/ggrepel.html
  
  annotate("label", x = xLim_left, y=0.0015, label=as.character(paste0("Entry Price: ",round(entryPrice,3),"")), size=3.5, hjust=0) +
  geom_point(data=ss_end, aes(x=xLim_right, y=LimitX100), color= ss_end$Color) + #add endpoint as scatterplot
  # idea: http://www.sthda.com/english/wiki/ggplot2-texts-add-text-annotations-to-a-graph-in-r-software
  geom_label_repel(data=ss_end, aes(x=xLim_right, y=LimitX100, label = Label_Extended),
                   fill= ss_end$Color, #box col.
                   color= ss_end$TextColor,#text col
                   size = 3.5, #text size
                   force = 0.2, #repulsion force (avoid 2x labels to get too close) #v3.5: changed from 1
                   min.segment.length = 0, #segments , this size as not plotted
                   segment.colour = ss_end$Color,
                   segment.linetype = 4, #"dotdash"
                   direction = "both", # segment direction: "both", "x", "y" #v3.5: changed to x
                   nudge_x = ss_end$nudge_x, #v3.5 change: now pushes label to the RIGHT
                   #nudge_x = -ss_end$nudge_x, #pushes label to the left
                   nudge_y = ss_end$nudge_y #pushes label up/downwards
  ) 

#color coded background
plot4 <- plot3 +
  theme(plot.background = element_rect(fill = bg_color),
        panel.background = element_rect(fill = panel_color))

#return(plot4)}


}