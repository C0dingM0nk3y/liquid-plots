LiqPlots_Trends <- function(nudge=TRUE){
  #> Plots pool ASSET COMPOSITION over time
  #> All variables that are not defined here come from GlobalEnv
  
  if (nudge==TRUE){
    trends_lim <- plot_nudged}
  else{
    trends_lim <- plot_lim}
  
  ## Y-RANGE options
  #> Default is to plot up to +25% ref Price (up) and -25% price (down)
  #> If real data are outside of this range, then it plots 10% above and below min and max
  yLim_offset <- 0.25
  yLim_up <- max(start_price*(1+yLim_offset), max(pool_LAST$PoolPrice)*1.1, max(limits_DF$Limit_Price )*1.025)
  yLim_down <- min(start_price*(1-yLim_offset), min(pool_LAST$PoolPrice)*0.9, min(limits_DF$Limit_Price )*0.975)
  
  # CONVERTS DATES to POSIXct
  pool_LAST[,"Date_UTC"] %<>% as.POSIXct(tz = "UTC")
  
  # OTHER QUICK CALC
  pool_LAST[,"Label"] <- ifelse(pool_LAST$PriceChange>1, paste0("Swap to ",coin2), paste0("Swap to ",coin1))
  
  # PLOT
  plot0 <- pool_LAST %>%
    ggplot(aes(x=Date_UTC)) +
    theme_classic() +
    theme(panel.grid.major.y=element_line(), 
          panel.grid.minor.y=element_line(linetype="dashed"))+
    ggtitle(poolName)+
    scale_x_datetime(timezone = "UTC", limits = trends_lim) +
    scale_y_continuous(limits = c(yLim_down, yLim_up))+
    #scale_x_datetime(limits = plot_lim) + #v3.5: removed to allow stop-loss label to be plotted outside of area.
    xlab("Datetime (UTC)")
  
  ## 1. ADD: annotations and threshohs (bottom layer)
  limits_ss <- subset(limits_DF, !(Label_root =="Current")) #enpoints, minus current
  
  plot1 <- plot0 +
    geom_hline(yintercept = start_price, linetype="dashed") +
    geom_hline(yintercept = limits_ss$Limit_Price, linetype="dashed", color= limits_ss$Color) #all beside "Current"
    
    #geom_label_repel(data= subset(limits_DF, Label_root=="Current"), aes(x=xLim_left, y=start_price, label = as.character(paste0("Entry Price: ",round(start_price,3),""))),
    #                 size=3.5, force = 1, fill= "white", direction="y") #, nudge_y = -start_price*0.1) 
    
  ## 2. ADD: PriceTrends
  plot2 <- plot1 +
    geom_line(data=pool_LAST, aes(x=Date_UTC, y=PoolPrice), na.rm = TRUE) +
    geom_point(aes(y=PoolPrice, fill=Label),shape=21, colour= "black",  size=2, na.rm = TRUE) +
    ylab(poolName)
  
  ## 3. ADD: Endpoints
    
  plot3 <- plot2 +
    geom_point(data=limits_DF, aes(x=xLim_right, y=Limit_Price), color= limits_DF$Color) + #add endpoint as scatterplot
    annotate("label", x = xLim_left, y=start_price*0.96, label=as.character(paste0("Entry Price: ",round(start_price,3),"")), size=3.5, hjust=0) +
    geom_label_repel(data=limits_DF, aes(x=xLim_right, y=Limit_Price, label = Label_Extended),
                     fill= limits_DF$Color, #box col.
                     color= limits_DF$TextColor,#text col
                     size = 3.5, #text size
                     force = 0.2, #repulsion force (avoid 2x labels to get too close) #v3.5: changed from 1
                     min.segment.length = 0, #segments , this size as not plotted
                     segment.colour = limits_DF$Color,
                     segment.linetype = 4, #"dotdash"
                     direction = "both", # segment direction: "both", "x", "y" #v3.5: changed to x
                     nudge_x = limits_DF$nudge_x, #v3.5 change: now pushes label to the RIGHT
                     #nudge_x = -limits_DF$nudge_x, #pushes label to the left
                     nudge_y = limits_DF$nudge_y #pushes label up/downwards
    ) 
  

  return(plot3)
}

LiqPlots_ILChanges <- function(stopLossTolerance = 0.01, # expressed as max IL% that is tolerate for this pool
                            bg_color = "white",
                            panel_color = "white"){

  valStart <- start_DF[1,"ValueTOT"]
  valNow <- end_DF[1,"ValueTOT"]
  valChangeX100 <- (valNow-valStart)/valStart
  
  
  ## PLOT AESTETICS
  #> Color Code DataPoints
  pool_LAST[,"Label"] <- ifelse(pool_LAST$PriceChange>1, paste0("Swap to ",coin2), paste0("Swap to ",coin1))
  
  # colorMatrix
  #Change background color if pool is "at risk"
  if(end_ROInet<0){bg_color <- "orange"}
  if(end_ROInet<(stopLossTolerance*-1)){bg_color <- "red"}
  
  
  ## 0. PLOT INIT
  fullTitle <- sprintf("%s: %se [if HODL: %se (%s%s%%)]", #%% = '%'
                       poolName, round(valNow,2), round(valStart,2), 
                       ifelse(valChangeX100>0, "+", ""),  round(valChangeX100*100,1)) # PLOT TITLE
  
  plot0 <- pool_LAST %>%
    ggplot(aes(x=Date_UTC)) +
    theme_classic() +
    theme(panel.grid.major.y=element_line(), 
          panel.grid.minor.y=element_line(linetype="dashed"))+
    ggtitle(fullTitle)+
    scale_x_datetime(timezone = "UTC") +
    #scale_x_datetime(limits = plot_lim) + #v3.5: removed to allow stop-loss label to be plotted outside of area.
    xlab("Datetime (UTC)")
  
  ## 1. ADD: Price/IL
  plot1 <- plot0 +
    geom_line(data=pool_LAST, aes(x=Date_UTC, y=IL), na.rm = TRUE) +
    geom_point(aes(y=IL, fill=Label),shape=21 , color="black", size=2, na.rm = TRUE) +
    geom_point(aes(y=IL),shape=21 , color="black", size=2, na.rm = TRUE) +
    #scale_fill_manual(name = paste0("FavTrend: ",favTrend), values = labelColors, labels=legNames) +
    scale_y_continuous(labels = scales::percent_format(accuracy = NULL),
                       #minor_breaks = seq(-5, 5, 0.001), 
                       breaks=seq(-5, 5, 0.005)) +
    ylab("Impermanent Loss (%)| Cumul Earnings (%)") +
    geom_hline(yintercept = 0)

  
  ## 2. ADD: CumulEARN
  
  plot2 <- plot1 +
    #geom_line(data= claim_CALC, aes(x=Date_UTC, y=CumulX100_AVG), color="blue", na.rm = TRUE, linetype="dashed") +
    #geom_line(data= claim_CALC, aes(x=Date_UTC, y=-CumulX100_AVG), color="blue", na.rm = TRUE, linetype="dashed") +
    geom_line(data= claim_CALC, aes(x=Date_UTC, y=Cum_ValTOTx100), color="blue", na.rm = TRUE) +
    geom_line(data= claim_CALC, aes(x=Date_UTC, y=-Cum_ValTOTx100), color="blue", na.rm = TRUE) +
    geom_line(data= claim_CALC, aes(x=Date_UTC, y=Cum_ValTOTx100+stopLossTolerance), color="red", na.rm = TRUE) +
    geom_line(data= claim_CALC, aes(x=Date_UTC, y=-Cum_ValTOTx100-stopLossTolerance), color="red", na.rm = TRUE)
  
  ## 3. ADD: Endpoints
  plot3 <- plot2 +
    #coord_cartesian(xlim = c(xLim_left, xLim_right + 1000000)) + 
    #> idea for the future: allow to expand grid size, so to be able to plot lables at the RIGHT of the plot.
    #> example: https://mran.microsoft.com/snapshot/2017-08-20/web/packages/ggrepel/vignettes/ggrepel.html
    annotate("label", x = xLim_left, y=-0.0015, label=as.character(paste0("Entry Price: ",round(start_price,3),"")), size=3.5, hjust=0) +
    #geom_label_repel(data= subset(limits_DF, Label_root=="Current"), aes(x=xLim_left, y=0, label = as.character(paste0("Entry Price: ",round(start_price,3),""))),
    #                 size=3.5, force = 1, fill= "white", direction="y", nudge_y = -0.002) +
    geom_point(data=limits_DF, aes(x=xLim_right, y=LimitX100), color= limits_DF$Color) + #add endpoint as scatterplot
    # idea: http://www.sthda.com/english/wiki/ggplot2-texts-add-text-annotations-to-a-graph-in-r-software
    geom_label_repel(data=limits_DF, aes(x=xLim_right, y=LimitX100, label = Label_Extended),
                     fill= limits_DF$Color, #box col.
                     color= limits_DF$TextColor,#text col
                     size = 3.5, #text size
                     force = 0.2, #repulsion force (avoid 2x labels to get too close) #v3.5: changed from 1
                     min.segment.length = 0, #segments , this size as not plotted
                     segment.colour = limits_DF$Color,
                     segment.linetype = 4, #"dotdash"
                     direction = "both", # segment direction: "both", "x", "y" #v3.5: changed to x
                     nudge_x = limits_DF$nudge_x, #v3.5 change: now pushes label to the RIGHT
                     #nudge_x = -limits_DF$nudge_x, #pushes label to the left
                     nudge_y = limits_DF$nudge_y #pushes label up/downwards
    ) 
  
  #color coded background
  plot4 <- plot3 +
    theme(plot.background = element_rect(fill = bg_color),
          panel.background = element_rect(fill = panel_color))
  
  return(plot4)
}

LiqPlots_Swaps <- function(){
  #> Plots automatic swap events due to the prided liquidity
  
  #> PLOT SUBSET
  #pool_SWAPS <- subset(pool_LAST, subset = !(is.na(Swap1_X100)))
  pool_SWAPS <- pool_LAST #TEST
  
  ## PLOT AESTETICS
  yLim_offset <- 0.05 # 5%
  maxSwap <- max(abs(c(pool_SWAPS$Swap1_X100, pool_SWAPS$Swap2_X100)))
  
  ySwap_range <- max(yLim_offset, maxSwap)*1.1 #applyes to both up and down (symmetrical)
  
  # Automatic title
  if (end_DF[1,"Qnt1"] > start_qnt1){
    in_qnt <- end_DF[1,"Qnt1"] - start_qnt1
    in_coin <- coin1
    out_qnt <- end_DF[1,"Qnt2"]- start_qnt2
    out_coin <- coin2}
  else{
    in_qnt <- end_DF[1,"Qnt2"] - start_qnt2
    in_coin <- coin2
    out_qnt <- end_DF[1,"Qnt1"]- start_qnt1
    out_coin <- coin1}
  
  swap_Title <- sprintf("Swapped %s %s for %s %s [as if price: %s]", 
                        signif(in_qnt, 4), in_coin, signif(out_qnt, 4), out_coin, round(out_qnt/in_qnt,2))
  
  #PLOT
  plot0 <- pool_LAST %>%
    ggplot(aes(x=Date_UTC)) +
    ggtitle(swap_Title)+
    theme_classic() +
    theme(panel.grid.major.y=element_line(), 
          panel.grid.minor.y=element_line(linetype="dashed"))+
    scale_x_datetime(timezone = "UTC", limits = plot_nudged) +
    scale_y_continuous(limits = c(-ySwap_range, ySwap_range),
                       labels = scales::percent_format(accuracy = NULL),
                       #minor_breaks = seq(-5, 5, 0.001), 
                       breaks=seq(-0.5, 5, 0.05)) +
    xlab("Datetime (UTC)")
  
  ## 1. ADD: Swap data
  plot1 <- plot0 +
    geom_line(data=pool_SWAPS, aes(x=Date_UTC, y=Swap1_X100, color="orange"), linewidth=1.5) +
    geom_line(data=pool_SWAPS, aes(x=Date_UTC, y=Swap2_X100, color="purple"), linewidth=1.5) +
    #geom_point(aes(y=PoolPrice, fill=Label),shape=21, colour= "black",  size=2, na.rm = TRUE) +
    ylab("Coin Swaps")
  
  return(plot1)
  
}


plotLimits.Calc <- function(stopLossTolerance = 0.01,
                             calc_nudgeParam = TRUE,
                             hide_slbase = TRUE){
  
  # Helper function. It allows to calculate important pool reference prices (aka. limits, e.g. stop loss, braek eaven..)
  # it also calculate details required for plotting (color, legend, offsets...)
  # required inputs: ..... from GlobalEnv (e.g. end_DF...)
  
  # empty DF to begin calculations
  limits_df <- data.frame(Direction = c("UP", "DOWN"),
                          SL_X100 = c(stopLossTolerance, -stopLossTolerance),
                          Sign = c(1,-1))
  
  # BreakEven Limits (earnings)
  limits_df[,"BE_X100"] <- ifelse(limits_df[,"Direction"]=="DOWN", #change sign if Direction == "DOWN"
                                  -end_maxEarm, end_maxEarm) 
  
  # Adjusted StopLoss (includes earnings)
  limits_df[,"SL_X100adj"] <- limits_df[,"SL_X100"] + limits_df[,"BE_X100"]
  
  # tidy up dataframe
  limits_df2 <-  pivot_longer(limits_df, cols=c("SL_X100", "BE_X100", "SL_X100adj"), 
                                              names_to = "Type", values_to = "LimitX100")
  
  # calculate the Ratio at which the pool with reach Limit
  for (r in 1:nrow(limits_df2)){
    limits_df2[r, "Limit_Ratio"] <- ILtoRatio(limits_df2[r, "LimitX100"], trend = limits_df2[r, "Direction"])}
  
  # Converts Ratio to Price
  limits_df2[, "Limit_Price"] <- limits_df2[, "Limit_Ratio"]* start_price
  
  # Add "PriceNow"
  limits_df2 <- add_row(limits_df2, 
                           Sign = 1,
                           Type = "PriceNOW",
                           LimitX100 = RatioToIL(PriceChange = (end_price/start_price)), #IL at price NOW (for plotting axis) 
                           Limit_Ratio = end_price/start_price, # Ratio
                           Limit_Price = end_price) # Price
  
  # Makes LABELS                           
  bool <- limits_df2$Limit_Price>100 #round down or trunc depending on how big is the price
  limits_df2[bool, "Label_Text"] <- round(limits_df2[bool, "Limit_Price"],0) %>% unlist() %>% as.character() #round and convert to text
  limits_df2[!(bool), "Label_Text"] <- signif(limits_df2[!(bool), "Limit_Price"],3) %>% unlist() %>% as.character() #round to significant digits
  
  # Customuze labels
  # color palette: http://sape.inf.usi.ch/sites/default/files/ggplot2-colour-names.png
  limits_df2[, c("Label_root", "TextColor")] <- NA #add_empty
  for (r in 1:nrow(limits_df2)){
    label <- limits_df2[r, "Type"]
    if(str_starts(label, "SL")){
      limits_df2[r, c("Label_root", "Color", "TextColor")] <- c("Stop-loss", "firebrick1", "white") %>% as.list()}
    if(str_starts(label, "BE")){
      limits_df2[r, c("Label_root", "Color", "TextColor")] <- c("Break-even", "dodgerblue3", "white") %>% as.list()}
    if(str_starts(label, "PriceNOW")){
      limits_df2[r, c("Label_root", "Color", "TextColor")] <- c("Current", "springgreen3", "black") %>% as.list()}
    
    #Extended labels
    limits_df2[r, "Label_Extended"] <- paste(limits_df2[r, "Label_root"], limits_df2[r, "Label_Text"], sep=": ") %>% unlist
    if(str_starts(label, "SL")){ #stop-loss only
      limits_df2[r, "Label_Extended"] <- sprintf("%s (%s%%): %s",
                                                 limits_df2[r, "Label_root"],round(stopLossTolerance*100,1), limits_df2[r, "Label_Text"])}
  }
  
  # Color Coded PRICE
  limit_price <- subset(limits_df2, Type=="PriceNOW", select="LimitX100")
  limit_be <- subset(limits_df2, Type=="BE_X100", select="LimitX100") %>% max() #upper limit
  limit_sl <- subset(limits_df2, Type=="SL_X100adj", select="LimitX100") %>% max() #upper limit
  
  if (abs(limit_price)>abs(limit_be)){limits_df2[limits_df2$Type=="PriceNOW", "Color"] <- "orange"}
  if (abs(limit_price)>abs(limit_sl)){limits_df2[limits_df2$Type=="PriceNOW", "Color"] <- "red"}
  
  # OTHER PARAM: Nudge offset
  
  if (calc_nudgeParam){
    #static param
    xLim_left <- min(pool_LAST[["Date_UTC"]], claim_CALC[["Date_UTC"]]) %>% as.POSIXct()
    xLim_right <- max(pool_LAST[["Date_UTC"]], claim_CALC[["Date_UTC"]]) %>% as.POSIXct()
    nudge_x <- (datetime_to_sec(xLim_right)- datetime_to_sec(xLim_left)) %>% #distance (in UNIX time!) from label and datapoint.
      as.numeric()*0.4 #multiply by 0.4 (size of 2/5 of the plot area)) #v3.5 changed from 0.2
    
    # Nudge adjustment
    limits_df2[, "nudge_x"] <- nudge_x #create defaults
    limits_df2[, "nudge_y"] <- 0 #create defaults
    
    #correct signs
    limits_df2[, "nudge_y"] <- limits_df2[, "nudge_y"]*limits_df2[, "Sign"]
  }
  
  # OTHER PARAM: hide basic Stop-Loss (not adjusted)
  if (hide_slbase){
    limits_df2 <- subset(limits_df2, !(Type ==  "SL_X100")) #Hides limits that should not be plotted
  }
  
  limits_df2 <- limits_df2[order(limits_df2$Limit_Ratio, decreasing = T),]
  
  return(limits_df2)
}

