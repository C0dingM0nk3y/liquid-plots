LiqPlots_Trends <- function(){
  #> Plots pool ASSET COMPOSITION over time
  #> All variables that are not defined here come from GlobalEnv
  

  ## Y-RANGE options
  #> Default is to plot up to +50% ref Price (up) and 1/2 price (down)
  #> If real data are outseide of this range, then it plots 10% above and below min and max
  yLim_up <- max(start_price*1.5, max(pool_LAST$PoolPrice)*1.1)
  yLim_down <- min(start_price*0.5, min(pool_LAST$PoolPrice)*0.9)
  
  # CONVERTS DATES to POSIXct
  pool_LAST[,"Date_UTC"] %<>% as.POSIXct(tz = "UTC")
  
  # OTHER QUICK CALC
  pool_LAST[,"Label"] <- ifelse(pool_LAST$PriceChange>1, "ABOVE", "BELOW") #USEFUL?
  
  
  # PLOT
  plot0 <- pool_LAST %>%
    ggplot(aes(x=Date_UTC)) +
    theme_classic() +
    theme(panel.grid.major.y=element_line(), 
          panel.grid.minor.y=element_line(linetype="dashed"))+
    ggtitle(poolName)+
    scale_x_datetime(timezone = "UTC", limits = plot_lim) +
    scale_y_continuous(limits = c(yLim_down, yLim_up))+
    #scale_x_datetime(limits = plot_lim) + #v3.5: removed to allow stop-loss label to be plotted outside of area.
    xlab("Datetime (UTC)")
  
  ## 1. ADD: PriceTrends
  plot1 <- plot0 +
    geom_line(data=pool_LAST, aes(x=Date_UTC, y=PoolPrice), na.rm = TRUE) +
    geom_point(aes(y=PoolPrice, fill=Label),shape=21, colour= "black",  size=2, na.rm = TRUE) +
    ylab(poolName)
  
  ## [Optional] ADD refData
  if (is.data.frame(pool_REF)){#if not FALSE: var is either FALSE, or a data.frame
    plot1 <- plot1 +
      geom_line(data=pool_REF, aes(x=Date_UTC, y=PoolPrice), na.rm = TRUE) +
      geom_point(data=pool_REF, aes(x=Date_UTC, y=PoolPrice), na.rm = TRUE, shape=21, colour= "black", fill="white")
  }
  
  ## 2. ADD: annotations (top layer)
  plot2 <- plot1 +
    geom_hline(yintercept = start_price, linetype="dashed") +
    annotate("label", x = xLim_left, y=start_price*1.05, label=as.character(paste0("Entry Price: ",round(start_price,3),"")),
             size=3.5, hjust=0)

  return(plot2)
}
