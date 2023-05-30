LiqPlots_Trends <- function(){
  
  # Plots TREND price.
  # All variable come from GlobalEnv

  ## X-RANGE options
  # calculate xLim (min and max dates)
  xLim_left <- start_date %>% as.POSIXct(tz = "UTC")
  xLim_right <- end_date %>% as.POSIXct(tz = "UTC")
  plot_lim <- c(xLim_left, xLim_right)
  
  ## Y-RANGE options
  #> Default is to plot up to +50% ref Price (up) and 1/2 price (down)
  #> If real data are outseide of this range, then it plots 10% above and below min and max
  yLim_up <- max(entryPrice*1.5, max(snap_H$PoolPrice)*1.1)
  yLim_down <- min(entryPrice*0.5, min(snap_H$PoolPrice)*0.9)
  
  # CONVERTS DATES to POSIXct
  snap_H[,"Date_UTC"] %<>% as.POSIXct(tz = "UTC")
  
  # OTHER QUICK CALC
  snap_H[,"Label"] <- ifelse(snap_H$PriceChange>1, "ABOVE", "BELOW") #USEFUL?
  
  
  # PLOT
  plot0 <- snap_H %>%
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
    geom_line(data=snap_H, aes(x=Date_UTC, y=PoolPrice), na.rm = TRUE) +
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
    geom_hline(yintercept = entryPrice, linetype="dashed") +
    annotate("label", x = xLim_left, y=entryPrice*1.1, label=as.character(paste0("Entry Price: ",round(entryPrice,3),"")),
             size=3.5, hjust=0)

  return(plot2)
}
