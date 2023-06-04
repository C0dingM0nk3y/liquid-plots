#### @ script_PLOTS_v1 @ ####

#> Scripts take "id" and "dir.REF.single" "dir.TABLES.single" variables from GlobalEnv and use them to
#> 1. Rebuild all paths and upload data
#> 2. Perform calculations required for plotting
#> 3. Produce different plots
#> 4. Return a collage of plots, as a single 'fig'


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
