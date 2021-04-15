
darktones <- c(RColorBrewer::brewer.pal(n=7,"Reds")[7],
               RColorBrewer::brewer.pal(n=4,"Reds")[4],
               RColorBrewer::brewer.pal(n=3,"Oranges")[3],
               "#FDE725FF",
               RColorBrewer::brewer.pal(n=3,"Greens")[3])

make_sparkline <- function(st,df=sparkline,earlydec=FALSE) {
  # by default, cuts off volatile early december dates; but you can specify earlydec=TRUE to include it
 datestart <- ifelse(earlydec==FALSE,lubridate::mdy("12/26/2020"),lubridate::mdy("1/1/2000"))
 
 sparkline.throughput <- df %>%
   mutate(firstpercent=dose_number_1/doses_delivered) %>%
   mutate(secondpercent=dose_number_2/doses_delivered) %>%
   mutate(totalpercent=doses_total/doses_delivered) %>%
   #mutate_at(vars(contains("percent")),funs(if_else(!is.finite,NA_real_,.))) %>%
   mutate(buckets=cut(totalpercent,breaks=c(-0.01,0.495,0.595,0.695,0.795,10),
                      labels=c("<50%","50-59%", "60-69%","70-79%", "80%+"  )))
 
 gsp <- ggplot(data=filter(sparkline.throughput,state==st,date>=datestart)) +
  labs(title = paste("Daily percentage of", st, "total doses delivered as of 3 days\r
		prior that have been administered as a first dose (lower dark portion)\r
		or second dose (upper light portion) since January 1, 2021"),
       y = "Percent of doses delivered that have been administered as first and second doses")+
  theme(#panel.background = element_blank(),	
        #plot.title=element_text(size = 14, hjust = 0.5),
        axis.title.x=element_blank(),
        #making the legend separately
        legend.position="none",
        #make a square around the plot
        #panel.border = element_rect(colour = "black", fill=NA, size=2))+
  )+
   theme_bw()+
  #the solid color is not actually an area plot - it needs to be many multiple daily rectangles,
  #because it needs the possibility of changing color on any given day
  #this is the lighter color with transparency/alpha = 0.4 representing second doses 
  #its actually total doses with the second dose portion peeking up above first doses
  geom_rect(color = NA, aes(xmin=date-.5, xmax = date+.5,ymin=0, ymax=totalpercent,fill=buckets),
            alpha=0.4)+
  scale_fill_manual(values = darktones, drop = FALSE)+
  #and this is the first doses (darker color with no transparency, plotted on top)
  geom_rect(color = NA, aes(xmin=date-.5, xmax = date+.5,ymin=0, ymax=firstpercent, fill=buckets))+
  # these are the thick lines that trace over the tops of the rectangles.
  geom_line(size = 1.4, color = "grey25", aes(x = date, y = totalpercent))+
  geom_line(size = 1.4, color = "grey50", aes(x = date, y = firstpercent))
 return(gsp)
}
  
  
  
  