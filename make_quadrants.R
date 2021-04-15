


# labels for color palette
darktones <- c(RColorBrewer::brewer.pal(n=7,"Reds")[7],
               RColorBrewer::brewer.pal(n=4,"Reds")[4],
               RColorBrewer::brewer.pal(n=3,"Oranges")[3],
               "#FDE725FF",
               RColorBrewer::brewer.pal(n=3,"Greens")[3])

names(darktones) <- as.factor(c("<50%","50-59%", "60-69%","70-79%","80%+"))

make_quadrants <- function(states,df=countyquad,save=TRUE) {
  # this function accepts a county-level dataframe with 7-day averaged daily administration data 
  # must have columns for dose_number_1, dose_number_2, doses_total, doses_delivered,
  # county_fips code, and state abbreviation
  require(tidyverse)
  require(plotly)

# filter for most recent date and selected state.  Calculate throughput percentages.
scatter_throughput <- filter(df,date==max(date,na.rm=T) & state==states) %>%
      mutate(firstpercent=dose_number_1/doses_delivered) %>%
      mutate(secondpercent=dose_number_2/doses_delivered) %>%
      mutate(totalpercent=doses_total/doses_delivered) %>%
      mutate(throughputcat=cut(totalpercent,breaks=c(-10,0.5,0.6,0.7,0.8,9999),
                               labels=c("<50%","50-59%", "60-69%","70-79%","80%+"))) %>%
      left_join(county.name,by=c("county_fips")) %>%
      mutate_at(vars(contains("dose")),funs(round(.,digits=1)))

# this data table will be merged with svi and placed on the data tab of the report
saveRDS(mutate_at(scatter_throughput,vars(contains("percent")),funs(scales::percent(.,accuracy=0.1))),
        file="state_datatable.rds")

# dashed median reference line (vertical)
dose1_line_state = data.frame(
  x =rep(median(scatter_throughput$firstpercent,na.rm=T),2),
  y = c(0,2*max(scatter_throughput$secondpercent,na.rm=T)),
  county_median=c("Median","Median"))

# dashed median reference line (horizontal)
dose2_line_state = data.frame(
  x =c(0,2*max(scatter_throughput$firstpercent,na.rm=T)),
  y = rep(median(scatter_throughput$secondpercent,na.rm=T),2),
  county_median=c("Median","Median"))

# remove NA points
scatter_throughput <- filter(scatter_throughput,!is.na(throughputcat)) %>% droplevels() %>% ungroup() 

# create the ggplot static version.  
g_statescatter <-  
  ggplot(scatter_throughput, aes(x=firstpercent,y=secondpercent,
                                 text=paste("County FIPS: ",county,"</br>Seven-day averages of:
                                       </br>First dose - ",dose_number_1,
                                            "</br>Second dose - ",dose_number_2,
                                            "</br>Lagged doses delivered - ",doses_delivered)))+
   geom_point(alpha=0.6,aes(size=doses_delivered,color=throughputcat))+
   scale_color_manual(values = darktones)+
  # this allows the axes to vary with the data range
  scale_y_continuous(labels=scales::percent_format(accuracy=1),
                     limits = c(0,2*max(scatter_throughput$secondpercent,na.rm=T)),
                     breaks = seq(0,2*max(1,max(scatter_throughput$secondpercent,na.rm=T)),by=.05))+
  scale_x_continuous(labels=scales::percent_format(accuracy=1),
                     limits = c(0,2*max(scatter_throughput$firstpercent,na.rm=T)),
                     breaks = seq(0,2*max(1.7,max(scatter_throughput$firstpercent,na.rm=T)),by=.10))+
  labs(x = "Percent of delivered doses administered as 1st doses", 
       y = "Percent of delivered doses administered as 2nd doses",
       #color= "average overall throughput",
       title="County-level throughput: average first/second doses \nadministered as % delivered")+
  geom_line(data = dose1_line_state, linetype="dashed", color="grey",aes(x=x,y=y,name=county_median),inherit.aes=F)+
  geom_line(data = dose2_line_state, linetype="dashed", color="grey",aes(x=x,y=y,name=county_median),inherit.aes=F)+
  labs(name="Median doses")+
  theme_bw()+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),legend.title = element_blank())

# use ggplotly to create an html version of the static ggplot
p_statescatter <- ggplotly(g_statescatter,tooltip="text",width = 800, height = 550) %>%
  # tweak the appearance of the plotly object including starting zoom level and legend
  layout(xaxis = list(range = c(0, min(1.7,1.2*max(scatter_throughput$firstpercent,na.rm=T)))),
         yaxis = list(range = c(0, min(1,1.2*max(scatter_throughput$secondpercent,na.rm=T))))) %>%
  config(displayModeBar = T) %>% layout(margin=list(t = 75)) %>% 
  #layout(legend = list(font = list(size=10))) %>%
  layout(legend = list(title = list(text="Overall average <br> throughput category")))

if(save==TRUE) {saveRDS(p_statescatter,"p_statescatter.rds")}
#if you want to quickly check only the scatterplot by itself, use save==F in the function call.
if(save==FALSE) {return(p_statescatter)}

}


