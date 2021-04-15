svi_leaflet_palette <- colorFactor(as.vector(biscale::bi_pal("GrPink",preview = FALSE)), 
                                   svimapdata$lvl_scale)

# usacountymap <- rgdal::readOGR("us_countymap/tl_2016_us_county.shp")
# create a lookup for county names, state names, county fips, statefips, state abb
# county.name <- select(usacountymap@data,STATEFP,county_fips=GEOID,county=NAME) %>% 
#  left_join(state.fips,by="STATEFP") 

# takes an updated df containing most recent svi (i.e. 2018) joined to 1st dose % coverage (at county)
# returns a leaflet object or .rds for markdown report
# see prep_svi_data.R
make_svimap <- function(st,df=svimapdata,save=TRUE) {
  getstatefips <- state.fips %>% filter(state.name==st | state.abb==st) %>% select(STATEFP) %>% as.character()
  statecountymap <- usacountymap[usacountymap$STATEFP==getstatefips,] 
  if(st %in% c(state.abb,"DC","PR")) {statecountymap <- merge(statecountymap,df,by=c("GEOID"))}
  #if(st %in% territories$state.abb & st!="DC") {statecountymap <- merge(statecountymap,df,by=c("STATEFP"))}
  
  statesvimap <- leaflet() %>%
    addTiles(options=tileOptions(opacity=0.5)) %>%
    addPolygons(data=statecountymap, color="grey25",weight=1.1,
                fillColor = ~svi_leaflet_palette(statecountymap@data$lvl_scale),
                fillOpacity=0.8,
                label=paste(statecountymap$NAME," County"),
                popup=paste0(statecountymap$NAME," County","<br/>",
                             "FIPS: ",statecountymap$GEOID,"<br/>",
                             "SVI: ",round(statecountymap$svi,2)," (",statecountymap$lvl_svi,") ","<br/>",
                             "1st dose Vaccine coverage: ",scales::percent(statecountymap$coverage,accuracy=0.01),
                             #"1st dose Vaccine coverage: ",statecountymap$coverage,
                             " (",statecountymap$lvl_coverage,") ")) 
  # addLegend(position="bottomright",pal=svi_leaflet_palette,values=statecountymap@data$lvl_scale,
  #           title="SVI x coverage tertiles",na.label = "NA")
  # 
  if(save==TRUE) {saveRDS(statesvimap,file="statesvimap.rds") }
  else return(statesvimap)
}  
