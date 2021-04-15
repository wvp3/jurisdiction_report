source("make_quadrants.R")
source("make_svimap.R")
source("make_sparkline.R")


# you need to first prepare the most recent data 
# see prep_county_throughput.R, prep_state_throughput.R and prep_svi_data.R

make_jurisdiction_report <- function(st,format="html",qc=FALSE) {
  statetitle <- as.data.frame(cbind(state.name,state.abb)) %>% 
    bind_rows(territories) %>% filter(state.abb==st | state.name==st) 
  filenamest <- statetitle %>% select(state.name) %>% as.character() 
  print(filenamest)
  make_quadrants(st) 
  make_sparkline(st) %>% saveRDS("state_sparkline.rds")
  make_svimap(st)
  statemergedtable <- left_join(readRDS("state_datatable.rds"),
            select(svimapdata,county_fips,svi,tert_svi_state,firstdose_coverage=coverage,tert_coverage_state),
            by="county_fips") %>%
    left_join(statetitle,by=c("state"="state.abb")) %>% 
    # this next select command will vary; need to customize 
    select(state,state.name=state.name.x,county,county_fips,contains("dose"),contains("percent"),
           throughputcat,contains("svi"),contains("coverage")) %>%
    mutate(county_fips=ifelse(is.na(county_fips) | 
                              county_fips %in% c("UNK","Unknown","unknown",""),
                              "<missing>",county_fips)) %>%
    mutate(state.name=ifelse(is.na(state.name),filenamest,state.name))
    saveRDS(statemergedtable,file="state_mergedtable.rds")

  if(format=="html") {rmarkdown::render("all_county_throughput.rmd",output_file=paste0(
    filenamest,"_",as.character(lubridate::today())))    }
  if(format=="pdf") {rmarkdown::render("all_county_throughput_pdf.rmd",output_file=paste0(
    filenamest,"_",as.character(lubridate::today())))    }
  if(qc==TRUE) {statemergedtable %>% summarise_at(vars(contains("dose")),funs(sum),na.rm=T) %>% print()}
}

purrr::map(state.abb,possibly(make_jurisdiction_report,otherwise=NA))
#purrr::map(state.name,possibly(make_jurisdiction_report,otherwise=NA))
