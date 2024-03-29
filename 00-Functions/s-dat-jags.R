s_dat_jags <- function(dat, years, days){
  nYears <- length(years)
  nDays <- days
  #   filter data first
  #   needed variables: smolts, schools, flow, temp, temp_air, rain, rainbf
  dat_f <- dat %>% filter(Year %in% years, day <= days) %>% group_by(Year) %>% 
    group_split(.keep=F) %>% unlist(recursive = F) %>% as.data.frame() 
  
  data = list(
    nYears = nYears,
    nDays = nDays,
    Smolts = dat_f %>% select(matches("smolts\\.|smolts$")) %>% as.matrix(),
    Schools = dat_f %>% select(matches("schools\\.|schools$")) %>% as.matrix(),
    Flow = dat_f %>% select(matches("flow\\.|flow$")) %>% as.matrix(),
    Temp = dat_f %>% select(matches("meanTemp\\.|meanTemp$")) %>% as.matrix(),
    Temp_air = dat_f %>% select(matches("temp_air\\.|temp_air$")) %>% as.matrix(), 
    Temp_air_sum30 = dat_f %>% select(matches("tempSum30\\.|tempSum30$")) %>% as.matrix(), 
    Rain = dat_f %>% select(matches("rain\\.|rain$")) %>% as.matrix(),
    Rain_bf = dat_f %>% select(matches("rainbf\\.|rainbf$")) %>% as.matrix(),
    side_east = dat_f %>% select(matches("side_east\\.|side_east$")) %>% as.matrix(),
    side_west = dat_f %>% select(matches("side_west\\.|side_west$")) %>% as.matrix()
  )
  return(data)
}