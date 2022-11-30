# Combine smolt, covariate and side stream datasets for the time series

source("01-Data/data-smolts.R")
source("01-Data/data-covariates.R")
source("01-Data/data-sides.R")


dat0216_all<-as_tibble(dat_smolts_0216)%>%
  full_join(dat_flow_0216, by=NULL)%>%
  full_join(dat_temp_0216, by=NULL)%>% 
  mutate(date = as_date(paste(Year, Month, Day)))%>% 
  left_join(., wttr_0216, by = "date")

# Use function smdwrg_m to collect annual data from 2017->
data17 <- smdwrg_m(nls17, wtemp17, disc_all, wttr17)
data18 <- smdwrg_m(nls18, wtemp18, disc_all, wttr18)
data19 <- smdwrg_m(nls19, wtemp19, disc_all, wttr19)
data20 <- smdwrg_m(nls20, wtemp20, disc_all, wttr20)
data21 <- smdwrg_m(nls21, wtemp21, disc_all, wttr21)


dat1721_all <-bind_rows(data17[[2]], data18[[2]])%>%
  bind_rows(data19[[2]]) %>% 
  bind_rows(data20[[2]]) %>% 
  bind_rows(data21[[2]])#%>%
#mutate(date=as.Date(date)) # Don't use, this messes the dates for some strange reason!


# COMBINE smolt and covariate data from 2002-2016 and 2017->

data0221 <- full_join(dat0216_all, dat1721_all)

# Set schools if smolts== 0 or 1
data0221 <- data0221 %>% mutate(
  schools = if_else(smolts==0, 0.001, schools),
  schools = if_else(smolts==1, 1, schools)
  
)


dat<-full_join(data0221,side_east)%>%# Combine with side stream data
  full_join(side_west)%>%
  select(-humi, -wind, -press)


dat_m <- left_join(dat, tempsum %>% select(date,tempSum30), by = "date")

df0221<-s_dat_jags(dat_m, years, n_days) 

saveRDS(df0221, file="01-Data/df0221.RDS")
saveRDS(dat_m, file="01-Data/dat0221.RDS")


#View(data0221%>%filter(Year==2018))
#View(dat1721_all%>%filter(Year==2018))
#View(dat_m%>%filter(Year==2018))

# View(dat)
# View(dat%>%filter(is.na(side_east)==F |is.na(side_west)==F))
# View(dat%>%filter(Year==2002))

