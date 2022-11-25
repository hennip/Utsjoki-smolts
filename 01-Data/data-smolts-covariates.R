# Annual data from 1.6. - 31.8. (92 days)
###############################################

# Smolts 2002-2016
# =================
ColNames<-c("smolts", "empty", "n_schools", "school_size")
Day<-c(c(1:30), c(1:31), c(1:31))
Month<-c(rep(6,30), rep(7,31), rep(8,31))


D02<-read_xls(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2002.xls"),
              sheet=1, na="", 
              range="B5:F60", col_names=c("smolts", "prop%", "n_schools", "prop2%","school_size"))
tmp<-array(NA, dim=c(6,5));colnames(tmp)<-colnames(D02)
D02<-rbind(tmp,D02) #1.6.-6.6. missing
tmp<-array(0, dim=c(30,5));colnames(tmp)<-colnames(D02)
D02<-rbind(D02, tmp) #2.8.-31.8. missing but replace with zeros
D02<-D02%>% mutate(day=c(1:92))%>%
  mutate(Year=2002)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D03<-read_xls(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2003.xls"),
              sheet=1, na="", 
              range="Z11:AD71", col_names=c("smolts", "empty", "prop%", "n_schools", "school_size"))
tmp<-array(0, dim=c(31,5));colnames(tmp)<-colnames(D03)
D03<-rbind(D03, tmp) #1.8.-31.8. missing but replace with zeros
D03<-D03%>% mutate(day=c(1:92))%>%
  mutate(Year=2003)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)


D04<-read_xls(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2004.xls"),
              sheet=1, na="", 
              range="Z8:AD68", col_names=c("smolts", "empty", "prop%", "n_schools", "school_size"))
tmp<-array(0, dim=c(31,5));colnames(tmp)<-colnames(D04)
D04<-rbind(D04, tmp) #1.8.-31.8. missing but replace with zeros
D04<-D04%>% mutate(day=c(1:92))%>%
  mutate(Year=2004)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D05<-read_xls(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2005.xls"),
              sheet=1, na="", 
              range="Z10:AC70", col_names=ColNames)
D05[23,]<-array(NA,dim=c(1,4)) # 23.6. 00-09 missing
tmp<-array(0, dim=c(31,4));colnames(tmp)<-colnames(D05)
D05<-rbind(D05, tmp) #1.8.-31.8. missing but replace with zeros
D05<-D05 %>% mutate(day=c(1:92))%>%
  mutate(Year=2005)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)
#View(D05)

D06<-read_xls(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2006.xls"),
              sheet=1, na="", range="Z23:AD114", 
              col_names=c("smolts", "empty", "prop%", "n_schools", "school_size"))%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2006)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D07<-read_xls(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2007.xls"),
              sheet=1, na="", range="Z8:AC94", 
              col_names=ColNames)
tmp<-array(0, dim=c(5,4));colnames(tmp)<-colnames(D07)
D07<-rbind(tmp, D07) #1.6.-5.6. missing but replace with zeros
D07<-D07 %>%   
  mutate(day=c(1:92))%>%
  mutate(Year=2007)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D08<-read_xls(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2008.xls"),
              sheet=1, na="", range="Z25:AC116",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2008)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D09<-read_xls(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2009.xls"),
              sheet=1, na="", range="Z16:AC107",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2009)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D10<-read_xls(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2010.xls"),
              sheet=1, na="", range="Z16:AC107",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2010)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D11<-read_xlsx(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2011.xlsx"),
              sheet=1, na="", range="Z16:AC107", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2011)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D12<-read_xlsx(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2012.xlsx"),
               sheet=1, na="", range="Z8:AC99", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2012)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D13<-read_xlsx(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2013.xlsx"),
               sheet=1, na="", range="Z9:AC100",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2013)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D14<-read_xlsx(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2014.xlsx"),
               sheet=1, na="", range="Z8:AC99", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2014)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D15<-read_xlsx(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2015.xlsx"),
               sheet=1, na="", range="Z8:AC83", col_names=ColNames)
tmp<-array(0, dim=c(16,4));colnames(tmp)<-colnames(D15)
D15<-rbind(D15, tmp) #16.8.-31.8. missing but replace with zeros
D15<-D15%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2015)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)
#View(D15)

D16<-read_xlsx(str_c(pathIn,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2016.xlsx"),
               sheet=1, na="", range="Z8:AC99", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2016)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)
#View(D16)


dat_smolts<-
  D02%>%full_join(D03, by=NULL)%>%
  full_join(D04, by=NULL)%>%
  full_join(D05, by=NULL)%>%
  full_join(D06, by=NULL)%>%
  full_join(D07, by=NULL)%>%
  full_join(D08, by=NULL)%>%
  full_join(D09, by=NULL)%>%
  full_join(D10, by=NULL)%>%
  full_join(D11, by=NULL)%>%
  full_join(D12, by=NULL)%>%
  full_join(D13, by=NULL)%>%
  full_join(D14, by=NULL)%>% 
  full_join(D15, by=NULL)%>% 
  full_join(D16, by=NULL)%>% 
  
  # What should schools be when smolts==0 ?
  # mutate(schools=if_else(smolts==0, NA_real_, school_size))%>%
  #mutate(schools=if_else(smolts==0, 1, school_size))%>%
  mutate(schools=if_else(smolts==0, 0.001, school_size))%>%
  
  # What should schools be when smolts==NA
  # mutate(schools=if_else(is.na(smolts)==T, 1, schools))%>%
  mutate(schools=if_else(is.na(smolts)==T, NA_real_, schools))%>%
  
  select(Year,Month,Day,day,smolts, schools)
#View(dat_smolts)

# Smolts, flow & water temperature 2017-2021
# =================

nls17 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2017.xlsx"))
wtemp17 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_veden lampo_2016-2017.xlsx"),range="A2:C8903")%>% 
mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))

disc17 <- read_excel(str_c(pathIn,"01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx"), sheet="Patoniva_virtaama_1963-2020", na=c("","-"))
wttr17 <- read_excel(str_c(pathIn,"FMI/wttr_17.xlsx"), sheet = "Havainnot")

data17 <- smdwrg_m(nls17, wtemp17, disc17, wttr17)
#view(data17[[1]])


nls18 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2018.xlsx")) %>% 
  rename(Klo = Hours)
wtemp18 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki2018.xls"),range="A2:C2892") %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
disc18 <- read_excel(str_c(pathIn,"01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx"), sheet="Patoniva_virtaama_1963-2020", na=c("","-"))
wttr18 <- read_excel(str_c(pathIn,"FMI/wttr_18.xlsx"), sheet = "Havainnot")

data18 <- smdwrg_m(nls18, wtemp18, disc18, wttr18)
#view(data18[[2]])


nls19 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2019.xlsx"), sheet="RAAKADATA") %>% 
  rename(Klo = Hours)
wtemp19 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_veden lampo_2018-2019.xlsx"), range="A2:C8639") %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
disc19 <- read_excel(str_c(pathIn,"01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx"), sheet="Patoniva_virtaama_1963-2020", na=c("","-"))
wttr19 <- read_excel(str_c(pathIn,"FMI/wttr_19.xlsx"), sheet = "Havainnot")

data19 <- smdwrg_m(nls19, wtemp19, disc19, wttr19)
#view(data19[[2]])


nls20 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2020_FINAL.xlsx")) %>% 
  rename(Klo = Hours)
wtemp20 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki2020.xlsx"), range="A2:C2954") %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
disc20 <- read_excel(str_c(pathIn,"01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx"), sheet="Patoniva_virtaama_1963-2020", na=c("","-"))
wttr20 <- read_excel(str_c(pathIn,"FMI/wttr_20.xlsx"), sheet = "Havainnot")

data20 <- smdwrg_m(nls20, wtemp20, disc20, wttr20)
#view(data20[[2]])

nls21 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2021_30112021.xlsx"), sheet="RAAKADATA") %>% 
  rename(Klo = Hours)
wtemp21 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki2021.xlsx"), range="A2:C3954") %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
# Never mind the warnigs, 2021 numbers are given as text
disc21 <- read_excel(str_c(pathIn,"01.5-Data_raw/Virtaama_Patoniva 1963-2021.xlsx"), sheet="Patoniva_virtaama_1963-2021", na=c("","-"))
wttr21 <- read_excel(str_c(pathIn,"FMI/wttr_21.xlsx"), sheet = "Havainnot")

data21 <- smdwrg_m(nls21, wtemp21, disc21, wttr21)


dat1721_all <- bind_rows(data17[[2]], data18[[2]]) %>% 
  bind_rows(data19[[2]]) %>% 
  bind_rows(data20[[2]]) %>% 
  bind_rows(data21[[2]])


dat1721 <- bind_rows(data17[[1]], data18[[1]]) %>% 
  bind_rows(data19[[1]]) %>% 
  bind_rows(data20[[1]]) %>% 
  bind_rows(data21[[1]])







# Flow 2002-2016
# ============
dat_flow<-read_xlsx(str_c(pathIn,"UTSJOKI VIRTAAMADATA/Virtaama_Patoniva 1963-2020.xlsx"),
               sheet="Patoniva_virtaama_1963-2020", range="A2:D21063",#skip=1,
               col_names = c("Day", "Month", "Year", "flow"), na=c("", "-") )%>%
  filter(Year>2001 & Year<2017)%>%
  filter(Month==6 | Month==7 | Month==8)

#View(filter(dat_flow, Year==2016))

# Temperature
# ============
T02 <- read_excel(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/temperaturlogger2002kam3.xls"), range = "A4:B435", col_names = 
                    T, sheet="Panu") %>% 
  transmute(
    Date = paiva,
    Temp = lampotila
  ) 

T03<-read_xls(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2003.xls"),
              sheet = "Utsjoki_raakadata", range="I8:J3074",
              col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T04<-read_xls(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2004_cam4.xls"),
              sheet = "Min-max lampotilat", range="A4:B1478",
              col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

# Note! Some weird logger points removed from 2005 data (15.6. & 21.6.)
T05<-read_xls(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2005_cam4.xls"),
              skip=36, col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T06<-read_xls(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2006.xls"),
              range="A36:B1834", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T07<-read_xls(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2007.xls"),
              range="A3:B2244",col_names=c("Date", "Temp"))%>%
  mutate(Date=date(parse_datetime(Date, "%d. %B %Y %H:%M")))

T08<-read_xls(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2008.xls"),
              sheet="UTS2008", range="A35:B3060", col_names=c("Date", "Temp"))%>%
      mutate(Date=date(parse_datetime(Date, "%d. %B %Y %H:%M")))

T09<-read_xls(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2009.xls"),
              sheet="UTS2009", range="A34:E3928",
              col_names=c("Day", "month", "Year","time", "Temp"))%>%
  select(Day, month, Year, Temp)%>%
  mutate(Month=if_else(
  month=="June", 6, if_else(
    month=="July", 7, if_else(
      month=="August", 8, NA_real_))))%>%
  filter(is.na(Month)==F)%>%
  select(-month)%>%
  group_by(Year,Month,Day)%>%
  summarize(meanTemp=mean(Temp))

T10<-read_xls(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2010.xls"),
              range="A36:B1834", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T11<-read_xlsx(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2011.xlsx"),
              sheet="Sheet1", range="B6:C4040", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T12<-read_xlsx(str_c(pathIn,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2012.xlsx"),
               sheet="Sheet1", range="B6:C4612", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T13<-read_xlsx(str_c(pathIn,
"UTSJOKI_VEDEN LAMPO/Utsjoki_veden lampo_2013-2014.xlsx"),
               sheet="Sheet1", range="B6:C8749", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T14<-read_xlsx(str_c(pathIn,
                     "UTSJOKI_VEDEN LAMPO/Utsjoki_veden lampo_2014-2015.xlsx"),
               sheet="Sheet1", range="B7:C8634", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

# 2015 Temperature data from Utsjoki is missing, logger failure

#T15<-

T16<-read_xlsx(str_c(pathIn,
                     "UTSJOKI_VEDEN LAMPO/Utsjoki_veden lampo_2016.xlsx"),
               sheet=1, range="B3:C3862", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))


#View(T14)

dat_temp<-T02%>%
  full_join(T03, by=NULL)%>%
  full_join(T04, by=NULL)%>%
  full_join(T05, by=NULL)%>%
  full_join(T06, by=NULL)%>%
  full_join(T07, by=NULL)%>%
  full_join(T08, by=NULL)%>%
  full_join(T10, by=NULL)%>%
  full_join(T11, by=NULL)%>%
  full_join(T12, by=NULL)%>%
  full_join(T13, by=NULL)%>%
  full_join(T14, by=NULL)%>%
  full_join(T16, by=NULL)%>%
  group_by(Date)%>%
  summarize(meanTemp=mean(Temp))%>%
  mutate(Year=year(as.POSIXct(Date)))%>%
  mutate(Day=day(as.POSIXct(Date)))%>%
  mutate(Month=month(as.POSIXct(Date)))%>%  
  select(Year,Month,Day,meanTemp)%>%
  full_join(T09, by=NULL)%>%
  filter(Month==6 | Month==7 | Month==8)

# Temperature data 2015 missing, logger failure
dat15<-filter(dat_temp, Year==2016)%>%
mutate(Year=2015, meanTemp=NA)

dat_temp<-full_join(dat_temp, dat15)


dat0216<-dat_smolts%>%
  full_join(dat_flow, by=NULL)%>%
  full_join(dat_temp, by=NULL)

#   reading weathers from 2002 to 2016
#   and compiling together

w0204 <- read_excel(str_c(pathIn,"FMI/wttr_0204.xlsx"), sheet = "Havainnot")
w0507 <- read_excel(str_c(pathIn,"FMI/wttr_0507.xlsx"), sheet = "Havainnot")
w0810 <- read_excel(str_c(pathIn,"FMI/wttr_0810.xlsx"), sheet = "Havainnot")
w1113 <- read_excel(str_c(pathIn,"FMI/wttr_1113.xlsx"), sheet = "Havainnot")
w1416 <- read_excel(str_c(pathIn,"FMI/wttr_1416.xlsx"), sheet = "Havainnot")

wttr_0216 <- bind_rows(w0204, w0507) %>% 
  bind_rows(., w0810) %>% 
  bind_rows(.,w1113) %>% 
  bind_rows(.,w1416) %>% 
  
  transmute(
    date = as_date(paste(Vuosi, Kk, Pv)),
    klo = format(Klo, format = "%H:%M"),
    press = `Ilmanpaine (msl) (hPa)`,
    rain = if_else(`Sademaara (mm)` < 0, 0, `Sademaara (mm)`),
    humi = `Suhteellinen kosteus (%)`,
    temp_air = `Ilman lampotila (degC)`,
    wind = `Tuulen nopeus (m/s)`,
    id = as.POSIXct(paste(Vuosi, Kk, Pv,klo), format = "%Y-%m-%d%H")
  ) %>% 
  
  group_by(date) %>% 
  summarise(
    press = mean(press, na.rm =T),
    rain = sum(rain, na.rm =T),
    humi = mean(humi, na.rm =T),
    temp_air = mean(temp_air, na.rm =T),
    wind = mean(wind, na.rm =T)
  ) 



#   getting amount of days from last rain
rainbf <- c()
frain = FALSE
for(i in 1:length(wttr_0216$rain)){
  print(i)
  if(wttr_0216$rain[i] > 0){
    rainbf[i] <- 0
    frain = TRUE
  }
  else if(frain == FALSE){rainbf[i] <- 0}
  else{
    bf = 1
    flag = F
    while (flag == F) {
      if(i-bf <= 1){ 
        #print("alussa")
        flag = T}
      else{ 
        #print("l?pi")
        if(wttr_0216$rain[i-bf] == 0){ bf = bf+1;flag = F}
        else if(wttr_0216$rain[i-bf] != 0){ flag = T}
      }
    }
    rainbf[i] = bf
  }
  
}


wttr_0216 %<>%
  select_all() %>% 
  mutate(
    rainbf = rainbf
  ) 


dat0216_all <- dat0216 %>% 
  mutate(
    date = as_date(paste(Year, Month, Day))
  ) %>% 
  left_join(., wttr_0216, by = "date")



# COMBINE all data from 2002-2016 and 2017->

data0221 <- bind_rows(dat0216_all, dat1721_all)

data0221 %>% filter(schools < 1, schools !=0.001)


#data0221 %>% filter(smolts == 0, schools != 0.001)
#data0221 %>% filter(smolts == 1, schools != 1)
#data0221 %>% filter(smolts >= 1, schools < 1)

data0221 <- data0221 %>% mutate(
  schools = if_else(smolts==0, 0.001, schools),
  schools = if_else(smolts==1, 1, schools)
  
)

#data0221 %>% filter(smolts == 0, schools != 0)
#data0221 %>% filter(smolts == 1, schools != 1)

view(data0221)



