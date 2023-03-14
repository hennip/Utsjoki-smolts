# Flow 2002-2016
# ================
dat_flow_0216<-read_xlsx(str_c(pathIn,"UTSJOKI VIRTAAMADATA/Virtaama_Patoniva 1963-2020.xlsx"),
                    sheet="Patoniva_virtaama_1963-2020", range="A2:D21063",#skip=1,
                    col_names = c("Day", "Month", "Year", "flow"), na=c("", "-") )%>%
  filter(Year>2001 & Year<2017)%>%
  filter(Month==6 | Month==7 | Month==8)

#View(filter(dat_flow, Year==2016))

# Temperature 2002-2016
# =======================
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

dat_temp_0216<-full_join(dat_temp, dat15)



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
  #print(i)
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


# Air temperature data 2002-2021 for whole year
t0221 <- read_csv("01-Data/temp0221.csv")
#View(t0221)


# 30 days temperature sum
tempsum <- t0221 %>% 
  transmute(
    date = paste(Vuosi, Kk, sep = '-') %>%
      paste(., Pv, sep = '-') %>% as.POSIXct(.),
    temp = `lampo_ka`,
    tempSum30 = rollFUN(temp, sum, 30, location = "right")
  )%>%
  mutate(date= as.Date(date))
#tempsum

# Water temperatures 2017-present
wtemp17 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_veden lampo_2016-2017.xlsx"),range="A2:C8903")%>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
wtemp18 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki2018.xls"),range="A2:C2892") %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
wtemp19 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_veden lampo_2018-2019.xlsx"), range="A2:C8639") %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
wtemp20 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki2020.xlsx"), range="A2:C2954") %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
wtemp21 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki2021.xlsx"), range="A2:C3954") %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))

# Never mind the warnings, 2021 numbers are given as text
disc_all <- read_excel(str_c(pathIn,"01.5-Data_raw/Virtaama_Patoniva 1963-2021.xlsx"), sheet="Patoniva_virtaama_1963-2021", na=c("","-"))

wttr17 <- read_excel(str_c(pathIn,"FMI/wttr_17.xlsx"), sheet = "Havainnot")
wttr18 <- read_excel(str_c(pathIn,"FMI/wttr_18.xlsx"), sheet = "Havainnot")
wttr19 <- read_excel(str_c(pathIn,"FMI/wttr_19.xlsx"), sheet = "Havainnot")
wttr20 <- read_excel(str_c(pathIn,"FMI/wttr_20.xlsx"), sheet = "Havainnot")
wttr21 <- read_excel(str_c(pathIn,"FMI/wttr_21.xlsx"), sheet = "Havainnot")


tmp<-readRDS(str_c(pathIn,"FMI/weather2022.rds"))

#data22 <- smdwrg_m(nls22, wtemp22, disc_all, wttr22)

