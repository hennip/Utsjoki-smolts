# Annual smolt data from 1.6. - 31.8. (92 days)
ColNames<-c("smolts", "empty", "n_schools", "school_size")
Day<-c(c(1:30), c(1:31), c(1:31))
Month<-c(rep(6,30), rep(7,31), rep(8,31))


D05<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2005.xls"),
              sheet="Smolttimäärät", na="", 
              range="Z10:AC70", col_names=ColNames)
D05[23,]<-rep(NA,4) # 23.6. 00-09 missing
tmp<-array(0, dim=c(31,4));colnames(tmp)<-colnames(D05)
D05<-rbind(D05, tmp) #1.8.-31.8. missing but replace with zeros
D05<-D05 %>% mutate(day=c(1:92))%>%
  mutate(Year=2005)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)
#View(D05)

D06<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2006.xls"),
              sheet="Smolttimäärät", na="", range="Z23:AD114", 
              col_names=c("smolts", "empty", "prop%", "n_schools", "school_size"))%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2006)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D07<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2007.xls"),
              sheet="Smolttimäärät", na="", range="Z8:AC94", 
              col_names=ColNames)
tmp<-array(0, dim=c(5,4));colnames(tmp)<-colnames(D07)
D07<-rbind(tmp, D07) #1.6.-5.6. missing but replace with zeros
D07<-D07 %>%   
  mutate(day=c(1:92))%>%
  mutate(Year=2007)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D08<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2008.xls"),
              sheet="Smolttimäärät", na="", range="Z25:AC116",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2008)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D09<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2009.xls"),
              sheet="Smolttimäärät", na="", range="Z16:AC107",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2009)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D10<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2010.xls"),
              sheet="Smolttimäärät", na="", range="Z16:AC107",col_names=c("smolts", "empty", "n_schools", "school_size"))%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2010)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D11<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2011.xlsx"),
              sheet="Smolttimäärät", na="", range="Z16:AC107", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2011)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D12<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2012.xlsx"),
               sheet="Smolttimäärät", na="", range="Z8:AC99", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2012)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D13<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2013.xlsx"),
               sheet="Smolttimäärät", na="", range="Z9:AC100",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2013)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D14<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2014.xlsx"),
               sheet="Smolttimäärät", na="", range="Z8:AC99", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2014)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)


dat_smolts<-
  D05%>%full_join(D06, by=NULL)%>%
  full_join(D07, by=NULL)%>%
  full_join(D08, by=NULL)%>%
  full_join(D09, by=NULL)%>%
  full_join(D10, by=NULL)%>%
  full_join(D11, by=NULL)%>%
  full_join(D12, by=NULL)%>%
  full_join(D13, by=NULL)%>%
  full_join(D14, by=NULL)%>% 
  mutate(schools=if_else(smolts==0, NA_real_, school_size))%>%
  select(Year,Month,Day,day,smolts, schools)

# Flow
# ============
dat_flow<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI VIRTAAMADATA/Virtaama_Patoniva 1963-2014.xlsx"),
               sheet="Patoniva_virtaama_1963-2014") %>%
  rename(Year="Vuosi", Day="Päivä", Month="Kuukausi", flow="Virtaama")%>%
  filter(Year>2004)

# Temperature
# ============
# Note! Some weird logger points left out from 2005 data (15.6. & 21.6.)
T05<-read_xls(paste(sep="",pathIn2,"UTSJOKI_VEDEN LÄMPÖ/Temperature_Utsjoki2005_cam4.xls"),
              skip=36, col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T06<-read_xls(paste(sep="",pathIn2,"UTSJOKI_VEDEN LÄMPÖ/Temperature_Utsjoki2006.xls"),
              range="A36:B1834", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T07<-read_xls(paste(sep="",pathIn2,"UTSJOKI_VEDEN LÄMPÖ/Temperature_Utsjoki2007.xls"),
              range="A3:B2244",col_names=c("Date", "Temp"))%>%
  mutate(Date=date(parse_datetime(Date, "%d. %B %Y %H:%M")))

T08<-read_xls(paste(sep="",pathIn2,"UTSJOKI_VEDEN LÄMPÖ/Temperature_Utsjoki2008.xls"),
              sheet="UTS2008", range="A35:B3060", col_names=c("Date", "Temp"))%>%
      mutate(Date=date(parse_datetime(Date, "%d. %B %Y %H:%M")))

T09<-read_xls(paste(sep="",pathIn2,"UTSJOKI_VEDEN LÄMPÖ/Temperature_Utsjoki2009.xls"),
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

T10<-read_xls(paste(sep="",pathIn2,"UTSJOKI_VEDEN LÄMPÖ/Temperature_Utsjoki2010.xls"),
              range="A36:B1834", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T11<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI_VEDEN LÄMPÖ/Temperature_Utsjoki2011.xlsx"),
              sheet="Sheet1", range="B6:C4040", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T13<-read_xlsx(paste(sep="",pathIn2,
"UTSJOKI_VEDEN LÄMPÖ/Utsjoki_veden lämpö_2013-2014.xlsx"),
               sheet="Sheet1", range="B6:C8749", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T14<-read_xlsx(paste(sep="",pathIn2,
                     "UTSJOKI_VEDEN LÄMPÖ/Utsjoki_veden lämpö_2014-2015.xlsx"),
               sheet="Sheet1", range="B7:C8634", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

View(T14)

dat_temp<-T05%>%
  full_join(T06, by=NULL)%>%
  full_join(T07, by=NULL)%>%
  full_join(T08, by=NULL)%>%
  full_join(T10, by=NULL)%>%
  full_join(T11, by=NULL)%>%
  full_join(T13, by=NULL)%>%
  full_join(T14, by=NULL)%>%
  group_by(Date)%>%
  summarize(meanTemp=mean(Temp))%>%
  mutate(Year=year(as.POSIXct(Date)))%>%
  mutate(Day=day(as.POSIXct(Date)))%>%
  mutate(Month=month(as.POSIXct(Date)))%>%  
  select(Year,Month,Day,meanTemp)%>%
  full_join(T09, by=NULL)%>%
  filter(Month==6 | Month==7 | Month==8)

dat<-dat_smolts%>%
  full_join(dat_flow, by=NULL)%>%
  full_join(dat_temp, by=NULL)

ggplot(dat)+
  geom_line(aes(x = day, y = meanTemp, color=as.factor(Year)), size=1.2)

ggplot(dat)+
  geom_line(aes(x = day, y = flow, color=as.factor(Year)), size=1.2)+
  coord_cartesian(ylim=c(0,160))


ggplot(filter(dat, Year==2014))+
  geom_line(aes(x = day, y = meanTemp, color=as.factor(Year)), size=1.2)