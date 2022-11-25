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


dat_smolts_0216<-
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
  
  
  
nls17 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2017.xlsx"))
nls18 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2018.xlsx")) %>% 
  rename(Klo = Hours)
nls19 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2019.xlsx"), sheet="RAAKADATA") %>% 
  rename(Klo = Hours)
nls20 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2020_FINAL.xlsx")) %>% 
  rename(Klo = Hours)
nls21 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2021_30112021.xlsx"), sheet="RAAKADATA") %>% 
  rename(Klo = Hours)
