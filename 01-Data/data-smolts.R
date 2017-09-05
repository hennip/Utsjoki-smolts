# Annual smolt data from 1.6. - 31.8. (92 days)
ColNames<-c("smolts", "empty", "n_schools", "school_size")

D05<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2005.xls"),
              sheet="Smolttimäärät", na="", 
              range="Z10:AC70", col_names=ColNames)
D05[23,]<-rep(NA,4) # 23.6. 00-09 missing
tmp<-array(0, dim=c(31,4));colnames(tmp)<-colnames(D05)
D05<-rbind(D05, tmp) #1.8.-31.8. missing but replace with zeros
D05<-D05 %>% mutate(day=c(1:92))%>%
  mutate(year=2005)%>%
  select(year,smolts, school_size)

D06<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2006.xls"),
              sheet="Smolttimäärät", na="", range="Z23:AD114", 
              col_names=c("smolts", "empty", "prop%", "n_schools", "school_size"))%>% 
  mutate(day=c(1:92))%>%
  mutate(year=2006)%>%
  select(year,smolts, school_size)

D07<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2007.xls"),
              sheet="Smolttimäärät", na="", range="Z8:AC94", 
              col_names=ColNames)
tmp<-array(0, dim=c(5,4));colnames(tmp)<-colnames(D07)
D07<-rbind(tmp, D07) #1.6.-5.6. missing but replace with zeros
D07<-D07 %>%   
  mutate(day=c(1:92))%>%
  mutate(year=2007)%>%
  select(year,smolts, school_size)

D08<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2008.xls"),
              sheet="Smolttimäärät", na="", range="Z25:AC116",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(year=2008)%>%
  select(year,smolts, school_size)

D09<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2009.xls"),
              sheet="Smolttimäärät", na="", range="Z16:AC107",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(year=2009)%>%
  select(year,smolts, school_size)

D10<-read_xls(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2010.xls"),
              sheet="Smolttimäärät", na="", range="Z16:AC107",col_names=c("smolts", "empty", "n_schools", "school_size"))%>% 
  mutate(day=c(1:92))%>%
  mutate(year=2010)%>%
  select(year,smolts, school_size)

D11<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2011.xlsx"),
              sheet="Smolttimäärät", na="", range="Z16:AC107", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(year=2011)%>%
  select(year,smolts, school_size)

D12<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2012.xlsx"),
               sheet="Smolttimäärät", na="", range="Z8:AC99", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(year=2012)%>%
  select(year,smolts, school_size)

D13<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2013.xlsx"),
               sheet="Smolttimäärät", na="", range="Z9:AC100",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(year=2013)%>%
  select(year,smolts, school_size)

D14<-read_xlsx(paste(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2014.xlsx"),
               sheet="Smolttimäärät", na="", range="Z8:AC99", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(year=2014)%>%
  select(year,smolts, school_size)





#View(D07)



View(D14)

# if smolts==0, school_size=0