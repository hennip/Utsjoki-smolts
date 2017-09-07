# Modify dat_all into input form for JAGS

#dat_all%>%group_by(Year)%>%
#  summarize(N_smolts=sum(smolts, na.rm=T),
#            ave_sch=mean(schools, na.rm=T),
#            ave_flow=mean(flow, na.rm=T),
#            ave_temp=mean(meanTemp, na.rm=T)
#  )

#View(filter(dat_all, Year==2006))
# These do the same
#df_smolts[,1]<-filter(dat_all, Year==y)$smolts
#df_smolts[,1]<-filter(dat_all, Year==y)[["smolts"]]

smolts_data_to_jags<-function(x){ # x: vector of years of data to include
  nyears<-length(x)
  
  df_tmp<-array(NA, dim=c(92,nyears))
  
  df_smolts<-df_tmp
  df_schools<-df_tmp
  df_flow<-df_tmp
  df_temp<-df_tmp
  for(i in 1:nyears){
    df_smolts[,i]<-filter(dat_all, Year==x[i])$smolts
    df_schools[,i]<-filter(dat_all, Year==x[i])$schools
    df_flow[,i]<-filter(dat_all, Year==x[i])$flow
    df_temp[,i]<-filter(dat_all, Year==x[i])$meanTemp
  }
  colnames(df_smolts)<-x
  colnames(df_schools)<-x
  colnames(df_flow)<-x
  colnames(df_temp)<-x

  df<-list(df_smolts,
           df_schools,
           df_flow,
           df_temp)
  names(df)<-c("Smolts", "Schools", "Flow", "Temp")
  return(df)
}

#years<-c(2005:2009,2011,2013:2014) # temp data missing for 2012 and partly for 2010
#years<-2014

#smolts_data_to_jags(years)

