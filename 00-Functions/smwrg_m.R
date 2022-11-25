

smdwrg_m <- function(d_nls, d_wtemp, d_disc, d_wttr, read_data = F){
  #   first reading in all the datas from excel
  
  #   nousulohet ja smoltit (migrating salmons and smolts)
  if(read_data == T){
    sm_r <- read_excel(d_nls)
    print("Smolts OK")
    #   veden l?mp?tila (water temp)
    t_r <- read_excel(d_wtemp, skip = 1)
    print("Water temp OK")
    #   virtaama (discharge)
    d_r <- read_excel(d_disc)
    print("Discharge OK")
    #   s?? (weather)
    wtr_r <- read_excel(d_wttr, sheet = "Havainnot")
    print("Weather OK")
  }
  else{
    sm_r <- d_nls
    t_r <- d_wtemp
    d_r <- d_disc
    wtr_r <- d_wttr
  }
  
  #   start of wranglinng
  
  #   SMOLTS    #
  ###############
  
  sm <- sm_r %>%
    select(Date, Klo, Smolt) %>%
    transmute(
      date = as_date(Date),
      klo = format(Klo, format="%H:%M:%S"),
      smolt = Smolt,
      id = as.POSIXct(paste(date,klo), format = "%Y-%m-%d%H"))
  
  #   smolts by hour
  smolt_hourly <- sm %>% 
    group_by(id) %>% 
    summarise(n_schools = sum(smolt>0),
              #n_schools = sum(!is.na(smolt)),
              smolt = sum(smolt, na.rm =T),
              school_size = sum(smolt)/n_schools) %>% 
    select(id, smolt, n_schools, school_size)
  
  #   smolts by day
  smolt_daily <- sm %>% 
    group_by(date) %>% 
    summarise(n_schools = sum(smolt>0),
              #n_schools = sum(!is.na(smolt)),
              smolt = sum(smolt, na.rm =T),
              school_size = sum(smolt)/n_schools) %>% 
    select(date , smolt, n_schools, school_size)
  
  smolt_daily <- smolt_daily %>% mutate(
    day = cumsum(!is.na(date))
  ) 
  
  print("Smolts wrangling done!")
  
  #   WATER TEMP    #
  ###################
  #   temperatures hourly
  colnames(t_r) <- c("ind","DT", "TEMP")
  td <- t_r %>% 
    #select(`Date Time, GMT+03:00`, `Temp, ?C (LGR S/N: 10754831, SEN S/N: 10754831, LBL: Water)`) %>% 
    select(DT, TEMP) %>% 
    transmute(
      #date = as_date(`Date Time, GMT+03:00`),
      date = as_date(DT),
      #temp = `Temp, ?C (LGR S/N: 10754831, SEN S/N: 10754831, LBL: Water)`,
      temp = as.numeric(TEMP),
      id = DT
    )
  td <- td %>% 
    mutate(
      sus = sustemp(temp)
    )
  
  temp_daily <- td %>%
    filter(!(sus)) %>% 
    group_by(date) %>% 
    summarise(temp = mean(temp))
  
  
  #return(list(td, temp_daily))
  
  print("Water temperature wrangling done!")
  #   DISCHARGE   #
  #################
  d <- d_r %>% 
    select_all() %>% 
    transmute(
      date = as.POSIXct(paste(Vuosi, Kuukausi, Paiva), format = "%Y %m %d"),
      disc = Virtaama
    ) 
  
  print("Discharge wrangling done!")
  #   WEATHERDATA   #
  ###################
  #   all weather
  wtr_all <- wtr_r %>%  
    select_all() %>% 
    transmute(
      date = as_date(paste(Vuosi, Kk, Pv)),
      klo = format(Klo, format = "%H:%M"),
      press = `Ilmanpaine (msl) (hPa)`,
      rain = `Sademaara (mm)`,
      humi = `Suhteellinen kosteus (%)`,
      temp_air = `Ilman lampotila (degC)`,
      wind = `Tuulen nopeus (m/s)`,
      id = as.POSIXct(paste(date,klo), format = "%Y-%m-%d%H")
    )  
  #   hourly weather
  wttr_hourly <- wtr_all %>% 
    group_by(id) %>% 
    summarise(
      press = mean(press, na.rm =T),
      rain = sum(rain, na.rm =T),
      humi = mean(humi, na.rm =T),
      temp_air = mean(temp_air, na.rm =T),
      wind = mean(wind, na.rm =T)
    )
  #   daily weather
  wttr_daily <- wtr_all %>% 
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
  for(i in 1:length(wttr_daily$rain)){
    if(wttr_daily$rain[i] > 0){
      rainbf[i] <- 0
    }
    else{
      count = 0
      bf = 1
      if(i-bf == 1){
        count = bf
      }
      else{
        while (wttr_daily$rain[i-bf]==0) {
          bf = bf+1
        }
        count = bf
      }
      rainbf[i] = count
    }
  }
  wttr_daily %<>%
    select_all() %>% 
    mutate(
      rainbf = rainbf
    )
  
  print("Weather wrangling done!")
  #   colllecting data together
  
  data_all <- left_join(smolt_daily, temp_daily, by ="date") %>% 
    left_join(., d, by ="date") %>%  
    left_join(., wttr_daily, by = "date") %>% 
    select_all() %>% 
    mutate(
      Year = as.numeric(format(date, format("%Y"))),
      Month = as.numeric(format(date, format("%m"))),
      Day = as.numeric(format(date, format("%d"))),
      day = day,
      flow = disc,
      smolts = smolt,
      schools = school_size,
      #schools = if_else(school_size == "NaN", 0.001, school_size),
      meanTemp = temp,
      disc = NULL,
      smolt = NULL,
      temp = NULL
    ) %>% 
    select(Year, Month, Day, day, smolts, schools, flow, meanTemp, date, 
           press, rain, humi, temp_air, rainbf)
  print("Data combined!")
  data <- data_all %>% 
    select(Year,
           Month,
           Day,
           day,
           flow,
           smolts,
           schools,
           meanTemp)
  
  return(
    list(
      data,
      data_all
    )
  )
  
}
