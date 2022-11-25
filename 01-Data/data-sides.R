
#==========================================================================
# Call and combine side stream data separately for eastern and western side
#==========================================================================


# 2004 western side

side_west04<-read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_smoltit 2004_rantauomat.xls"), 
                        sheet="Kam 9_smoltit",range="A6:Z29", col_names = c("date", 0:23, "side_west"), na="!" )%>%
  transmute(
    date = as.Date(date),
    side_west = side_west
  )
side_west04

(side_west<-side_west04)

# 2020 eastern side

side_east20 <- read_excel(str_c(pathIn,"01.5-Data_raw/Utsjoki_lisakamerat_kalat 2020_final_08102020.xlsx")) %>% 
  select(Date, Smolt) %>% 
  transmute(
    date = as.Date(Date),
    side_east = Smolt
  )%>% 
  group_by(date) %>% 
  summarise(
    side_east = sum(side_east, na.rm =T)
  )
#View(side_east20)

# 2021 eastern side

side_east21<-tibble(date=as.Date(c("2021-06-18","2021-06-29","2021-06-30","2021-07-05","2021-07-07")), 
                   side_east=c(9,165,71,79,45))

(side_east<-side_east20%>%full_join(side_east21))
#View(side_east)
