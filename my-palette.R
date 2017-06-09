library(RColorBrewer)
library(wesanderson)

#my_palette = c(brewer.pal(5, "Set1")[c(1,3,4,5)], brewer.pal(5, "Pastel1")[c(2,5,1,3)])


display.brewer.pal(n=8,"Dark2")
display.brewer.pal(n=8,"Set2")
display.brewer.pal(n=8,"Set1")
display.brewer.pal(n=10,"Paired")
display.brewer.pal(n=8,"RdBu")

#my_palette = c(
#  wes.palette(n=5,"Darjeeling"))

my_palette = c(
  brewer.pal(8, "Set1"))
  
#my_palette = c(
#  brewer.pal(5, "Dark2"),  brewer.pal(5, "Set2"),
#  brewer.pal(10, "Paired")[c(2,1,4,3)])
##  brewer.pal(8, "RdBu")[c(8,6,2,3)])
palette(my_palette)


#my_palette = c(
#  brewer.pal(5, "Dark2")[1], brewer.pal(5, "Set2")[1],
#  brewer.pal(5, "Dark2")[2], brewer.pal(5, "Set2")[2]
#)


