#   vec is data to be used
#   FUN is function to be used (sum, mean etc)
#   width defines the width of rolling window
#   location defines, where in the window current observation shold be located
#   possible values are "left", "center", "right"

rollFUN <- function(vec, FUN, width, location, naOm=F, naRm=F){
  #   length of data
  n <- length(vec)
  #   roll left curretn ubs is at the beginning of the eindow
  if(location == "left"){
    roll = rollLEFT(vec, width)
  }
  #   current bs is at he end of the window 
  else if(location == "right"){
    roll = rollRIGHT(vec, width)
  }
  #   current obs is in the centere of the rolling window
  else if(location == "center"){
    roll = rollCENTER(vec, width)
  }
  
  #return(roll)
  res <- c()
  for(i in 1:n){
    vals <- vec[roll[[i]]]
    #   naOm  omits NAvals, if they is any
    if(naOm == T){vals = na.omit(vals)}
    #   naRm removes na values 
    if(naRm == T){
      id <- which(is.na(vec) == T)
      vals <- vals[- id]
    }
    
    res[i] <- FUN(vals)
    
  }
  return(res)
  
  
}

rollLEFT <- function(vec, width){
  #   length of data
  n <- length(vec)
  width = width-1
  #   definingn start and endpoints for roll
  roll <- list()
  for(i in 1:n){
    j = 0
    if((i + width) > n){
      j = 1
      while((i+width-j)>n){j = j+1}
    }
    
    #times = ifelse(j-1>0, j, 0)
    ids <- c(i:(i+width-j), rep(i+width-j, j) )
    roll[[i]] = ids
  }
  return(roll)
}

rollRIGHT <- function(vec, width){
  #   length of data
  n <- length(vec)
  width = width-1
  #   definingn start and endpoints for roll
  roll <- list()
  for(i in 1:n){
    
    j = 0
    
    if((i - width) < 1){
      j = 1
      while((i-width+j)<1){j = j+1}
    }
    #times = ifelse(j-1>0, j, 0)
    #print(j)
    ids <- c(rep(i-width+j, j), (i-width+j):i )
    roll[[i]] = ids
  }
  return(roll)
}

rollCENTER <- function(vec, width){
  if(width == 1){
    roll = list()
    for(i in 1:length(vec)){
      roll[[i]] = i 
    }
  }
  else{
    left <- ceiling(width/2)
    right <- width-left
    roll_right <- rollRIGHT(vec, right) %>% 
      lapply(., function(x, subt_amnt) ifelse( (x-subt_amnt) != 0,  x-subt_amnt, 1),
             subt_amnt = 1)
    roll_left <- rollLEFT(vec, left)
    roll = mapply(c, roll_right,roll_left, SIMPLIFY = F)
  }
  return(roll)
}
