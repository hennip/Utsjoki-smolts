# sustemp etsii epäilyttäviä lämpötila-arvoja tilanteessa
# jossa loggeri siirretään ilmasta veteen
# temp tulee Luken vedenlämpöloggerin tiedostoista
sustemp <- function(temp,oal = 15, dff = -5){
  n = length(temp)
  nas <- which(is.na(temp))
  for(i in nas){temp[i] <- temp[i-1]} 
  sus = c()
  if(temp[1] > oal){sus[1] <- T}
  else{sus[1] <- F}
  for(i in 2:n){
    if( temp[i]-temp[i-1] < dff ){
      count = 0
      j = 1
      diff = temp[i]-temp[i-1]
      sus[i] <- F
      while(diff <  dff){
        j = j+1
        if(i-j == 0){diff = 0}
        else{
          diff = temp[i]-temp[i-j]
          sus[i-j] <- T}
      }
    }
    else{sus[i] <- F}
  }
  return(sus)
}