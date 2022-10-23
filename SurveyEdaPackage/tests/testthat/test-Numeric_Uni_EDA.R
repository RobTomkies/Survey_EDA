#logrelative error
LRE <- function(x,c){
  if(c==0){return(round(-log(abs(x), base = 10)))}
  else{return(round(-log((abs(x-c)/c) , base = 10)))}
}
