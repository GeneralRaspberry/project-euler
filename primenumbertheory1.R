###primenumbertheory
primefunction<-function(x){
  store<-vector(length = 6)
repeat{
  for(i in store){
  f<-2
  y<-seq_len(f)
  p<-y[f%%y==0]
  if(length(p)==2){
    store[i]<-max(p)
  }
  f<-f+1
  if(all(is.na(store)==FALSE))break
}
  }
  return (max(store))
}

primefunction(6)


#############################

store<-c()
primefunction2<-function(x){
  f<-1
  while(length(store)<x){
    y<-seq_len(f)
    p<-y[f%%y==0]
    if(length(p)==2){
       store<-c(store,f)
    }
    f<-f+1
  }
  return(max(store))
}


primefunction2(6)