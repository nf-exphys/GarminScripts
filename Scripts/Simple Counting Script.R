
#Counting to see how many times something occurred in Fit.DF 
j=0
y=1350
for(i in 1:y){
  
  if (Fit.DF[[i]]$sum_data$timestamp == -1){
    j <- j+1
    
  }
  ifelse(i == y, print(j), next)
}