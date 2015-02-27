pollutantmean = function(directory, pollutant, id=1:332) {
  
  sum = 0.0
  count = 0
  for (i in id) {
    
    FilePath = paste(directory, sprintf("%.3i.csv",i), sep="") 
##    print(FilePath)
    data = read.csv(FilePath)
    names(data) = c("date", "sulfate", "nitrate", "id")
    sum = sum + sum(data[pollutant],na.rm =TRUE)
    count = count +sum(!is.na(data[pollutant]))
  }
  
  return(round(sum/count, digits =3)) 
}
