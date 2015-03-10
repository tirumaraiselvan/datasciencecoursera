pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)  
  sum = 0.0
  count = 0
  for (i in id) {
    
    FilePath = paste(directory, sprintf("/%.3i.csv",i), sep="") 
    ##print(FilePath)
    data = read.csv(FilePath, header = TRUE, colClasses = c("factor","numeric","numeric","integer") )
    sum = sum + sum(data[pollutant],na.rm =TRUE)
    count = count +sum(!is.na(data[pollutant]))
  }
  
  return(sum/count) ##round()?
}
