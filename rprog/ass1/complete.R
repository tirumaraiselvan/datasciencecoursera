complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  id_vector = numeric(length(id))
  nobs_vector = numeric(length(id))
  iter = 0
  for (i in id) {
    iter = iter + 1
    count = 0
    FilePath = paste(directory, sprintf("/%.3i.csv",i), sep="") 
    ## print(FilePath)
    data = read.csv(FilePath, header = TRUE ,na.strings = c("NA","NULL"))
    count = length(intersect(which(!is.na(data$sulfate)), which(!is.na(data$nitrate))))
    id_vector[iter] = i
    nobs_vector[iter] = count
  }
  
  return(data.frame(id = id_vector, nobs = nobs_vector))
}