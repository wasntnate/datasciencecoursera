pollutantmean <- function(directory, pollutant, id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  points <- NULL;
  for(x in id){
    #
    # Determine the file path
    if(x < 10){
      fileName <- paste(directory,"\\00",x,".csv", sep="");
    } else if(x < 100){
      fileName <- paste(directory,"\\0",x,".csv", sep="");
    } else{
      fileName <- paste(directory,"\\",x,".csv", sep="");
    }    
    
    #
    # Read the CSV file
    csv <- read.csv(fileName);
    pol <- csv[pollutant];
    pol <- pol[!is.na(pol)]
    
    #
    # Update the vector
    if(is.null(points)){
      points = pol;
    }else{
      points = c(points, pol);
    }
  }
  
  mean(points);
}