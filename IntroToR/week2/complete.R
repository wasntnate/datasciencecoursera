complete <- function(directory, id = 1:332) {
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
    total <- sum(c(0,complete.cases(csv)))
    
    #
    # Update the vector
    if(is.null(points)){
      points = total;
    }else{
      points = c(points, total);
    }
  }
  
  #
  # return
  nobs = points;
  data.frame(cbind(id,nobs), row.names=id)
}