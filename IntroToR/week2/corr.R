corr <- function(directory, threshold=0){
  files <- list.files(directory, full.names=TRUE);
  
  values <- NULL;
  for(file in files){
    #
    # Read the CSV file
    csv <- read.csv(file);
    cases <- complete.cases(csv);
    total <- sum(c(0,cases));
    
    if(total < threshold){
      #values = c(values, 0);
      next;
    }
    
    #
    # Extract cases
    nitrate = csv$nitrate[cases];
    sulfate = csv$sulfate[cases];
    
    if(is.null(values)){
      values <- cor(nitrate,sulfate);
    }else{
      values <- c(values, cor(nitrate,sulfate));
    }
  }
  
  #return
  invisible(values);
}