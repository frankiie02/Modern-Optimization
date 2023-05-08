
getData <- function(){
  data <- read.csv('data.csv')
  
  data$status <- as.factor(data$status)
  data <- data[ -c(1, 2)]
  #data(data) ##################################### hint
  dataDT <<- data  ##################################### hint
  
  return (dataDT)
  
}

classfeatureFitness <- function(string,xx,yy) {
 
  inc <- which(string == 1)  #'inc' includes those features/variables for which 'string' contains 1
 
  if(sum(inc)==0)                          
    return(0)                          
  
   
  outcome <-"status"  ##################################### hint
  inputs <- paste(names(xx)[inc], collapse =" + ")
  
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- rpart(formula = fRpart, method="class", control = rpart.control(minsplit = 3),
               data = dataDT)
  
  t_pred = predict(DT,dataDT, type='class')
  
  #Maximise accuracy
  #return( mean(dataDT$status == t_pred)) ##################################### hint
  
  #Maximise accuracy and minimise the number of features
  return( mean(dataDT$status == t_pred) * (1 - sum(string == 1)/length(string) ) )
}

