best<- function(state, outcome){
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  if(!state %in% data$State){
    stop("Invalid state")
  }
  switch(outcome,"heart attack"={
        column=11
      },"heart failure"={
        column=17
      }, "pneumonia"={
        column=23
      },stop("Invalid Outcome"))
  #Select the related data from the whole dataset as per the inputs to the "Best" function
  selectdata<-data[data$State==state,c(2,column)]
  index<-which.min(selectdata[,2])
  # Retrieve Hospital Name for the minimum value
  hospitalname<-selectdata[,1][index]
  return(hospitalname)
}
#Unit Test Cases
best("TX","heart attack")
