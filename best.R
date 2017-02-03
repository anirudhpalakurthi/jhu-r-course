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
  selectdata<-data[data$State==state,c(2,column)]
  index<-which.min(selectdata[,2])
  hospitalname<-selectdata[,1][index]
  return(hospitalname)
}
best("TX","heart attack")
source(best)