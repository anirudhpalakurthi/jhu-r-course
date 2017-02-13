#Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
#state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
#The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
#of the hospital that has the ranking specified by the num argument

rankhospital<- function(state,outcome,num){
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #Checking validity of state
  if(!state %in% data$State){
    stop("Invalid State")
  }
  #Validity check of the outcome category
  switch(outcome, "heart attack"={
    col=11
  }, "heart failure"={
    col=17
  }, "pneumonia"={col=23},
  stop("Invalid Outcome"))
  
  #Excluding missing values in the state
  statedata<-data[(data$State==state),]
  statedata[,11]<-as.numeric(statedata[,11])
  statedata<-statedata[complete.cases(statedata),]
  
  #Validity check: Number
  rows<-nrow(statedata)
  if(is.numeric(num)&&num>rows){
    stop("NA") # Error if the selected number is more than number of rows in the state
  }
  
  #Assigning Best and worst numbers to the outcome
  else if(!is.numeric(num)){
    switch(num,best={num=1},worst={num=rows},
           stop("Invalid Number Input"))
  }
  
  #Order the records by the outcome first, then by the hospital name
  sorteddata<-statedata[order(as.numeric(statedata[,col]),statedata[,2]),]
  hospitalname<-sorteddata[num,2]
  return(hospitalname)
}

#UNITTESTS
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
