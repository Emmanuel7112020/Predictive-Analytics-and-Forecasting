#Heating3 <- read.delim("Heating3.dat")

database<-read.csv2("Heating3.csv",header=TRUE)

View(database)

summary(database)

table(database$choice,database$region)  # gets the contingency table

prop.table(table(database$choice,database$region))  # gives the market share right away; sum of entire table gives 1

#unique(database$region)
#database$region <- as.character(database$region)

#Question 1


library(apollo)	d
library(tidyverse)
apollo_initialise()

#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"

#Model 1


apollo_control=list(modelName="Exam Model",modelDescr="Some Description",indivID="ID")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(b_in   =0,
              b_oc   =0,
              asc_gr =0,
              asc_ec =0,
              asc_er =0,
              asc_hp =0)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['gc']]	=          + b_in* ic.gc  + b_oc*oc.gc 
  V[['gr']] 	= asc_gr + b_in* ic.gr  + b_oc*oc.gr 
  V[['ec']] 	= asc_ec + b_in* ic.ec  + b_oc*oc.ec 
  V[['er']] 	= asc_er + b_in* ic.er  + b_oc*oc.er 
  V[['hp']] 	= asc_hp + b_in* ic.hp  + b_oc*oc.hp   
  
  
  mnl_settings = list(						      							### Define settings for model 
    alternatives = c(gc=1,gr=2,ec=3,er=4,hp=5),									### component	
    avail        =  1,
    choiceVar    = choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  # for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


ExamModel= apollo_estimate(apollo_beta,
                       apollo_fixed,
                       apollo_probabilities,
                       apollo_inputs)

apollo_modelOutput(ExamModel)




#Question c
#Predict the market share given 30% drop in operation cost of heat pump

database_old <- database

database$oc.hp <- database$oc.hp*0.7

apollo_inputs = apollo_validateInputs()

View(database)
predictions = apollo_prediction(ExamModel, apollo_probabilities, apollo_inputs)
predictions<-data.frame(predictions)

View(predictions)

predictions<-predictions[3:7] # to extract the gc,gr,ec,er & hp columns

market_shares <- colMeans(predictions)
market_shares
sum(market_shares)  # confirms that all probabilities sum up to 1


#################################
# Old database without price reduction of any alternative to confirm that price reduction above makes a difference in market shares
View(database_old)
apollo_inputs = apollo_validateInputs()

predictions_original = apollo_prediction(ExamModel, apollo_probabilities, apollo_inputs)
predictions_original<-data.frame(predictions_original)

View(predictions_original)
predictions_original<-predictions_original[3:7] # to extract the gc,gr,ec,er & hp columns

market_shares_original <- colMeans(predictions_original)
market_shares_original
sum(market_shares_original)  # confirms that all probabilities sum up to 1

