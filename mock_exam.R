# use read.csv for comma separated and read.csv2 for semi-colon separated file

database<-read.csv2(file="Telephone.csv",header=TRUE)
View(database)

############
#A
##############
sum(database$choice==1)
sum(database$choice==2)
sum(database$choice==3)
sum(database$choice==4)
sum(database$choice==5)



############
#B
##############

library(apollo)	
library(tidyverse)
apollo_initialise()

apollo_control=list(modelName="Specification",
                    modelDescr="Some Description",indivID="ID")

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_BM   = 0,
              asc_LF   = 0,
              asc_EF		= 0,
              asc_MF		= 0,
             #beta_SM	= 0,
              #beta_BM	= 0,
              #beta_LF	= 0,
              #beta_EF	= 0,
              beta_cost	= 0)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['SM']]		= 		      + beta_cost * log(cost2)
  V[['BM']] 	= asc_BM    + beta_cost * log(cost1)
  V[['LF']] 	= asc_LF    + beta_cost * log(cost3) 
  V[['EF']] 	= asc_EF    + beta_cost * log(cost4)
  V[['MF']] 	= asc_MF    + beta_cost * log(cost5)
  
  
  mnl_settings = list(						      							### Define settings for model 
    alternatives = c(SM=2, BM=1, LF=3, EF=4,MF=5),									### component	
    avail        =  list(SM=avail2,BM=avail1,LF=avail3,EF=avail4,MF=avail5),
    choiceVar    = choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Specification = apollo_estimate(apollo_beta,
                             apollo_fixed,
                             apollo_probabilities,
                             apollo_inputs)

apollo_modelOutput(Specification)


############
#C
##############

library(apollo)	
library(tidyverse)
apollo_initialise()

apollo_control=list(modelName="Specifications",
                    modelDescr="Some Description",indivID="ID")

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_BM     = 0,
              asc_LF     = 0,
              asc_EF		 = 0,
              asc_MF		 = 0,
              beta_cost	 = 0,
              beta_user1 = 0,
              beta_user2 = 0,
              beta_user3 = 0,
              beta_user4 = 0,
              beta_user5 = 0)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['SM']]		= 		      + beta_cost * log(cost2) + beta_user2*users
  V[['BM']] 	= asc_BM    + beta_cost * log(cost1) + beta_user1*users
  V[['LF']] 	= asc_LF    + beta_cost * log(cost3) + beta_user3*users 
  V[['EF']] 	= asc_EF    + beta_cost * log(cost4) + beta_user4*users
  V[['MF']] 	= asc_MF    + beta_cost * log(cost5) + beta_user5*users
  
  
  mnl_settings = list(						      							### Define settings for model 
    alternatives = c(SM=2, BM=1, LF=3, EF=4,MF=5),									### component	
    avail        =  list(SM=avail2,BM=avail1,LF=avail3,EF=avail4,MF=avail5),
    choiceVar    = choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Specifications = apollo_estimate(apollo_beta,
                                apollo_fixed,
                                apollo_probabilities,
                                apollo_inputs)

apollo_modelOutput(Specifications)

