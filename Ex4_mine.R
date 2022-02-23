#QUESTION 1

database=read.csv(file="ChoiceLab.csv",header=TRUE)
database<-database[order(database$ID),]

View(database)

library(apollo)								# run apollo package
apollo_initialise()

#Question 1
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID")
apollo_beta=c(asc_stay     = 0,
              b_age      = 0,
              b_nbempl		 = 0,
              b_negprofit	= 0)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['leave']]  =  0
  V[['stay']]   =  asc_stay + b_age*Age+b_nbempl*NbEmpl+b_negprofit*NegProfit
  
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


BaseSpec = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(BaseSpec)


#=====================================================================================================
#QUESTION 2

database=read.csv(file="ChoiceLab.csv",header=TRUE)
database<-database[order(database$ID),]


library(apollo)								# run apollo package
apollo_initialise()

#Question 1
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID")
apollo_beta=c(asc_stay     = 0,
              b_age      = 0,
              b_nbempl		 = 0,
              b_negprofit	= 0)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['leave']]  =  0
  V[['stay']]   =  asc_stay + b_age*log(Age)+b_nbempl*log(NbEmpl)+b_negprofit*NegProfit
  
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


BaseSpec = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(BaseSpec)


#=====================================================================================================
#QUESTION 3

database=read.csv(file="ChoiceLab.csv",header=TRUE)
database<-database[order(database$ID),]


library(apollo)								# run apollo package
apollo_initialise()


apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID")
apollo_beta=c(asc_stay    = 0,
              beta_age1      = 0,
              beta_age2      =0,
              beta_age3      =0,
              beta_nbempl		= 0,
              beta_negprofit	=0)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  Age1<-pmin(database$Age,50)
  Age2<-pmax(0,pmin(database$Age-50,50))	
  Age3<-pmax(0,database$Age-100)
  
  V = list()								 ### List of utilities
  V[['leave']]  =  0
  V[['stay']] =   asc_stay + beta_age1*Age1 + beta_age2*Age2+ beta_age3*Age3 + beta_nbempl*log(NbEmpl)+beta_negprofit*log(NegProfit)
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(leave=1, stay=0),				 ### component	
    avail        =  1,
    choiceVar    = Choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


BaseSpec = apollo_estimate(apollo_beta,
                           apollo_fixed,
                           apollo_probabilities,
                           apollo_inputs)

apollo_modelOutput(BaseSpec)


