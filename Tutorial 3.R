#How to implement a model in apollo and minimize the Log-likelihood Function

install.packages("apollo")						# get apollo package
library(apollo)						        		# run apollo package



apollo_initialise()

database=read.csv(file="ModeChoiceData.csv",header=TRUE,row.names=1)   #get mode choice data


#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"

apollo_control=list(modelName="BaseSpec",
                    modelDescr="Some Description",indivID="ID")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_rail              = 0,
              b_cost                = 0)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['car']]  = 		    b_cost * cost_car
  V[['rail']] = asc_rail +  b_cost * cost_rail  
  
  mnl_settings = list(						       ### Define settings for model 
    alternatives = c(car=1, rail=2),					 ### component	
    avail        = list(car=av_car, rail=av_rail),
    choiceVar    = choice,
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







#Specification 1


apollo_control=list(modelName="Spec1",modelDescr="",indivID="ID")


apollo_beta=c(asc_rail = 0,
              b_cost   = 0,
              b_tt     = 0)


apollo_fixed=NULL

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['car']]  = 		  + b_cost * cost_car + b_tt*time_car
  V[['rail']] = asc_rail +  b_cost * cost_rail+ b_tt*(time_rail+access_rail)  	
  
  
  mnl_settings = list(						       ### Define settings for MNL model 
    alternatives = c(car=1, rail=2),				 ### component	
    avail        = list(car=av_car, rail=av_rail),
    choiceVar    = choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using MNL model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Spec1 = apollo_estimate(apollo_beta,
                        apollo_fixed,
                        apollo_probabilities,
                        apollo_inputs)

apollo_modelOutput(Spec1)



#Specification 2


apollo_control=list(modelName="Spec2",modelDescr=" ",indivID="ID")


apollo_beta=c(asc_car = 0,
              b_cost   = 0,
              b_tt     = 0)


apollo_fixed=NULL

apollo_inputs = apollo_validateInputs()



apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['car']]  = asc_car   + b_cost * cost_car + b_tt*time_car
  V[['rail']] =             b_cost * cost_rail+ b_tt*(time_rail+access_rail)  	
  
  
  mnl_settings = list(						       ### Define settings for MNL model 
    alternatives = c(car=1, rail=2),				 ### component	
    avail        = list(car=av_car, rail=av_rail),
    choiceVar    = choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using MNL model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Spec2 = apollo_estimate(apollo_beta,
                        apollo_fixed,
                        apollo_probabilities,
                        apollo_inputs)

apollo_modelOutput(Spec2)







#Specification 3

apollo_control=list(modelName="Spec3",modelDescr=" ", indivID="ID")


apollo_beta=c(asc_rail              = 0,
              b_cost                = 0,
              b_tt1		             	= 0,
              b_tt2                 = 0)



apollo_fixed=NULL

apollo_inputs = apollo_validateInputs()



apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['car']]  = 		   b_cost * cost_car + b_tt1*time_car
  V[['rail']] = asc_rail + b_cost * cost_rail+ b_tt2*(time_rail+access_rail)  
  
  
  mnl_settings = list(						       ### Define settings for MNL model 
    alternatives = c(car=1, rail=2),				 ### component	
    avail        = list(car=av_car, rail=av_rail),
    choiceVar    = choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using MNL model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Spec3 = apollo_estimate(apollo_beta,
                        apollo_fixed,
                        apollo_probabilities,
                        apollo_inputs)

apollo_modelOutput(Spec3)









#Specification 4

apollo_control=list(modelName="Spec4",modelDescr=" ",indivID="ID")


apollo_beta=c(asc_rail              = 0,
              b_cost                = 0,
              b_tt1			            = 0,
              b_tt2                 = 0,
              asc_female	         	= 0,
              asc_business      		= 0)



#apollo_fixed=c("asc_business")
apollo_fixed=NULL

apollo_inputs = apollo_validateInputs()



apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['car']]  = 		 				    				    + b_cost * cost_car + b_tt1*time_car
  V[['rail']] = asc_rail +asc_business*(business==1) +asc_female*(female==1)+ b_cost * cost_rail+ b_tt2*(time_rail+access_rail)  
  
  
  mnl_settings = list(						       ### Define settings for MNL model 
    alternatives = c(car=1, rail=2),				 ### component	
    avail        = list(car=av_car, rail=av_rail),
    choiceVar    = choice,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using MNL model
  
  P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
  ### for same ID
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function
  
  return(P)
}


Spec4 = apollo_estimate(apollo_beta,
                        apollo_fixed,
                        apollo_probabilities,
                        apollo_inputs)

apollo_modelOutput(Spec4)


