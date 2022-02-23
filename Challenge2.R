######### SPECIFICATION 5.1  #########################################################################

library(apollo) # run apollo package
apollo_initialise() 
database=read.csv(file="ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="Spec5", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail        = 0, 
              beta1_cost      = 0,
              beta4_gender	  = 0,
              beta5_business  = 0,
              beta2_tt        =0,
              beta3_tt        =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] =   beta1_cost * cost_car + beta3_tt*time_car
  V[['rail']] = asc_rail+ beta4_gender *(female==1) + beta5_business *(business==1)+ beta1_cost * cost_rail +  beta2_tt* time_rail
  
  mnl_settings = list( ### Define settings for model 
    alternatives = c(car=1, rail=2), ### component 
    avail        = list(car=av_car, rail=av_rail), 
    choiceVar    = choice, 
    V            = V) 
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality) ### Compute probabilities using model 
  
  P = apollo_panelProd(P, apollo_inputs, functionality)  ### Take product across observation 
  ### for same ID 
  
  P = apollo_prepareProb(P, apollo_inputs, functionality) ### Prepare and return outputs of function 
  return(P) 
} 

Spec5 = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(Spec5) 

########FOLLOW-UP QUESTION:  INTERPRETATION OF COEFFICIENTS ###############
###asc_rail=beta0: The average effect of the difference of errors of the two choices(rail and car) and/or of all factors that are not included in the model on 
#utility of rail is -1.491184. In other words, errors and all factors not included in the model reduce utility of rail by -1.491184.

###beta4_gender=beta4:If a commuter is a female, it is more likely rail(relative to car) will be chosen; being a female increases rail utility by 0.203026 relative to car. 

###beta5_business=beta5: if a commuter is on a business trip, then it is more likely the commuter will use the rail(relative to car). Business trip increases the utility of rail by 1.299928 relative to car;


######### SPECIFICATION 5.2 (MODEL WITH WIFI) ##################################################################################################

library(apollo) # run apollo package
apollo_initialise() 
database=read.csv(file="ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="Spec5", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail        = 0, 
              beta1_cost      = 0,
              beta4_gender	  = 0,
              beta5_business  = 0,
              beta2_tt        =0,
              beta3_tt        =0,
              beta_wifi       =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] =   beta1_cost * cost_car + beta3_tt*time_car
  V[['rail']] = asc_rail+ beta4_gender *(female==1) + beta5_business *(business==1)+ beta1_cost * cost_rail +  beta2_tt* time_rail + beta_wifi *(service_rail == 2)  
  
  mnl_settings = list( ### Define settings for model 
    alternatives = c(car=1, rail=2), ### component 
    avail        = list(car=av_car, rail=av_rail), 
    choiceVar    = choice, 
    V            = V) 
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality) ### Compute probabilities using model 
  
  P = apollo_panelProd(P, apollo_inputs, functionality)  ### Take product across observation 
  ### for same ID 
  
  P = apollo_prepareProb(P, apollo_inputs, functionality) ### Prepare and return outputs of function 
  return(P) 
} 

Spec5 = apollo_estimate(apollo_beta, 
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs) 
apollo_modelOutput(Spec5) 

##ESTIMATES OF MODEL WITH WIFI AVAILABILITY
# Just as observed using the previous model, the average effect of the error difference between car and rail reduces the utility of rail by -1.695403;being a female
# and on a business trip as well as having wifi service availability makes it more likely to use the rail. The marginal impacts of female gender,business trip and
#wifi on rail utility relative to car are 0.211077,  1.306626 and 0.649152 respectively.Cost and travel times using car and rail reduce utility of both car and rail.