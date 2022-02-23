# BASELINE

library(apollo) # run apollo package 
apollo_initialise() 
database=read.csv("D:/ORBA Winter Semester 2020-2021/Predictive Analytics and Forecasting/Winter 2021/ModeChoiceData.csv",header = TRUE,row.names=1)
View(database)
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
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] = b_cost * cost_car 
  V[['rail']] = asc_rail +  b_cost * cost_rail   
  
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

BaseSpec = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(BaseSpec) 

#################################
 # what to change
#-the coefficient and the specification

#############################################
  # SPECIFICATION 1
#############################################
  library(apollo) # run apollo package ; Apollo accepts data as Database only(not even database1 or 2)
apollo_initialise() 
database=read.csv("D:/ORBA Winter Semester 2020-2021/Predictive Analytics and Forecasting/Winter 2021/ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail              = 0, 
              b_cost                = 0,
              b_tt                   =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] = b_cost * cost_car +b_tt*time_car
  V[['rail']] = asc_rail +  b_cost * cost_rail +  b_tt* time_rail
  
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

BaseSpec = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(BaseSpec) 
##########################
#Results
################################
#LL(final)                        : -2680.955
#Estimates:
#  Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
#asc_rail   -0.236890    0.102336      -2.315    0.103570        -2.287
#b_cost     -0.050492    0.002325     -21.715    0.002428       -20.793
#b_tt       -0.007776  6.4296e-04     -12.094  6.3187e-04       -12.306

#Changing the reference ASC does not change the LL estimation
###############################################
  # SPECIFICATION 2
###############################################
  
  library(apollo) # run apollo package ; Apollo accepts data as Database only(not even database1 or 2)
apollo_initialise() 
database=read.csv("D:/ORBA Winter Semester 2020-2021/Predictive Analytics and Forecasting/Winter 2021/ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_car              = 0, 
              b_cost                = 0,
              b_tt                   =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] = asc_car + b_cost * cost_car +b_tt*time_car
  V[['rail']] = b_cost * cost_rail +  b_tt* time_rail
  
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

BaseSpec = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(BaseSpec) 

#Results
#LL(final)                        : -2680.955
#Estimates:
  #Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
#asc_car    0.236890    0.102336       2.315    0.103570         2.287
#b_cost    -0.050492    0.002325     -21.715    0.002428       -20.793
#b_tt      -0.007776  6.4296e-04     -12.094  6.3187e-04       -12.306

LL(final)                        : -2680.955
Estimates:
 # Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
#asc_rail   -0.236890    0.102336      -2.315    0.103570        -2.287
#b_cost     -0.050492    0.002325     -21.715    0.002428       -20.793
#b_tt       -0.007776  6.4296e-04     -12.094  6.3187e-04       -12.306


#Changing the reference ASC does not change the LL estimation
#################################################################
  # SPECIFICATION 3
################################################################# 
  
  library(apollo) # run apollo package ; Apollo accepts data as Database only(not even database1 or 2)
apollo_initialise() 
database=read.csv("D:/ORBA Winter Semester 2020-2021/Predictive Analytics and Forecasting/Winter 2021/ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail              = 0, 
              b_cost               = 0,
              b_tt2                =0,
              b_tt3                =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] =  b_cost * cost_car + b_tt3*time_car
  V[['rail']] = asc_rail+ b_cost * cost_rail +  b_tt2* time_rail
  
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

BaseSpec = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(BaseSpec) 

#Results
#LL(final)                        : -2679.77
#Estimates:
#  Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
#asc_rail   -0.784462    0.370265      -2.119    0.355807        -2.205
#b_cost     -0.049736    0.002368     -21.007    0.002469       -20.145
#b_tt2      -0.004931    0.001954      -2.524    0.001975        -2.496
#b_tt3      -0.008191  6.9812e-04     -11.732  6.6112e-04       -12.389

##################################################################
  # SPECIFICATION 4
 ################################################ 
  
  library(apollo) # run apollo package ; Apollo accepts data as Database only(not even database1 or 2)
apollo_initialise() 
database=read.csv("D:/ORBA Winter Semester 2020-2021/Predictive Analytics and Forecasting/Winter 2021/ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail              = 0, 
              b_cost               = 0,
              b_income              =0,
              b_tt4                =0,
              b_tt5                =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] =   b_cost * cost_car + b_tt5*time_car
  V[['rail']] = asc_rail+ b_income *income + b_cost * cost_rail +  b_tt4* time_rail
  
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

BaseSpec = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(BaseSpec) 

#Results
#LL(final)                        : -2662.782
#Estimates:
#Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
#asc_rail   -1.349152    0.384427      -3.510    0.384286        -3.511
#b_cost     -0.050590    0.002387     -21.192    0.002511       -20.143
#b_income   1.129e-05   1.946e-06       5.800   2.693e-06         4.191
#b_tt4      -0.004826    0.001960      -2.462    0.001987        -2.429
#b_tt5      -0.008396  7.0220e-04     -11.956  6.6271e-04       -12.669

#R starts with null model and then do some iterations to get an optimal solution
######################################################################
  # CHALLENGE 5.1
########################################################
  
  library(apollo) # run apollo package ; Apollo accepts data as Database only(not even database1 or 2)
apollo_initialise() 
database=read.csv("D:/ORBA Winter Semester 2020-2021/Predictive Analytics and Forecasting/Winter 2021/ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail              = 0, 
              b_cost               = 0,
              asc_female	   = 0,
              asc_business         = 0,
              b_tt2                =0,
              b_tt3                =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] =   b_cost * cost_car + b_tt3*time_car
  V[['rail']] = asc_rail+ asc_female *(female==1) + asc_business*(business==1)+ b_cost * cost_rail +  b_tt2* time_rail
  
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

BaseSpec = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(BaseSpec) 





#Interpretation
#B_0: The alternative-speci???c constant for an alternative captures the average effect on utility of all factors that are not included in the model.  the alternative specific constants are considered to represent the average effect 
#of all factors that influence the choice but are not included in the utility specification.  For example, factors such as  comfort, safety, privacy and reliability may be excluded due to the difficulty associated with 
#their measurement. In other words, average of the difference of errors between the two choices(rail and car). The mean of the net error
#if a commuter is on a business trip, then it is more likely that such commuter uses the rail. Business trip increases the utility of rail by  1.299928;
#If a commuter is a female, it is more likely rail will be chosen; being a female increases rail utility by 0.203026. Cost and travel time reduces
#the utility of both car and rail.



# CHALLENGE 5.2
library(apollo) # run apollo package ; Apollo accepts data as Database only(not even database1 or 2)
apollo_initialise() 
database=read.csv("D:/ORBA Winter Semester 2020-2021/Predictive Analytics and Forecasting/Winter 2021/ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail              = 0, 
              b_cost               = 0,
              asc_female	         	= 0,
              asc_business      		= 0,
              b_wifi                =0,
              b_tt2                =0,
              b_tt3                =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] =   b_cost * cost_car + b_tt3*time_car
  V[['rail']] = asc_rail+ asc_female *(female==1) + asc_business*(business==1)+ b_cost * cost_rail + b_tt2*(time_rail)+ b_wifi * (service_rail == 2) 
  
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

BaseSpec = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(BaseSpec) 




##################################################
#2.1
###############################################

library(apollo) # run apollo package ; Apollo accepts data as Database only(not even database1 or 2)
apollo_initialise() 
database=read.csv("D:/ORBA Winter Semester 2020-2021/Predictive Analytics and Forecasting/Winter 2021/ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail              = 0, 
              b_cost               = 0,
              b_gender	          = 0,
              b_business         = 0,
              b_tt2                =0,
              b_tt3                =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] =   b_cost * cost_car + b_tt3*time_car
  V[['rail']] = asc_rail+ b_gender *(female==1) + b_business*(business==1)+ b_cost * cost_rail +  b_tt2* time_rail
  
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

BaseSpec = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(BaseSpec) 



###########################
2.2
#############################
library(apollo) # run apollo package ; Apollo accepts data as Database only(not even database1 or 2)
apollo_initialise() 
database=read.csv("D:/ORBA Winter Semester 2020-2021/Predictive Analytics and Forecasting/Winter 2021/ModeChoiceData.csv",header = TRUE,row.names=1)

#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that is "ID" 
apollo_control=list(modelName="BaseSpec", 
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail              = 0, 
              b_cost               = 0,
              b_gender	         	= 0,
              b_business      		= 0,
              b_wifi                =0,
              b_tt2                =0,
              b_tt3                =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['car']] =   b_cost * cost_car + b_tt3*time_car
  V[['rail']] = asc_rail+ b_gender *(female==1) + b_business*(business==1)+ b_cost * cost_rail + b_tt2*(time_rail)+ b_wifi * (service_rail == 2) 
  
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

BaseSpec = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(BaseSpec) 


#LL(final)                        : -2496.124
#Estimates:
#  Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
#asc_rail     -1.695403    0.393509      -4.308    0.392829        -4.316
#b_cost       -0.057695    0.002569     -22.458    0.002652       -21.754
#b_gender      0.211077    0.068654       3.075    0.073994         2.853
#b_business    1.306626    0.080789      16.173    0.089605        14.582
#b_wifi        0.649152    0.080205       8.094    0.079643         8.151
#b_tt2        -0.007498    0.002084      -3.598    0.002074        -3.616
#b_tt3        -0.010567  7.6593e-04     -13.796  7.1961e-04       -14.684
