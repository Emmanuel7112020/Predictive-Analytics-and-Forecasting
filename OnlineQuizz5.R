library(mlogit) 
library (tidyr) 
data("Mode", package = "mlogit") 
Mode$ID <- 1:length(Mode$choice) 
database <-  Mode

View(database)
str(database)
class(database$choice)
database$choice <- as.character(database$choice)

#How to implement a model in apollo and minimize the Log-likelihood Function 

library(apollo) # run apollo package 


apollo_control=list(modelName="Multi logit",
                    modelDescr="Some Description",indivID="ID") 

#Define name and starting values for the coefficients to be estimated 
apollo_beta=c(asc_rail              = 0,
              asc_car               =0,
              asc_bus               =0,
              b_cost                = 0,
              b_time                 =0) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary  
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### function exit 
  P = list() ### Create list of probabilities P 
  
  V = list() ### List of utilities 
  V[['carpool']] = b_cost * cost.carpool+ b_time * time.carpool
  V[['car']] = asc_car +  b_cost * cost.car +  b_time * time.car
  V[['bus']] = asc_bus +  b_cost * cost.bus +  b_time * time.bus
  V[['rail']] = asc_rail +  b_cost * cost.rail +  b_time * time.rail
  
  
  mnl_settings = list( ### Define settings for model 
    alternatives = c(car='car',carpool='carpool',bus='bus',rail='rail'), ### component 
    avail        = 1, 
    choiceVar    = choice, 
    V            = V) 
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality) ### Compute probabilities using model 
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)  ### Take product across observation 
  ### for same ID 
  
  P = apollo_prepareProb(P, apollo_inputs, functionality) ### Prepare and return outputs of function 
  return(P) 
} 

Multilogit = apollo_estimate(apollo_beta, 
                           apollo_fixed, 
                           apollo_probabilities, 
                           apollo_inputs) 
apollo_modelOutput(Multilogit) 


probs <- apollo_prediction(Multilogit,apollo_probabilities,apollo_inputs)
probs <- data.frame(probs)
head(probs)
probs <- probs[,1:4]
b_cost <-Multilogit$estimate["b_cost"]
cost <- database[2:5]
elast.cost <- b_cost*cost*(1-probs)
elast.cost
min(elast.cost[,4])
max(elast.cost[,4])


database$cost.rail <- database$cost.rail*0.95
apollo_inputs= apollo_validateInputs()
probs_new= apollo_prediction(Multilogit,apollo_probabilities,apollo_inputs)
probs_new <- data.frame(probs_new)

mean(probs_new$rail)

