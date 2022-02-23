#################
#1
####################

database <- read.csv2(file="quebec.csv",header=TRUE)
View(database)

library(apollo)
apollo_initialise()


#set some controls 
#indicate the name (in quotes) of the column in the data which contains the identifer variable 
#for individual decision makers. For our data set that i s "ID" 

apollo_control=list(modelName="Baseline Model", 
                    modelDescr="Some Description",indivID="obs") 

# run apollo package 

#Define name and starting values for the coefficients t o be estimated 


apollo_beta=c(asc_2 =0,
              asc_3 =0, 
              asc_4=0,
              asc_5=0, 
              asc_6=0, 
              asc_7=0, 
              asc_8=0, 
              asc_9 =0, 
              beta_oc = 0, 
              beta_fc = 0) 


#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary 
apollo_inputs = apollo_validateInputs() 

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs)              ### Attach inputs and detach after 
  on.exit(apollo_detach(apollo_beta, apollo_inputs))      ### function exit 
  
  P = list()                                            ### Create list of  probabilities P 
  V = list()                                              ### List of utilities 
  V [['gg']]  =        beta_oc * op_cost.1 + beta_fc * fix_cost.1
  V [['ge']]  = asc_2 +beta_oc * op_cost.2 + beta_fc * fix_cost.2
  V [['deo']] = asc_3 +beta_oc * op_cost.3 + beta_fc * fix_cost.3
  V [['dee']] = asc_4 +beta_oc * op_cost.4 + beta_fc * fix_cost.4
  V [['oo']]  = asc_5 +beta_oc * op_cost.5 + beta_fc * fix_cost.5
  V [['oe']]  = asc_6 +beta_oc * op_cost.6 + beta_fc * fix_cost.6 
  V [['ee']]  = asc_7 +beta_oc * op_cost.7 + beta_fc * fix_cost.7 
  V [['we']]  = asc_8 +beta_oc * op_cost.8 + beta_fc * fix_cost.8 
  V [['wee']] = asc_9 +beta_oc * op_cost.9 + beta_fc * fix_cost.9 
  
  mnl_settings   = list( 
    alternatives = c(gg=1, ge=2, deo=3, dee=4, oo=5, oe=6, ee=7, we=8, wee=9), 
    avail        = list(gg=avail.1, ge=avail.2, deo=avail.3, dee=avail.4, oo=avail.5,  oe=avail.6, ee=avail.7, we=avail.8, wee=avail.9),
    choiceVar    = choice, 
    V            =V) 
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality) ### Compute probabilities using model 
  #P = apollo_panelProd(P, apollo_inputs, functionality) ### Take product across observation 
  ### for same ID 
  P = apollo_prepareProb(P, apollo_inputs, functionality) ### Prepare and return outputs of function 
  
  return(P) 
} 


BaseModel = apollo_estimate(apollo_beta, 
                            apollo_fixed, 
                            apollo_probabilities, 
                            apollo_inputs) 
apollo_modelOutput(BaseModel) 


#############
#2. compute predicted choice probabilities 
###########

#"by hand" 
estimates<-BaseModel$estimate 
asc<-c(0,estimates[1:8]) 
beta_1<-estimates[9] 
beta_2<-estimates[10] 
op_cost<-database[1,14:22] 
fix_cost<-database[1,23:31] 
util<-asc+beta_1*op_cost+beta_2*fix_cost 
util
e_util<-exp(util) 
p<-e_util/sum(e_util) 
names(p) <- paste("Alt",1:9)
p
sum(p)

#"using R" 
predictions = apollo_prediction(BaseModel, apollo_probabilities, apollo_inputs) 
predictions<-data.frame(predictions) 
predictions[1,] 

head(predictions)
rowSums(predictions[,-c(1,2,12)])

###################
#Question 3 
###################

database <- read.csv2(file="quebec.csv",header=TRUE)

library(apollo)                        # run apollo package 
apollo_initialise() 

apollo_control=list(modelName="Model 2",modelDescr="Some Description",indivID="obs") 

#Define name and starting values 
apollo_beta=c(asc_2 =0,
	     asc_3 =0, 
	     asc_4 =0, 
	     asc_5 =0, 
	     asc_6 =0,
	     asc_7 =0,
	     asc_8 =0, 
	     asc_9 =0, 
	     asc_i2 =0, 
	     asc_i3 =0,
	     asc_i4 =0,
	     asc_i5 =0, 
	     asc_i6 =0, 
	     asc_i7 =0,
	     asc_i8 =0,
	     asc_i9 =0, 
	     beta_oc=0, 
	     beta_fc =0, 
	     beta_fc_i=0 
	 ) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary 
apollo_inputs = apollo_validateInputs() 

apollo_probabilities =function(apollo_beta, apollo_inputs, functionality="estimate"){ 

apollo_attach(apollo_beta, apollo_inputs) 
on.exit(apollo_detach(apollo_beta, apollo_inputs)) 

P = list() 

V = list() 
V[['gg']]  =   beta_oc * op_cost.1 + beta_fc * fix_cost.1 + beta_fc_i*fix_cost.1*income 
V[['ge']]  =  asc_2 + asc_i2*income +beta_oc* op_cost.2 +beta_fc * fix_cost.2 +beta_fc_i*fix_cost.2*income
V[['deo']] = asc_3 + asc_i3*income +beta_oc* op_cost.3 +beta_fc * fix_cost.3 +beta_fc_i*fix_cost.3*income
V[['dee']] = asc_4 + asc_i4*income +beta_oc* op_cost.4 +beta_fc * fix_cost.4 +beta_fc_i*fix_cost.4*income
V[['oo']]  = asc_5 + asc_i5*income +beta_oc* op_cost.5 +beta_fc * fix_cost.5 +beta_fc_i*fix_cost.5*income 
V[['oe']]  = asc_6 + asc_i6*income +beta_oc* op_cost.6 +beta_fc * fix_cost.6 +beta_fc_i*fix_cost.6*income
V[['ee']]  = asc_7 + asc_i7*income +beta_oc* op_cost.7 +beta_fc * fix_cost.7 +beta_fc_i*fix_cost.7*income
V[['we']]  = asc_8 + asc_i8*income +beta_oc* op_cost.8 +beta_fc * fix_cost.8 +beta_fc_i*fix_cost.8*income
V[['wee']] = asc_9 + asc_i9*income +beta_oc * op_cost.9 + beta_fc * fix_cost.9 + beta_fc_i*fix_cost.9*income

mnl_settings   = list( 
  alternatives = c(gg=1, ge=2, deo=3, dee=4, oo=5, oe=6, ee=7, we=8, wee=9), 
  avail        = list(gg=avail.1, ge=avail.2, deo=avail.3, dee=avail.4, oo=avail.5,  oe=avail.6, ee=avail.7, we=avail.8, wee=avail.9),
  choiceVar    = choice, 
  V            =V) 

P[["model"]] = apollo_mnl(mnl_settings, functionality) ### Compute probabilities using model 
#P = apollo_panelProd(P, apollo_inputs, functionality) ### Take product across observation 
### for same ID 
P = apollo_prepareProb(P, apollo_inputs, functionality) ### Prepare and return outputs of function 

return(P) 
} 

Model2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs) 
			
apollo_modelOutput(Model2) 

# To compare which model between basemodel and model 2 is better in terms of higher log likelihood: Model2 is better  
BaseModel$maximum
Model2$maximum

############
# 3 Alternative way of doing interaction effect
###################

database <- read.csv2(file="quebec.csv",header=TRUE)

library(apollo)                        # run apollo package 
apollo_initialise() 

apollo_control=list(modelName="Model 3",modelDescr="Some Description",indivID="obs") 

#Define name and starting values 
apollo_beta=c(asc_2 =0,
              asc_3 =0, 
              asc_4 =0, 
              asc_5 =0, 
              asc_6 =0,
              asc_7 =0,
              asc_8 =0, 
              asc_9 =0, 
              asc_i2 =0, 
              asc_i3 =0,
              asc_i4 =0,
              asc_i5 =0, 
              asc_i6 =0, 
              asc_i7 =0,
              asc_i8 =0,
              asc_i9 =0, 
              beta_oc=0, 
              beta_fc =0, 
              beta_fc_i=0 
) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary 
apollo_inputs = apollo_validateInputs() 

apollo_probabilities =function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) 
  
  P = list() 
  
  V = list() 
  V[['gg']]  =   beta_oc * op_cost.1 + beta_fc * fix_cost.1 + beta_fc_i*fix_cost.1/income 
  V[['ge']]  =  asc_2 + asc_i2*income +beta_oc* op_cost.2 +beta_fc * fix_cost.2 +beta_fc_i*fix_cost.2/income
  V[['deo']] = asc_3 + asc_i3*income +beta_oc* op_cost.3 +beta_fc * fix_cost.3 +beta_fc_i*fix_cost.3/income
  V[['dee']] = asc_4 + asc_i4*income +beta_oc* op_cost.4 +beta_fc * fix_cost.4 +beta_fc_i*fix_cost.4/income
  V[['oo']]  = asc_5 + asc_i5*income +beta_oc* op_cost.5 +beta_fc * fix_cost.5 +beta_fc_i*fix_cost.5/income 
  V[['oe']]  = asc_6 + asc_i6*income +beta_oc* op_cost.6 +beta_fc * fix_cost.6 +beta_fc_i*fix_cost.6/income
  V[['ee']]  = asc_7 + asc_i7*income +beta_oc* op_cost.7 +beta_fc * fix_cost.7 +beta_fc_i*fix_cost.7/income
  V[['we']]  = asc_8 + asc_i8*income +beta_oc* op_cost.8 +beta_fc * fix_cost.8 +beta_fc_i*fix_cost.8/income
  V[['wee']] = asc_9 + asc_i9*income +beta_oc * op_cost.9 + beta_fc * fix_cost.9 + beta_fc_i*fix_cost.9/income
  
  mnl_settings   = list( 
    alternatives = c(gg=1, ge=2, deo=3, dee=4, oo=5, oe=6, ee=7, we=8, wee=9), 
    avail        = list(gg=avail.1, ge=avail.2, deo=avail.3, dee=avail.4, oo=avail.5,  oe=avail.6, ee=avail.7, we=avail.8, wee=avail.9),
    choiceVar    = choice, 
    V            =V) 
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality) ### Compute probabilities using model 
  #P = apollo_panelProd(P, apollo_inputs, functionality) ### Take product across observation 
  ### for same ID 
  P = apollo_prepareProb(P, apollo_inputs, functionality) ### Prepare and return outputs of function 
  
  return(P) 
} 

Model3 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs) 

apollo_modelOutput(Model3) 

# To compare which model between models 2 and 3 is better in terms of higher log likelihood
Model2$maximum
Model3$maximum

#beta_fc_i     -1.3964 higher cost and lower income reduces utility

#######################
#direct elasticities 
#########################

probs = apollo_prediction(Model2, apollo_probabilities, apollo_inputs) 

probs<-data.frame(probs) 

head(probs)

probs<-probs[,3:11] 

Beta_fc<-Model2$estimate["beta_fc"]+Model2$estimate["beta_fc_i"]*database$income 
Beta_oc<-Model2$estimate["beta_oc"] 

FC<-database[,23:31] 
OC<-database[,14:22] 

elast.ic<-Beta_fc * FC * (1-probs) 
elast.oc<-Beta_oc * OC * (1-probs) 

head(elast.ic)
head(elast.oc)

hist(elast.ic[,7],breaks=54,xlab="Price elasticity of installation cost of electric-electric alternative") 

plot(FC[,7],elast.ic[,7],pch=20,xlab="Installation cost of electric-electric alternative", ylab="Price elasticity of installation cost of electric-electric alternative") 

install.packages("ggplot2")  # get ggplot package 
library(ggplot2) 

temp<-data.frame(elast.ic$fix_cost.7,FC$fix_cost.7,as.factor(database$income)) 

names(temp)<-c("elast","fc","income") 

ggplot(temp,aes(x=fc,y=elast,color=income))+ 
  geom_point(size=1.0)+ 
  scale_color_brewer(palette="Blues")+ 
  theme(text=element_text(size=14))+ 
  xlab("Installation cost of electric-electric alternative")+ 
  ylab("Price elasticity of installation cost of electric-electric alternative") 


#######################
#cross elasticities 
#########################
crosselast.ic=-Beta_fc*FC$fix_cost.5*probs$oo
crosselast.oc=-Beta_oc*OC$op_cost.5*probs$oo
head(crosselast.ic)

#######################
#IIA
#########################
database <- read.csv2(file="quebec.csv",header=TRUE)

library(apollo)                        # run apollo package 
apollo_initialise() 

apollo_control=list(modelName="Model 2",modelDescr="Some Description",indivID="obs") 

#Define name and starting values 
apollo_beta=c(asc_2 =0,
              asc_3 =0, 
              asc_4 =0, 
              asc_5 =0, 
              asc_6 =0,
              asc_7 =0,
              asc_8 =0, 
              #asc_9 =0, 
              asc_i2 =0, 
              asc_i3 =0,
              asc_i4 =0,
              asc_i5 =0, 
              asc_i6 =0, 
              asc_i7 =0,
              asc_i8 =0,
             # asc_i9 =0, 
              beta_oc=0, 
              beta_fc =0, 
              beta_fc_i=0 
) 

#all coefficients may be altered, none is fixed 
apollo_fixed=NULL 

#check if you have defined everything necessary 
apollo_inputs = apollo_validateInputs() 

apollo_probabilities =function(apollo_beta, apollo_inputs, functionality="estimate"){ 
  
  apollo_attach(apollo_beta, apollo_inputs) 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) 
  
  P = list() 
  
  V = list() 
  V[['gg']]  =   beta_oc * op_cost.1 + beta_fc * fix_cost.1 + beta_fc_i*fix_cost.1*income 
  V[['ge']]  =  asc_2 + asc_i2*income +beta_oc* op_cost.2 +beta_fc * fix_cost.2 +beta_fc_i*fix_cost.2*income
  V[['deo']] = asc_3 + asc_i3*income +beta_oc* op_cost.3 +beta_fc * fix_cost.3 +beta_fc_i*fix_cost.3*income
  V[['dee']] = asc_4 + asc_i4*income +beta_oc* op_cost.4 +beta_fc * fix_cost.4 +beta_fc_i*fix_cost.4*income
  V[['oo']]  = asc_5 + asc_i5*income +beta_oc* op_cost.5 +beta_fc * fix_cost.5 +beta_fc_i*fix_cost.5*income 
  V[['oe']]  = asc_6 + asc_i6*income +beta_oc* op_cost.6 +beta_fc * fix_cost.6 +beta_fc_i*fix_cost.6*income
  V[['ee']]  = asc_7 + asc_i7*income +beta_oc* op_cost.7 +beta_fc * fix_cost.7 +beta_fc_i*fix_cost.7*income
  V[['we']]  = asc_8 + asc_i8*income +beta_oc* op_cost.8 +beta_fc * fix_cost.8 +beta_fc_i*fix_cost.8*income
 # V[['wee']] = asc_9 + asc_i9*income +beta_oc * op_cost.9 + beta_fc * fix_cost.9 + beta_fc_i*fix_cost.9*income
  
  mnl_settings   = list( 
    alternatives = c(gg=1, ge=2, deo=3, dee=4, oo=5, oe=6, ee=7, we=8), 
    avail        = list(gg=avail.1, ge=avail.2, deo=avail.3, dee=avail.4, oo=avail.5,  oe=avail.6, ee=avail.7, we=avail.8),
    choiceVar    = choice, 
    V            =V) 
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality) ### Compute probabilities using model 
  #P = apollo_panelProd(P, apollo_inputs, functionality) ### Take product across observation 
  ### for same ID 
  P = apollo_prepareProb(P, apollo_inputs, functionality) ### Prepare and return outputs of function 
  
  return(P) 
} 

Model2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs) 

apollo_modelOutput(Model2) 

Beat_o <- Model2$estimate
Beat_s <- Model2_sub$estimate

V_o <- Models2$varcov
V_s <- V_o[c(1:7,9:15,17:19),c(1:7,9:15,17:19)]
