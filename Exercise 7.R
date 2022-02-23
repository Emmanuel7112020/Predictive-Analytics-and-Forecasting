#telephone <- read.delim("telephone.dat")

database<-read.csv2("Telephone.csv",header=TRUE)


#Question 1


library(apollo)	

apollo_initialise()

#set some controls

#indicate the name (in quotes) of the column in the data which contains the identifer variable
#for individual decision makers. For our data set that is "ID"

#Model 1


apollo_control=list(modelName="MC2",modelDescr="Some Description",indivID="ID")


#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_BM             = 0,
              asc_LF             = 0,
		  asc_EF		   = 0,
		  asc_MF		   = 0,
		  beta_cost		   = 0		
)


#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary  

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		

  P = list()								 ### Create list of probabilities P
  
  V = list()								 ### List of utilities
  V[['SM']]		= 		+ beta_cost * (cost2)
  V[['BM']] 	= asc_BM    + beta_cost * (cost1)
  V[['LF']] 	= asc_LF    + beta_cost * (cost3) 
  V[['EF']] 	= asc_EF    + beta_cost * (cost4)
  V[['MF']] 	= asc_MF    + beta_cost * (cost5)


  
   mnl_settings = list(						      							### Define settings for model 
   alternatives = c(SM=2, BM=1, LF=3, EF=4,MF=5),									### component	
   avail        =  list(SM=avail2,BM=avail1,LF=avail3,EF=avail4,MF=avail5),
   choiceVar    = as.character(choice),
   V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 ### Compute probabilities using model
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)	 ### Take product across observation
										 ### for same ID
 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 ### Prepare and return outputs of function

  return(P)
}


Tel1 = apollo_estimate(apollo_beta,
				apollo_fixed,
				apollo_probabilities,
				apollo_inputs)



apollo_modelOutput(Tel1)




#Question 2
#determine the actual market shares


actual_shares<-numeric(5)

for(i in 1:5){	

	actual_shares[i]<-length(subset(database,choice==i)$choice)

}

actual_shares<-actual_shares/length(database$choice)




#predict the market shares with your model considering each individual observation


predictions = apollo_prediction(Tel1, apollo_probabilities, apollo_inputs)

predictions=data.frame(predictions)

predictions<-predictions[3:7]

colMeans(predictions)





























#Question 2
#predict the market shares based on an average individual


mean1<-mean(subset(database,avail1==1)$cost1)
mean2<-mean(subset(database,avail2==1)$cost2)
mean3<-mean(subset(database,avail3==1)$cost3)
mean4<-mean(subset(database,avail4==1)$cost4)
mean5<-mean(subset(database,avail5==1)$cost5)

database_bu<-database

database$cost1<-mean1
database$cost2<-mean2
database$cost3<-mean3
database$cost4<-mean4
database$cost5<-mean5


apollo_inputs = apollo_validateInputs()

predictions2 = apollo_prediction(Tel1, apollo_probabilities, apollo_inputs)

predictions2<-data.frame(predictions2)

predictions2<-predictions2[,3:7]

colMeans(predictions2)


#Question 4 
#Impact of price changes

database<-database_bu

factors<-seq(0.5,1.5,0.05)
pred_share<-matrix(0,21,6)
pred_share[,6]<-factors

cost1_base<-database$cost1

for(i in 1:21){

	database$cost1<-cost1_base*factors[i]

	apollo_inputs = apollo_validateInputs()

	predictions_new = apollo_prediction(Tel1, apollo_probabilities, apollo_inputs)

	pred_share[i,1:5]<-apply(predictions_new[,3:7],2,mean)	
	
}

pred_share<-pred_share[,c(2,1,3:6)]

pred_share<-data.frame(pred_share)

names(pred_share)<-c("BM","SM","LF","EF","MF","factor")



plot(pred_share$factor,pred_share$BM,type="l",ylim=c(0,0.65),
     xlab="discount factor", ylab="predicted market share")

lines(pred_share$factor,pred_share$SM,col="blue")
lines(pred_share$factor,pred_share$LF,col="green")
lines(pred_share$factor,pred_share$EF,col="red")
lines(pred_share$factor,pred_share$MF,col="purple")

legend("topleft", legend=c("BM", "SM","LF","EF","MF"),
       col=c("black", "blue","green","red","purple"),
       lty=c(1,1,1,1,1))


library(tidyr)
library(ggplot2)

pred_share2 <- gather(pred_share, alternative, marketshare, BM:MF, factor_key=TRUE)

ggplot(pred_share2, aes(factor, marketshare))+
 	   geom_bar(aes(fill=alternative), stat="identity", width = 0.025) +
  	   scale_fill_brewer(palette = "Dark2") +
  	   labs(y="predicted market share", x="discount factor BM ")+
 	   theme_bw()




#example revenue for discount factor of 1


database<-database_bu

apollo_inputs = apollo_validateInputs()

predictions = apollo_prediction(Tel1, apollo_probabilities, apollo_inputs)

predictions<-data.frame(predictions)

market_shares<-predictions[3:7]

revenue<-numeric(6)

revenue[1]<-market_shares$BM %*% database$cost1
revenue[2]<-market_shares$SM %*% database$cost2
revenue[3]<-market_shares$LF %*% database$cost3
revenue[4]<-market_shares$EF %*% database$cost4
revenue[5]<-market_shares$MF %*% database$cost5
  
revenue[6]<-sum(revenue[1:5])  
  
  




#changing revenues

revenue<-matrix(0,21*6,3)
revenue<-data.frame(revenue)

key<-seq(0.5,1.5,0.05)

for(i in 1:21){
  
  key2<-(i-1)*6+1
  
  revenue[key2:(key2+5),1]<-rep(key[i],6)
  
  revenue[key2,2]<-"BM"
  revenue[key2+1,2]<-"SM"
  revenue[key2+2,2]<-"LF"
  revenue[key2+3,2]<-"EF"
  revenue[key2+4,2]<-"MF"
  revenue[key2+5,2]<-"total"
  
  database$cost1<-cost1_base*factors[i]
  
  apollo_inputs = apollo_validateInputs()
  
  predictions_new = apollo_prediction(Tel1, apollo_probabilities, apollo_inputs)
  
  predictions_new<-data.frame(predictions_new)
  
  
  revenue[key2,3]<-predictions_new$BM%*%database$cost1
  revenue[key2+1,3]<-predictions_new$SM%*%database$cost2
  revenue[key2+2,3]<-predictions_new$LF%*%database$cost3
  revenue[key2+3,3]<-predictions_new$EF%*%database$cost4
  revenue[key2+4,3]<-predictions_new$MF%*%database$cost5
  revenue[key2+5,3]<-sum(revenue[key2:(key2+4),3])
  
}




names(revenue)<-c("factor","alt","spending")

ggplot(revenue, aes(x=factor, y=spending, group=1)) + 
  geom_line()+
  #geom_point() +
  labs(y="expected revenue", x="discount factor for alternative BM")+
  theme(axis.ticks.x = element_blank()) +
  # scale_x_discrete(
  #  breaks=c("0.25","0.5","0.75","1","1.25"),
  #  labels=c("0.25","0.50","0.75","1.00","1.25")
  # ) +
  facet_wrap( ~ alt, ncol=3)
































