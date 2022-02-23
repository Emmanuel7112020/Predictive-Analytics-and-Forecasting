#1.  Function for Contingency Table for Reference Group: Every input must be defined within the function and not outside

cont_table<-function(low,high){
# Income Distribution Per Car Type   
low<-c(55,15)
high<-c(20,35)
car_income<-cbind(low,high)
rownames(car_income)<-c("car A","car B")
print(car_income)
}
cont_table(low,high)


#2.  Function for Predicted Market Share for Second Group
market_share<- function(low,high) {
temp<-cont_table(low,high)
#Conditional Probabilities Calculations
cp_car_a_low<-(temp[1,1]/sum(temp[1:2,1:2]))/(sum(temp[1:2,1])/sum(temp[1:2,1:2])) 
cp_car_a_high<-(temp[1,2]/sum(temp[1:2,1:2]))/(sum(temp[1:2,2])/sum(temp[1:2,1:2])) 
cp_car_b_low<-(temp[2,1]/sum(temp[1:2,1:2]))/(sum(temp[1:2,1])/sum(temp[1:2,1:2])) 
cp_car_b_high<-(temp[2,2]/sum(temp[1:2,1:2]))/(sum(temp[1:2,2])/sum(temp[1:2,1:2])) 
#Income Group Distribution and Income Group Proportions
low<-as.vector(80)
high<-as.vector(30)
low_prop<-low/sum(low,high)
high_prop<-high/sum(low,high)
#Market Share Calculations
mshare_a<-100*(cp_car_a_low*low_prop + cp_car_a_high*high_prop)
mshare_b<-100*(cp_car_b_low*low_prop + cp_car_b_high*high_prop)
mshare<-c(mshare_a,mshare_b)
names(mshare)<-c("Car A Market Share","Car B Market Share")
print(mshare)
}
market_share(low,high)




