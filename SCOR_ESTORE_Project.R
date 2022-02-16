library(e1071)
library(ggplot2)
library(extraDistr)
set.seed(616)

# Scor-estore.com Business (1-7 months)
n <-1000
Page_Views_Growth <-0.6
Page_views_sigma <-0.55
Yealy_EBITDA_multiple <-15.1

rtri<-function(n,min,ml,max){
  cvt<-function(U){
    F<-(ml-min)/(max-min)
    if (U<F) {min+(U*(max-min)*(ml-min))^.5}
    else {max-((1-U)*(max-min)*(max-ml))^.5}
  }
  y<-runif(n)
  sapply(y,cvt)
}

Page_Views <-rtri(n,90,110,130)
Conversion_Rate <-rtri(n,0.08,0.14,0.2)
Margin_per_purcharse <- 0.88
Monthly_operating_cost <-rtri(n,4.54,5.27,6)
EBITDA <-Page_Views*Conversion_Rate*Margin_per_purcharse-Monthly_operating_cost
Valuation <-pmax(0,Yealy_EBITDA_multiple*12*EBITDA)

# Evaluation
Sell_Business_Revenue <- 500
Discount_Rate <-(Sell_Business_Revenue/1.2)^0.5
Excepted_DiscountRate_Value <-Discount_Rate*0.15
Invest_Cost <-90
Net_Profit <-Excepted_DiscountRate_Value-Invest_Cost


# August 
Page_Views2<-Page_Views+Page_Views*(Page_Views_Growth*(1/12)+Page_views_sigma*sqrt(1/12)*rnorm(n,0,1))
Conversion_Rate2 <-Conversion_Rate
Margin_per_purcharse2 <-Margin_per_purcharse+rnorm(n,0,0.08)
Monthly_operating_cost2<-Monthly_operating_cost
EBITDA2 <-Page_Views2*Conversion_Rate2*Margin_per_purcharse2-Monthly_operating_cost2
Valuation <-pmax(0,Yealy_EBITDA_multiple*12*EBITDA2)
                                                 
#Page_Views_simu
Page_Views_simu <- data.frame(July = Page_Views)
for (i in 2:12) {
  Page_Views_simu[i] <- Page_Views_simu[i-1]+ Page_Views_simu[i-1]*(Page_Views_Growth*(1/12)+Page_views_sigma*sqrt(1/12)*rnorm(n,0,1))
}
names(Page_Views_simu) <- c("M0-M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18")
#Conversion_Rate_simu
Conversion_Rate_simu <- data.frame(July = Conversion_Rate)
for (i in 2:12) {
  Conversion_Rate_simu[i] <- Conversion_Rate_simu[i-1]
}
names(Conversion_Rate_simu) <- c("M0-M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18")
#Margin_per_purcharse_simu
Margin_per_purcharse_simu <- data.frame(July = rep(Margin_per_purcharse,n))
for (i in 2:12) {
  Margin_per_purcharse_simu[i] <- Margin_per_purcharse_simu[i-1]+rnorm(n,0,0.08)
}                  
names(Margin_per_purcharse_simu) <- c("M0-M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18")
#Monthly_operating_cost_simu
Monthly_operating_cost_simu <- data.frame(July = Monthly_operating_cost) 
for (i in 2:12) {
  Monthly_operating_cost_simu[i] <- Monthly_operating_cost_simu[i-1]
}      
names(Monthly_operating_cost_simu) <- c("M0-M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18")
#Monthly_operating_cost_simu
EBITDA_simu <- data.frame(July = rep(1,n)) 
for (i in 1:12) {
  EBITDA_simu[,i] <- Page_Views_simu[,i]*Conversion_Rate_simu[,i]*Margin_per_purcharse_simu[,i]-Monthly_operating_cost_simu[,i]
}
names(EBITDA_simu) <- c("M0-M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18")
#Valuation_simu
Valuation_simu <- data.frame(July =rep(1,n)) 
for (i in 1:12) {
  Valuation_simu[,i] <- pmax(0,Yealy_EBITDA_multiple*12*EBITDA_simu[,i])
}
names(Valuation_simu) <- c("M0-M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18")


SimTable <- sapply(Page_Views_simu,mean) %>%
  rbind(sapply(Conversion_Rate_simu,mean)*100) %>%
  rbind(sapply(Margin_per_purcharse_simu,mean)) %>%
  rbind(sapply(Monthly_operating_cost_simu,mean)) %>%
  rbind(sapply(EBITDA_simu,mean)) %>%
  rbind(sapply(Valuation_simu,mean)) %>%
  as.data.frame()
names(SimTable) <- c("Initial¨CMo. 7","Month 8","Month 9","Month 10","Month 11",
                     "Month 12","Month 13","Month 14","Month 15","Month 16",
                     "Month 17","Month 18")
row.names(SimTable) <- c("Page Views","Conversion rate",
                         "Margin per purchase", "Monthly operating cost",
                         "EBITDA", "Valuation")

write.csv(SimTable,"D:/BU/AD 616/Project/Simulation Result.csv", row.names = FALSE)
#MEAN
#EBITDA <- data.frame(a = 1)
#for (i in 1:12) {
#  EBITDA[,i] <- mean(EBITDA_simu[,i])
#}


#-150,000 discounted 12 months +2*valuation/3 discounted 18 months
#Or
#valuation/3 discounted 18 months
option1 <- ((2*Valuation_simu[,12]/3)/1.2)^0.5 -(150/1.2)^0.5
option2 <- ((Valuation_simu[,12]/3)/1.2)^0.5


# simulation model
cutoff <- data.frame(cutoffp = rep(1,1))
for (i in 1:25) {
  value <- ifelse(EBITDA_simu$M12 > i, option1, option2)
  cutoff[i,] <-mean(value)
}

which.max(cutoff$cutoffp)

# best solution when EBITDA is large than 11
value <- ifelse(EBITDA_simu$M12 >11, option1, option2)
mean(value)

plot(cutoff$cutoffp)


# probability that EBTIDA is more than 18
sum(EBITDA_simu$M12>18)/n

# if cut-off point is 18, the expected evaluation should be£»
cutoff_18 <-0.35*10+0.15*35+0.15*(760*0.185+410*0.815)+0.35*(-90)
cutoff_18

# probability that EBTIDA is more than 11
sum(EBITDA_simu$M12>11)/n

# if cut-off point is 11, the expected evaluation should be£»
cutoff_11<-0.35*10+0.15*35+0.15*(760*0.494+410*0.506)+0.35*(-90)
cutoff_11


# Final month evaluation
month18_valuation <-mean(Valuation_simu$M18)
month18_valuation


3328*2*(1/3+0.17)
3328*2/3-90

3328*2*(1/3+0.17)

3328/3-90

100/1.2^0.5 - 90
150/1.2^0.5 - 90 - 25
3350/1.2^1.5 - 250/1.2^1.5
(3350-250+100)/1.2
(3328*2/3)/(1.2^1.5)-90-150/1.2
3350/(1.2^1.5)
3350/1.2

#Decision tree looks good?
#For the final valuation, which value should we use? The final valuation in the simulation or The expected biz sales amounts in the case.
#For the decision tree, the cut off point, which one should we use? The one in the case or the best cutoff point we got from the simulation.
#For the control of a viable company part, case said that the control of a viable company worth 100,000. When we determine whether we buy 17% share to get the control right.
#Should we add that 100,000 to the payoff?
#How long should our presentation be?


