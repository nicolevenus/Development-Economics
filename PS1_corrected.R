# Libraries
library(foreign)
library(stargazer)
library(ggplot2)

rm(list = ls())

setwd('C:/Users/Nicole/Documents/CEMFI/Development Economics')

# TBE weights
# write a read me and a file describing the results


# WEALTH ############################################################################
# housing and durable good wealth
data1 <- read.dta('Uganda_2009/GSEC14.dta')
data1[data1$h14q2=="Land",5] <- NA # exclude land here to get the values from the 
# agricultural survey
wealth <- aggregate(data1$h14q5, by=list(Category=data1$HHID), FUN=sum, na.rm=TRUE)
colnames(wealth) <- c('HHID', 'h14q5')

# land wealth
data2 <- read.dta('Uganda_2009/AGSEC2A.dta')
data2 <- data2[, c(1, 9)]
data2 <- aggregate(data2$a2aq10, by=list(Category=data2$HHID), FUN=sum, na.rm=TRUE)
colnames(data2) <- c('HHID', 'a2aq10')
wealth <- merge(wealth, data2, by='HHID', all = TRUE)

# fishery equipment
data3 <- read.dta('Uganda_2009/AGSEC9C.dta')
data3 <- data.frame(data3$HHID, data3$a9q10c*data3$a9q10d)
colnames(data3) <- c("HHID", "fish_val")
data3 <- aggregate(data3$fish_val, by=list(Category=data3$HHID), FUN=sum, na.rm=TRUE)
colnames(data3) <- c("HHID", "fish_val")
wealth <- merge(wealth, data3, by='HHID', all = TRUE)

# cattle
data4 <- read.dta('Uganda_2009/AGSEC6A.dta')
data4 <- data.frame(data4$HHID, data4$a6aq5*data4$a6aq6)
colnames(data4) <- c("HHID", "cattle_val")
data4 <- aggregate(data4$cattle_val, by=list(Category=data4$HHID), FUN=sum, na.rm=TRUE)
colnames(data4) <- c("HHID", "cattle_val")
wealth <- merge(wealth, data4, by='HHID', all = TRUE)

# small animals
data5 <- read.dta('Uganda_2009/AGSEC6B.dta')
data5 <- data.frame(data5$HHID, data5$a6bq5*data5$a6bq6)
colnames(data5) <- c("HHID", "small_val")
data5 <- aggregate(data5$small_val, by=list(Category=data5$HHID), FUN=sum, na.rm=TRUE)
colnames(data5) <- c("HHID", "small_val")
wealth <- merge(wealth, data5, by='HHID', all = TRUE)

# poultry
data6 <- read.dta('Uganda_2009/AGSEC6C.dta')
data6 <- data.frame(data6$HHID, data6$a6cq5*data6$a6cq6)
colnames(data6) <- c("HHID", "poultry_val")
data6 <- aggregate(data6$poultry_val, by=list(Category=data6$HHID), FUN=sum, na.rm=TRUE)
colnames(data6) <- c("HHID", "poultry_val")
wealth <- merge(wealth, data6, by='HHID', all = TRUE)

# urban rural variable
data7 <- read.dta('Uganda_2009/GSEC1.dta')
data7 <- data7[,c(1,3)]
colnames(data7) <- c("HHID", "urban")
wealth <- merge(wealth, data7, by='HHID', all = TRUE)

# clean data set
wealth <- wealth[nchar(wealth[,1])>9,]

# add wealth
wealth$wealth_t <- apply(wealth[,2:7], 1, sum, na.rm=TRUE)

data40 <- read.dta('Uganda_2009/GSEC2.dta')
general <- data40[data40$h2q4=="Head",c("HHID", "h2q8")]
wealth <- merge(general, wealth, by='HHID', all = TRUE)

# trimming
boxplot(wealth$wealth_t)
hist(wealth$wealth_t)
qh <- quantile(wealth$wealth_t, 0.99, na.rm=TRUE)
ql <- quantile(wealth$wealth_t, 0.01, na.rm=TRUE)
wealth$wealth_t[wealth$wealth_t>qh] <- NA
wealth$wealth_t[wealth$wealth_t<ql] <- NA
boxplot(wealth$wealth_t)
hist(wealth$wealth_t)

# construct the log wealth values
wealth$wealth_t_log <- log(wealth$wealth_t)
wealth$wealth_t_log[wealth$wealth_t==0] <- NA
wealth$wealth_t_log[wealth$wealth_t<0] <- NA

var(wealth$wealth_t_log, na.rm=TRUE)
var(wealth$wealth_t_log[wealth$urban=="Urban"], na.rm=TRUE)
var(wealth$wealth_t_log[wealth$urban=="Rural"], na.rm=TRUE)

hist(wealth$wealth_t_log[wealth$urban=="Urban"])
hist(wealth$wealth_t_log[wealth$urban=="Rural"])

# CONSUMPTION #######################################################################

#data11 <- read.dta('Uganda_2009/GSEC1.dta')
#data11 <- data.frame(data11$HHID, data11$h1bq2b)
#colnames(data11) <- c("HHID", "month")

# food
data8 <- read.dta('Uganda_2009/GSEC15b.dta')
data8 <- data8[,c(1,6,8,10,12)]
colnames(data8)[1] <- "HHID"

data8 <- data.frame(data8[,1],apply(data8[,2:5], 1, sum, na.rm=TRUE))
colnames(data8) <- c("HHID", "fd")
#data8 <- merge(data11, data8, by="HHID", all = TRUE)

data8 <- aggregate(data8$fd, by=list(Category=data8$HHID), FUN=sum, na.rm=TRUE)
colnames(data8) <- c("HHID", "fd")
data8$fd <- data8$fd*52 # convert weekly consumption to annual
consumption <- data8

# non-food non-durables
data9 <- read.dta('Uganda_2009/GSEC15c.dta')
data9 <- data9[,c(1,5,7,9)]
data9 <- data.frame(data9[,1],apply(data9[,2:4], 1, sum, na.rm=TRUE))
colnames(data9) <- c("HHID", "nfnd")
data9 <- aggregate(data9$nfnd, by=list(Category=data9$HHID), FUN=sum, na.rm=TRUE)
colnames(data9) <- c("HHID", "nfnd")
data9$nfnd <- data9$nfnd*12
consumption <- merge(consumption, data9, by="HHID", all = TRUE)

# semi-durables
data10 <- read.dta('Uganda_2009/GSEC15d.dta')
data10 <- data10[,c(1,3,4,5)]
data10 <- data.frame(data10[,1],apply(data10[,2:4], 1, sum, na.rm=TRUE))
colnames(data10) <- c("HHID", "sd")
data10 <- aggregate(data10$sd, by=list(Category=data10$HHID), FUN=sum, na.rm=TRUE)
colnames(data10) <- c("HHID", "sd")
consumption <- merge(consumption, data10, by="HHID", all = TRUE)

# sum up consumption
consumption$cons_t <- apply(consumption[,2:4], 1, sum, na.rm=TRUE)
#consumption$cons_t[consumption$cons_t==0] <- NA
#consumption$cons_t <- log(consumption$cons_t)

# urban rural variable
consumption <- merge(data7, consumption, by='HHID', all = TRUE)
consumption <- merge(general, consumption, by='HHID', all = TRUE)

###
# trimming
boxplot(consumption$cons_t)
hist(consumption$cons_t)
qh <- quantile(consumption$cons_t, 0.99, na.rm=TRUE)
ql <- quantile(consumption$cons_t, 0.01, na.rm=TRUE)
consumption$cons_t[consumption$cons_t>qh] <- NA
consumption$cons_t[consumption$cons_t<ql] <- NA
boxplot(consumption$cons_t)
hist(consumption$cons_t)

# construct the log consumption values
consumption$cons_t_log <- log(consumption$cons_t)
consumption$cons_t_log[consumption$cons_t==0] <- NA
consumption$cons_t_log[consumption$cons_t<0] <- NA

var(consumption$cons_t_log, na.rm=TRUE)
var(consumption$cons_t_log[consumption$urban=="Urban"], na.rm=TRUE)
var(consumption$cons_t_log[consumption$urban=="Rural"], na.rm=TRUE)

hist(consumption$cons_t_log[consumption$urban=="Urban"])
hist(consumption$cons_t_log[consumption$urban=="Rural"])

# INCOME ###########################################################################
# labour income - ok
data12 <- read.dta('Uganda_2009/GSEC8.dta')

# main
data12$wph <- NA
data12$h8q31c <- as.character(data12$h8q31c)
data12$h8q31c[is.na(data12$h8q31c)] <- "none"
for(i in 1:nrow(data12)){
  if (data12$h8q31c[i]=="Hour") {
    data12$wph[i] <- (data12$h8q31a[i]+data12$h8q31b[i])}
  if (data12$h8q31c[i]=="Day") {
    data12$wph[i] <- (data12$h8q31a[i]+data12$h8q31b[i])/8}#16
  if (data12$h8q31c[i]=="Week") {
    data12$wph[i] <- (data12$h8q31a[i]+data12$h8q31b[i])/40}#7*16
  if (data12$h8q31c[i]=="Month") {
    data12$wph[i] <- (data12$h8q31a[i]+data12$h8q31b[i])/(40*4.5)}#7*16*4.5
}

data12$hpw <- data12$h8q36a+data12$h8q36b+data12$h8q36c+data12$h8q36d+
  data12$h8q36e+data12$h8q36f+data12$h8q36g
data12$hpw <- data12$h8q70
data12$main <- data12$wph*data12$h8q70*data12$h8q69*data12$h8q52
data12$main_horas <- data12$h8q70*data12$h8q69*data12$h8q30

# secondary
data12$wph <- NA
data12$h8q45c <- as.character(data12$h8q45c)
data12$h8q45c[is.na(data12$h8q45c)] <- "none"
for(i in 1:nrow(data12)){
  if (data12$h8q45c[i]=="1") {
    data12$wph[i] <- (data12$h8q45a[i]+data12$h8q45b[i])}
  if (data12$h8q45c[i]=="2") {
    data12$wph[i] <- (data12$h8q45a[i]+data12$h8q45b[i])/8}
  if (data12$h8q45c[i]=="3") {
    data12$wph[i] <- (data12$h8q45a[i]+data12$h8q45b[i])/40}
  if (data12$h8q45c[i]=="4") {
    data12$wph[i] <- (data12$h8q45a[i]+data12$h8q45b[i])/(40*4.5)}
}

data12$hpw <- data12$h8q73
#data12$secondary <- data12$wph*data12$hpw*4.5*data12$h8q44
data12$secondary <- data12$wph*data12$h8q73*data12$h8q72*data12$h8q44
data12$secondary_horas <- data12$h8q73*data12$h8q72*data12$h8q44

###
data12$h8q04 <- abs(as.numeric(data12$h8q04)-2)

lab_supply <- data.frame(data12$HHID,data12$PID,data12$main_horas, data12$secondary_horas,data12$h8q04)
colnames(lab_supply) <- c("HHID", "PID","hours1","hours2","employed")

data12 <- data.frame(data12$HHID, data12$main, data12$secondary,data12$main_horas, data12$secondary_horas,data12$h8q04)
colnames(data12) <- c("HHID", "main", "secondary","hours1","hours2","emp")
labour_income <- data.frame(data12[,1], apply(data12[,2:3], 1, sum, na.rm=TRUE),apply(data12[,4:5], 1, sum, na.rm=TRUE),data12$emp)
colnames(labour_income) <- c("HHID", "labour","hours","emp")
labour_inc1 <- aggregate(labour_income$labour, by=list(Category=labour_income$HHID), FUN=sum, na.rm=TRUE)
labour_inc2 <- aggregate(labour_income$hours, by=list(Category=labour_income$HHID), FUN=sum, na.rm=TRUE)
labour_inc3 <- aggregate(labour_income$emp, by=list(Category=labour_income$HHID), FUN=mean, na.rm=TRUE)
labour_income <- data.frame(labour_inc1,labour_inc2[,2],labour_inc3[,2])
colnames(labour_income) <- c("HHID", "labour","hours","emp")

labour_income <- merge(general, labour_income, by='HHID', all = TRUE)


personas <- read.dta('Uganda_2009/GSEC2.dta')
educ <- read.dta('Uganda_2009/GSEC4.dta')
trabajo <- merge(personas[,c(2,4,9)], lab_supply, by='PID', all = TRUE)
trabajo <- merge(trabajo,educ[,c(2,9)])
trabajo <- merge(data7, trabajo, by='HHID', all = TRUE)
trabajo$hours <- trabajo$hours1+trabajo$hours2
###

# income from crop farming - ok

# first visit
data15 <- read.dta('Uganda_2009/AGSEC5A.dta')

average_conversion <- aggregate(data15$a5aq6d, by=list(data15$a5aq6c), FUN=mean, na.rm=TRUE)
data15$a5aq7d <- NA

for(i in 1:nrow(data15)){
  if (!is.na(data15$a5aq6c[i])&!is.na(data15$a5aq7c[i])){
    if (data15$a5aq6c[i]==data15$a5aq7c[i]){
      data15$a5aq7d[i] <- data15$a5aq6d[i]
    }else{
      if (length(average_conversion[data15$a5aq7c[i]==average_conversion[,1],2])!=0){
        data15$a5aq7d[i] <- average_conversion[data15$a5aq7c[i]==average_conversion[,1],2]
      }else{
        data15$a5aq7d[i] <- NA
      }
    }
  }else{
    data15$a5aq7d[i] <- NA
  }
}
data15$ppk<- data15$a5aq8/(data15$a5aq7a*data15$a5aq7d) # changed to 7d

data16 <- read.dta('Uganda_2009/GSEC1.dta')
data16 <- data.frame(data16$HHID, data16$region)
colnames(data16) <- c("HHID", "region")
data15 <- merge(data16, data15, by="HHID", all = TRUE)

regions <- unique(data15$region)
crops <- unique(data15$a5aq4)

crops <- crops[-c(1,2)] 

data15$pag <- NA

for(i in 1:length(crops)){ # compute median price per crop and region
  for(j in 1:length(regions)){
    data15$pag[data15$region==regions[[j]]&data15$a5aq4==crops[[i]]] <- median(data15$ppk[data15$region==regions[[j]]&data15$a5aq4==crops[[i]]], na.rm=TRUE)
  }
}

data15$ag_income <- rowSums(data.frame(data15$a5aq8, 
                                       (data15$a5aq6a*data15$a5aq6d-data15$a5aq7a*data15$a5aq7d)*data15$pag,
                                       -data15$a5aq10), na.rm=TRUE) # value of products sold+value of products eaten-transport costs

crop_incomefv <- aggregate(data15$ag_income, by=list(Category=data15$HHID), FUN=sum, na.rm=TRUE)

# second visit
data15 <- read.dta('Uganda_2009/AGSEC5B.dta')

average_conversion <- aggregate(data15$a5bq6d, by=list(data15$a5bq6c), FUN=mean, na.rm=TRUE)
data15$a5bq7d <- NA

for(i in 1:nrow(data15)){
  if (!is.na(data15$a5bq6c[i])&!is.na(data15$a5bq7c[i])){
    if (data15$a5bq6c[i]==data15$a5bq7c[i]){
      data15$a5bq7d[i] <- data15$a5bq6d[i]
    }else{
      if (length(average_conversion[data15$a5bq7c[i]==average_conversion[,1],2])!=0){
        data15$a5bq7d[i] <- average_conversion[data15$a5bq7c[i]==average_conversion[,1],2]
      }else{
        data15$a5bq7d[i] <- NA
      }
    }
  }else{
    data15$a5bq7d[i] <- NA
  }
}
data15$ppk<- data15$a5bq8/(data15$a5bq7a*data15$a5bq7d) # changed to 7d

data16 <- read.dta('Uganda_2009/GSEC1.dta')
data16 <- data.frame(data16$HHID, data16$region)
colnames(data16) <- c("HHID", "region")
data15 <- merge(data16, data15, by="HHID", all = TRUE)

regions <- unique(data15$region)
crops <- unique(data15$a5bq4)

crops <- crops[-c(1,2)] 

data15$pag <- NA

for(i in 1:length(crops)){ # compute median price per crop and region
  for(j in 1:length(regions)){
    data15$pag[data15$region==regions[[j]]&data15$a5bq4==crops[[i]]] <- median(data15$ppk[data15$region==regions[[j]]&data15$a5bq4==crops[[i]]], na.rm=TRUE)
  }
}

data15$ag_income <- rowSums(data.frame(data15$a5bq8, 
                                       (data15$a5bq6a*data15$a5bq6d-data15$a5bq7a*data15$a5bq7d)*data15$pag,
                                       -data15$a5bq10), na.rm=TRUE) # value of products sold+value of products eaten-transport costs

crop_incomesv <- aggregate(data15$ag_income, by=list(Category=data15$HHID), FUN=sum, na.rm=TRUE)

crop_income_minus_transport <- merge(crop_incomefv, crop_incomesv, by="Category", all = TRUE) # revenues+home consumption-transport in 
colnames(crop_income_minus_transport) <- c("HHID", "crop sales and home consumption minus transport in first season",
                                           "crop sales and home consumption minus transport in second season")

# first and second visit

data21 <- read.dta('Uganda_2009/AGSEC3A.dta')
data21$cost_3a <- rowSums(data.frame(data21$a3aq8, # organic fertilizer, first visit
                                     data21$a3aq19, # chemical fertilizer, first visit
                                     data21$a3aq31, # pesticides, first visit
                                     data21$a3aq43), na.rm=TRUE) # hired labour paid, first visit

cost_a <- aggregate(data21$cost_3a, by=list(Category=data21$HHID), FUN=sum, na.rm=TRUE)
colnames(cost_a) <- c("HHID", "cost of fertilizer, pesticides and labour in first season")

data22 <- read.dta('Uganda_2009/AGSEC3B.dta')
data22$cost_3b <- rowSums(data.frame(data22$a3bq8, # organic fertilizer, second visit
                                     data22$a3bq19, # chemical fertilizer, second visit
                                     data22$a3bq31, # pesticides, second visit
                                     data22$a3bq43), na.rm=TRUE) # hired labour paid, second visit

cost_b <- aggregate(data22$cost_3b, by=list(Category=data22$HHID), FUN=sum, na.rm=TRUE)
colnames(cost_b) <- c("HHID", "cost of fertilizer, pesticides and labour in second season")

data25 <- read.dta('Uganda_2009/AGSEC4A.dta')
seeds_a <- aggregate(data25$a4aq11, by=list(Category=data25$HHID), FUN=sum, na.rm=TRUE)
colnames(seeds_a) <- c("HHID", "seed cost in first season")
# seeds, first visit
data27 <- read.dta('Uganda_2009/AGSEC4B.dta')
seeds_b <- aggregate(data27$a4bq11, by=list(Category=data27$HHID), FUN=sum, na.rm=TRUE)
colnames(seeds_b) <- c("HHID", "seed cost in second season")
# seeds, second visit

data29 <- read.dta('Uganda_2009/AGSEC2B.dta')
land <- aggregate(data29$a2bq9, by=list(Category=data29$HHID), FUN=sum, na.rm=TRUE)
colnames(land) <- c("HHID", "cost for land")
# rent for land to be paid per parcel and hh

crop_income <- merge(crop_income_minus_transport,cost_a, by="HHID", all=TRUE)
crop_income <- merge(crop_income,cost_b, by="HHID", all=TRUE)
crop_income <- merge(crop_income,seeds_a, by="HHID", all=TRUE)
crop_income <- merge(crop_income,seeds_b, by="HHID", all=TRUE)
crop_income <- merge(crop_income,land, by="HHID", all=TRUE)

crop_income[,9] <- apply(crop_income[,2:3], 1, sum, na.rm=TRUE)-apply(crop_income[,4:8], 1, sum, na.rm=TRUE)
colnames(crop_income)[9] <- c("total income from crop farming")

# livestock sales ok 
big_animals <- read.dta('Uganda_2009/AGSEC6A.dta')
big_animals$income <- big_animals$a6aq15
big_animals <- aggregate(big_animals$income, by=list(Category=big_animals$HHID), FUN=sum, na.rm=TRUE)
colnames(big_animals) <- c("HHID", "net sales of big animals")

small_animals <- read.dta('Uganda_2009/AGSEC6B.dta')
small_animals$income <- small_animals$a6bq15
small_animals <- aggregate(small_animals$income, by=list(Category=small_animals$HHID), FUN=sum, na.rm=TRUE)
colnames(small_animals) <- c("HHID", "net sales of small animals")

poultry <- read.dta('Uganda_2009/AGSEC6C.dta')
poultry$income <- poultry$a6cq15
poultry <- aggregate(poultry$income, by=list(Category=poultry$HHID), FUN=sum, na.rm=TRUE)
colnames(poultry) <- c("HHID", "net sales of poultry")

animals_cost <- read.dta('Uganda_2009/AGSEC7.dta')
animals_cost <- aggregate(animals_cost$a7q4, by=list(Category=animals_cost$HHID), FUN=sum, na.rm=TRUE)
colnames(animals_cost) <- c("HHID", "cost of holding animals")

livestock_sales <- merge(big_animals, small_animals, by="HHID", all=TRUE)
livestock_sales <- merge(livestock_sales, poultry, by="HHID", all=TRUE)
livestock_sales <- merge(livestock_sales, animals_cost, by="HHID", all=TRUE)
livestock_sales[,6] <- apply(data.frame(apply(livestock_sales[,2:4], 1, sum, na.rm=TRUE),
                                        -livestock_sales[,5]), 1, sum, na.rm=TRUE)
colnames(livestock_sales)[6] <- c("net income from livestock sales")

# livestock products ok 
animal_products <- read.dta('Uganda_2009/AGSEC8.dta')
animal_products$price <- animal_products$a8q7/animal_products$a8q6
animal_products$price[animal_products$price==Inf] <- NA
animal_products$price[animal_products$price==0] <- NA

animal_products$income <- rowSums(data.frame(animal_products$a8q7, animal_products$a8q8*animal_products$price), na.rm=TRUE)*
  animal_products$a8q3
livestock_products <- aggregate(animal_products$income, by=list(Category=animal_products$HHID),
                                FUN=sum, na.rm=TRUE)
colnames(livestock_products) <- c("HHID", "income from livestock products")

# income from fishery ok
fishery_A <- read.dta('Uganda_2009/AGSEC9A.dta')
fishery_B <- read.dta('Uganda_2009/AGSEC9B.dta')
fishery_B <- reshape(fishery_B, idvar = "HHID", timevar = "a9q6purp", direction = "wide")
fishery_C <- read.dta('Uganda_2009/AGSEC9C.dta')
fishery_D <- read.dta('Uganda_2009/AGSEC9D.dta')
fishery_D <- fishery_D[,c(1,3,4)]
fishery_D <- reshape(fishery_D, idvar = "HHID", timevar = "a9q11a", direction = "wide")

fishery <- merge(fishery_A, fishery_B, by="HHID", all=TRUE)
fishery <- merge(fishery, fishery_D, by="HHID", all=TRUE)

fishery$a9q9[fishery$a9q9==0] <- NA
fishery$a9q8[fishery$a9q8==0] <- NA
fishery$price <- median(fishery$a9q9/(fishery$a9q5*fishery$a9q6prop.Sold/100), na.rm =  TRUE)
fishery$sold <- fishery$a9q3*fishery$a9q4*fishery$a9q9
fishery$eaten <- fishery$a9q3*fishery$a9q4*fishery$a9q5*fishery$a9q6prop.Eaten*fishery$price
fishery$total <- rowSums(data.frame(fishery$sold, fishery$eaten), na.rm=TRUE)
fishery_income <- data.frame(fishery$HHID, 
                             rowSums(data.frame(fishery$total, -fishery[,12:19]), na.rm=TRUE))
colnames(fishery_income) <- c("HHID", "fishery income")

# income from business activity - ok
business <- read.dta('Uganda_2009/GSEC12.dta')
business$income <- rowSums(data.frame(business$h12q13,- business$h12q15, - business$h12q16,
                                      - business$h12q17), na.rm=TRUE)
business_income <- aggregate(business$income, by=list(Category=business$HHID), FUN=sum, na.rm=TRUE)
colnames(business_income) <- c("HHID", "business income")

# non-labour non-agricultural income - ok
data14 <- read.dta('Uganda_2009/GSEC11.dta')
data14$h11aq05[data14$h11aq03=="Crop farming Enterprises"] <- NA
data14$h11aq06[data14$h11aq03=="Crop farming Enterprises"] <- NA
data14$h11aq05[data14$h11aq03=="Other Agricultural Enterprises"] <- NA
data14$h11aq06[data14$h11aq03=="Other Agricultural Enterprises"] <- NA
data14$h11aq05[data14$h11aq03=="Non-agricultural Enterprises"] <- NA
data14$h11aq06[data14$h11aq03=="Non-agricultural Enterprises"] <- NA
data14$nli <- rowSums(data.frame(data14$h11aq05,data14$h11aq06), na.rm=TRUE)

data14 <- data.frame(data14$HHID, data14$nli)
colnames(data14) <- c("HHID", "nli")
non_labour <- aggregate(data14$nli, by=list(Category=data14$HHID), FUN=sum, na.rm=TRUE)
colnames(non_labour) <- c("HHID", "nli")

# merge all types of income
income <- merge(labour_income, crop_income[,c(1,9)], by="HHID", all = TRUE)
income <- merge(income, livestock_sales[,c(1,6)], by="HHID", all = TRUE)
income <- merge(income, livestock_products, by="HHID", all = TRUE)
income <- merge(income, fishery_income, by="HHID", all = TRUE)
income <- merge(income, business_income, by="HHID", all = TRUE)
income <- merge(income, non_labour, by="HHID", all = TRUE)

income$inc_t <- apply(income[,2:8], 1, sum, na.rm=TRUE)

income <- merge(data7, income, by='HHID', all = TRUE)

income <- merge(general, income, by='HHID', all = TRUE)

###
boxplot(income$inc_t)
hist(income$inc_t)
qh <- quantile(income$inc_t, 0.99, na.rm=TRUE)
ql <- quantile(income$inc_t, 0.01, na.rm=TRUE)
income$inc_t[income$inc_t>qh] <- NA
income$inc_t[income$inc_t<ql] <- NA
boxplot(income$inc_t)
hist(income$inc_t)

# construct the log consumption values
income$inc_t_log <- log(income$inc_t)
income$inc_t_log[income$inc_t==0] <- NA
income$inc_t_log[income$inc_t<0] <- NA

var(income$inc_t_log, na.rm=TRUE)
var(income$inc_t_log[income$urban=="Urban"], na.rm=TRUE)
var(income$inc_t_log[income$urban=="Rural"], na.rm=TRUE)

hist(income$inc_t_log[income$urban=="Urban"])
hist(income$inc_t_log[income$urban=="Rural"])

# QUESTION 1 ######################################################################
# 1
table1 <- data.frame(c(mean(consumption$cons_t[consumption$urban=="Rural"], na.rm=TRUE),
  mean(income$inc_t[income$urban=="Rural"], na.rm=TRUE),
  mean(wealth$wealth_t[wealth$urban=="Rural"], na.rm=TRUE))*0.0005,
c(mean(consumption$cons_t[consumption$urban=="Urban"], na.rm=TRUE),
  mean(income$inc_t[income$urban=="Urban"], na.rm=TRUE),
  mean(wealth$wealth_t[wealth$urban=="Urban"], na.rm=TRUE))*0.0005,
c(mean(consumption$cons_t, na.rm=TRUE),
  mean(income$inc_t, na.rm=TRUE),
  mean(wealth$wealth_t, na.rm=TRUE))*0.0005)

colnames(table1) <- c("Rural", "Urban", "Total")
rownames(table1) <- c("Consumption", "Income", "Wealth")

stargazer(table1, summary=FALSE)

# 2
plot1 <- ggplot()+geom_histogram(aes(x=consumption$cons_t_log[consumption$urban=="Rural"],
                            y=..density..), binwidth = 0.2,
                        color="darkblue", fill="darkblue", alpha=0.5)+
  geom_histogram(aes(x=consumption$cons_t_log[consumption$urban=="Urban"], 
                     y=..density..), binwidth = 0.2,
                color="orange", fill="orange", alpha=0.5)+
  xlab('')+ xlim(5, 20)

plot2 <- ggplot()+geom_histogram(aes(x=income$inc_t_log[income$urban=="Rural"],
                            y=..density..), binwidth = 0.2,
                        color="darkblue", fill="darkblue", alpha=0.5)+
  geom_histogram(aes(x=income$inc_t_log[income$urban=="Urban"], 
                     y=..density..), binwidth = 0.2,
                 color="orange", fill="orange", alpha=0.5)+
  xlab('')+ xlim(5, 20)

plot3 <- ggplot()+geom_histogram(aes(x=wealth$wealth_t_log[wealth$urban=="Rural"],
                            y=..density..), binwidth = 0.2,
                        color="darkblue", fill="darkblue", alpha=0.5)+
  geom_histogram(aes(x=wealth$wealth_t_log[wealth$urban=="Urban"], 
                     y=..density..), binwidth = 0.2,
                 color="orange", fill="orange", alpha=0.5)+
  xlab('')+ xlim(5, 20)

table2 <- data.frame(c(var(consumption$cons_t_log[consumption$urban=="Rural"], na.rm=TRUE),
                       var(income$inc_t_log[income$urban=="Rural"], na.rm=TRUE),
                       var(wealth$wealth_t_log[wealth$urban=="Rural"], na.rm=TRUE)),
                     c(var(consumption$cons_t_log[consumption$urban=="Urban"], na.rm=TRUE),
                       var(income$inc_t_log[income$urban=="Urban"], na.rm=TRUE),
                       var(wealth$wealth_t_log[wealth$urban=="Urban"], na.rm=TRUE)),
                     c(var(consumption$cons_t_log, na.rm=TRUE),
                       var(income$inc_t_log, na.rm=TRUE),
                       var(wealth$wealth_t_log, na.rm=TRUE)))

colnames(table2) <- c("Rural", "Urban", "Total")
rownames(table2) <- c("Consumption", "Income", "Wealth")

stargazer(table2, summary=FALSE)

# 3
group1 <- merge(consumption, income, by="HHID", all=TRUE)
group2 <- merge(consumption, wealth, by="HHID", all=TRUE)
group3 <- merge(income, wealth, by="HHID", all=TRUE)

table3 <- cor(data.frame(group1$cons_t_log[group1$urban.x=="Urban"],
                         group1$inc_t_log[group1$urban.x=="Urban"]),
                        use="complete.obs")
colnames(table3) <- c("Consumption", "Income")
rownames(table3) <- c("Consumption", "Income")

table4 <- cor(data.frame(group2$cons_t_log[group2$urban.x=="Urban"],
                         group2$wealth_t_log[group2$urban.x=="Urban"]),
                        use="complete.obs")
colnames(table4) <- c("Consumption", "Wealth")
rownames(table4) <- c("Consumption", "Wealth")

table5 <- cor(data.frame(group3$inc_t_log[group3$urban.x=="Urban"],
                         group3$wealth_t_log[group3$urban.x=="Urban"]),
                        use="complete.obs")
colnames(table5) <- c("Income", "Wealth")
rownames(table5) <- c("Income", "Wealth")

stargazer(table3, summary=FALSE)
stargazer(table4, summary=FALSE)
stargazer(table5, summary=FALSE)

table3 <- cor(data.frame(group1$cons_t_log[group1$urban.x=="Rural"],
                         group1$inc_t_log[group1$urban.x=="Rural"]),
              use="complete.obs")
colnames(table3) <- c("Consumption", "Income")
rownames(table3) <- c("Consumption", "Income")

table4 <- cor(data.frame(group2$cons_t_log[group2$urban.x=="Rural"],
                         group2$wealth_t_log[group2$urban.x=="Rural"]),
              use="complete.obs")
colnames(table4) <- c("Consumption", "Wealth")
rownames(table4) <- c("Consumption", "Wealth")

table5 <- cor(data.frame(group3$inc_t_log[group3$urban.x=="Rural"],
                         group3$wealth_t_log[group3$urban.x=="Rural"]),
              use="complete.obs")
colnames(table5) <- c("Income", "Wealth")
rownames(table5) <- c("Income", "Wealth")

stargazer(table3, summary=FALSE)
stargazer(table4, summary=FALSE)
stargazer(table5, summary=FALSE)

table3 <- cor(data.frame(group1$cons_t_log,
                         group1$inc_t_log),
              use="complete.obs")
colnames(table3) <- c("Consumption", "Income")
rownames(table3) <- c("Consumption", "Income")

table4 <- cor(data.frame(group2$cons_t_log,
                         group2$wealth_t_log),
              use="complete.obs")
colnames(table4) <- c("Consumption", "Wealth")
rownames(table4) <- c("Consumption", "Wealth")

table5 <- cor(data.frame(group3$inc_t_log,
                         group3$wealth_t_log),
              use="complete.obs")
colnames(table5) <- c("Income", "Wealth")
rownames(table5) <- c("Income", "Wealth")

stargazer(table3, summary=FALSE)
stargazer(table4, summary=FALSE)
stargazer(table5, summary=FALSE)

# 4
lcc_level <- aggregate(consumption$cons_t_log, by=list(Category=consumption$h2q8), 
                      FUN=mean, na.rm=TRUE)
ggplot(lcc_level)+geom_line(aes(x=Category, y=x))+xlim(15, 65)+
  xlab('age')+ylab('Mean of log hh consumption')

lci_level <- aggregate(income$inc_t_log, by=list(Category=income$h2q8.x), 
                      FUN=mean, na.rm=TRUE)
ggplot(lci_level)+geom_line(aes(x=Category, y=x))+xlim(15, 65)+
  xlab('age')+ylab('Mean of log hh income')

lcw_level <- aggregate(wealth$wealth_t_log, by=list(Category=wealth$h2q8), 
                       FUN=mean, na.rm=TRUE)
ggplot(lcw_level)+geom_line(aes(x=Category, y=x))+xlim(15, 65)+
  xlab('age')+ylab('Mean of log hh income')

lcc_varlog <- aggregate(consumption$cons_t_log, by=list(Category=consumption$h2q8), 
                       FUN=var, na.rm=TRUE)
ggplot(lcc_varlog)+geom_line(aes(x=Category, y=x))+xlim(15, 65)+
  xlab('age')+ylab('Mean of var log hh consumption')

lci_varlog <- aggregate(income$inc_t_log, by=list(Category=income$h2q8.x), 
                       FUN=var, na.rm=TRUE)
ggplot(lci_varlog)+geom_line(aes(x=Category, y=x))+xlim(15, 65)+
  xlab('age')+ylab('Mean of var log hh income')

lcw_varlog <- aggregate(wealth$wealth_t_log, by=list(Category=wealth$h2q8), 
                       FUN=var, na.rm=TRUE)
ggplot(lcw_varlog)+geom_line(aes(x=Category, y=x))+xlim(15, 65)+
  xlab('age')+ylab('Mean of var log hh income')

corr_by_age <- data.frame(sort(unique(group1$h2q8.x)))
corr_by_age$corr <- NA

for(i in 1:nrow(corr_by_age)){
  if(sum(complete.cases(data.frame(group1$cons_t_log[group1$h2q8.x==corr_by_age[i,1]],
             group1$inc_t_log[group1$h2q8.x==corr_by_age[i,1]])))>0){
  corr_by_age[i,2] <- cor(data.frame(group1$cons_t_log[group1$h2q8.x==corr_by_age[i,1]],
                           group1$inc_t_log[group1$h2q8.x==corr_by_age[i,1]]),
                          use="complete.obs")[2,1]}else{
                            corr_by_age[i,2] <- NA
                          }
}
colnames(corr_by_age) <- c("age", "correlation")
ggplot(corr_by_age)+geom_line(aes(x=age, y=correlation))+xlim(15, 65)+
  xlab('age')+ylab('Corr of log consumption and income')

corr_by_age <- data.frame(sort(unique(group2$h2q8.x)))
corr_by_age$corr <- NA

for(i in 1:nrow(corr_by_age)){
  if(sum(complete.cases(data.frame(group2$cons_t_log[group2$h2q8.x==corr_by_age[i,1]],
                                   group2$wealth_t_log[group2$h2q8.x==corr_by_age[i,1]])))>0){
    corr_by_age[i,2] <- cor(data.frame(group2$cons_t_log[group2$h2q8.x==corr_by_age[i,1]],
                                       group2$wealth_t_log[group2$h2q8.x==corr_by_age[i,1]]),
                            use="complete.obs")[2,1]}else{
                              corr_by_age[i,2] <- NA
                            }
}
colnames(corr_by_age) <- c("age", "correlation")
ggplot(corr_by_age)+geom_line(aes(x=age, y=correlation))+xlim(15, 65)+
  xlab('age')+ylab('Corr of log consumption and wealth')

corr_by_age <- data.frame(sort(unique(group3$h2q8.x)))
corr_by_age$corr <- NA

for(i in 1:nrow(corr_by_age)){
  if(sum(complete.cases(data.frame(group3$inc_t_log[group3$h2q8.x==corr_by_age[i,1]],
                                   group3$wealth_t_log[group3$h2q8.x==corr_by_age[i,1]])))>0){
    corr_by_age[i,2] <- cor(data.frame(group3$inc_t_log[group3$h2q8.x==corr_by_age[i,1]],
                                       group3$wealth_t_log[group3$h2q8.x==corr_by_age[i,1]]),
                            use="complete.obs")[2,1]}else{
                              corr_by_age[i,2] <- NA
                            }
}
colnames(corr_by_age) <- c("age", "correlation")
ggplot(corr_by_age)+geom_line(aes(x=age, y=correlation))+xlim(15, 65)+
  xlab('age')+ylab('Corr of log income and wealth')


# 5

all <- merge(group1, group2, by="HHID", all=TRUE)
q1 <- quantile(all$inc_t_log, 0.2, na.rm=TRUE)
q2 <- quantile(all$inc_t_log, 0.4, na.rm=TRUE)
q3 <- quantile(all$inc_t_log, 0.6, na.rm=TRUE)
q4 <- quantile(all$inc_t_log, 0.8, na.rm=TRUE)

all$quantile <- NA
all$quantile[all$inc_t_log<=q1] <- 1
all$quantile[(q1<all$inc_t_log)&(all$inc_t_log<=q2)] <- 2
all$quantile[(q2<all$inc_t_log)&(all$inc_t_log<=q3)] <- 3
all$quantile[(q3<all$inc_t_log)&(all$inc_t_log<=q4)] <- 4
all$quantile[(all$inc_t_log>q4)] <- 5

table6 <- data.frame(rbind(aggregate(all$wealth_t_log, by=list(all$quantile), 
          FUN=sum, na.rm=TRUE)[,2],
aggregate(all$cons_t_log.x , by=list(all$quantile), 
          FUN=sum, na.rm=TRUE)[,2]))
table6[1,] <- table6[1,]/sum(table6[1,])*100
table6[2,] <- table6[2,]/sum(table6[2,])*100
rownames(table6) <- c("wealth", "consumption")
colnames(table6) <- c("Q1", "Q2", "Q3", "Q4", "Q5")
stargazer(table6, summary=FALSE)

# QUESTION 2 ######################################################################
# 1
labour_income <- merge(data7, labour_income, by='HHID', all = TRUE)
table6 <- data.frame(c(mean(labour_income$hours[labour_income$urban=="Rural"], na.rm=TRUE),
                       mean(labour_income$emp[labour_income$urban=="Rural"], na.rm=TRUE)),
                     c(mean(labour_income$hours[labour_income$urban=="Urban"], na.rm=TRUE),
                       mean(labour_income$emp[labour_income$urban=="Urban"], na.rm=TRUE)),
                     c(mean(labour_income$hours, na.rm=TRUE),
                       mean(labour_income$emp, na.rm=TRUE)))

colnames(table6) <- c("Rural", "Urban", "Total")
rownames(table6) <- c("Hours", "Employment")

stargazer(table6, summary=FALSE)

# 2
plot1 <- ggplot()+geom_histogram(aes(x=labour_income$hours[labour_income$urban=="Rural"],
                                     y=..density..),# binwidth = 100,
                                 color="darkblue", fill="darkblue", alpha=0.5)+
  geom_histogram(aes(x=labour_income$hours[labour_income$urban=="Urban"], 
                     y=..density..),# binwidth = 100,
                 color="orange", fill="orange", alpha=0.5)+
  xlab('')#+ xlim(-10,5000)
plot1

plot2 <- ggplot()+geom_histogram(aes(x=labour_income$emp[labour_income$urban=="Rural"],
                                     y=..density..), binwidth = 0.1,
                                 color="darkblue", fill="darkblue", alpha=0.5)+
  geom_histogram(aes(x=labour_income$emp[labour_income$urban=="Urban"], 
                     y=..density..), binwidth = 0.1,
                 color="orange", fill="orange", alpha=0.5)+
  xlab('')
plot2

labour_income$hours_log <- log(labour_income$hours)
labour_income$hours_log[labour_income$hours_log==-Inf] <- NA
labour_income$emp_log <- log(labour_income$emp)
labour_income$emp_log[labour_income$emp_log==-Inf] <- NA
table7 <- data.frame(c(var(labour_income$hours_log[labour_income$urban=="Rural"], na.rm=TRUE),
                       var(labour_income$emp_log[labour_income$urban=="Rural"], na.rm=TRUE)),
                     c(var(labour_income$hours_log[labour_income$urban=="Urban"], na.rm=TRUE),
                       var(labour_income$emp_log[labour_income$urban=="Urban"], na.rm=TRUE)),
                     c(var(labour_income$hours_log, na.rm=TRUE),
                       var(labour_income$emp_log, na.rm=TRUE)))

colnames(table7) <- c("Rural", "Urban", "Total")
rownames(table7) <- c("Hours", "Employment")

stargazer(table7, summary=FALSE)

# 3
table8 <- cor(data.frame(labour_income$hours,
                         labour_income$emp),
              use="complete.obs")
colnames(table8) <- c("Hours", "Employment")
rownames(table8) <- c("Hours", "Employment")

stargazer(table8, summary=FALSE)

table9 <- cor(data.frame(labour_income$hours[labour_income$urban=="Rural"],
                         labour_income$emp[labour_income$urban=="Rural"]),
              use="complete.obs")
colnames(table9) <- c("Hours", "Employment")
rownames(table9) <- c("Hours", "Employment")

stargazer(table9, summary=FALSE)

table9 <- cor(data.frame(labour_income$hours[labour_income$urban=="Urban"],
                         labour_income$emp[labour_income$urban=="Urban"]),
              use="complete.obs")
colnames(table9) <- c("Hours", "Employment")
rownames(table9) <- c("Hours", "Employment")

stargazer(table9, summary=FALSE)

# 4
lcc_level <- aggregate(labour_income$hours_log, by=list(Category=labour_income$h2q8), 
                       FUN=mean, na.rm=TRUE)
ggplot(lcc_level)+geom_line(aes(x=Category, y=x))+xlim(20, 65)+
  xlab('age')+ylab('Mean of log hh hours')

lci_level <- aggregate(labour_income$emp_log, by=list(Category=labour_income$h2q8), 
                       FUN=mean, na.rm=TRUE)
ggplot(lci_level)+geom_line(aes(x=Category, y=x))+xlim(20, 65)+
  xlab('age')+ylab('Mean of log hh emp')

lcc_varlog <- aggregate(labour_income$hours_log, by=list(Category=labour_income$h2q8), 
                        FUN=var, na.rm=TRUE)
ggplot(lcc_varlog)+geom_line(aes(x=Category, y=x))+xlim(20, 65)+
  xlab('age')+ylab('Var of log hh hours')

lci_varlog <- aggregate(labour_income$emp_log, by=list(Category=labour_income$h2q8), 
                        FUN=var, na.rm=TRUE)
ggplot(lci_varlog)+geom_line(aes(x=Category, y=x))+xlim(20, 65)+
  xlab('age')+ylab('Var of log hh emp')

corr_by_age <- data.frame(sort(unique(labour_income$h2q8)))
corr_by_age$corr <- NA

for(i in 1:nrow(corr_by_age)){
  if(sum(complete.cases(data.frame(labour_income$hours_log[labour_income$h2q8==corr_by_age[i,1]],
                                   labour_income$emp_log[labour_income$h2q8==corr_by_age[i,1]])))>0){
    corr_by_age[i,2] <- cor(data.frame(labour_income$hours_log[labour_income$h2q8==corr_by_age[i,1]],
                                       labour_income$emp_log[labour_income$h2q8==corr_by_age[i,1]]),
                            use="complete.obs")[2,1]}else{
                              corr_by_age[i,2] <- NA
                            }
}
colnames(corr_by_age) <- c("age", "correlation")
ggplot(corr_by_age)+geom_line(aes(x=age, y=correlation))+xlim(20, 65)+
  xlab('age')+ylab('Corr of log hours and employment')

# 1
table6 <- data.frame(c(mean(trabajo$hours[trabajo$h2q3=="FEMALE"], na.rm=TRUE),
                       mean(trabajo$employed[trabajo$h2q3=="FEMALE"], na.rm=TRUE)),
                     c(mean(trabajo$hours[trabajo$h2q3=="MALE"], na.rm=TRUE),
                       mean(trabajo$employed[trabajo$h2q3=="MALE"], na.rm=TRUE)),
                     c(mean(trabajo$hours, na.rm=TRUE),
                       mean(trabajo$employed, na.rm=TRUE)))

colnames(table6) <- c("Female", "Male", "Total")
rownames(table6) <- c("Hours", "Employment")

stargazer(table6, summary=FALSE)

table6 <- data.frame(c(mean(trabajo$hours[trabajo$h2q3=="FEMALE"], na.rm=TRUE),
                       mean(trabajo$employed[trabajo$h2q3=="FEMALE"], na.rm=TRUE)),
                     c(mean(trabajo$hours[trabajo$h2q3=="MALE"], na.rm=TRUE),
                       mean(trabajo$employed[trabajo$h2q3=="MALE"], na.rm=TRUE)),
                     c(mean(trabajo$hours, na.rm=TRUE),
                       mean(trabajo$employed, na.rm=TRUE)))

colnames(table6) <- c("Female", "Male", "Total")
rownames(table6) <- c("Hours", "Employment")

stargazer(table6, summary=FALSE)

# 2
plot1 <- ggplot()+geom_histogram(aes(x=trabajo$hours[trabajo$h2q3=="FEMALE"],
                                     y=..density..),# binwidth = 100,
                                 color="darkblue", fill="darkblue", alpha=0.5)+
  geom_histogram(aes(x=trabajo$hours[trabajo$h2q3=="MALE"], 
                     y=..density..),# binwidth = 100,
                 color="orange", fill="orange", alpha=0.5)+
  xlab('')#+ xlim(-10,5000)
plot1

plot2 <- ggplot()+geom_histogram(aes(x=trabajo$employed[trabajo$h2q3=="FEMALE"],
                                     y=..density..), #binwidth = 0.1,
                                 color="darkblue", fill="darkblue", alpha=0.5)+
  geom_histogram(aes(x=trabajo$employed[trabajo$h2q3=="MALE"], 
                     y=..density..), #binwidth = 0.1,
                 color="orange", fill="orange", alpha=0.5)+
  xlab('')
plot2

trabajo$hours_log <- log(trabajo$hours)
trabajo$hours_log[trabajo$hours_log==-Inf] <- NA
trabajo$emp_log <- log(trabajo$employed)
trabajo$emp_log[trabajo$emp_log==-Inf] <- NA
table7 <- data.frame(c(var(trabajo$hours_log[trabajo$h2q3=="Female"], na.rm=TRUE),
                       var(trabajo$emp_log[trabajo$h2q3=="Female"], na.rm=TRUE)),
                     c(var(trabajo$hours_log[trabajo$h2q3=="Male"], na.rm=TRUE),
                       var(trabajo$emp_log[trabajo$h2q3=="Male"], na.rm=TRUE)),
                     c(var(trabajo$hours_log, na.rm=TRUE),
                       var(trabajo$emp_log, na.rm=TRUE)))

colnames(table7) <- c("Female", "Male", "Total")
rownames(table7) <- c("Hours", "Employment")

stargazer(table7, summary=FALSE)

# education
trabajo$type <- substr(trabajo$h4q7, 11,11)
trabajo$grade <- substr(trabajo$h4q7, 13,13)

trabajo$educ <- NA
trabajo$educ[trabajo$type=="P"&trabajo$grade<7] <- 0 # less than primary
trabajo$educ[trabajo$type=="P"&trabajo$grade==7] <- 1 # primary completed
trabajo$educ[trabajo$type!="P"] <- 2 # secondary or more

table6 <- data.frame(c(mean(trabajo$hours[trabajo$educ==0], na.rm=TRUE),
                       mean(trabajo$employed[trabajo$educ==0], na.rm=TRUE)),
                     c(mean(trabajo$hours[trabajo$educ==1], na.rm=TRUE),
                       mean(trabajo$employed[trabajo$educ==1], na.rm=TRUE)),
                     c(mean(trabajo$hours[trabajo$educ==2], na.rm=TRUE),
                       mean(trabajo$employed[trabajo$educ==2], na.rm=TRUE)))

colnames(table6) <- c("Less than primary c.", "Primary c.", "Secondary or more")
rownames(table6) <- c("Hours", "Employment")

stargazer(table6, summary=FALSE)

# 2
plot1 <- ggplot()+geom_histogram(aes(x=trabajo$hours[trabajo$educ==0],
                                     y=..density..),# binwidth = 100,
                                 color="darkblue", fill="darkblue", alpha=0.5)+
  geom_histogram(aes(x=trabajo$hours[trabajo$educ==1], 
                     y=..density..),# binwidth = 100,
                 color="orange", fill="orange", alpha=0.5)+
  geom_histogram(aes(x=trabajo$hours[trabajo$educ==2], 
                     y=..density..),# binwidth = 100,
                 color="orange", fill="green", alpha=0.5)+
  xlab('')#+ xlim(-10,5000)
plot1

plot2 <- ggplot()+geom_histogram(aes(x=trabajo$employed[trabajo$educ=="FEMALE"],
                                     y=..density..), #binwidth = 0.1,
                                 color="darkblue", fill="darkblue", alpha=0.5)+
  geom_histogram(aes(x=trabajo$employed[trabajo$educ=="MALE"], 
                     y=..density..), #binwidth = 0.1,
                 color="orange", fill="orange", alpha=0.5)+
  xlab('')
plot2

trabajo$hours_log <- log(trabajo$hours)
trabajo$hours_log[trabajo$hours_log==-Inf] <- NA
trabajo$emp_log <- log(trabajo$employed)
trabajo$emp_log[trabajo$emp_log==-Inf] <- NA
table7 <- data.frame(c(var(trabajo$hours_log[trabajo$educ==0], na.rm=TRUE),
                       var(trabajo$emp_log[trabajo$educ==0], na.rm=TRUE)),
                     c(var(trabajo$hours_log[trabajo$educ==1], na.rm=TRUE),
                       var(trabajo$emp_log[trabajo$educ==1], na.rm=TRUE)),
                     c(var(trabajo$hours_log[trabajo$educ==2], na.rm=TRUE),
                       var(trabajo$emp_log[trabajo$educ==2], na.rm=TRUE)),
                     c(var(trabajo$hours_log, na.rm=TRUE),
                       var(trabajo$emp_log, na.rm=TRUE)))

colnames(table7) <- c("Less than primary completed", "Primary completed", "Secondary or more")
rownames(table7) <- c("Hours", "Employment")

stargazer(table7, summary=FALSE)

# QUESTION 3 #######################################################################
data1 <- read.dta('Uganda_2009/GSEC1.dta')
data1 <- data.frame(data1$HHID, data1$h1aq1)
colnames(data1) <- c("HHID", "district")

labour_income <- merge(labour_income, data1, by="HHID", all=TRUE)
labour_income <- merge(labour_income, income, by="HHID", all=TRUE)
labour_income <- merge(labour_income, wealth, by="HHID", all=TRUE)
labour_income <- merge(labour_income, consumption, by="HHID", all=TRUE)

hours_bd <- aggregate(labour_income$hours.x, by=list(Category=labour_income$district), FUN=mean, na.rm=TRUE)
income_bd <- aggregate(labour_income$inc_t, by=list(Category=labour_income$district), FUN=mean, na.rm=TRUE)
wealth_bd <- aggregate(labour_income$wealth_t, by=list(Category=labour_income$district), FUN=mean, na.rm=TRUE)
consumption_bd <- aggregate(labour_income$cons_t, by=list(Category=labour_income$district), FUN=mean, na.rm=TRUE)

hours_sc <- merge(hours_bd, income_bd, by="Category", all=TRUE)
wealth_sc <- merge(wealth_bd, income_bd, by="Category", all=TRUE)
consumption_sc <- merge(consumption_bd, income_bd, by="Category", all=TRUE)

ggplot(hours_sc[,2:3],aes(x=x.x, y=x.y))+geom_point()+    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)+
  xlab("hours")+ylab("income")

ggplot(wealth_sc[,2:3],aes(x=x.x, y=x.y))+geom_point()+    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)+
  xlab("wealth")+ylab("income")

ggplot(consumption_sc[,2:3],aes(x=x.x, y=x.y))+geom_point()+    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)+
  xlab("consumption")+ylab("income")
 #

hours_bd <- aggregate(labour_income$hours_log , by=list(Category=labour_income$district), FUN=var, na.rm=TRUE)
income_bd <- aggregate(labour_income$inc_t_log, by=list(Category=labour_income$district), FUN=var, na.rm=TRUE)
wealth_bd <- aggregate(labour_income$wealth_t_log, by=list(Category=labour_income$district), FUN=var, na.rm=TRUE)
consumption_bd <- aggregate(labour_income$cons_t_log, by=list(Category=labour_income$district), FUN=var, na.rm=TRUE)

hours_sc <- merge(hours_bd, income_bd, by="Category", all=TRUE)
wealth_sc <- merge(wealth_bd, income_bd, by="Category", all=TRUE)
consumption_sc <- merge(consumption_bd, income_bd, by="Category", all=TRUE)

ggplot(hours_sc[,2:3],aes(x=x.x, y=x.y))+geom_point()+    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)+
  xlab("hours")+ylab("income")

ggplot(wealth_sc[,2:3],aes(x=x.x, y=x.y))+geom_point()+    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)+
  xlab("wealth")+ylab("income")

ggplot(consumption_sc[,2:3],aes(x=x.x, y=x.y))+geom_point()+    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)+
  xlab("consumption")+ylab("income")
