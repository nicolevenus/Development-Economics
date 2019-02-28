# Libraries
library(foreign)
library(reshape)
library(ggplot2)
library(plyr)
library(stargazer)

rm(list = ls())

setwd('C:/Users/Nicole/Desktop')
#setwd('C:/Users/jsanc/OneDrive/Documentos/Uni/Máster/2nd year/2nd term/Development Economics/HW 3')

data <- read.dta('dataUGA.dta')

data$year <- as.numeric(substr(data$wave, 1, 4))

# Check any duplicates
data$warn = 0
ii=0
for (i in unique(data$hh)){
  ii = ii +1
  for (t in unique(data$year)){
    if (length(data[(data$year==t)&(data$hh==i),1])>1){
      data[(data$year==t)&(data$hh==i),]$warn = 1
    }
  }
}
#Drop duplicated hh
data = data[data$warn==0,]

# check if hh are observed only for one year
data$change <- 0
for(i in unique(data$hh)){
  if(length(unique(data$hh==i))==1){
    data$change[data$hh==i] <- 50
  }
}

# old data is without dropping observations
data_old    = data 
data_for_Q4 = data
#data_old$lc = log(data_old$ctotal)

#data_old    = data_old[is.na(data_old$lc)==0,]
#data_old    = data_old[is.infinite(data_old$lc)==0,]

# drop households which are only observed one year
#data = data[data$hh %in% change[change[,1]<50,2],]
data = data[data$counthh>2,]

D <- data.frame(cbind(data$hh,data$year,data$ctotal,data$inctotal,data$familysize,data$age,data$age_sq, data$region,data$ethnic,data$female,data$urban))
colnames(D) <- c('id', 'year', 'cons', 'inc', 'N', 'a','a2','r','e','s','u')

D$cons = log(D$cons)
D$inc_or  = D$inc
D$inc  = log(D$inc)
D$N    = log(D$N)

D <- D[rowSums(is.na(D))==0,]
D <- D[is.infinite(D$inc)==0,]
D <- D[is.infinite(D$cons)==0,]

D$y09 <- as.numeric(D$year == 2009)
D$y10 <- as.numeric(D$year == 2010)
D$y11 <- as.numeric(D$year == 2011)
D$y12 <- as.numeric(D$year == 2012)
D$y13 <- as.numeric(D$year == 2013)
D$y14 <- as.numeric(D$year == 2014)

f.eth = factor(D$e)
f.r = factor(D$r)

# compute residuals
D$c <- lm(cons ~ 1 + a +a2 + N + y09 + y10 + y11 + y12 +y13 +y14 +f.r+ s+ u + f.eth, data = D)$residuals
D$i <- lm(inc ~ 1 + a +a2 + N + y09 + y10 + y11 + y12 +y13 +y14 +f.r + s+ u + f.eth, data = D)$residuals

D$tot_cons = 0
j = 0
for (t in unique(data$year)){
  j = j+1
  for (r in unique(D$r)){
    #tot_cons[j] = mean(lc_old[data_old$year == t], na.rm = T)
    D[(D$year == t)&(D$r==r),]$tot_cons = log(mean(data_old$ctotal[(data_old$year == t)&(data_old$region==r)], na.rm = T))
  }
}

# Keep only if we have data on income (they will drop anyways in the regression)
D=D[is.na(D$i)==0,]
D=D[is.na(D$tot_cons)==0,]

D=D[is.infinite(D$c)==0,]
#DD <- D[,c(1,2,8,18,19,20)]
DD <- D[,c(1:4,8,12,19,20,21)]

DD$y = 0

for (i in unique(DD$id)){
  DD[DD$id == i,]$y = sum(DD[DD$id == i,]$year)
}

DD <- DD[DD$y>5000,]

####

phi <- beta <- matrix(NA, ncol = 1, nrow =length(unique(DD$id)))
total_D <- matrix(NA, ncol= 6, nrow = 1)
colnames(total_D) <- c('id','year','region', 'cons', 'inc', 'tot')
D_2 <- matrix(NA, ncol= 4, nrow = 1)
colnames(D_2) <- c('id', 'inc', 'region', 'beta')

ii = 0
for (i in unique(DD$id)){
  ii=ii+1
  dat <- DD[DD$id==i,]
  dat <- dat[order(dat$year),]
  dat$tdiff[2:nrow(dat)] <- diff(dat$year)
  # this is the annualisation of changes
  dat$cdiff[2:nrow(dat)] <- diff(dat$c)/dat$tdiff[2:nrow(dat)]
  dat$idiff[2:nrow(dat)] <- diff(dat$i)/dat$tdiff[2:nrow(dat)]
  dat$tot_consdiff[2:nrow(dat)] <- diff(dat$tot_cons)/dat$tdiff[2:nrow(dat)]
  #original <- nrow(dat)
  ## impute the rows that are missing (balanced the panel)
  #for(j in 2:original){
  #  if(unlist(dat$tdiff[j])>1){
  #  mat <- data.frame(matrix(rep(dat[j,], each=dat$tdiff[[j]]), 
  #                nrow=dat$tdiff[[j]]))
  #  colnames(mat) <- colnames(dat)
  #  dat <- rbind(dat, mat)
  #  }else{
  #    dat <- rbind(dat, dat[j,])
  #  }
  #}
  #dat <- dat[(original+1):nrow(dat),]
  #dat$cdiff <- unlist(dat$cdiff)
  #dat$idiff <- unlist(dat$idiff)
  #dat$tot_consdiff <- unlist(dat$tot_consdiff)
  #dat$inc_or <- unlist(dat$inc_or)
  #dat$r <- unlist(dat$r)
  #dat$id <- unlist(dat$id)
  
  
  reg <- lm(cdiff ~ 1  + idiff + tot_consdiff, data=dat)
  beta[ii] = reg$coeff[2]
  phi[ii] = reg$coeff[3]
  
  #NEW
  dat2 <- data.frame(t(c(colMeans(dat[,c(1,5,6)]),beta[ii])))
  colnames(dat2) <- c('id', 'region', 'inc', 'beta')
  
  D_2 <- rbind(D_2,dat2)
  
  dat3 <- data.frame(dat[-1,c(1,2,5,12:14)])
  colnames(dat3) <- c('id','year','region', 'cons', 'inc', 'tot')
  
  total_D <- rbind(total_D,dat3)
}
total_D <- total_D[-1,] #For Q3
D_2 <- D_2[-1,]



beta <- as.data.frame(beta)
q_beta <- quantile(beta$V1, probs=c(0.05, 0.95), na.rm=TRUE)
beta_plot <- as.data.frame(beta[beta$V1>q_beta[1],])
beta_plot <- as.data.frame(beta_plot[beta_plot<q_beta[2]])
colnames(beta_plot) <- "beta"
ggplot(beta_plot, aes(beta)) + geom_histogram(aes(y = ..density..), fill="blue", alpha=0.2)+
  ylab('Density')+xlab('Beta coefficient')

phi <- as.data.frame(phi)
q_phi <- quantile(phi$V1, probs=c(0.05, 0.95), na.rm=TRUE)
phi_plot <- as.data.frame(phi[phi$V1>q_phi[1],])
phi_plot <- as.data.frame(phi_plot[phi_plot<q_phi[2]])
colnames(phi_plot) <- "phi"
ggplot(phi_plot, aes(phi)) + geom_histogram(aes(y = ..density..), fill="red", alpha=0.2)+
  ylab('Density')+xlab('Phi coefficient')

table <- data.frame(c(mean(beta_plot[,1], na.rm=TRUE),
median(beta_plot[,1], na.rm=TRUE)),
c(mean(phi_plot[,1], na.rm=TRUE),
median(phi_plot[,1], na.rm=TRUE)))
rownames(table) <- c("Mean", "Median")
colnames(table) <- c("Beta", "Phi")

# Q2

D_2_org = D_2
D_2 <- transform(D_2, Q = cut(D_2$inc,breaks = quantile(D_2$inc,  probs = c(0, 2, 4, 6, 8, 10)/10), labels = c(1, 2, 3, 4,5),include.lowest=TRUE))

q_beta <- quantile(D_2$beta, probs=c(0.05, 0.95), na.rm=TRUE)
D_2 <- D_2[D_2$beta>q_beta[1],]
D_2 <- D_2[D_2$beta<q_beta[2],]


Q2 <- matrix(NA, ncol= 2, nrow = 5)
for (i in 1:5){
  Q2[i,] <- c(mean(D_2[D_2$Q==i,]$beta),median(D_2[D_2$Q==i,]$beta))
}

colnames(Q2) <- c("Mean", "Median")
rownames(Q2) <- c("1", "2", "3", "4", "5")

D_2 = D_2_org
D_2 <- transform(D_2, Q = cut(D_2$beta,breaks = quantile(D_2$beta,  probs = c(0, 2, 4, 6, 8, 10)/10), labels = c(1, 2, 3, 4,5),include.lowest=TRUE))

q_beta <- quantile(D_2$beta, probs=c(0.05, 0.95), na.rm=TRUE)
D_2 <- D_2[D_2$beta>q_beta[1],]
D_2 <- D_2[D_2$beta<q_beta[2],]

Q2 <- matrix(NA, ncol= 2, nrow = 5)
for (i in 1:5){
  Q2[i,] <- c(mean(D_2[D_2$Q==i,]$inc),median(D_2[D_2$Q==i,]$inc))
}

colnames(Q2) <- c("Mean", "Median")
rownames(Q2) <- c("1", "2", "3", "4", "5")

#Q3
reg_tot <- lm(cons ~ 1  + inc + tot, data=total_D)
stargazer(reg_tot$coefficients, summary=FALSE)

# -----Q4-------
# ------URBAN---
data <- data_for_Q4[data_for_Q4$urban==1,]

data_old    = data 
data_old$lc = log(data_old$ctotal)

data_old    = data_old[is.na(data_old$lc)==0,]
data_old    = data_old[is.infinite(data_old$lc)==0,]

# drop households which are only observed one year
#data = data[data$hh %in% change[change[,1]<50,2],]
data = data[data$counthh>2,]

D <- data.frame(cbind(data$hh,data$year,data$ctotal,data$inctotal,data$familysize,data$age,data$age_sq, data$region,data$ethnic,data$female,data$urban))
colnames(D) <- c('id', 'year', 'cons', 'inc', 'N', 'a','a2','r','e','s','u')

D$cons = log(D$cons)
D$inc_or  = D$inc
D$inc  = log(D$inc)
D$N    = log(D$N)

D <- D[rowSums(is.na(D))==0,]
D <- D[is.infinite(D$inc)==0,]
D <- D[is.infinite(D$cons)==0,]

D$y09 <- as.numeric(D$year == 2009)
D$y10 <- as.numeric(D$year == 2010)
D$y11 <- as.numeric(D$year == 2011)
D$y12 <- as.numeric(D$year == 2012)
D$y13 <- as.numeric(D$year == 2013)
D$y14 <- as.numeric(D$year == 2014)

f.eth = factor(D$e)
f.r = factor(D$r)

# compute residuals
D$c <- lm(cons ~ 1 + a +a2 + N + y09 + y10 + y11 + y12 +y13 +y14 +f.r+ s+ u + f.eth, data = D)$residuals
D$i <- lm(inc ~ 1 + a +a2 + N + y09 + y10 + y11 + y12 +y13 +y14 +f.r + s+ u + f.eth, data = D)$residuals

D$tot_cons = 0
j = 0
for (t in unique(data$year)){
  j = j+1
  for (r in unique(D$r)){
    #tot_cons[j] = mean(lc_old[data_old$year == t], na.rm = T)
    D[(D$year == t)&(D$r==r),]$tot_cons = log(mean(data_old$ctotal[(data_old$year == t)&(data_old$region==r)], na.rm = T))
  }
}

# Keep only if we have data on income (they will drop anyways in the regression)
D=D[is.na(D$i)==0,]
D=D[is.infinite(D$c)==0,]
#DD <- D[,c(1,2,8,18,19,20)]
DD <- D[,c(1:4,8,12,19,20,21)]

DD$y = 0

for (i in unique(DD$id)){
  DD[DD$id == i,]$y = sum(DD[DD$id == i,]$year)
}

DD <- DD[DD$y>5000,]

####

phi <- beta <- matrix(NA, ncol = 1, nrow =length(unique(DD$id)))
total_D <- matrix(NA, ncol= 6, nrow = 1)
colnames(total_D) <- c('id','year','region', 'cons', 'inc', 'tot')
D_2 <- matrix(NA, ncol= 4, nrow = 1)
colnames(D_2) <- c('id', 'inc', 'region', 'beta')

ii = 0
for (i in unique(DD$id)){
  ii=ii+1
  dat <- DD[DD$id==i,]
  dat <- dat[order(dat$year),]
  dat$tdiff[2:nrow(dat)] <- diff(dat$year)
  # this is the annualisation of changes
  dat$cdiff[2:nrow(dat)] <- diff(dat$c)/dat$tdiff[2:nrow(dat)]
  dat$idiff[2:nrow(dat)] <- diff(dat$i)/dat$tdiff[2:nrow(dat)]
  dat$tot_consdiff[2:nrow(dat)] <- diff(dat$tot_cons)/dat$tdiff[2:nrow(dat)]
  original <- nrow(dat)
  # impute the rows that are missing (balanced the panel)
  for(j in 2:original){
    if(unlist(dat$tdiff[j])>1){
      mat <- data.frame(matrix(rep(dat[j,], each=dat$tdiff[[j]]), 
                               nrow=dat$tdiff[[j]]))
      colnames(mat) <- colnames(dat)
      dat <- rbind(dat, mat)
    }else{
      dat <- rbind(dat, dat[j,])
    }
  }
  dat <- dat[(original+1):nrow(dat),]
  dat$cdiff <- unlist(dat$cdiff)
  dat$idiff <- unlist(dat$idiff)
  dat$tot_consdiff <- unlist(dat$tot_consdiff)
  dat$inc_or <- unlist(dat$inc_or)
  dat$r <- unlist(dat$r)
  dat$id <- unlist(dat$id)
  
  
  reg <- lm(cdiff ~ 1  + idiff + tot_consdiff, data=dat)
  beta[ii] = reg$coeff[2]
  phi[ii] = reg$coeff[3]
  
  #NEW
  dat2 <- data.frame(t(c(colMeans(dat[,c(1,5,6)]),beta[ii])))
  colnames(dat2) <- c('id', 'region', 'inc', 'beta')
  
  D_2 <- rbind(D_2,dat2)
  
  dat3 <- data.frame(dat[-1,c(1,2,5,12:14)])
  colnames(dat3) <- c('id','year','region', 'cons', 'inc', 'tot')
  
  total_D <- rbind(total_D,dat3)
}
total_D <- total_D[-1,] #For Q3
D_2 <- D_2[-1,]

reg_u <- lm(cons ~ 1  + inc + tot, data=total_D)
reg_u$coefficients

beta <- as.data.frame(beta)
q_beta <- quantile(beta$V1, probs=c(0.05, 0.95), na.rm=TRUE)
beta_plot_u <- as.data.frame(beta[beta$V1>q_beta[1],])
beta_plot_u <- as.data.frame(beta_plot_u[beta_plot_u<q_beta[2]])
colnames(beta_plot_u) <- "beta"

phi <- as.data.frame(phi)
q_phi <- quantile(phi$V1, probs=c(0.05, 0.95), na.rm=TRUE)
phi_plot_u <- as.data.frame(phi[phi$V1>q_phi[1],])
phi_plot_u <- as.data.frame(phi_plot_u[phi_plot_u<q_phi[2]])
colnames(phi_plot_u) <- "phi"

D_2_org = D_2
D_2 <- transform(D_2, Q = cut(D_2$inc,breaks = quantile(D_2$inc,  probs = c(0, 2, 4, 6, 8, 10)/10), labels = c(1, 2, 3, 4,5),include.lowest=TRUE))

q_beta <- quantile(D_2$beta, probs=c(0.05, 0.95), na.rm=TRUE)
D_2 <- D_2[D_2$beta>q_beta[1],]
D_2 <- D_2[D_2$beta<q_beta[2],]


Q2 <- matrix(NA, ncol= 2, nrow = 5)
for (i in 1:5){
  Q2[i,] <- c(mean(D_2[D_2$Q==i,]$beta),median(D_2[D_2$Q==i,]$beta))
}

colnames(Q2) <- c("Mean", "Median")
rownames(Q2) <- c("1", "2", "3", "4", "5")

D_2 = D_2_org
D_2 <- transform(D_2, Q = cut(D_2$beta,breaks = quantile(D_2$beta,  probs = c(0, 2, 4, 6, 8, 10)/10), labels = c(1, 2, 3, 4,5),include.lowest=TRUE))

q_beta <- quantile(D_2$beta, probs=c(0.05, 0.95), na.rm=TRUE)
D_2 <- D_2[D_2$beta>q_beta[1],]
D_2 <- D_2[D_2$beta<q_beta[2],]

Q2 <- matrix(NA, ncol= 2, nrow = 5)
for (i in 1:5){
  Q2[i,] <- c(mean(D_2[D_2$Q==i,]$inc),median(D_2[D_2$Q==i,]$inc))
}

colnames(Q2) <- c("Mean", "Median")
rownames(Q2) <- c("1", "2", "3", "4", "5")



# ------RURAL------
data <- data_for_Q4[data_for_Q4$urban==0,]
# Copy all stuff again 

data_old    = data 
data_old$lc = log(data_old$ctotal)

data_old    = data_old[is.na(data_old$lc)==0,]
data_old    = data_old[is.infinite(data_old$lc)==0,]

# drop households which are only observed one year
#data = data[data$hh %in% change[change[,1]<50,2],]
data = data[data$counthh>2,]

D <- data.frame(cbind(data$hh,data$year,data$ctotal,data$inctotal,data$familysize,data$age,data$age_sq, data$region,data$ethnic,data$female,data$urban))
colnames(D) <- c('id', 'year', 'cons', 'inc', 'N', 'a','a2','r','e','s','u')

D$cons = log(D$cons)
D$inc_or  = D$inc
D$inc  = log(D$inc)
D$N    = log(D$N)

D <- D[rowSums(is.na(D))==0,]
D <- D[is.infinite(D$inc)==0,]
D <- D[is.infinite(D$cons)==0,]

D$y09 <- as.numeric(D$year == 2009)
D$y10 <- as.numeric(D$year == 2010)
D$y11 <- as.numeric(D$year == 2011)
D$y12 <- as.numeric(D$year == 2012)
D$y13 <- as.numeric(D$year == 2013)
D$y14 <- as.numeric(D$year == 2014)

f.eth = factor(D$e)
f.r = factor(D$r)

# compute residuals
D$c <- lm(cons ~ 1 + a +a2 + N + y09 + y10 + y11 + y12 +y13 +y14 +f.r+ s+ u + f.eth, data = D)$residuals
D$i <- lm(inc ~ 1 + a +a2 + N + y09 + y10 + y11 + y12 +y13 +y14 +f.r + s+ u + f.eth, data = D)$residuals

D$tot_cons = 0
j = 0
for (t in unique(data$year)){
  j = j+1
  for (r in unique(D$r)){
    #tot_cons[j] = mean(lc_old[data_old$year == t], na.rm = T)
    D[(D$year == t)&(D$r==r),]$tot_cons = log(mean(data_old$ctotal[(data_old$year == t)&(data_old$region==r)], na.rm = T))
  }
}

# Keep only if we have data on income (they will drop anyways in the regression)
D=D[is.na(D$i)==0,]
D=D[is.infinite(D$c)==0,]
#DD <- D[,c(1,2,8,18,19,20)]
DD <- D[,c(1:4,8,12,19,20,21)]

DD$y = 0

for (i in unique(DD$id)){
  DD[DD$id == i,]$y = sum(DD[DD$id == i,]$year)
}

DD <- DD[DD$y>5000,]

####

phi <- beta <- matrix(NA, ncol = 1, nrow =length(unique(DD$id)))
total_D <- matrix(NA, ncol= 6, nrow = 1)
colnames(total_D) <- c('id','year','region', 'cons', 'inc', 'tot')
D_2 <- matrix(NA, ncol= 4, nrow = 1)
colnames(D_2) <- c('id', 'inc', 'region', 'beta')

ii = 0
for (i in unique(DD$id)){
  ii=ii+1
  dat <- DD[DD$id==i,]
  dat <- dat[order(dat$year),]
  dat$tdiff[2:nrow(dat)] <- diff(dat$year)
  # this is the annualisation of changes
  dat$cdiff[2:nrow(dat)] <- diff(dat$c)/dat$tdiff[2:nrow(dat)]
  dat$idiff[2:nrow(dat)] <- diff(dat$i)/dat$tdiff[2:nrow(dat)]
  dat$tot_consdiff[2:nrow(dat)] <- diff(dat$tot_cons)/dat$tdiff[2:nrow(dat)]
  original <- nrow(dat)
  # impute the rows that are missing (balanced the panel)
  for(j in 2:original){
    if(unlist(dat$tdiff[j])>1){
      mat <- data.frame(matrix(rep(dat[j,], each=dat$tdiff[[j]]), 
                               nrow=dat$tdiff[[j]]))
      colnames(mat) <- colnames(dat)
      dat <- rbind(dat, mat)
    }else{
      dat <- rbind(dat, dat[j,])
    }
  }
  dat <- dat[(original+1):nrow(dat),]
  dat$cdiff <- unlist(dat$cdiff)
  dat$idiff <- unlist(dat$idiff)
  dat$tot_consdiff <- unlist(dat$tot_consdiff)
  dat$inc_or <- unlist(dat$inc_or)
  dat$r <- unlist(dat$r)
  dat$id <- unlist(dat$id)
  
  
  reg <- lm(cdiff ~ 1  + idiff + tot_consdiff, data=dat)
  beta[ii] = reg$coeff[2]
  phi[ii] = reg$coeff[3]
  
  #NEW
  dat2 <- data.frame(t(c(colMeans(dat[,c(1,5,6)]),beta[ii])))
  colnames(dat2) <- c('id', 'region', 'inc', 'beta')
  
  D_2 <- rbind(D_2,dat2)
  
  dat3 <- data.frame(dat[-1,c(1,2,5,12:14)])
  colnames(dat3) <- c('id','year','region', 'cons', 'inc', 'tot')
  
  total_D <- rbind(total_D,dat3)
}
total_D <- total_D[-1,] #For Q3
D_2 <- D_2[-1,]

reg_r <- lm(cons ~ 1  + inc + tot, data=total_D)
reg_r$coefficients


beta <- as.data.frame(beta)
q_beta <- quantile(beta$V1, probs=c(0.05, 0.95), na.rm=TRUE)
beta_plot_r <- as.data.frame(beta[beta$V1>q_beta[1],])
beta_plot_r <- as.data.frame(beta_plot_r[beta_plot_r<q_beta[2]])
colnames(beta_plot_r) <- "beta"


phi <- as.data.frame(phi)
q_phi <- quantile(phi$V1, probs=c(0.05, 0.95), na.rm=TRUE)
phi_plot_r <- as.data.frame(phi[phi$V1>q_phi[1],])
phi_plot_r <- as.data.frame(phi_plot_r[phi_plot_r<q_phi[2]])
colnames(phi_plot_r) <- "phi"

D_2_org = D_2
D_2 <- transform(D_2, Q = cut(D_2$inc,breaks = quantile(D_2$inc,  probs = c(0, 2, 4, 6, 8, 10)/10), labels = c(1, 2, 3, 4,5),include.lowest=TRUE))

q_beta <- quantile(D_2$beta, probs=c(0.05, 0.95), na.rm=TRUE)
D_2 <- D_2[D_2$beta>q_beta[1],]
D_2 <- D_2[D_2$beta<q_beta[2],]


Q2 <- matrix(NA, ncol= 2, nrow = 5)
for (i in 1:5){
  Q2[i,] <- c(mean(D_2[D_2$Q==i,]$beta),median(D_2[D_2$Q==i,]$beta))
}

colnames(Q2) <- c("Mean", "Median")
rownames(Q2) <- c("1", "2", "3", "4", "5")

D_2 = D_2_org
D_2 <- transform(D_2, Q = cut(D_2$beta,breaks = quantile(D_2$beta,  probs = c(0, 2, 4, 6, 8, 10)/10), labels = c(1, 2, 3, 4,5),include.lowest=TRUE))

q_beta <- quantile(D_2$beta, probs=c(0.05, 0.95), na.rm=TRUE)
D_2 <- D_2[D_2$beta>q_beta[1],]
D_2 <- D_2[D_2$beta<q_beta[2],]

Q2 <- matrix(NA, ncol= 2, nrow = 5)
for (i in 1:5){
  Q2[i,] <- c(mean(D_2[D_2$Q==i,]$inc),median(D_2[D_2$Q==i,]$inc))
}

colnames(Q2) <- c("Mean", "Median")
rownames(Q2) <- c("1", "2", "3", "4", "5")


# ------GRAPHS and RESULTS -----

beta_plot_u$u <- 'Urban'
beta_plot_r$u <- 'Rural'
beta_comb     <- rbind(beta_plot_u,beta_plot_r)

phi_plot_u$u <- 'Urban'
phi_plot_r$u <- 'Rural'
phi_comb     <- rbind(phi_plot_u,phi_plot_r)

ggplot(beta_comb, aes(beta, fill = u)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')+
  scale_fill_discrete(name = "")+
  xlab('Beta')+ylab('Density')

ggplot(phi_comb, aes(phi, fill = u)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')+
  scale_fill_discrete(name = "")+
  xlab('Phi')+ylab('Density')

results <- matrix(0L, nrow = 4, ncol = 2)
colnames(results) <- c('Urban','Rural')
rownames(results) <- c('Mean   (beta)','Median (beta)','Mean   (phi)','Median (phi)')

results[1,1] <- mean(beta_plot_u[,1], na.rm=TRUE)
results[2,1] <- median(beta_plot_u[,1], na.rm=TRUE)

results[3,1] <- mean(phi_plot_u[,1], na.rm=TRUE)
results[4,1] <- median(phi_plot_u[,1], na.rm=TRUE)

results[1,2] <- mean(beta_plot_r[,1], na.rm=TRUE)
results[2,2] <- median(beta_plot_r[,1], na.rm=TRUE)

results[3,2] <- mean(phi_plot_r[,1], na.rm=TRUE)
results[4,2] <- median(phi_plot_r[,1], na.rm=TRUE)

colnames(results) <- c("Rural", "Urban")
rownames(results) <- c("Mean beta", "Median beta", "Mean phi", "Median phi")
stargazer(reg_u$coefficients, summary=FALSE)
stargazer(reg_r$coefficients, summary=FALSE)
