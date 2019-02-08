# Libraries
library(foreign)
library(stargazer)
library(ggplot2)
library(xlsx)
library(MASS)

rm(list = ls())


set.seed(9999)

# QUESTION 1 ##########################################################################
n <- 1000
sigmasq_epsilon <- 0.2
sigmasq_u <- 0.2
beta <- 0.99^(1/12)
t <- 40

epsilon <- matrix(NA, nrow=n, ncol=t)
for(i in 1:t){
  epsilon[,i] <- rlnorm(n, meanlog = 0, sdlog = sqrt(sigmasq_epsilon))
}

z <- rlnorm(n, meanlog = 0, sdlog = sqrt(sigmasq_u))

eta <- 1

u <- function(c, eta){
  if (eta>1){
    utility <- (c^(1-eta))/(1-eta)
  }
  if (eta==1){
    utility <- log(c)
  }
  return(utility)
}

#det_seas <- read.xlsx('data.xlsx', sheetIndex = 1)
#rownames(det_seas) <- det_seas[,1]
#det_seas <- det_seas[,2:4]



#mid  <- c(-0.147,-0.37,0.141,.131,.09,.058,.036,.036,.036,.002,-.033,-.082)
#high <- c(-.293,-.739,.282,.262,.180,.116,.072,.072,.072,.004,-.066,-.164)
#low  <- c(-.073,-.185,.071,.066,.045,.029,.018,.018,.018,.001,-.017,-.041)
mid  <- c(864,691,1151,1140,1094,1060,1037,1037,1037,1002,968,921)/1000
high <- c(727,381,1303,1280,1188,1119,1073,1073,1073,1004,935,843)/1000
low  <- c(932,845,1076,1070,1047,1030,1018,1018,1018,1001,984,961)/1000
det_seas = cbind(mid,high,low)

#mid_s  <- c(85,68,290,283,273,273,239,205,188,188,171,137)/1000
#high_s <- c(171,137,580,567,546,546,478,410,376,376,341,273)/1000
#low_s  <- c(43,34,145,142,137,137,119,105,94,94,85,68)/1000
mid_s  <- c(85,68,290,283,273,273,239,205,188,188,171,137)/1000
high_s <- c(171,137,580,567,546,546,478,410,376,376,341,273)/1000
low_s  <- c(43,34,145,142,137,137,119,102,94,94,85,68)/1000
stoch_seas <- cbind(mid_s,high_s,low_s)


# eta=1
sum_welfare_a <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc <- matrix(0, nrow=n, ncol=3)

sum_welfare_nscr <- matrix(0, nrow=n, ncol=3)

sum_welfare_b <- matrix(0, nrow=n, ncol=3)

sum_beta <- 0

for(s in 1:3){
  for(i in 1:n){
    for(j in 1:40){
      for(m in 1:12){
        sum_welfare_a[i,s] <- sum_welfare_a[i,s]+beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_sc[i,s] <- sum_welfare_sc[i,s]+beta^(12*j)*beta^(m-1)*u(z[i]*exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_nscr[i,s] <- sum_welfare_nscr[i,s]+beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s],eta)
        
        sum_welfare_b[i,s] <- sum_welfare_b[i,s]+beta^(12*j)*beta^(m-1)*u(z[i],eta)
      }
    }
  }
}

for(j in 1:40){
  for(m in 1:12){
    sum_beta <- sum_beta+beta^(12*j)*beta^(m-1) # For "undiscounting"
  }
}

welfare_sc <- exp((sum_welfare_sc-sum_welfare_a)/sum_beta)-1
sc <- data.frame(apply(welfare_sc, 2, mean, na.rm = TRUE),
apply(welfare_sc, 2, median, na.rm = TRUE),
t(apply(welfare_sc, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
#colnames(sc) <- c("Mean", "Median", "Min", "5%", "20%", "40%", "60%", "80%", "95%", "Max")
sc <- data.frame(sc[c(3,1,2),])
rownames(sc) <- c("Low", "Middle", "High")
colnames(sc) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")
stargazer(sc, summary = FALSE, title = "Seasonality component")

welfare_nscr <- exp((sum_welfare_nscr-sum_welfare_a)/sum_beta)-1
nscr <- data.frame(apply(welfare_nscr, 2, mean, na.rm = TRUE),
apply(welfare_nscr, 2, median, na.rm = TRUE),
t(apply(welfare_nscr, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
nscr <- data.frame(nscr[c(3,1,2),])
colnames(nscr) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")
#nscr <- nscr[1,]
rownames(nscr) <- c("Low", "Middle", "High")
stargazer(nscr, summary = FALSE, title = "Risk component")

welfare_b <- exp((sum_welfare_b-sum_welfare_a)/sum_beta)-1
b <- data.frame(apply(welfare_b, 2, mean, na.rm = TRUE),
                   apply(welfare_b, 2, median, na.rm = TRUE),
                   t(apply(welfare_b, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
colnames(b) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")
b <- b[c(3,1,2),]
rownames(b) <- c("Low", "Middle", "High")
stargazer(b, summary = FALSE, title = "Both seasonality and risk components")

stargazer(sc, summary = FALSE, title = "No seasonality")
stargazer(nscr, summary = FALSE, title = "No risk")
stargazer(b, summary = FALSE, title = "No seasonality and no risk")


# eta=2
eta=2

sum_welfare_a <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc <- matrix(0, nrow=n, ncol=3)

sum_welfare_nscr <- matrix(0, nrow=n, ncol=3)

sum_welfare_b <- matrix(0, nrow=n, ncol=3)

sum_beta <- 0

for(s in 1:3){
  for(i in 1:n){
    for(j in 1:40){
      for(m in 1:12){
        sum_welfare_a[i,s] <- sum_welfare_a[i,s]+beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_sc[i,s] <- sum_welfare_sc[i,s]+beta^(12*j)*beta^(m-1)*u(z[i]*exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_nscr[i,s] <- sum_welfare_nscr[i,s]+beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s],eta)
        
        sum_welfare_b[i,s] <- sum_welfare_b[i,s]+beta^(12*j)*beta^(m-1)*u(z[i],eta)
      }
    }
  }
}

for(j in 1:40){
  for(m in 1:12){
    sum_beta <- sum_beta+beta^(12*j)*beta^(m-1)
  }
}

welfare_sc <- (sum_welfare_sc/sum_welfare_a)^(1/(1-eta))-1
sc <- data.frame(apply(welfare_sc, 2, mean, na.rm = TRUE),
                 apply(welfare_sc, 2, median, na.rm = TRUE),
                 t(apply(welfare_sc, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
#colnames(sc) <- c("Mean", "Median", "Min", "5%", "20%", "40%", "60%", "80%", "95%", "Max")
sc <- data.frame(sc[c(3,1,2),])
rownames(sc) <- c("Low", "Middle", "High")
colnames(sc) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")
stargazer(sc, summary = FALSE, title = "Seasonality component")

welfare_nscr <- (sum_welfare_nscr/sum_welfare_a)^(1/(1-eta))-1
nscr <- data.frame(apply(welfare_nscr, 2, mean, na.rm = TRUE),
                   apply(welfare_nscr, 2, median, na.rm = TRUE),
                   t(apply(welfare_nscr, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
nscr <- data.frame(nscr[c(3,1,2),])
colnames(nscr) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")
#nscr <- nscr[1,]
rownames(nscr) <- c("Low", "Middle", "High")
stargazer(nscr, summary = FALSE, title = "Risk component")

welfare_b <- (sum_welfare_b/sum_welfare_a)^(1/(1-eta))-1
b <- data.frame(apply(welfare_b, 2, mean, na.rm = TRUE),
                apply(welfare_b, 2, median, na.rm = TRUE),
                t(apply(welfare_b, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
colnames(b) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")
b <- b[c(3,1,2),]
rownames(b) <- c("Low", "Middle", "High")
stargazer(b, summary = FALSE, title = "Both seasonality and risk components")

stargazer(sc, summary = FALSE, title = "No seasonality")
stargazer(nscr, summary = FALSE, title = "No risk")
stargazer(b, summary = FALSE, title = "No seasonality and no risk")



# eta=4
eta=4

sum_welfare_a <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc <- matrix(0, nrow=n, ncol=3)

sum_welfare_nscr <- matrix(0, nrow=n, ncol=3)

sum_welfare_b <- matrix(0, nrow=n, ncol=3)

sum_beta <- 0

for(s in 1:3){
  for(i in 1:n){
    for(j in 1:40){
      for(m in 1:12){
        sum_welfare_a[i,s] <- sum_welfare_a[i,s]+beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_sc[i,s] <- sum_welfare_sc[i,s]+beta^(12*j)*beta^(m-1)*u(z[i]*exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_nscr[i,s] <- sum_welfare_nscr[i,s]+beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s],eta)
        
        sum_welfare_b[i,s] <- sum_welfare_b[i,s]+beta^(12*j)*beta^(m-1)*u(z[i],eta)
      }
    }
  }
}

for(j in 1:40){
  for(m in 1:12){
    sum_beta <- sum_beta+beta^(12*j)*beta^(m-1)
  }
}

welfare_sc <- (sum_welfare_sc/sum_welfare_a)^(1/(1-eta))-1
sc <- data.frame(apply(welfare_sc, 2, mean, na.rm = TRUE),
                 apply(welfare_sc, 2, median, na.rm = TRUE),
                 t(apply(welfare_sc, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
#colnames(sc) <- c("Mean", "Median", "Min", "5%", "20%", "40%", "60%", "80%", "95%", "Max")
sc <- data.frame(sc[c(3,1,2),])
rownames(sc) <- c("Low", "Middle", "High")
colnames(sc) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")
stargazer(sc, summary = FALSE, title = "Seasonality component")

welfare_nscr <- (sum_welfare_nscr/sum_welfare_a)^(1/(1-eta))-1
nscr <- data.frame(apply(welfare_nscr, 2, mean, na.rm = TRUE),
                   apply(welfare_nscr, 2, median, na.rm = TRUE),
                   t(apply(welfare_nscr, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
nscr <- data.frame(nscr[c(3,1,2),])
colnames(nscr) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")
#nscr <- nscr[1,]
rownames(nscr) <- c("Low", "Middle", "High")
stargazer(nscr, summary = FALSE, title = "Risk component")

welfare_b <- (sum_welfare_b/sum_welfare_a)^(1/(1-eta))-1
b <- data.frame(apply(welfare_b, 2, mean, na.rm = TRUE),
                apply(welfare_b, 2, median, na.rm = TRUE),
                t(apply(welfare_b, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
colnames(b) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")
b <- b[c(3,1,2),]
rownames(b) <- c("Low", "Middle", "High")
stargazer(b, summary = FALSE, title = "Both seasonality and risk components")

stargazer(sc, summary = FALSE, title = "No seasonality")
stargazer(nscr, summary = FALSE, title = "No risk")
stargazer(b, summary = FALSE, title = "No seasonality and no risk")


######## 1.2 ######

set.seed(123345)

mid_s  <- c(85,68,290,283,273,273,239,205,188,188,171,137)/1000
high_s <- c(171,137,580,567,546,546,478,410,376,376,341,273)/1000
low_s  <- c(43,34,145,142,137,137,119,102,94,94,85,68)/1000
stoch_seas <- cbind(mid_s,high_s,low_s)

epsilon_m <- matrix(NA, nrow=12, ncol=3)
for(i in 1:3){
  epsilon_m[,i] <- rlnorm(12, meanlog = 0, sdlog = sqrt(stoch_seas[,i]))
}


# eta=1
eta=1
sum_welfare_a <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc1 <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc2 <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc3 <- matrix(0, nrow=n, ncol=3)

sum_welfare_nscr <- matrix(0, nrow=n, ncol=3)

sum_welfare_b <- matrix(0, nrow=n, ncol=3)

sum_beta <- 0

for(s in 1:3){
  for(i in 1:n){
    for(j in 1:40){
      for(m in 1:12){
        sum_welfare_a[i,s] <- sum_welfare_a[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*exp(-stoch_seas[m,s]/2)*
                                     epsilon_m[m,s]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)

        sum_welfare_sc1[i,s] <- sum_welfare_sc1[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*exp(-stoch_seas[m,s]/2)*epsilon_m[m,s]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)

        sum_welfare_sc2[i,s] <- sum_welfare_sc2[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)

        sum_welfare_sc3[i,s] <- sum_welfare_sc3[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)

        sum_welfare_nscr[i,s] <- sum_welfare_nscr[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*exp(-stoch_seas[m,s]/2)*
                                     epsilon_m[m,s],eta)

        sum_welfare_b[i,s] <- sum_welfare_b[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i],eta)
      }
    }
  }
}

for(j in 1:40){
  for(m in 1:12){
    sum_beta <- sum_beta+beta^(12*j)*beta^(m-1)
  }
}

welfare_sc1 <- exp((sum_welfare_sc1-sum_welfare_a)/sum_beta)-1
sc1 <- data.frame(apply(welfare_sc1, 2, mean, na.rm = TRUE),
                 apply(welfare_sc1, 2, median, na.rm = TRUE),
                 t(apply(welfare_sc1, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
sc1 <- data.frame(sc1[c(3,1,2),])
rownames(sc1) <- c("Low", "Middle", "High")
colnames(sc1) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_sc2 <- exp((sum_welfare_sc2-sum_welfare_a)/sum_beta)-1
sc2 <- data.frame(apply(welfare_sc2, 2, mean, na.rm = TRUE),
                  apply(welfare_sc2, 2, median, na.rm = TRUE),
                  t(apply(welfare_sc2, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
#colnames(sc) <- c("Mean", "Median", "Min", "5%", "20%", "40%", "60%", "80%", "95%", "Max")
sc2 <- data.frame(sc2[c(3,1,2),])
rownames(sc2) <- c("Low", "Middle", "High")
colnames(sc2) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_sc3 <- exp((sum_welfare_sc3-sum_welfare_a)/sum_beta)-1
sc3 <- data.frame(apply(welfare_sc3, 2, mean, na.rm = TRUE),
                  apply(welfare_sc3, 2, median, na.rm = TRUE),
                  t(apply(welfare_sc3, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
sc3 <- data.frame(sc3[c(3,1,2),])
rownames(sc3) <- c("Low", "Middle", "High")
colnames(sc3) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_nscr <- exp((sum_welfare_nscr-sum_welfare_a)/sum_beta)-1
nscr <- data.frame(apply(welfare_nscr, 2, mean, na.rm = TRUE),
                   apply(welfare_nscr, 2, median, na.rm = TRUE),
                   t(apply(welfare_nscr, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
nscr <- data.frame(nscr[c(3,1,2),])
rownames(nscr) <- c("Low", "Middle", "High")
colnames(nscr) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_b <- exp((sum_welfare_b-sum_welfare_a)/sum_beta)-1
b <- data.frame(apply(welfare_b, 2, mean, na.rm = TRUE),
                apply(welfare_b, 2, median, na.rm = TRUE),
                t(apply(welfare_b, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
b <- data.frame(b[c(3,1,2),])
rownames(b) <- c("Low", "Middle", "High")
colnames(b) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

stargazer(sc1, summary = FALSE, title = "No deterministic seasonality")
stargazer(sc2, summary = FALSE, title = "No stochastic seasonality")
stargazer(sc3, summary = FALSE, title = "No seasonality")
stargazer(nscr, summary = FALSE, title = "No risk")
stargazer(b, summary = FALSE, title = "No seasonality and no risk")

# eta=2
eta=2
sum_welfare_a <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc1 <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc2 <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc3 <- matrix(0, nrow=n, ncol=3)

sum_welfare_nscr <- matrix(0, nrow=n, ncol=3)

sum_welfare_b <- matrix(0, nrow=n, ncol=3)

sum_beta <- 0

for(s in 1:3){
  for(i in 1:n){
    for(j in 1:40){
      for(m in 1:12){
        sum_welfare_a[i,s] <- sum_welfare_a[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*exp(-stoch_seas[m,s]/2)*
                                     epsilon_m[m,s]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_sc1[i,s] <- sum_welfare_sc1[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*exp(-stoch_seas[m,s]/2)*epsilon_m[m,s]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_sc2[i,s] <- sum_welfare_sc2[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_sc3[i,s] <- sum_welfare_sc3[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_nscr[i,s] <- sum_welfare_nscr[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*exp(-stoch_seas[m,s]/2)*
                                     epsilon_m[m,s],eta)
        
        sum_welfare_b[i,s] <- sum_welfare_b[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i],eta)
      }
    }
  }
}

for(j in 1:40){
  for(m in 1:12){
    sum_beta <- sum_beta+beta^(12*j)*beta^(m-1)
  }
}

welfare_sc1 <- (sum_welfare_sc1/sum_welfare_a)^(1/(1-eta))-1
sc1 <- data.frame(apply(welfare_sc1, 2, mean, na.rm = TRUE),
                  apply(welfare_sc1, 2, median, na.rm = TRUE),
                  t(apply(welfare_sc1, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
sc1 <- data.frame(sc1[c(3,1,2),])
rownames(sc1) <- c("Low", "Middle", "High")
colnames(sc1) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_sc2 <- (sum_welfare_sc2/sum_welfare_a)^(1/(1-eta))-1
sc2 <- data.frame(apply(welfare_sc2, 2, mean, na.rm = TRUE),
                  apply(welfare_sc2, 2, median, na.rm = TRUE),
                  t(apply(welfare_sc2, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
#colnames(sc) <- c("Mean", "Median", "Min", "5%", "20%", "40%", "60%", "80%", "95%", "Max")
sc2 <- data.frame(sc2[c(3,1,2),])
rownames(sc2) <- c("Low", "Middle", "High")
colnames(sc2) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_sc3 <- (sum_welfare_sc3/sum_welfare_a)^(1/(1-eta))-1
sc3 <- data.frame(apply(welfare_sc3, 2, mean, na.rm = TRUE),
                  apply(welfare_sc3, 2, median, na.rm = TRUE),
                  t(apply(welfare_sc3, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
sc3 <- data.frame(sc3[c(3,1,2),])
rownames(sc3) <- c("Low", "Middle", "High")
colnames(sc3) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_nscr <- (sum_welfare_nscr/sum_welfare_a)^(1/(1-eta))-1
nscr <- data.frame(apply(welfare_nscr, 2, mean, na.rm = TRUE),
                   apply(welfare_nscr, 2, median, na.rm = TRUE),
                   t(apply(welfare_nscr, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
nscr <- data.frame(nscr[c(3,1,2),])
rownames(nscr) <- c("Low", "Middle", "High")
colnames(nscr) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_b <- (sum_welfare_b/sum_welfare_a)^(1/(1-eta))-1
b <- data.frame(apply(welfare_b, 2, mean, na.rm = TRUE),
                apply(welfare_b, 2, median, na.rm = TRUE),
                t(apply(welfare_b, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
b <- data.frame(b[c(3,1,2),])
rownames(b) <- c("Low", "Middle", "High")
colnames(b) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

stargazer(sc1, summary = FALSE, title = "No deterministic seasonality")
stargazer(sc2, summary = FALSE, title = "No stochastic seasonality")
stargazer(sc3, summary = FALSE, title = "No seasonality")
stargazer(nscr, summary = FALSE, title = "No risk")
stargazer(b, summary = FALSE, title = "No seasonality and no risk")


# eta=4
eta=4
sum_welfare_a <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc1 <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc2 <- matrix(0, nrow=n, ncol=3)

sum_welfare_sc3 <- matrix(0, nrow=n, ncol=3)

sum_welfare_nscr <- matrix(0, nrow=n, ncol=3)

sum_welfare_b <- matrix(0, nrow=n, ncol=3)

sum_beta <- 0

for(s in 1:3){
  for(i in 1:n){
    for(j in 1:40){
      for(m in 1:12){
        sum_welfare_a[i,s] <- sum_welfare_a[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*exp(-stoch_seas[m,s]/2)*
                                     epsilon_m[m,s]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_sc1[i,s] <- sum_welfare_sc1[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*exp(-stoch_seas[m,s]/2)*epsilon_m[m,s]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_sc2[i,s] <- sum_welfare_sc2[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_sc3[i,s] <- sum_welfare_sc3[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*
                                     exp(-sigmasq_epsilon/2)*epsilon[i,j],eta)
        
        sum_welfare_nscr[i,s] <- sum_welfare_nscr[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i]*det_seas[m,s]*exp(-stoch_seas[m,s]/2)*
                                     epsilon_m[m,s],eta)
        
        sum_welfare_b[i,s] <- sum_welfare_b[i,s]+
          beta^(12*j)*beta^(m-1)*u(z[i],eta)
      }
    }
  }
}

for(j in 1:40){
  for(m in 1:12){
    sum_beta <- sum_beta+beta^(12*j)*beta^(m-1)
  }
}

welfare_sc1 <- (sum_welfare_sc1/sum_welfare_a)^(1/(1-eta))-1
sc1 <- data.frame(apply(welfare_sc1, 2, mean, na.rm = TRUE),
                  apply(welfare_sc1, 2, median, na.rm = TRUE),
                  t(apply(welfare_sc1, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
sc1 <- data.frame(sc1[c(3,1,2),])
rownames(sc1) <- c("Low", "Middle", "High")
colnames(sc1) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_sc2 <- (sum_welfare_sc2/sum_welfare_a)^(1/(1-eta))-1
sc2 <- data.frame(apply(welfare_sc2, 2, mean, na.rm = TRUE),
                  apply(welfare_sc2, 2, median, na.rm = TRUE),
                  t(apply(welfare_sc2, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
#colnames(sc) <- c("Mean", "Median", "Min", "5%", "20%", "40%", "60%", "80%", "95%", "Max")
sc2 <- data.frame(sc2[c(3,1,2),])
rownames(sc2) <- c("Low", "Middle", "High")
colnames(sc2) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_sc3 <- (sum_welfare_sc3/sum_welfare_a)^(1/(1-eta))-1
sc3 <- data.frame(apply(welfare_sc3, 2, mean, na.rm = TRUE),
                  apply(welfare_sc3, 2, median, na.rm = TRUE),
                  t(apply(welfare_sc3, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
sc3 <- data.frame(sc3[c(3,1,2),])
rownames(sc3) <- c("Low", "Middle", "High")
colnames(sc3) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_nscr <- (sum_welfare_nscr/sum_welfare_a)^(1/(1-eta))-1
nscr <- data.frame(apply(welfare_nscr, 2, mean, na.rm = TRUE),
                   apply(welfare_nscr, 2, median, na.rm = TRUE),
                   t(apply(welfare_nscr, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
nscr <- data.frame(nscr[c(3,1,2),])
rownames(nscr) <- c("Low", "Middle", "High")
colnames(nscr) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

welfare_b <- (sum_welfare_b/sum_welfare_a)^(1/(1-eta))-1
b <- data.frame(apply(welfare_b, 2, mean, na.rm = TRUE),
                apply(welfare_b, 2, median, na.rm = TRUE),
                t(apply(welfare_b, 2, function(x) quantile(x,prob=c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95)))))
b <- data.frame(b[c(3,1,2),])
rownames(b) <- c("Low", "Middle", "High")
colnames(b) <- c("Mean", "Median", "5%", "20%", "40%", "60%", "80%", "95%")

stargazer(sc1, summary = FALSE, title = "No deterministic seasonality")
stargazer(sc2, summary = FALSE, title = "No stochastic seasonality")
stargazer(sc3, summary = FALSE, title = "No seasonality")
stargazer(nscr, summary = FALSE, title = "No risk")
stargazer(b, summary = FALSE, title = "No seasonality and no risk")
