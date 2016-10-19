
#######################################################################
## Test function
#######################################################################
ppv_equal_test <- function(n) {
  n0<-n[1:9]
  n1<-n[10:18]
  a1 <- sum(n1[c(1,2,3)])
  a2 <- sum(n1[c(1,4,7)])
  b1 <- sum(n0[c(5,6)])
  b2 <- sum(n0[c(5,8)])
  c1 <- sum(n1[c(4,5,6)])
  c2 <- sum(n1[c(2,5,8)])
  
  l <- (a1+b1+c1)*(a1-a2)
  S <- (a1+b1+c1)*((a1-a2)^2+(a1+b1+c1)*(sum(n1[c(2,3,4,7)]))+2*(a1-a2)*(n1[2]+n1[3]-n1[4]))
  chisq <- l^2/S
  p.value <- pchisq(chisq, df=1, lower.tail=F)
  return(list(chisq.statistics=chisq, p.value=p.value))
}
ppv_nonequal_test <- function(n) {
  n0<-n[1:9]
  n1<-n[10:18]
  a1 <- sum(n1[c(1,2,3)])
  a2 <- sum(n1[c(1,4,7)])
  b1 <- sum(n0[c(5,6)])
  b2 <- sum(n0[c(5,8)])
  c1 <- sum(n1[c(4,5,6)])
  c2 <- sum(n1[c(2,5,8)])
  
  l <- a1*c2+a1*b2-a2*c1-a2*b1
  S <- a1*(b2+c2)*(a1+b2+c2+2*n1[2])+a2*(b1+c1)*(a2+b1+c1+2*n1[4])-2*n1[1]*(b1+c1)*(b2+c2)-2*a1*a2*c(n1[5]+n0[5])
  chisq <- l^2/S
  p.value <- pchisq(chisq, df=1, lower.tail=F)
  return(list(chisq.statistics=chisq, p.value=p.value))
}

#######################################################################
## Sample from Multinomial distribution
#######################################################################

simul_data_generate <- function(num, N, p1, p0, seed) {
  
  ppv1 <- round(sum(p1[c(1,2,3)]) / sum(p1[c(1,2,3,4,5,6)], p0[c(5,6)]), 4)
  ppv2 <- round(sum(p1[c(1,4,7)]) / sum(p1[c(1,2,4,5,7,8)], p0[c(5,8)]), 4)
  xi1 <- round(sum(p1[1:6])/sum(p1), 4)
  xi2 <- round(sum(p1[c(1,4,7,2,5,8)])/sum(p1), 4)
  eta1 <- round(sum(p0[c(8,9)])/sum(p0[c(5,6,8,9)]), 4)
  eta2 <- round(sum(p0[c(6,9)])/sum(p0[c(5,6,8,9)]), 4)
  
  ## rmultinom ��?? generate
  #set.seed(seed)
  data <- rmultinom(num, N, c(p0,p1))
  return(list(c(ppv1,ppv2,xi1,xi2,eta1,eta2), data))  
}

#######################################################################
## Probability Generation
#######################################################################
# combination with repitition under constraints
# const.ind : constraints indicator
# const.val : constraints limit
# const : constraints direction
combn_rep <- function(n,r, const.ind=0, const.val=0, const=c("greater","less","both")) {
  if(n==0) out <- rep(0,r)
  if((n!=0 && const.ind==0) || (const.ind!=0 && const=="less" && const.val >= n)) out<-diff(c(0,sort(sample(0:n,(r-1), replace=T)), n))
  if(n!=0 && const.ind!=0 && const=="greater"){
    out <- rep(0,r)
    k1 <- n-const.val
    out[const.ind] <- const.val + diff(c(0,sort(sample(0:k1,1, replace=T)), k1))[1]
    k2 <- n-out[const.ind]
    out[out==0] <- diff(c(0,sort(sample(0:k2,(r-2), replace=T)), k2))
  }
  if(n!=0 && const.ind!=0 && const=="less" && const.val < n){
    out <- rep(0,r)
    k1 <- const.val
    out[const.ind] <- diff(c(0,sort(sample(0:k1,1, replace=T)), k1))[1]
    k2 <- n-out[const.ind]
    out[out==0] <- diff(c(0,sort(sample(0:k2,(r-2), replace=T)), k2))
  }
  
  if(n!=0 && const.ind!=0 && const=="both"){
    if(length(const.val)!=2) stop("lower and upper constraint value are needed")
    out <- rep(0,r)
    k1 <- const.val[2] - const.val[1]
    out[const.ind] <- const.val[1] + diff(c(0,sort(sample(0:k1,1, replace=T)), k1))[1]
    k2 <- n-out[const.ind]
    out[out==0] <- diff(c(0,sort(sample(0:k2,(r-2), replace=T)), k2))
  }
  
  return(out)}
# denominator and nominator for fractions
denom <- function(x) as.numeric(strsplit(attributes(x)$fracs, "/")[[1]][2])
nom <-   function(x) as.numeric(strsplit(attributes(x)$fracs, "/")[[1]][1])


prob_generate <- function(pi, xi1, xi2, ppv1, ppv2, equal_hypothesis, max.try=1000) {
  max.denom <- 10000
  d.e <- 100
  
  #####################################################################################
  ## specify p0
  #####################################################################################
  ## x3=p08+p09
  ## x4=p06+p09
  t <- 1
  while(t < 100){
    
    eta1_lwb <- 1-((1-ppv1)*xi1*pi)/(1-pi)
    eta2_lwb <- 1-((1-ppv2)*xi2*pi)/(1-pi)
    
    if(equal_hypothesis == TRUE){
      eta_lwb <- max(eta1_lwb, eta2_lwb)
      eta1 <- runif(1, eta_lwb, 1)
      eta2 <- eta1
    }
    
    if(equal_hypothesis == FALSE){
      eta1 <- runif(1, eta1_lwb, 1)
      eta2 <- runif(1, eta2_lwb, 1)
    }
    
    
    x3 <- (1-pi)*eta1
    x4 <- (1-pi)*eta2
    
    lwr <- -(1-pi)*(1-eta1-eta2)
    upp <- min(x3,x4)
    
    i<-1
    
    while(i < max.try) {
      p0 <- rep(0,9)
      
      ## p0 setting
      # p01=p02=p03=0p4=p07=0
      p0[9] <- runif(1, lwr, upp)
      p0[5] <- p0[9] + (1-pi)*(1-eta1-eta2)
      p0[8] <- -p0[9]+x3
      p0[6] <- -p0[9]+x4
      
      if(all(p0[c(5,6,8,9)]>0)) break;
      i<-i+1
      if(i==max.try) {p0<-rep(NA,9)}
    }
    
    #####################################################################################
    ## specify p1
    #####################################################################################
    ## x1=p11+p12+p13+p14+p15+p16
    ## x2=p11+p12+p14+p15+p17+p18
    ## x5=p11+p12+p13
    ## x6=p11+p14+p17
    ## x7=p14+p15+p16
    ## x8=p12+p15+p18
    x1 <- pi*xi1
    x2 <- pi*xi2
    x5 <- ppv1*(pi*xi1+(1-eta1)*(1-pi))
    x6 <- ppv2*(pi*xi2+(1-eta2)*(1-pi))
    x7 <- x1-x5
    x8 <- x2-x6
    
    j<-1
    if(x7>0 && x8>0){
      while(j < max.try) {
        p1 <- rep(0,9)
        
        ## p11, p12, p13
        frac_x5 <- fractions(x5, max.denominator=max.denom)
        nom_x5 <- nom(frac_x5)
        denom_x5 <- denom(frac_x5)
        
        if(nom_x5<100) {
          nom_x5 <-nom_x5*d.e
          denom_x5 <-denom_x5*d.e
        }
        if(nom_x5 > 1000000000) {
          nom_x5 <- round(nom_x5/d.e)
          denom_x5 <- denom_x5/d.e
        }
        
        p1[c(1,2,3)] <- combn_rep(nom_x5, 3)/denom_x5
        
        ## p14, p15, p16
        frac_x7 <- fractions(x7, max.denominator=max.denom)
        nom_x7 <- nom(frac_x7)
        denom_x7 <- denom(frac_x7)
        if(nom_x7<100) {
          nom_x7 <-nom_x7*d.e
          denom_x7 <-denom_x7*d.e
        }
        if(nom_x7 > 1000000000) {
          nom_x7 <- round(nom_x7/d.e)
          denom_x7 <- denom_x7/d.e
        }
        
        p1[c(4,5,6)] <- combn_rep(nom_x7, 3)/denom_x7
        p1[7] <- x6-sum(p1[c(1,4)])
        p1[8] <- x2-sum(p1[c(1,2,4,5,7)])
        p1[9] <- pi-sum(p1)
        
        if(all(p1>0)) break;
        j<-j+1
        if(j==max.try) {p1<-rep(NA,9)}
      }
      
    } else {
      p1<-rep(NA,9)
    }
    
    if(!any(is.na(c(p0,p1)))) break;
    t <- t+1
    if(t==100) {p0<-rep(NA,9)
    p1 <- rep(NA,9)
    }
  }
  return(list(data.frame(p1=p1, p0=p0),c(x7,x8), c(eta1, eta2)))
}
wrap_prob_generate <- function(PI, XI, PPV, equal_hypothesis){
  ## equal sens, spec
  
  prob_set <- data.frame(pi=numeric(), xi1=numeric(), xi2=numeric(), eta1=numeric(), eta2=numeric(), ppv1=numeric(), ppv2=numeric(), p11=numeric(), p12=numeric(), p13=numeric(), p14=numeric(), p15=numeric(), p16=numeric(), p17=numeric(), p18=numeric(), p19=numeric(), p01=numeric(), p02=numeric(), p03=numeric(), p04=numeric(), p05=numeric(), p06=numeric(), p07=numeric(), p08=numeric(), p09=numeric())
  for(i in 1:length(PI)) {
    for(j in 1:nrow(XI)) {
      for(k in 1:nrow(PPV)){
        
        prob <- prob_generate(PI[i], XI[j,1], XI[j,2], PPV[k,1], PPV[k,2], equal_hypothesis)
        
        eta1 <- prob[[3]][1]
        eta2 <- prob[[3]][2]
        
        prob_set_temp <- data.frame(pi=PI[i], xi1=XI[j,1], xi2=XI[j,2], eta1=eta1,eta2=eta2, ppv1=PPV[k,1],ppv2=PPV[k,2], t(data.frame(c(prob[[1]][,1], prob[[1]][,2]))), row.names=NULL)
        colnames(prob_set_temp)[8:25] <- c(paste0("p1",1:9), paste0("p0",1:9))
        prob_set <- rbind(prob_set, prob_set_temp)
        
      }
    }
  }
  return(prob_set)
}

#######################################################################
## Main simulation functions
#######################################################################
simulation <- function(equal_hypothesis, N, prob, n_data=10000, seed=881128) {
  p1 <- as.numeric(prob[grep("p1", colnames(prob))])
  p0 <- as.numeric(prob[grep("p0", colnames(prob))])
  ## p1,p0 generate
  simul_data <- simul_data_generate(n_data, N, p1, p0, seed)
  
  ppv1 <- simul_data[[1]][1]
  ppv2 <- simul_data[[1]][2]
  xi1 <- simul_data[[1]][3]
  xi2 <- simul_data[[1]][4]
  eta1 <- simul_data[[1]][5]
  eta2 <- simul_data[[1]][6]
  test1 <- round(prob$ppv1,4) == ppv1; test2 <- round(prob$ppv2,4) == ppv2; test3 <- round(prob$xi1, 4) == xi1; test4 <- round(prob$xi2,4) == xi2; test5 <- round(prob$eta1,4) == eta1; test6 <- round(prob$eta2,4) == eta2
  
  if(!all(c(test1,test2,test3,test4,test5,test6))) stop("error!")
  
  test_pval <- rep(0,n_data)
  if(equal_hypothesis==TRUE){
    for(i in 1:n_data) test_pval[i] <- ppv_equal_test(simul_data[[2]][,i])$p.value
  }
  if(equal_hypothesis==FALSE){
    for(i in 1:n_data) test_pval[i] <- ppv_nonequal_test(simul_data[[2]][,i])$p.value
  }
  
  result_prob <- sum(test_pval<0.05, na.rm=T)/n_data
  
  final <- data.frame(N)
  prob_mat <- c(p1, p0)
  prob_mat<-data.frame(t(prob_mat))
  colnames(prob_mat) <- c(paste("p1", 1:9, sep=""),paste("p0", 1:9, sep=""))
  
  result_mat <- data.frame(t(c(ppv1, ppv2, abs(ppv1-ppv2), prob$pi, prob$xi1,prob$xi2, prob$eta1, prob$eta2, result_prob)))
  
  colnames(result_mat)[1:8] <- c("PPV1","PPV2", "d0", "PI", "XI1", "XI2", "ETA1","ETA2")
  if(ppv1==ppv2) colnames(result_mat)[9] <- "nominal"
  if(ppv1!=ppv2) colnames(result_mat)[9] <- "power"
  
  result <- cbind(final,prob_mat, result_mat)
  rownames(result) <- 1
  return(result)
}
wrap_simulation <- function(case=c("nominal","power"), equal_hypothesis, N, prob) {
  
  for(i in 1:nrow(prob)){
    for(j in 1:length(N)){
      if(i==1 && j==1) {
        output <- simulation(equal_hypothesis, N[j], prob[i,])
      } else {
        output <- rbind(output, simulation(equal_hypothesis, N[j], prob[i,]))
      }
    }
  }
  
  ###
  result <- prob
  
  for(n in 1:length(N)){
    result_temp <- data.frame(rep(NA,nrow(result)))
    colnames(result_temp) <- paste0("N", N[n])
    result <- cbind(result, result_temp)
  }
  
  for(i in 1:nrow(result)) 
    set_list[i,26:(26+length(N)-1)] <- output[output$PI == result$pi[i] & output$PPV1 == result$ppv1[i] & output$PPV2 == result$ppv2[i] & output$XI1 == result$xi1[i] & output$XI2 == result$xi2[i] & output$ETA1 == result$eta1[i] & output$ETA2 == result$eta2[i],][,case]
  return(set_list)
}

