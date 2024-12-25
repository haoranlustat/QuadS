rm(list=ls())
library(dplyr)
library(pROC)
library(ggplot2)
library(gss)
library(earth)
library(bigsplines)
source("./functions/function.R")
source("./functions/forward_backward.R")
source("./functions/model.R")

############################## Data Generation ################################

Tmax = 50 
Tmin = 10
prop_prepaid  = 0.5

print(prop_prepaid)

R_train_set = 50
num_R_train = length(R_train_set)
num_methods = 5
num_sims = 1

print(R_train_set*2)
print(prop_prepaid)
print(num_sims)

### functions ###
generate_data = function(R_train, Tmax, Tmin=10, prop_prepaid){

Tmax = Tmax+1
t_lag = 1
prop_unprepaid = 1 - prop_prepaid
R_train_prepaid   = round(prop_prepaid * R_train)
R_train_unprepaid = R_train + R_train_prepaid

train_data_list = list() # list for hmm
test_data_list = list()

# train_df = data.frame() # df for logistic reg
# test_df = data.frame()


case_R = 1
while(case_R <= R_train){

  pi_i = c(0.9,0.1)
 
  z = data.frame(z1 =runif(Tmax+1,-0.5,1.5),
                 z2 =runif(Tmax+1,2.5,3))
 
  sap = sapply(z$z1,function(x){
    if(x<=0.5){
      x = x+0.5
      result =.3*(1e6*(x^11*(1-x)^6)+1e4*(x^6*(1-x)^10))-2
    }else{
      x = x - 1
      x = x+0.5
      result =.3*(1e6*(x^11*(1-x)^6)+1e4*(x^6*(1-x)^10))-2
    }
    result
  })
  logit_ap = sap*z$z2
  p_ap = exp(logit_ap)/(exp(logit_ap)+1)
  p_pp = p_ap
  p_aa = 1- p_ap
  p_pa = 1-p_pp
  # plot(z$z1,logit_ap,"l")
  
  A = lapply(1:length(p_aa),function(i){
    mat = matrix(0,2,2)
    mat[1,1] = p_aa[i]; mat[1,2] = p_ap[i]
    mat[2,1] = p_pa[i]; mat[2,2] = p_pp[i]
    mat
  })
  A = A[2:length(A)]
 
  sap = sapply(z$z1,function(x){
    if(x<=0.5){
      result =.3*(1e6*(x^11*(1-x)^6)+1e4*(x^3*(1-x)^10))-2
    }else{
      x = x - 1
      result =.3*(1e6*(x^11*(1-x)^6)+1e4*(x^3*(1-x)^10))-2
    }
    result
  })
  logit_ap = sap*z$z2
  p_ap = exp(logit_ap)/(exp(logit_ap)+1)
  

  sap = sapply(z$z1,function(x){
    if(x<=0.5){
      x = -x
      result =.3*(1e6*(x^11*(1-x)^6)+1e4*(x^3*(1-x)^10))-2
    }else{
      x = x - 1
      x = -x
      result =.3*(1e6*(x^11*(1-x)^6)+1e4*(x^3*(1-x)^10))-2
    }
    result
  })
  logit_pp = sap*z$z2
  p_pp = exp(logit_pp)/(exp(logit_pp)+1)
  p_aa = 1- p_ap
  p_pa = 1-p_pp
  B = lapply(1:length(p_aa),function(i){
    mat = matrix(0,2,2)
    mat = matrix(0,2,2)
    mat[1,1] = p_aa[i]; mat[1,2] = p_ap[i]
    mat[2,1] = p_pa[i]; mat[2,2] = p_pp[i]
    mat
  })
  B = B[1:(length(B)-1)]
  
  x0 = sample(c(1,2),size=1,prob = pi_i)
  xt = rep(0,Tmax)
  xt[1] = x0
  y = rep(3,Tmax)
  for(i in 1:(Tmax)){
    emission_mat = B[[i]]
    y[i] = sample(c(1,2),size=1,prob = emission_mat[xt[i],]) 
    if(y[i] ==2){
      break
    }else{
      if(i < (Tmax)){
        transition_mat = A[[i]]
        xt[i+1] = sample(c(1,2),size=1,prob = transition_mat[xt[i],]) }
    }
  }
  
  current_dat  =  data.frame(z[2:nrow(z),],y)
  wh = which(y %in% 3)
  
  if(length(wh) >0){
    if(wh[1] <= Tmin ){
      current_dat = NULL
    }
    if( wh[1]  > Tmin){
      end_time = wh[1] -1
      current_dat = current_dat[1:end_time,]
      
      
      if(case_R<=R_train_prepaid){
        if(nrow(current_dat) < (Tmax-1)){
        train_data_list[[case_R]] = current_dat
        }
      }
      
      else{
        if(nrow(current_dat) == (Tmax-1)){
          train_data_list[[case_R]] = current_dat
        }
      }
    }
  }
  
    case_R = length(train_data_list)+1
}

# 



############################################################
######normalize############
############################################################
min1 = apply(do.call("rbind",train_data_list),2,min)
max1 = apply(do.call("rbind",train_data_list),2,max)
train_data_list = lapply(train_data_list,function(xx){
  wh_z = which( colnames(xx) %in% setdiff(colnames(xx),"y"))
  for(i in wh_z){
    xx[,i] = (xx[,i] - min1[i])/ (max1[i] - min1[i])
  }
  xx
})


train_data_list= lapply(train_data_list,function(ii){
  x= ii[1:(nrow(ii)-t_lag),1:2]
  y= ii[(1+t_lag):nrow(ii),3]
  data.frame(x,y)
})



shuffled_train_data_list = list()
shuffled_index_set = sample(1:R_train, R_train, replace = F)

for(i in 1:R_train){
  shuffled_index = shuffled_index_set[i]
  shuffled_train_data_list[shuffled_index] = train_data_list[i]
}

shuffled_train_data_list
}



### simulation ###

  

R_train = R_train_set
R_test  = R_train_set

train_data_list = generate_data(R_train, Tmax, Tmin, prop_prepaid)

test_data_list = generate_data(R_test, Tmax, Tmin, prop_prepaid)


###### QuadS settings ========================================= ###



parameters = NULL
parameters$max_EM_iter = 20
parameters$EM_tol = 1e-4
parameters$num_states=2
parameters$data_list = train_data_list
parameters$EPS = 2.220446049250313e-16
parameters$with_randomness = TRUE
parameters$covariates_initial = NULL
parameters$covariates_transition = c('z1','z2')
parameters$covariates_emissions = c('z1','z2')
parameters$responses_emissions = c('y')
parameters$model_type = "gssanova"


### QuadS estimation  ========================================= ###



self = QuadS(parameters)
temp = prediction_auc2(parameters,self)
hat_y = temp$hat_y
true_y = temp$true_y

auc_train= auc(true_y,hat_y)
print(auc_train)

parameters$data_list = test_data_list
temp = prediction_auc2(parameters,self)
hat_y = temp$hat_y
true_y= temp$true_y

auc_test = auc(true_y,hat_y)
print(auc_test)
