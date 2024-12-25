
######initialization############
initial = function(parameters){
  num_states=parameters$num_states
  EM_tol=parameters$EM_tol
  max_EM_iter=parameters$max_EM_iter
  # EPS = parameters$EPS
  with_randomness = parameters$with_randomness
  responses_emissions = parameters$responses_emissions
  covariates_initial = parameters$covariates_initial
  covariates_transition =  parameters$covariates_transition 
  covariates_emissions = parameters$covariates_emissions 
  data_list = parameters$data_list
  num_emissions = length(responses_emissions)
  num_seqs = length(data_list)
  
  #with_randomness = TRUE,num_seqs,num_states,data_list,responses_emissions,EPS
  self = NULL
  #1. initialize log_gammas
  self$log_gammas = lapply(1:num_seqs,function(seq){
    df = data_list[[seq]]
    init_log_gamma = log(matrix(0.5,nrow =nrow(df),ncol=num_states))
    init_log_gamma
  })
  
  #2. initialize log_xis
  self$log_xis = lapply(1:num_seqs,function(seq){
    # cat(seq,"\n")
    df = data_list[[seq]]
    init_log_xi = log(array(0.5,c(num_states, num_states,nrow(df) - 1)))
    init_log_xi
  })
  
  #3.with_randomness
  if (with_randomness == TRUE){
    for (st in 1:num_states){
      temp = unlist(sapply(self$log_gammas,function(x){x[,st]}))
      # if (sum(exp(temp)) < EPS){
      # there is no any sample associated with this state
      for(i in 1:length(self$log_gammas)){
        temp = self$log_gammas[[i]]
        self$log_gammas[[i]][,st] = runif(nrow(temp), min = 0, max = 1)
        # }
      }
    }
    
    for (st in 1:num_states){
      lap = lapply(self$log_xis,function(x){
        t(x[st,,]) }); 
      temp = do.call("rbind",lap)
      # if (sum(exp(temp)) < EPS){
      # there is no any sample associated with this state
      for(i in 1:length(self$log_xis)){
        temp = self$log_xis[[i]]
        self$log_xis[[i]][st,,] = runif(dim(temp)[3]*num_states, min = 0, max = 1)
      }
      # }
    } 
  }
  
  ##############load exsiting ##############
  # dat = read.csv("../data/log_gammas.csv",header =T)
  # self$log_gammas[[1]] = dat[1:(nrow(dat)/2),]
  # self$log_gammas[[2]]  = dat[(nrow(dat)/2+1):nrow(dat),]
  # 
  # dat = read.csv("../data/log_xis.csv",header =T)
  # dat1 = dat[1:(nrow(dat)/2),]
  # temp1 = array(dat1, c(2,2,dim(self$log_xis[[1]])[3]))
  # self$log_xis[[1]] = apply(temp1,c(1,3),function(x){ x })
  # 
  # dat2 = dat[(nrow(dat)/2+1):nrow(dat),]
  # temp2 = array(dat2, c(2,2,dim(self$log_xis[[2]])[3]))
  # self$log_xis[[2]]  = apply(temp2,c(1,3),function(x){ x })
  ##############end:load exsiting ##############
  
  
  #4. initialize log_likelihood
  self$log_likelihoods = rep(-Inf,num_seqs)
  self$log_likelihood = -Inf
  
  #5. initialize initial probability
  self$inp_initials = lapply(1:num_seqs,function(seq){
    dat = data_list[[seq]]
    dat[1,covariates_initial]
  })
  self$inp_initials_all_sequences = do.call("rbind",self$inp_initials)
  
  #6.initialize input transition
  self$inp_transitions = lapply(1:num_seqs,function(seq){
    dat = data_list[[seq]]
    dat2 = as.data.frame(dat[2:nrow(dat),covariates_transition])
    colnames(dat2) = covariates_transition
    dat2
  })
  self$inp_transitions_all_sequences = do.call("rbind",self$inp_transitions)
  
  #7.initialize input emissions
  self$inp_emissions = lapply(1:num_seqs,function(seq){
    dat = data_list[[seq]]
    dat2 = as.data.frame(dat[1:nrow(dat),covariates_emissions])
    colnames(dat2) = covariates_emissions
    dat2
  })
  self$inp_emissions_all_sequences = do.call("rbind",self$inp_emissions)
  
  #8. out_emissions
  self$out_emissions = lapply(1:num_seqs,function(seq){
    dat = data_list[[seq]]
    dat[1:nrow(dat),responses_emissions]
  })
  self$out_emissions_all_sequences = unlist(self$out_emissions)
  return(self)
}



M_step = function(self,parameters){
  num_states=parameters$num_states
  EM_tol=parameters$EM_tol
  max_EM_iter=parameters$max_EM_iter
  # EPS = parameters$EPS
  with_randomness = parameters$with_randomness
  responses_emissions = parameters$responses_emissions
  covariates_initial = parameters$covariates_initial
  covariates_transition =  parameters$covariates_transition 
  covariates_emissions = parameters$covariates_emissions 
  data_list = parameters$data_list
  model_type = parameters$model_type
  num_emissions = length(responses_emissions)
  num_seqs = length(data_list)
  
  
  #1.  optimize initial model
  # X = self$inp_initials_all_sequences
  # Y = exp(do.call("rbind",lapply(self$log_gammas,function(ii){(ii[1,])})))
  # n_classes = ncol(Y)
  # n_samples = nrow(Y)
  # Y_repeat  = rep(1:n_classes, n_samples)
  # Y_repeat = Y_repeat-1
  # if(length(covariates_initial) == 0 ){
  #   temp_dat = data.frame(Y=Y_repeat)
  # }else{
  #   X_repeat = X[rep(seq_len(nrow(X)), each=n_classes),,drop=FALSE]
  #   temp_dat = data.frame(X_repeat,Y=Y_repeat)
  # }
  # if( max(temp_dat$Y) >1){
  #   temp_dat$Y = temp_dat$Y -1
  # }
  # sample_weight= rep(1,n_samples)
  # sample.weight = as.vector(t(Y))* rep(sample_weight, n_classes)
  # model1  = glm(Y~.,data=temp_dat, family="binomial",weights = sample.weight)
  # self$model_initial = model1
  
  #1. calculate initial probability
  temp = exp(do.call("rbind",lapply(self$log_gammas,function(ii){(ii[1,])})))
  temp1 = apply(temp,2,sum)
  self$pi = temp1/sum(temp1)
  
  #2. optimize transition models
  X = self$inp_transitions_all_sequences
  # model_transition = NULL
  
  for(st in 1:num_states){
    temp = lapply(self$log_xis,function(ii){t(ii[st,,])})
    Y = exp(do.call("rbind",temp))
    # Y = t(apply(Y,1,function(x){x/sum(x)}))  # normalize
    
    n_classes = ncol(Y)
    n_samples = nrow(Y)
    Y_repeat  = rep(1:n_classes, n_samples)
    Y_repeat = Y_repeat-1
    X_repeat = X[rep(seq_len(nrow(X)), each=n_classes),,drop=FALSE]
    # sample_weight= rep(1,n_samples)
    sample.weight = as.vector(t(Y))
    temp_dat = data.frame(X_repeat,Y=Y_repeat)
    if( max(temp_dat$Y) >1){
      temp_dat$Y = temp_dat$Y -1
    }
    
    if(model_type == "glm"){
      model1  = glm(Y~.,data=temp_dat, family="binomial",weights=sample.weight)
    }
    if(model_type == "mars"){
      model1 = earth(Y~.,data=temp_dat, weights=sample.weight,degree=3,glm=list(family=binomial))
      # summary(model1, digits = 2, style = "pmax")
    }
    if(model_type == "gssanova"){
      z_var = setdiff(colnames(temp_dat),"Y")
      fmla = as.formula(paste("Y ~ ", paste(z_var, collapse= "*")))
      model1 = gssanova(fmla,family="binomial",data=temp_dat,
                        # method="m",
                        weights = sample.weight,nbasis=30)
      
      ####big spline#######
      # z_var = setdiff(colnames(temp_dat),"Y")
      # ls = lapply(1:length(z_var), function(i){"cub"})
      # names(ls) = z_var
      # # sample.weight1 = round(sample.weight*100)
      # # temp_dat$Y = temp_dat$Y/sample.weight1
      # model1 = bigssg(Y~.,data=temp_dat,family="binomial",
      #                  type=ls,weights = sample.weight,nknot=20,rseed=1)
    }
    if(model_type == "mave"){
      model1 = mave(Y~., data=temp_dat, method = 'meanopg')
      # model1 = mave.dim(model1)
      
    }
    self$model_transition[[st]] = model1
    # coef(model1)
    
    # temp = lapply(1:num_states,function(st1){
    #   temp_dat = data.frame(X,Y=Y[,st1])
    #   # lm(Y~.,data=temp_dat)
    #  earth(Y~.,data=temp_dat, degree=1)
    #  
    # })
    # self$model_transition[[st]] = temp
    
  }
  
  #3. optimize emission models
  X = self$inp_emissions_all_sequences
  Y = self$out_emissions_all_sequences
  gammas = lapply(self$log_gammas,function(xxx){
    t(apply(xxx,1,function(x){exp(x)/sum(exp(x))})) 
  }) # normalized gammas
  for (st in 1:num_states){
    temp = lapply(self$log_gammas,function(ii){(ii[,st])})
    sample.weight = exp(unlist(temp))
    
    # temp = lapply(gammas,function(ii){(ii[,st])})
    # sample.weight = unlist(temp)
    
    
    temp_dat = data.frame(X,Y=Y)
    if( max(temp_dat$Y) >1){
      temp_dat$Y = temp_dat$Y -1
    }
    if(model_type == "glm"){
      model1  = glm(Y~.,data=temp_dat, family="binomial",weights=sample.weight)
    }
    if(model_type == "mars"){
      model1 = earth(Y~.,data=temp_dat, weights=sample.weight,degree=3,glm=list(family=binomial))
      # summary(model1, digits = 2, style = "pmax")
    }
    if(model_type == "gssanova"){
      z_var = setdiff(colnames(temp_dat),"Y")
      fmla = as.formula(paste("Y ~ ", paste(z_var, collapse= "*")))
      model1 = gssanova(fmla,family="binomial",data=temp_dat,
                        # method="m",
                        # type=list(z1="sphere",z2="cubic"),
                        weights = sample.weight,nbasis=30)
      
      ####big spline#######
      # z_var = setdiff(colnames(temp_dat),"Y")
      # ls = lapply(1:length(z_var), function(i){"cub"})
      # names(ls) = z_var
      # # sample.weight1 = round(sample.weight*100)
      # # temp_dat$Y = temp_dat$Y/sample.weight1
      # model1 = bigssg(Y~.,data=temp_dat,family="binomial",
      #                 type=ls,weights = sample.weight,nknot=20,rseed=1)
    }
    if(model_type == "mave"){
      model1 = mave(Y~., data=temp_dat, method='meanopg')
      # model1 = mave.dim(model1)
      
    }
    self$model_emissions[[st]] = model1
  }
  
  return(self)
}



E_step = function(self,parameters){
  num_states=parameters$num_states
  EM_tol=parameters$EM_tol
  max_EM_iter=parameters$max_EM_iter
  # EPS = parameters$EPS
  with_randomness = parameters$with_randomness
  responses_emissions = parameters$responses_emissions
  covariates_initial = parameters$covariates_initial
  covariates_transition =  parameters$covariates_transition 
  covariates_emissions = parameters$covariates_emissions 
  data_list = parameters$data_list
  model_type = parameters$model_type
  num_emissions = length(responses_emissions)
  num_seqs = length(data_list)
  
  
  # self$dfs_logStates = [[x, {}] for x in dfs]
  # self._initialize(with_randomness=True)
  self$log_xis  = NULL
  self$log_likelihoods = NULL
  self$log_gammas = NULL
  for( seq in 1:num_seqs ){
    n_records = nrow(data_list[[seq]])
    
    #1. initial probability
    # if(length(covariates_initial) == 0){
    #   newdata = data.frame(1)
    # }else{
    #   newdata = self$inp_initials[[seq]]
    # }
    # temp = predict(self$model_initial,newdata)
    # pred1 = exp( temp)/(1+ exp( temp))
    # log_prob_initial = log(c(1-pred1,pred1))
    
    #1. initial probability
    log_prob_initial = log(self$pi)
    
    #2. transition probability
    log_prob_transition = array(0,c(num_states, num_states,n_records - 1 ))
    for (st in 1:num_states){
      newdata = self$inp_transitions[[seq]]
      ##########method1#########
      if(model_type == "mave"){
        temp = predict(self$model_transition[[st]], as.matrix(newdata),dim=5)}
      else{temp = predict(self$model_transition[[st]], newdata)}
      # temp = predict(self$model_transition[[st]], newdata)$linear.predictors
      pred1 = exp( temp)/(1+ exp( temp))
      pred2 = 1- pred1
      log_prob_transition[st, ,] = rbind(log(pred2+0.0001),log(pred1+0.0001))
      #########method2#########
      # temp = NULL
      # for(st1 in 1:num_states){
      #   temp[[st1]] = predict(self$model_transition[[st]][[st1]], newdata, type="response")
      # }
      # temp0 = do.call("rbind",temp)
      # log_prob_transition[st, ,]  = apply(temp0,2,function(x){exp(x)/sum(exp(x))})
      #########End#########
    }
    
    #2. emission probability
    log_Ey = matrix(0,nrow = n_records, ncol = num_states)
    newdata = self$inp_emissions[[seq]]
    lap  = lapply(self$model_emissions,function(model){
      if(model_type == "mave"){
        temp = predict(model, as.matrix(newdata),dim=5)}
      else{temp = predict(model, newdata)}
      # temp = predict(model, newdata)$linear.predictors  #for big spline pakcage
      pred1 = exp( temp)/(1+ exp( temp))
      pred0 = 1 - pred1
      temp = log(cbind(pred0,pred1))
      sapply(1:nrow(temp),function(i){
        # cold_id = (self$out_emissions[[seq]] +1 )[i]
        cold_id = (self$out_emissions[[seq]] )[i]
        temp[i,cold_id]
      })
    })
    log_Ey = do.call("cbind",lap)
    
    
    
    #4. forward backward to calculate posterior
    temp = forward_backward(log_prob_initial, log_prob_transition, log_Ey, NULL)
    self$log_gammas[[seq]] = temp$log_gamma
    # self$log_gammas = rbind(self$log_gammas,temp$log_gamma)
    self$log_xis[[seq]] = temp$log_xi
    self$log_likelihoods = c( self$log_likelihoods, temp$log_likelihood)
  }
  
  
  self$log_likelihood = sum(self$log_likelihoods)
  return(self)
}


QuadS = function(parameters){
  num_states=parameters$num_states
  EM_tol=parameters$EM_tol
  max_EM_iter=parameters$max_EM_iter
  # EPS = parameters$EPS
  with_randomness = parameters$with_randomness
  responses_emissions = parameters$responses_emissions
  covariates_initial = parameters$covariates_initial
  covariates_transition =  parameters$covariates_transition 
  covariates_emissions = parameters$covariates_emissions 
  data_list = parameters$data_list
  model_type = parameters$model_type
  num_emissions = length(responses_emissions)
  num_seqs = length(data_list)
  
  
  ######initialization############
  self = initial(parameters)
  names(self)
  ########iteration em#########
  for(it in 1:max_EM_iter ){
    # cat(it,"\n")
    log_likelihood_prev = self$log_likelihood
    self = M_step(self,parameters)
    # self$model_initial
    # self$model_transition[[1]]
    # self$model_transition[[2]]
    self = E_step(self,parameters)
    # self$log_xis
    
    # logging.info('log likelihood of iteration {0}: {1:.4f}'.format(it, self.log_likelihood))
    # print(self$log_likelihood )
    if(abs(self$log_likelihood - log_likelihood_prev) < EM_tol){
      break
    }
    save(self,file="self.rda")
  }
  return(self)
}


prediction_auc2 = function(parameters,self){
  no_cores <- detectCores() - 1  
  # registerDoParallel(cores=no_cores)  
  # cl <- makeCluster(no_cores, type="FORK") 
  model_type = parameters$model_type
  num_states=parameters$num_states
  EM_tol=parameters$EM_tol
  max_EM_iter=parameters$max_EM_iter
  # EPS = parameters$EPS
  with_randomness = parameters$with_randomness
  responses_emissions = parameters$responses_emissions
  covariates_initial = parameters$covariates_initial
  covariates_transition =  parameters$covariates_transition 
  covariates_emissions = parameters$covariates_emissions 
  data_list = parameters$data_list
  num_emissions = length(responses_emissions)
  num_seqs = length(data_list)
  R=num_seqs
  # lap = parLapply(cl, 1:R, function(r){
    lap = lapply(1:R, function(r){
    # cat(r,"\n")
    zz0 = data_list[[r]][,covariates_initial]
    zz = data_list[[r]][,covariates_transition]
    zz = as.data.frame(zz)
    colnames(zz) = covariates_transition
    zz2 = data_list[[r]][,covariates_emissions]
    zz2 = as.data.frame(zz2)
    colnames(zz2) = covariates_transition
    true_y = data_list[[r]][,responses_emissions]
    n_time = nrow(data_list[[r]])
    pred_x = matrix(0,nrow=n_time,ncol = 2)# hidden states A, B
    pred_y = matrix(0,nrow=n_time,ncol = 2) # observed
    
    #1. initial probability
    pred_x[1,] = self$pi
    pred_y[1,] = 0
    
    for(t in 2:nrow(zz)){
      
      # 2. transition probability
      if(model_type == "mave"){
        temp = predict(self$model_transition[[ xx[t-1] ]],
                       as.matrix(zz[t,]),dim=5)}else{
        temp = as.data.frame(zz[t,])
        colnames(temp)  = covariates_transition
        temp1 = predict(self$model_transition[[ 1 ]],temp)
        temp2 = predict(self$model_transition[[ 2]],temp)
      }
      # temp = predict(self$model_transition[[ xx[t-1] ]],zz[t,])$linear.predictors
      
      pred1 = exp( temp1)/(1+ exp( temp1))
      trans1 = c(1-pred1,pred1)
      pred2 = exp( temp2)/(1+ exp( temp2))
      trans2 = c(1-pred2,pred2)
      tran_mat = rbind(trans2,trans2)
      pred_x[t,] = (pred_x[t-1,] %*% tran_mat)
     
      
      # 3. emission probability
      if(model_type == "mave"){
        temp = predict(self$model_emissions[[ xx[t] ]],as.matrix(zz2[t,]),dim=5)
      }else{
        temp = as.data.frame(zz2[t,])
        colnames(temp)  = covariates_emissions
        temp1 = predict(self$model_emissions[[ 1 ]], temp)
        temp2 = predict(self$model_emissions[[2 ]], temp)
      }
      # temp = predict(self$model_emissions[[ xx[t] ]],zz2[t,])$linear.predictors
      pred1 = exp( temp1)/(1+ exp( temp1))
      if(exp( temp1) == Inf){pred1=1}
      proba1 = c(1-pred1,pred1)
      pred2 = exp( temp2)/(1+ exp( temp2))
      if(exp( temp2) == Inf){pred2=1}
      proba2 = c(1-pred2,pred2)
      emis_mat = rbind(proba1,proba2)
      pred_y[t,] = (pred_x[t,] %*% emis_mat)
    }
    
    py1_hat= pred_y[-1,2];true_y = true_y[-1]; 
    data.frame(true_y=true_y,py1_hat=py1_hat)
    
  })
  # stopCluster(cl)
  lap2 = do.call("rbind",lap)
  # auc(lap2$true_y,lap2$py1_hat)
  list(hat_y = lap2$py1_hat,true_y = lap2$true_y,lap=lap)
}



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


