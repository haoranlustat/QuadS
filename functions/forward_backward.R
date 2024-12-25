forward_backward = function(log_prob_initial, log_prob_transition, log_Ey, log_state=NULL){
  log_delta = forward(log_prob_initial, log_prob_transition, log_Ey, log_state)
  log_tau = backward(log_prob_transition, log_Ey, log_state)
  log_likelihood = cal_log_likelihood(log_delta)
  log_gamma = cal_log_gamma(log_delta, log_tau, log_likelihood, log_state)
  log_xi = cal_log_xi(log_prob_transition, log_Ey, log_delta,
                                log_tau, log_likelihood, log_state)
  
  return(list(log_gamma=log_gamma, log_xi=log_xi, log_likelihood=log_likelihood))
}

forward = function(log_prob_initial, log_prob_transition, log_Ey, log_state={}){

  # assert log_prob_initial.ndim == 1
  # assert log_prob_transition.ndim == 3
  # assert log_Ey.ndim == 2
  t = nrow(log_Ey)
  k = ncol(log_Ey)
  log_delta = matrix(0,nrow=t, ncol=k)
  log_delta[1, ] = log_prob_initial + log_Ey[1, ]
  for (i in 2:t){
    temp0 = t(log_prob_transition[, , i - 1])
    temp1 = matrix(rep(log_delta[i - 1, ],ncol(temp0) ),ncol=ncol(temp0),byrow=T)
    temp = temp0 + temp1
    log_delta[i, ] =  log(apply(exp(temp),1,sum) ) + log_Ey[i, ]  # by row
  # assert dim(log_delta) == c(t, k)
  }
  return (log_delta)
}



backward = function(log_prob_transition, log_Ey, log_state={}){
  
  # assert log_prob_transition.ndim == 3
  # assert log_Ey.ndim == 2
  t = nrow(log_Ey)
  k = ncol(log_Ey)
  log_tau = matrix(0,nrow= t, ncol=k)
  for (i in seq(t-1,1,-1)){ 
    temp0 = log_prob_transition[, , i] 
    temp1 = (log_tau[i + 1, ] + log_Ey[i + 1, ])
    temp11 = matrix(rep(temp1,ncol(temp0) ),ncol=ncol(temp0),byrow=T)
    temp = temp0 + temp11
    log_tau[i, ] = log(apply(exp(temp),1,sum) ) 
  # assert log_tau.shape == (t, k)
  }
  return(log_tau)
}


cal_log_likelihood = function(log_delta){
  temp = log_delta[nrow(log_delta), ]
  log_likelihood = log(sum(exp(temp)))
  return(log_likelihood)
}

cal_log_gamma = function(log_delta, log_tau, log_likelihood, log_state={}){
  log_gamma = log_delta + log_tau - log_likelihood
  # for i in log_state:
  #   log_gamma[i, :] = log_state[i]
  return (log_gamma)
}

cal_log_xi = function(log_prob_transition, log_Ey, log_delta, log_tau, 
                           log_likelihood, log_state={}){
  k = ncol(log_Ey)
  
  if (nrow(log_prob_transition) == 0){
    return( matrix(0, nrow=k, ncol=k) )
  }else{
    log_p = log_prob_transition
    temp = (log_Ey + log_tau)[2:nrow(log_Ey + log_tau),]
    
    # temp0 = array(temp,c(1,ncol(temp),nrow(temp)))
    temp1 = array(0,c(k,k,nrow(temp) ))
    for(i in 1:dim(temp1)[3]){
      temp1[,,i] = matrix(rep(temp[i,],k),nrow=k,byrow=T)
    }
    
    temp = log_delta[1:(nrow(log_delta)-1), ]
    # temp0 = array(temp,c(ncol(temp),1,nrow(temp)))
    temp2 = array(0,c(k,k, nrow(temp)))
    for(i in 1:dim(temp2)[3]){
      temp2[,,i] = matrix(rep(temp[i,],k),nrow=k,byrow=F)
    }
    log_xi = temp1 + temp2 + log_p - log_likelihood
      
  }
  return (log_xi)
}

