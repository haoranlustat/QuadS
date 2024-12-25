
preprocess = function(train){
  ####delete loan id which only have one record############
  loanid_len = ddply(train, .(LOAN_ID),nrow)
  del_id = loanid_len$LOAN_ID[which(loanid_len$V1 < 5 )]
  if(length(del_id)>1){
    train = train[-which(train$LOAN_ID %in% del_id), ]
  }
  ####delete loan id which only have one record############
  
  #######preprocessing#############
  y = train$Zero_Bal_Code
  train$Zero_Bal_Code = NULL
  train$Zero_Bal_Code = y
  t_lag = 1
  dap = ddply(train,.(LOAN_ID),function(ii){
    x = ii[1:(nrow(ii)-t_lag),1:(ncol(ii)-1)]
    Zero_Bal_Code = ii[(1+t_lag):nrow(ii),ncol(ii)]
    cbind(x,Zero_Bal_Code)
  })
  # summary(dap)
  # dap2 = (dap %>% filter(STATE != 'PR'))
  
  # dap = train
  temp = ddply(dap,.(LOAN_ID),function(ii){
    return(sum(is.na(ii)))
  })
  loan_id_del = temp$LOAN_ID[temp$V1 == 0]
  dap2 = (dap %>% filter(LOAN_ID %in% loan_id_del))
  summary(dap2)
  train = dap2
  
  #######preprocessing2#############
  # train$Zero_Bal_Code %>% table
  # train$UNRATE = NULL
  # train$HPI = NULL
  # train$HPA = NULL
  train = train[train$Zero_Bal_Code %in% c("00","01"),]
  
  input = train
  input$BNT %>% summary
  input$EIT %>% summary
  input$EIT = NULL; input$BNT  = NULL;
  y = input$Zero_Bal_Code
  y = as.character(y)
  y[ y %in% "00"] = 1 # UNPREPAID
  y[y %in%  "01"] = 2 # PREPAID
  table(y)
  y  = as.numeric(y)
  input$Zero_Bal_Code = NULL
  loan_id  = input$LOAN_ID
  uni_id = unique(loan_id)
  R_ori = length(uni_id)
  colnames(input)
  input$LOAN_ID = NULL
  input$ACT_PERIOD = NULL
  
  # cate_names = c("STATE")
  # input_cate = input[,colnames(input) %in% cate_names]
  # input_numb = input[,!(colnames(input) %in% cate_names)]
  # input_numb = apply(input_numb,2,as.numeric)
  # input_numb = as.data.frame(scale(input_numb))
  # input = cbind(input_cate,input_numb)
  # colnames(input)[1:length(cate_names)] = cate_names
  
  input = apply(input,2,as.numeric)
  input = as.data.frame(scale(input))
  input$y = y
  input$loan_id = loan_id
  
  un_id = unique(input$loan_id )
  # input$y = input$y - 1
  lap = lapply(un_id,function(x){
    temp = input[input$loan_id %in% x,]
    temp$loan_id = NULL
    temp
  })
  data_list = lap
  
  
  return(list(data_list = data_list, loan_id = un_id))
}

# preprocess = function(train){
#   ####delete loan id which only have one record############
#   loanid_len = ddply(train, .(LOAN_ID),nrow)
#   del_id = loanid_len$LOAN_ID[which(loanid_len$V1 < 5 )]
#   if(length(del_id)>1){
#     train = train[-which(train$LOAN_ID %in% del_id), ]
#   }
#   ####delete loan id which only have one record############
#   
#   #######preprocessing#############
#   y = train$Zero_Bal_Code
#   train$Zero_Bal_Code = NULL
#   train$Zero_Bal_Code = y
#   t_lag = 1
#   dap = ddply(train,.(LOAN_ID),function(ii){
#     x = ii[1:(nrow(ii)-t_lag),1:(ncol(ii)-1)]
#     Zero_Bal_Code = ii[(1+t_lag):nrow(ii),ncol(ii)]
#     cbind(x,Zero_Bal_Code)
#   })
#   # summary(dap)
#   # dap2 = (dap %>% filter(STATE != 'PR'))
#   
#   # dap = train
#   temp = ddply(dap,.(LOAN_ID),function(ii){
#     return(sum(is.na(ii)))
#   })
#   loan_id_del = temp$LOAN_ID[temp$V1 == 0]
#   dap2 = (dap %>% filter(LOAN_ID %in% loan_id_del))
#   summary(dap2)
#   train = dap2
#   
#   #######preprocessing2#############
#   # train$Zero_Bal_Code %>% table
#   # train$UNRATE = NULL
#   # train$HPI = NULL
#   # train$HPA = NULL
#   train = train[train$Zero_Bal_Code %in% c("00","01"),]
#   
#   input = train
#   input$BNT %>% summary
#   input$EIT %>% summary
#   input$EIT = NULL; input$BNT  = NULL;
#   y = input$Zero_Bal_Code
#   y[ y %in% "00"] = 1 # UNPREPAID
#   y[y %in%  "01"] = 2 # PREPAID
#   table(y)
#   y  = as.numeric(y)
#   input$Zero_Bal_Code = NULL
#   loan_id  = input$LOAN_ID
#   uni_id = unique(loan_id)
#   R_ori = length(uni_id)
#   colnames(input)
#   input$LOAN_ID = NULL
#   input$ACT_PERIOD = NULL
#   
#   # cate_names = c("STATE")
#   # input_cate = input[,colnames(input) %in% cate_names]
#   # input_numb = input[,!(colnames(input) %in% cate_names)]
#   # input_numb = apply(input_numb,2,as.numeric)
#   # input_numb = as.data.frame(scale(input_numb))
#   # input = cbind(input_cate,input_numb)
#   # colnames(input)[1:length(cate_names)] = cate_names
#   
#   input = apply(input,2,as.numeric)
#   input = as.data.frame(scale(input))
#   input$y = y
#   input$loan_id = loan_id
#   
#   un_id = unique(input$loan_id )
#   input$y = input$y - 1
#   lap = lapply(un_id,function(x){
#     temp = input[input$loan_id %in% x,]
#     temp$loan_id = NULL
#     temp
#   })
#   data_list = lap
#   return(data_list)
# }

preprocess_x = function(train){
  ####delete loan id which only have one record############
  loanid_len = ddply(train, .(LOAN_ID),nrow)
  del_id = loanid_len$LOAN_ID[which(loanid_len$V1 < 5 )]
  if(length(del_id)>1){
    train = train[-which(train$LOAN_ID %in% del_id), ]
  }
  ####delete loan id which only have one record############
  
  #######preprocessing#############
  y = train$Zero_Bal_Code
  train$Zero_Bal_Code = NULL
  train$Zero_Bal_Code = y
  t_lag = 1
  dap = ddply(train,.(LOAN_ID),function(ii){
    x = ii[1:(nrow(ii)-t_lag),1:(ncol(ii)-1)]
    Zero_Bal_Code = ii[(1+t_lag):nrow(ii),ncol(ii)]
    cbind(x,Zero_Bal_Code)
  })
  # summary(dap)
  # dap2 = (dap %>% filter(STATE != 'PR'))
  
  # dap = train
  temp = ddply(dap,.(LOAN_ID),function(ii){
    return(sum(is.na(ii)))
  })
  loan_id_del = temp$LOAN_ID[temp$V1 == 0]
  dap2 = (dap %>% filter(LOAN_ID %in% loan_id_del))
  summary(dap2)
  train = dap2
  
  #######preprocessing2#############
  # train$Zero_Bal_Code %>% table
  # train$UNRATE = NULL
  # train$HPI = NULL
  # train$HPA = NULL
  train = train[train$Zero_Bal_Code %in% c("00","01"),]
  
  input = train
  input$BNT %>% summary
  input$EIT %>% summary
  input$EIT = NULL; input$BNT  = NULL;
  y = input$Zero_Bal_Code
  y = as.character(y)
  y[ y %in% "00"] = 1 # UNPREPAID
  y[y %in%  "01"] = 2 # PREPAID
  table(y)
  y  = as.numeric(y)
  input$Zero_Bal_Code = NULL
  loan_id  = input$LOAN_ID
  uni_id = unique(loan_id)
  R_ori = length(uni_id)
  colnames(input)
  input$LOAN_ID = NULL
  input$ACT_PERIOD = NULL
  
  # cate_names = c("STATE")
  # input_cate = input[,colnames(input) %in% cate_names]
  # input_numb = input[,!(colnames(input) %in% cate_names)]
  # input_numb = apply(input_numb,2,as.numeric)
  # input_numb = as.data.frame(scale(input_numb))
  # input = cbind(input_cate,input_numb)
  # colnames(input)[1:length(cate_names)] = cate_names
  
  input = apply(input,2,as.numeric)
  input = as.data.frame(scale(input))
  input$loan_id = loan_id
  input$y= y  
  return(input)
}

