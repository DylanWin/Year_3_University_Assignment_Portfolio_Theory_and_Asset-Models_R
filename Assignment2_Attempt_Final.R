# Portfolio Thoery and Asset model 
# Assignment 2 R code
# Note: Plese ensure latest version of Rtools to prevent error dueing package installation


packages_required<-c("base", "datasets", "graphics", "grDevices", "methods", "stats", "compiler", "utils", 'matrixcalc')

# Check if packages exist in device, or else install package
for(i in packages_required){
  if(is.element(i,installed.packages()[,1])==TRUE){
    next
  }else{
    install.packages(i)
  }
}

# Basic packages

library(base)
library(datasets)
library(graphics)
library(methods)
library(stats)
library(matrixcalc)

library(compiler) # Byte compiler package to run repeated code more speedily (cmpfun())
# Pls do not detach package 'utils', will lead to error warning but code still works with output
library(utils)    # if error remains then close RStudio tab and reopen again, error will be gone

# Code below writes term format as variance%%, covariance%%, and total returns%

# Function to carry out Task 1
Task_1_func<-function(n,returns_matrix){
  # Enable same random sample to be reproduced for value checking purposes
  set.seed(207)
  ## Task 1 i) ##
  # Create matrix of dimension 4x4
  x<-matrix(runif(16,0,100), nrow=4, ncol=4, byrow=TRUE)
  ## Task 1 ii) ##
  # Divide whole matrix by eight
  x<-x/8
  # Create a identity matrix of 4x4
  y<-diag(4)
  # Multiply matrix y 100
  y<-y*100
  A<-x+y
  transpose_A<-t(A)
  # Cov Var by %% format
  covariance_matrix<-transpose_A%*%A
  
  ## Task 1 iii) ##
  X<-matrix(rnorm(n,0,1), nrow=n, ncol = 4, byrow=TRUE)
  Y<-X%*%A
  Y[,1]<-Y[,1]+120
  Y[,2]<-Y[,2]+130
  Y[,3]<-Y[,3]+140
  Y[,4]<-Y[,4]+150
  
   if(returns_matrix==1){
     Y
   }else{
     covariance_matrix
   }
}
# Displays matrix Y
Task_1_func(100,1)
# Displays calculated covariance matrix
Task_1_func(100,0)

# Byte compiler function 
cmp_Task_1_func<-cmpfun(Task_1_func)

## Task 2 i) ##
# Function to carry out Task 2 i)
est_cov_matrix<-function(n, with_max_abs_value){
  # Enable same random sample to be reproduced for value checking purposes
  set.seed(207)
  # row names to be used for weights
  names<-c('asset1', 'asset2', 'asset3', 'asset4')
  pop1<-cmp_Task_1_func(n,1)[,1]
  pop2<-cmp_Task_1_func(n,1)[,2]
  pop3<-cmp_Task_1_func(n,1)[,3]
  pop4<-cmp_Task_1_func(n,1)[,4]
  
  # Grouping matrix together
  pop<-list(pop1,pop2,pop3,pop4)
  
  # Creating empty matrix 4x4 dimension
  cov_var_matrix<-matrix(0,nrow=4, ncol = 4)
  rownames(cov_var_matrix)<-names
  colnames(cov_var_matrix)<-names
  
  # For loop that assigns variance and covariance between assets to exact matrix entries
  # If same asset(i=j), then variance(diagonal entry), if different asset, then covar(non-diag entry)
  for(i in 1:4){
    for(j in 1:4){
      x<-pop[[i]]
      y<-pop[[j]]
      
      if(i!=j){
        # Calculates covariance of matrix
        cov_var_matrix[i,j]<-cov(x,y)
      }else{
        # Calcultes variance of matrix
        cov_var_matrix[i,j]<-var(x)
      }
    }
  }
  
  # If 1 returns max absolute value of matrix, 0 then return estimated cov var matrix
  if(with_max_abs_value==1){
    cat('max absolute value of all matrix entries is', '\n')
    max(cov_var_matrix)
  }else{
    cov_var_matrix
  }
}
est_cov_matrix(100,1)
est_cov_matrix(100,0)

# Function byte compiler
cmp_est_cov_matrix<-cmpfun(est_cov_matrix)

# Compare with covariance_matrix matrix
sum(est_cov_matrix(100,0))-sum(cmp_Task_1_func(100,0))
# Difference: 6564.701%%
sum(est_cov_matrix(500,0))-sum(cmp_Task_1_func(100,0))
# Difference: 3774.94%%
sum(est_cov_matrix(1000,0))-sum(cmp_Task_1_func(100,0))
# Difference: 786.925%%
sum(est_cov_matrix(10000,0))-sum(cmp_Task_1_func(100,0))
# Difference: 170.3221%%

# As seen from above, estimated portfolio variance approaches variance found in Task 1 
# as random sample 'n' generated increases

## Task 3 vi) ##
# Function that returns variance, returns and weights of portfolio 1 and 2
portfolio_1_2_vi<-function(show_port){
  # rownames of weights
  names<-c('asset1', 'asset2', 'asset3', 'asset4')
  # Extracting estimated cov var matrix from previous Task 1 vi) function
  Task_3_vi_est_cov_matrix<-cmp_est_cov_matrix(10000,0)
  # Removing rownames and colnames
  colnames(Task_3_vi_est_cov_matrix)<-NULL
  rownames(Task_3_vi_est_cov_matrix)<-NULL
  # portfolio mean from question
  port_1_mean<- 125
  port_2_mean<- 135
  asset_returns<-c(120,130,140,150)
  
  # Creating matrix based on Lagrange equations to calculate weights of portfolio
  # Assuming Ay=b where A, y and B are matrix, 
  # A is matrices_one, b is matrices_two (port 1) or matrices_three (port 2), 
  # y is weight_port_1 (port 1) or weight_port_2 (port 2)
  neg_asset_return_column_matrix<-matrix(-asset_returns, nrow=4, ncol=1)
  neg_asset_return_row_matrix<-t(neg_asset_return_column_matrix)
  
  asset_return_column_matrix<-matrix(asset_returns, nrow=4, ncol=1)
  asset_return_row_matrix<-t(asset_return_column_matrix)
  
  neg_ones_column_matrix<-matrix(-c(1,1,1,1), nrow=4, ncol = 1)
  neg_ones_row_matrix<-t(neg_ones_column_matrix)
  
  ones_column_matrix<-matrix(c(1,1,1,1), nrow=4, ncol = 1)
  ones_row_matrix<-t(ones_column_matrix)
  
  Task_3_vi_est_cov_matrix_x2<-2*Task_3_vi_est_cov_matrix
  matrices_one<-matrix(0, nrow=6, ncol=6, byrow=TRUE)
  matrices_one[1:4,1:4]<-Task_3_vi_est_cov_matrix_x2
  matrices_one[1:4,5]<-neg_asset_return_column_matrix
  matrices_one[1:4,6]<-neg_ones_column_matrix
  matrices_one[5,1:4]<-asset_return_row_matrix
  matrices_one[6,1:4]<-ones_row_matrix
  
  matrices_two<-matrix(c(0,0,0,0,port_1_mean, 1), nrow=6, ncol=1)
  matrices_three<-matrix(c(0,0,0,0,port_2_mean, 1), nrow=6, ncol=1)
  weight_port_1<-solve(matrices_one,matrices_two)
  weight_port_2<-solve(matrices_one,matrices_three)
  
  # Using matrices A and b to find matrices y
  weight_port_1<-as.matrix(weight_port_1[-5:-6,1])
  weight_port_2<-as.matrix(weight_port_2[-5:-6,1])
  
  # Assigning rownames of portfolio 1 and 2
  rownames(weight_port_1)<-names
  rownames(weight_port_2)<-names

  # Confirming that returns are same as expected portfolio return
  returns_portfolio_1<-asset_return_row_matrix%*%as.matrix(weight_port_1[1:4])
  returns_portfolio_2<-asset_return_row_matrix%*%as.matrix(weight_port_2[1:4])
  var_portfolio_1<-t(weight_port_1[1:4])%*%Task_3_vi_est_cov_matrix%*%weight_port_1[1:4]
  var_portfolio_2<-t(weight_port_2[1:4])%*%Task_3_vi_est_cov_matrix%*%weight_port_2[1:4]
  
  # 1,2, or 3 for variance, returns, or asset weights of portfolio 1 and
  # 4,5, or 6 for variance, returns or asset weights for portfolio 2
  if(show_port==1){
    var_portfolio_1
  }else if(show_port==2){
    returns_portfolio_1
  }else if(show_port==3){
    weight_port_1
  }else if(show_port==4){
    var_portfolio_2
  }else if(show_port==5){
    returns_portfolio_2
  }else{
    weight_port_2
  }
}
# Portfolio 1
# Variance
portfolio_1_2_vi(1)
# Returns
portfolio_1_2_vi(2)
# Asset weights
portfolio_1_2_vi(3)

# Portfolio 2
# Variance
portfolio_1_2_vi(4)
# Returns
portfolio_1_2_vi(5)
# Asset weights
portfolio_1_2_vi(6)

# Function byte compiler
cmp_portfolio_1_2_vi<-cmpfun(portfolio_1_2_vi)

## Task 3 vii) ##
# Function that plots effecient frontier for a given range of portfolio returns
effecient_frontier_vii<-function(){
  
  sd<-numeric(0)
  # a value to range portfolio total return% of 120% to 150%
  n<-31
  
  for(i in 1:n){
    portfolio_return<-119+(i)
    # Extract returns of portfolio 1 and 2
    returns_portfolio_1<-as.vector(cmp_portfolio_1_2_vi(2))
    returns_portfolio_2<-as.vector(cmp_portfolio_1_2_vi(5))
    # Remove assigned rownames and colnames
    rownames(returns_portfolio_1)<-NULL
    colnames(returns_portfolio_1)<-NULL
    rownames(returns_portfolio_2)<-NULL
    colnames(returns_portfolio_2)<-NULL
    
    # Calculate weights to investing in portfolio 1 and 2, t
    t<-(portfolio_return-returns_portfolio_1)/(returns_portfolio_2-returns_portfolio_1)
    # multiplying portfolio 1 and 2 variance with its weights then adding up to get the new variance
    final_weight<-(1-t)*as.matrix(portfolio_1_2_vi(3)[1:4])+t*as.matrix(portfolio_1_2_vi(6)[1:4])
    variance<-t(final_weight)%*%cmp_est_cov_matrix(10000,0)%*%final_weight
    sd[i]<-sqrt(variance)
  }
  # Plot expected return against standard deviation for ranging expected returns
  plot(sd, seq(120, 150, 1), main='Effecient frontier of 4 risky assets', xlab='standard deviation', ylab='expected return', type='l')
}
effecient_frontier_vii()

# Function byte compiler
cmp_effecient_frontier_vii<-cmpfun(effecient_frontier_vii)

## Task 3 viii) ##
# Function to return tangency portfolio weight or plot tangent line on effecient frontier
weight_or_value_tangency_port<-function(show_weight_return_sd){
  # Given risk free rate return
  risk_free_return<-105
  asset_returns<-c(120,130,140,150)
  
  
  asset_return_column_matrix<-matrix(asset_returns, nrow=4, ncol=1)
  asset_return_row_matrix<-t(asset_return_column_matrix)
  # Assume Ay=b with A,y, b being matrix
  # A is estimated covariance matrix, y is weights, b is excess returns of each asset
  # Find y by through%*% of (inverse of A) inverse_est_cov_matrix %*% b, OR solve(A,b)
  inverse_est_cov_matrix<-matrix.inverse(cmp_est_cov_matrix(10000,0))
  excess_returns_matrix<-matrix(asset_returns-risk_free_return, nrow = 4, ncol = 1)
  component_weight<-inverse_est_cov_matrix%*%excess_returns_matrix
  # sum of all weights (sum(y))
  sum_component<-sum(component_weight)
  # Final weight after adjusting to meet constraints
  weight_port<-(1/sum_component)*component_weight
  expected_return<-asset_return_row_matrix%*%weight_port
  variance<-t(weight_port)%*%cmp_est_cov_matrix(10000,0)%*%weight_port
  sd<-sqrt(variance)
  
  # 1 for asset weights, 2 for returns, and 3 for standard deviation
  if(show_weight_return_sd==1){
    weight_port
  }else if(show_weight_return_sd==2){
    expected_return
  }else{
    sd
  }
}
# Weights
weight_or_value_tangency_port(1)
#Expected return
weight_or_value_tangency_port(2)
# Standard deviation
weight_or_value_tangency_port(3)
# cmp_tangency_port<-cmpfun(weight_or_value_tangency_port)

## Task 3 ix) ##
# Function to plot Capital MArket Line
CML<-function(){
  # provided risk free rate returns
  risk_free_return<-105
  asset_returns<-c(120,130,140,150)
  sd<-0
  
  # Using risk_free_rate(+-)sqrt(H)*(varying dtadard deviation) to produce CML
  inverse_est_cov_matrix<-matrix.inverse(cmp_est_cov_matrix(10000,0))
  excess_returns_column_matrix<-matrix(asset_returns-risk_free_return, nrow=4, ncol=1)
  excess_returns_row_matrix<-matrix(asset_returns-risk_free_return, nrow=1, ncol=4)
  H<-excess_returns_row_matrix%*%inverse_est_cov_matrix%*%excess_returns_column_matrix
  
  y1<-numeric(0)
  y2<-numeric(0)
  sdev<-numeric(0)

  for(i in 1:200){
    sdev[i]<-sd+(i-1)/2
    y1[i]<-risk_free_return+sqrt(H)*sdev[i]
    y2[i]<-risk_free_return-sqrt(H)*sdev[i]
  }
  cmp_effecient_frontier_vii()
  lines(sdev, y1, type="l")
  lines(sdev, y2, type="l")
}
CML()

## Task 3 x) ##
# Function that calculates weights based on Lagrange method
est_weight_lagrange<-function(){
  # Assume Ay=b with A,y, b being matrix
  # A is matrices, y is weight, b is column_matrix
  # Find y by through solve(A,b)
  names<-c('asset1', 'asset2', 'asset3', 'asset4')
  column_matrix<-matrix(c(0,0,0,0,1), nrow=5, ncol=1)
  ones_column_matrix<-matrix(c(-1,-1,-1,-1,0), nrow=5, ncol = 1)
  ones_row_matrix<-t(-ones_column_matrix)
  
  matrices<-matrix(0, nrow=5, ncol=5, byrow=TRUE)
  matrices[1:4,1:4]<-cmp_est_cov_matrix(10000,0)*2
  matrices[1:5,5]<-ones_column_matrix
  matrices[5,1:5]<-ones_row_matrix

  weight<-solve(matrices,column_matrix)
  weight<-as.matrix(weight[1:4,1])
  rownames(weight)<-names
  weight
  
}
est_weight_lagrange()

# Function that calculates weights based on formula in question
est_weight_formula_or_var<-function(weight_or_variance){
  ## Answer from formula given in question based estimated cov matrix ##
  ones_column_matrix<-matrix(c(1,1,1,1), nrow=4, ncol = 1)
  ones_row_matrix<-t(ones_column_matrix)
  C<-cmp_est_cov_matrix(10000,0)
  C_inverse<-matrix.inverse(C)
  answer<-(ones_row_matrix%*%C_inverse%*%ones_column_matrix)
  # returns error if divided by 'answer', so instead I added sum which still yields same value
  answer_value<-sum(answer)
  answer_2<-(C_inverse%*%ones_column_matrix)
  est_covar_matrix_weights<-(answer_2)/answer_value
  
  #calculate min var
  cov_matrix<-cmp_est_cov_matrix(10000,0)
  ## min variance among all possible portfolios ##
  min_var_all_port<-t(est_covar_matrix_weights)%*%cov_matrix%*%est_covar_matrix_weights
  
  if(weight_or_variance==0){
    est_covar_matrix_weights
  }else{
    min_var_all_port
  }
  
}
# weights
est_weight_formula_or_var(0)
# Min variance
est_weight_formula_or_var(1)

# Function that calculates difference in weights of the above two methods
weight_diff<-function(){
  # Weight difference for two methods above
  difference_weights<-est_weight_lagrange()-est_weight_formula()
  difference_weights
  # +-5.5511153-17
}
weight_diff()

## Task 3 xi) ##
# Function to find portfolio weight under special constraint
portfolio_weight_xi<-function(){
  names<-c('asset1', 'asset2', 'asset3', 'asset4')
  column_matrix_x<-matrix(c(-1, -1, -1, -1,0), nrow=5, ncol=1)
  ones_row_matrix_x<-t(-column_matrix_x)
  
  # Assume Ay=b with A,y, b being matrix
  # A is matrices_six, y is weights12_restrain, b is column_matrix_seven
  # Find y by through solve(A,b)
  matrices_six<-matrix(0, nrow=6, ncol=6, byrow=TRUE)
  matrices_six[1:4,1:4]<-cmp_est_cov_matrix(10000,0)*2
  matrices_six[1:5,6]<-column_matrix_x
  matrices_six[1:2,5]<-matrix(c(-1,1), nrow=2, ncol=1)
  matrices_six[6,1:5]<-ones_row_matrix_x
  matrices_six[5,1:2]<-matrix(c(1,-1), nrow=1, ncol=2)
  column_matrix_seven<-matrix(c(0,0,0,0,0,1), nrow=6, ncol=1)
  weights12_restrain<-solve(matrices_six,column_matrix_seven)
  weights_portfolio<-as.matrix(weights12_restrain[1:4])
  rownames(weights_portfolio)<-names
  weights_portfolio
}
portfolio_weight_xi()

