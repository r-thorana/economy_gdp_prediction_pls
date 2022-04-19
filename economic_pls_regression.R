

regression_data = as.matrix(read.table("C:/MS/FALL_2021/MULTIVARIATE/R_assignments/Economic_Dataset.txt",header=TRUE))
print(colnames(regression_data))
y = matrix(regression_data[-c(5) ,1],ncol=1)
n = length(y)
X = regression_data[-c(5),-c(1)]
X = as.matrix(X)
p = ncol(X)
sdX_hat = array(1,p)

## Now, we are starting the algorithm
#######################################################
#######################################################
k = 2 
step = 0

muX_hat = colMeans(X)
muY_hat = mean(y)
print(muY_hat)
X_temp = matrix(0,n,p)
for (j in 1:p) X_temp[ ,j] = X[ ,j] - muX_hat[j]
y_temp = y - muY_hat
TSS = sum(y^2) - n*((muY_hat)^2)

B_hat = matrix(0,p,k)
R_hat = matrix(0, p, k)
q_hat = matrix(0,k,1)
Sigma_XY_Estimate_Function =
function(X_dummy,y_dummy)
{c(1/(n-1))*((t(X_dummy)-colMeans(X_dummy))%*%(y_dummy - mean(y_dummy)))}
for (step in 1:k)
{
 Sigma_XY_hat = Sigma_XY_Estimate_Function(X_temp,y_temp)
 b_hat = Sigma_XY_hat/sqrt(sum(Sigma_XY_hat^2))
 t_predictor = X_temp%*%b_hat
 r_hat = array(0,p)

 for (j in 1:p)
 {
 out = lm(X_temp[ ,j]~0+t_predictor) 
 r_hat[j] = out$coefficients
 X_temp[ ,j] = out$residuals
 }
 out = lm(y_temp~0+t_predictor)
 q_hat_value = out$coefficients
 y_temp = out$residuals
 B_hat[ , step] = b_hat
 R_hat[ , step] = r_hat
 q_hat[step] = q_hat_value
 SSE = sum(y_temp^2)
 R2 = 1 - SSE/TSS
 R2_adjusted = ((n-1)*R2 - step)/(n-step-1)
 print(R2_adjusted)
 #t_predictors = sum(X_temp *B_hat[ ,step])
 #print(t_predictors)
}
print(B_hat)
print(R_hat)
print(R2_adjusted)
print(q_hat)

## prediction for a future observation:

X_new = regression_data[5,-c(1) ]
print(X_new)
step = 0
X_temp_new = array(0,p)
for (j in 1:p) X_temp_new[j] = X_new[j] - muX_hat[j] 
y_pred_new = muY_hat
print(y_pred_new)
for (step in 1:k)
{
 t_predictor_new = sum(X_temp_new*B_hat[ ,step])
 
 for (j in 1:p)
 {
 X_temp_new[j] = X_temp_new[j] - R_hat[j,step]*t_predictor_new
 }
 y_pred_new = y_pred_new + q_hat[step]*t_predictor_new
 print(y_pred_new)
}
print(y_pred_new)


################ pls algorithm for 5 predictors##############

#######################################################
#######################################################
k = 4 
step = 0

muX_hat = colMeans(X)
muY_hat = mean(y)
X_temp = matrix(0,n,p)
for (j in 1:p) X_temp[ ,j] = X[ ,j] - muX_hat[j]
y_temp = y - muY_hat
TSS = sum(y^2) - n*((muY_hat)^2)

B_hat = matrix(0,p,k)
R_hat = matrix(0, p, k)
q_hat = matrix(0,k,1)
Sigma_XY_Estimate_Function =
function(X_dummy,y_dummy)
{c(1/(n-1))*((t(X_dummy)-colMeans(X_dummy))%*%(y_dummy - mean(y_dummy)))}
for (step in 1:k)
{
 Sigma_XY_hat = Sigma_XY_Estimate_Function(X_temp,y_temp)
 b_hat = Sigma_XY_hat/sqrt(sum(Sigma_XY_hat^2))
 t_predictor = X_temp%*%b_hat
 r_hat = array(0,p)

 for (j in 1:p)
 {
 out = lm(X_temp[ ,j]~0+t_predictor) 
 r_hat[j] = out$coefficients
 X_temp[ ,j] = out$residuals
 }
 out = lm(y_temp~0+t_predictor)
 q_hat_value = out$coefficients
 y_temp = out$residuals
 B_hat[ , step] = b_hat
 R_hat[ , step] = r_hat
 q_hat[step] = q_hat_value
 SSE = sum(y_temp^2)
 R2 = 1 - SSE/TSS
 R2_adjusted = ((n-1)*R2 - step)/(n-step-1)
 print(R2_adjusted)
 ##t_predictors = sum(X_temp *B_hat[ ,step])
 ##print(t_predictors)
}
print(B_hat)
print(R_hat)
print(R2_adjusted)
print(q_hat)

## prediction for a future observation:

X_new = regression_data[5,-c(1) ]
print(X_new)
step = 0
X_temp_new = array(0,p)
for (j in 1:p) X_temp_new[j] = X_new[j] - muX_hat[j] 
y_pred_new = muY_hat
print(y_pred_new)
for (step in 1:k)
{
 t_predictor_new = sum(X_temp_new*B_hat[ ,step])
 
 for (j in 1:p)
 {
 X_temp_new[j] = X_temp_new[j] - R_hat[j,step]*t_predictor_new
 }
 y_pred_new = y_pred_new + q_hat[step]*t_predictor_new
 print(y_pred_new)
}
print(y_pred_new)