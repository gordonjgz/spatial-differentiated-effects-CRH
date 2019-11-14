
## Calibration

alpha_a = 0.2
alpha_n = 0.1
beta_t=0.3
theta=2.5
sigma=0.5

#C = (beta_t*(wage_rent_ratio^aplha_a* z_a)^(sigma-1) + (1-beta_t)* (wage_rent_ratio^alpha_n*z_n)^(sigma-1) )^(1/(sigma-1))

#z_a = (1/rep_p_t) * (C/(wage_rent_ratio^alpha_a))

#z_n = (C/(w^alpha_n)) * ((1-beta_t)/(1-beta_t*(1/rel_p_t)^(sigma-1)))^(1/(1-sigma))

## solve z_a and z_n using optim

library(stats)

process <- function (v_a, n){
  #process <- function (v_a, n, u){
  rel_p_t=((1/beta_t)* (((1-alpha_n)*v_a)/ ((1-alpha_a)+(alpha_a-alpha_n)*v_a) ))^(1/(1-sigma)) # v_a : observed share of employment in agriculture
  wage_rent_ratio=(1/n) * ( ((1-alpha_a)*(1-alpha_n)) /( alpha_n*(1-alpha_a) + (alpha_a-alpha_n)*v_a)) # n : population densities
  
  fn <- function(a, b) {
    C = (beta_t*(wage_rent_ratio^alpha_a* a )^(sigma-1) + (1-beta_t)* (wage_rent_ratio^alpha_n*b )^(sigma-1) )^(1/(sigma-1)) #(17)
    ratio = (C)/ ((wage_rent_ratio)^alpha_a * a)
    mass_ratio = ((1-beta_t)/(1-beta_t*(1/rel_p_t)^(sigma-1)))^(1/(sigma-1))
    mass_ratio = (C)/ ((wage_rent_ratio)^alpha_n * b)
    return(c(ratio, mass_ratio))
  }
  
  fn2 <- function(x) crossprod(fn(x[1], x[2]) - c(rel_p_t, ((1-beta_t)/(1-beta_t*(1/rel_p_t)^(sigma-1)))^(1/(sigma-1))))
  result = optim(c(1, 1), fn2)$`par`
  z_a = result[1]
  z_n = result[2]
  u = (beta_t*(wage_rent_ratio^alpha_a* z_a )^(sigma-1) + (1-beta_t)* (wage_rent_ratio^alpha_n*z_n )^(sigma-1) )^(1/(sigma-1)) #u universal utility
  return(c(z_a, z_n, u))
  #z_a = u/(wage_rent_ratio^alpha_a) * 1/rel_p_t
  #z_n = u/(wage_rent_ratio^alpha_n) * ((1-beta_t)/ (1-beta_t*(rel_p_t^(1-sigma))))^ (1/(1-sigma))
  #return(c(z_a, z_n))
  
}

## Counterfactual # v_a is the original v_a (pre v_a)

counterfactual <-function (v_a, n, v_a_new, n_new){ #given old and new argi share and pop density
  #u =0.428420377
  #result =process(v_a, n, u)
  result =process(v_a, n)
  z_a = result[1]
  z_n = result[2]
  u = result[3]
  
  #u_new = 1
  #result_new =process(v_a_new, n_new, u_new)
  result_new =process(v_a_new, n_new)
  z_a_new = result_new[1]
  z_n_new = result_new[2]
  u_new = result_new[3]
  
  u_diff = (u_new-u)/u
  z_a_diff = log(z_a_new) - log(z_a)
  z_n_diff = log(z_n_new) - log(z_n)
  
  wage_rent_ratio_diff = ((1-alpha_n)*v_a * (u_diff-z_a_diff) +(1-alpha_a)*(1-v_a)*(u_diff - z_n_diff)) / ( alpha_a*(1-alpha_n)*v_a + alpha_n*(1-alpha_a)*(1-v_a))
  wage_rent_ratio_diff = max(wage_rent_ratio_diff, -0.99999)
  rel_p_t_diff =((1-alpha_a)* (1-v_a)* (alpha_a*z_n_diff - alpha_n* z_a_diff - (alpha_a - alpha_n)*u_diff)) / (alpha_a * (1-alpha_n)* v_a + alpha_n * (1-alpha_a)* (1-v_a))
  rel_p_t_diff = max(rel_p_t_diff, -0.99999)
  v_a_diff = (1+ (alpha_a-alpha_n)/(1-alpha_n)*v_a) * (1-sigma) * rel_p_t_diff
  v_a_diff = max(v_a_diff, -0.99999)
  n_diff = ((alpha_a-alpha_n)*v_a)/(alpha_n*(1-alpha_a)+(alpha_a-alpha_n)*v_a)*v_a_diff - wage_rent_ratio_diff # model predicted
  n_diff = max(n_diff, -0.99999)
  
  n_new_predict = n_diff*n+n
  v_a_new_predict = v_a_diff*v_a+v_a
  return (c(u, u_new, z_a_diff, z_n_diff)) # we have the predicted value of new agri share and pop density
}

data = read.csv("Final_data.csv")


# 2007 vs 2017

data_0717 = data.frame(cbind(as.character(data$city_name), data$density_city_2007, data$trade_ratio_city_2007, data$density_city_2017, data$trade_ratio_city_2017))
colnames(data_0717) = c("name", "n_2007", "v_a_2007", "n_2017", "v_a_2017")
data_0717$n_2007 = as.numeric(as.character(data_0717$n_2007))
data_0717$n_2017 = as.numeric(as.character(data_0717$n_2017))
data_0717$v_a_2007 = as.numeric(as.character(data_0717$v_a_2007))
data_0717$v_a_2017 = as.numeric(as.character(data_0717$v_a_2017))

cf_0717 = counterfactual(data_0717[1, ]$v_a_2007,  data_0717[1, ]$n_2007, data_0717[1, ]$v_a_2017, data_0717[1, ]$n_2017)

for(i in 2: 285){
  row = counterfactual(data_0717[i, ]$v_a_2007,  data_0717[i, ]$n_2007, data_0717[i, ]$v_a_2017, data_0717[i, ]$n_2017)
  cf_0717=rbind(cf_0717, row)
}

cf_0717 = cbind(as.character(data_0717$name), cf_0717)
cf_0717 = cbind(cf_0717, data_0717$v_a_2017, data_0717$n_2017)
colnames(cf_0717)=c("name", "u_07","u_17", "delta_z_m","delta_z_n", "v_a", "n")

cf_07_17_df = data.frame(cf_0717)

#cf_07_17_df$Model_Perf_va = as.numeric(as.character(cf_07_17_df$v_a_hat)) - as.numeric(as.character(cf_07_17_df$v_a))

#cf_07_17_df$Model_Perf_n = as.numeric(as.character(cf_07_17_df$n_hat)) - as.numeric(as.character(cf_07_17_df$n))

#cf_07_17_df$Model_Perf=NULL

#cf_07_17_df = cbind(cf_07_17_df, data$chr_dummy, data$year_chr, data$trade_accessible_dummy)

cf_07_17_df = cbind(cf_07_17_df, data$density_city_2007, data$trade_ratio_city_2007)

cf_07_17_df$v_a = as.numeric(as.character(cf_07_17_df$v_a))
#cf_07_17_df$v_a_hat = as.numeric(as.character(cf_07_17_df$v_a_hat))
cf_07_17_df$n = as.numeric(as.character(cf_07_17_df$n))
#cf_07_17_df$n_hat = as.numeric(as.character(cf_07_17_df$n_hat))

rownames(cf_07_17_df)=cf_07_17_df$name

#install.packages("car")
library(ggplot2)
library(ggrepel)
library(ggpubr)

#p1 = ggplot(cf_07_17_df, aes(v_a, v_a_hat)) +
#  geom_point(color = ifelse(abs(cf_07_17_df$Model_Perf_va)>0.08, "tomato2", "grey30"))+
#  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
 # geom_text(data=data.frame(x=0.18, y=0.15), aes(x, y), label = "y=x", size=5)+
 # geom_text_repel(data = subset(cf_07_17_df, abs(Model_Perf_va)>0.08), aes(label = name))+ labs(title = "Plot of Trade Sector Employment Shares, 2017", x="Actual Trade Sector Employment Shares", y="Predicted Trade Sector Employment Shares")

#p2 = ggplot(cf_07_17_df, aes(n, n_hat)) +
#  geom_point(color = ifelse(abs(cf_07_17_df$Model_Perf_n)>650, "tomato2", "grey30"))+
#  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
 # geom_text(data=data.frame(x=0, y=-150), aes(x, y), label = "y=x", size=5)+
 # geom_text_repel(data = subset(cf_07_17_df, abs(Model_Perf_n)>650), aes(label = name)) + labs(title = "Plot of Population Density, 2017", x="Actual Population Density(people/km^2)", y="Predicted Population Density(people/km^2)")

#ggarrange(p1, p2 ,ncol = 2, nrow = 1)

write.csv(cf_07_17_df, file="counterfactual_07_17.csv")


u_17 = as.numeric(as.character(cf_07_17_df$u_17))
u_07 = as.numeric(as.character(cf_07_17_df$u_07))

N_L = as.numeric(as.character(data$N_2017))
N_L_pre = as.numeric(as.character(data$N_2007))

#N_L = N_L[-121]
#N_L_pre = N_L_pre[-121]

N = sum(N_L)
N_pre = sum(N_L_pre)


a=1

eta=0.0108

matrix_after = read.csv("Travel_Matrix_CRH.csv")
city_row_a = matrix_after$X 


matrix_after$X = NULL

kappa_after = data.matrix(matrix_after)

#kappa_after = kappa_after[-286:-287, ]

kappa = kappa_after

#kappa[is.na(kappa)] = 10000000000
# Calibration 1
kappa = exp(eta* kappa^a)
 # Calibration 2
#kappa = 1+kappa^0.6

colnames(kappa) = NULL

sum_cost_after = colSums(kappa)
mean_cost_after = colSums(kappa)

c = matrix(rep(u_17,285), nrow = 285, ncol = 285, byrow = T)


kappa_c = kappa^(-theta) * c^(theta)



kappa_c_t = t(kappa_c) #l*o

N_0 = N_L_pre / N_pre

N_1 = N_L / N

amenity <-function(x){# x is a length 285 vector
  X = matrix(x, nrow = 285, ncol = 1, byrow = T)
  wma = kappa_c %*% X #dim(wma) = o*1 i.e. 285*1
  wma_inver = 1/wma
  wma_prepop = wma_inver * N_0 #o*1
  sum_on_o = kappa_c_t %*% wma_prepop
  sum_on_o_inv = 1/sum_on_o
  RHS = N_1 * sum_on_o_inv
  #return(c(RHS - X))
  return(RHS)
}


#b = fsolve(amenity, x0 = u)

b0 = u_17

count = 1

err =c()

err[1] = 100

#err = 100

while (err[count]>0.0001) {
#while (count<100) {
  b_new = amenity(b0)
  count = count+1
  err[count] = mean((b_new-b0)^2)
  b_out = b_new
  b0 = b_new
}

b_CRH = b_out

# now input kappa as the old travel matrix

matrix_before = read.csv("Travel_Matrix_Before.csv")
city_row_b = matrix_before$X 


matrix_before$X = NULL

kappa_before = data.matrix(matrix_before, rownames.force = NA)

kappa_before = kappa_before[-286:-287, ]

kappa = kappa_before

#kappa[is.na(kappa)] = 10000000000
kappa = exp(eta* kappa^a)
#kappa = 1+kappa^0.6

colnames(kappa) = NULL

sum_cost_before = colSums(kappa)
mean_cost_before = colSums(kappa)

c = matrix(rep(u_07,285), nrow = 285, ncol = 285, byrow = T)

kappa_c = kappa^(-theta) * c^(theta)

kappa_c_t = t(kappa_c) #l*o


#b = fsolve(amenity, x0 = u)

b0 = u_07

count = 1

err = c()

err[count] = 100

while (err[count]>0.0001) {
#while (count<100) {
  b_new = amenity(b0)
  count = count+1
  err[count] = mean((b_new-b0)^2)
  b_out = b_new
  b0 = b_new
}

b_reg = b_out

b=cbind(as.character(data$city_name),as.character(city_row_b), b_reg,as.character(city_row_a), b_CRH)

delta_b = log(b_CRH) - log(b_reg)

write.csv(x = b, file = "b_result.csv")

#delta_b_2 = log(b_reg) - log(b_CRH)

delta_z_m = as.numeric(as.character(cf_07_17_df$delta_z_m))
delta_z_n = as.numeric(as.character(cf_07_17_df$delta_z_n))

delta_n = log(as.numeric(as.character(cf_07_17_df$n))) - log(as.numeric(as.character(cf_07_17_df$`data$density_city_2007`)))

delta_v_m = as.numeric(as.character(cf_07_17_df$v_a)) - as.numeric(as.character(cf_07_17_df$`data$trade_ratio_city_2007`))

delta_v_n = log(1-as.numeric(as.character(cf_07_17_df$v_a))) - log(1-as.numeric(as.character(cf_07_17_df$`data$trade_ratio_city_2007`)))

N_L_0 = log(N_L_pre*10000)
N_L_1 = log(N_L*10000)

library(lfe)

# Shanghai Case

#b_SH_before = b[178, 3]
#b_SH_after = b[178, 5]

#N_SH_after = N_1[178]

#N_SH_before = N_0[178]

#u_SH = u[178]

#r = c()

#p=17

wma <- function(o){#o is the index of origin in kappa matrix
  result = 0
  for(k in 1: 285){
    if(o==k){
      result =result + (0.000000001)^(-theta) * as.numeric(as.character(b[k, 3])) * u[k]^theta
    }else{
      result = result + (kappa[o, k])^(-theta) * as.numeric(as.character(b[k, 3])) * u[k]^theta
    }
  }
  return(result)
}


#for (i in 1:285) {

 # r[i]=kappa[i, p]^(-theta) * u[p]^(theta)/wma(i) * N_0[i]
#}

#b_check = N_1[p] /sum(r)

# regression

#########################################################ols###############################################
ols_v_m = lm(delta_v_m~delta_b + city_cat - 1)

#Call:
 # lm(formula = delta_v_m ~ delta_b + city_cat - 1)

#Residuals:
 # Min       1Q   Median       3Q      Max 
#-0.36288 -0.02826 -0.00380  0.02905  0.23670 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#delta_b    -0.476828  0.0040974  19.114  < 2e-16 ***
 # city_cat1 -0.0602455  0.0331985  -1.815  0.07064 .  
#city_cat2 -0.0105862  0.0106299  -0.996  0.32016    
#city_cat3 -0.0007433  0.0085867  -0.087  0.93108    
#city_cat4  0.0131692  0.0049200   2.677  0.00787 ** 
 # ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.06637 on 280 degrees of freedom
#Multiple R-squared:  0.573,	Adjusted R-squared:  0.5654 
#F-statistic: 75.14 on 5 and 280 DF,  p-value: < 2.2e-16

ols_v_n =  lm(delta_v_n~delta_b + N_L_0)
#Call:
#  lm(formula = delta_v_n ~ delta_b + N_L_0)

#Residuals:
 # Min       1Q   Median       3Q      Max 
#-1.23454 -0.08980  0.01511  0.12221  0.59230 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.45802    0.27212   5.358 1.75e-07 ***
 # delta_b      0.07947    0.03174   2.504   0.0128 *  
 # N_L_0       -0.09823    0.01808  -5.433 1.20e-07 ***
#  ---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.2112 on 282 degrees of freedom
#Multiple R-squared:  0.1134,	Adjusted R-squared:  0.1071 
#F-statistic: 18.03 on 2 and 282 DF,  p-value: 4.267e-08
ols_n =  lm(delta_n~delta_b + city_cat - 1)
#Call:
 # lm(formula = delta_n ~ delta_b + city_cat - 1)

#Residuals:
 # Min       1Q   Median       3Q      Max 
#-0.37648 -0.04157 -0.00379  0.04242  0.54985 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#delta_b   0.014720   0.005028   2.927 0.003698 ** 
 # city_cat1 0.081085   0.040740   1.990 0.047532 *  
#  city_cat2 0.092794   0.013045   7.114 9.48e-12 ***
 # city_cat3 0.038754   0.010537   3.678 0.000282 ***
  #city_cat4 0.046634   0.006038   7.724 2.02e-13 ***
  #---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#################################################2sls########################################################

#iv
dist_to_four = read.csv("Distance_to_four.csv")
var_to_four = aggregate(x= as.numeric(as.character(dist_to_four$Total_Length)), by = list(dist_to_four$OriginID), FUN = var)

sum_to_four = aggregate(x= as.numeric(as.character(dist_to_four$Total_Length)), by = list(dist_to_four$OriginID), FUN = sum)

#zeta = var_to_four$x

zeta = sum_to_four$x

tsls2_v_m = ivreg(delta_v_m ~ delta_b + city_cat -1 | city_cat -1 + zeta)
summary(tsls2_v_m,vcov = sandwich, diagnostics = TRUE)

#Call:
 # ivreg(formula = delta_v_m ~ delta_b + city_cat - 1 | city_cat - 
  #        1 + zeta)

#Residuals:
 # Min         1Q     Median         3Q        Max 
#-0.4666310 -0.0277004 -0.0007928  0.0343363  0.2808472 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#delta_b    -0.117318   0.017638   6.652 1.52e-10 ***
#  city_cat1 -0.068516   0.053561  -1.279   0.2019    
#city_cat2 -0.012289   0.010069  -1.220   0.2233    
#city_cat3 -0.006005   0.011872  -0.506   0.6134    
#city_cat4  0.013310   0.005171   2.574   0.0106 *  
  
 # Diagnostic tests:
  #df1 df2 statistic  p-value    
#Weak instruments   1 280    12.562 0.000461 ***
 # Wu-Hausman         1 279     3.855 0.050598 .  
#Sargan             0  NA        NA       NA    
#---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.07636 on 280 degrees of freedom
#Multiple R-Squared: 0.4348,	Adjusted R-squared: 0.4247 
#Wald test: 10.48 on 5 and 280 DF,  p-value: 3.091e-09 


tsls2_n = ivreg(delta_n ~ delta_b + city_cat -1 | city_cat -1 + zeta)
summary(tsls2_n ,vcov = sandwich, diagnostics = TRUE)

#Call:
 # ivreg(formula = delta_n ~ delta_b + city_cat - 1 | city_cat - 
  #        1 + zeta)

#Residuals:
 # Min         1Q     Median         3Q        Max 
#-0.5272133 -0.0371277 -0.0007244  0.0406498  0.5353834 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#delta_b   0.054596   0.024450   2.233   0.0263 *  
 # city_cat1 0.072629   0.013503   5.379 1.59e-07 ***
  #city_cat2 0.091053   0.020690   4.401 1.53e-05 ***
  #city_cat3 0.033375   0.013720   2.432   0.0156 *  
  #city_cat4 0.046778   0.005235   8.935  < 2e-16 ***
  
  #Diagnostic tests:
  #df1 df2 statistic  p-value    
#Weak instruments   1 280    12.562 0.000461 ***
 # Wu-Hausman         1 279     2.788 0.096080 .  
#Sargan             0  NA        NA       NA    
#---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.09014 on 280 degrees of freedom
#Multiple R-Squared: 0.181,	Adjusted R-squared: 0.1664 
#Wald test: 31.55 on 5 and 280 DF,  p-value: < 2.2e-16 

#################
##Counterfactual#
#################

y.hat_v_m = fitted.values(ols_v_m)

cf_v_m = data.frame(cbind(as.character(cf_07_17_df$name),as.numeric(as.character(delta_v_m)), as.numeric(as.character(y.hat_v_m))))

colnames(cf_v_m) = c("name", "d_v_m", "d_v_m_hat")

cf_v_m$d_v_m = as.numeric((as.character(cf_v_m$d_v_m)))
cf_v_m$d_v_m_hat = as.numeric((as.character(cf_v_m$d_v_m_hat)))


cf_v_m$e = abs(cf_v_m$d_v_m - cf_v_m$d_v_m_hat)


p1 = ggplot(cf_v_m, aes(d_v_m, d_v_m_hat)) +
  geom_point(color = ifelse(cf_v_m$e>0.35, "tomato2", "grey30"))+
  #geom_point(color = "grey30") +
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=-0.3, y=-0.175), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_v_m, e>0.45), aes(label = name))+ 
  labs(title = "Counterfactual, Trade Employment Share", x= "Log Difference of the Actual Trade Employment Share", y="Fitted Value")
p1

y.hat_v_n = fitted.values(ols_v_n)

cf_v_n = data.frame(cbind(as.character(cf_07_17_df$name),as.numeric(as.character(delta_v_n)), as.numeric(as.character(y.hat_v_n))))

colnames(cf_v_n) = c("name", "d_v_n", "d_v_n_hat")

cf_v_n$d_v_n = as.numeric((as.character(cf_v_n$d_v_n)))
cf_v_n$d_v_n_hat = as.numeric((as.character(cf_v_n$d_v_n_hat)))

cf_v_n$e = abs(cf_v_n$d_v_n - cf_v_n$d_v_n_hat)


p2 = ggplot(cf_v_n, aes(d_v_n, d_v_n_hat)) +
  geom_point(color = ifelse(cf_v_n$e>0.35, "tomato2", "grey30"))+
  #geom_point(color = "grey30") +
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=-0.3, y=-0.2), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_v_n, e>0.42), aes(label = name))+ 
  labs(title = "Counterfactual, Non-Trade Employment Share", x= "Log Difference of the Actual Non-Trade Employment Share", y="Fitted Value")
p2

y.hat_n = fitted.values(ols_n)

cf_n = data.frame(cbind(as.character(cf_07_17_df$name),as.numeric(as.character(delta_n)), as.numeric(as.character(y.hat_n))))

colnames(cf_n) = c("name", "d_n", "d_n_hat")

cf_n$d_n = as.numeric((as.character(cf_n$d_n)))
cf_n$d_n_hat = as.numeric((as.character(cf_n$d_n_hat)))

cf_n$e = abs(cf_n$d_n - cf_n$d_n_hat)


p3 = ggplot(cf_n, aes(d_n, d_n_hat)) +
  geom_point(color = ifelse(cf_n$e>0.15, "tomato2", "grey30"))+
  #geom_point(color = "grey30") +
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=-0.08, y=-0.012), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_n, e>0.15), aes(label = name))+ 
  labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")

p3

library("cowplot")

plot_grid(p1, p2, p3, labels = "AUTO")

########################################################
#####Sufficient Statistics - Amenity and Trade Cost#####
########################################################

s_s_data = data.frame(cbind(sum_cost_after, sum_cost_before, mean_cost_after, mean_cost_before, b_reg, b_CRH))

colnames(s_s_data) = c("sum_cost_after", "sum_cost_before", "mean_cost_after","mean_cost_before", "b_reg", "b_CRH")

ggplot(s_s_data , aes(log(sum_cost_after), log(b_CRH))) +
  geom_point(color = "grey30") +
  geom_smooth(method = "lm")
  #labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")

ggplot(s_s_data , aes(log(sum_cost_before), log(b_reg))) +
  geom_point(color = "grey30") +
  geom_smooth(method = "lm")
#labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")

ggplot(s_s_data , aes(log(mean_cost_after), log(b_CRH))) +
  geom_point(color = "grey30") +
  geom_smooth(method = "lm")
#labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")

ggplot(s_s_data , aes(log(mean_cost_before), log(b_reg))) +
  geom_point(color = "grey30") +
  geom_smooth(method = "lm")
#labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")

P11 = ggplot(s_s_data, aes(x = log(b_reg))) +
  geom_histogram(aes(y =..density..), colour = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept=mean(log(b_reg))), color = "blue", linetype = "dashed", size = 1)

P22 = ggplot(s_s_data, aes(x = log(b_CRH))) +
  geom_histogram(aes(y =..density..), colour = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept=mean(log(b_CRH))), color = "blue", linetype = "dashed", size = 1)

b_reg_df = data.frame(cbind(b_reg))
b_CRH_df = data.frame(cbind(b_CRH))

cost_df_before = data.frame(cbind(sum_cost_before, mean_cost_before))
cost_df_after = data.frame(cbind(sum_cost_after, mean_cost_after))

colnames(b_reg_df) = c("log(b)")
colnames(b_CRH_df) = c("log(b)")

colnames(cost_df_before) = c("Sum_Travel_cost", "Mean_Travel_cost")
colnames(cost_df_after) = c("Sum_Travel_cost", "Mean_Travel_cost")

cost_df_before$category = 'Before CRH'
cost_df_after$category = 'After CRH'

b_reg_df$category = 'Before CRH'
b_CRH_df$category = 'After CRH'

rownames(b_reg_df) = NULL
rownames(b_CRH_df) = NULL

rownames(cost_df_before) = NULL
rownames(cost_df_after) = NULL

catlength = rbind(b_reg_df, b_CRH_df)
catlength$`log(b)` = log(catlength$`log(b)`)

p = ggplot(catlength, aes(`log(b)`, fill = category)) + geom_density(alpha = 0.5) + geom_vline(aes(xintercept=mean(log(b_reg))), color = "blue", linetype = "dashed", size = 0.5)+ geom_vline(aes(xintercept=mean(log(b_CRH))), color = "red", linetype = "dashed", size = 0.5)
   #+ labs(title = "Distribution of Amenity for Before and After CRH", xlab = "log(b)", ylab = "density")
p1 = p + ggtitle("Density Plot of Amenity") + xlab("Log Amenity") + ylab("")


catlen = rbind(cost_df_before, cost_df_after)
catlen$Sum_Travel_cost = log(catlen$Sum_Travel_cost)
catlen$Mean_Travel_cost = log(catlen$Mean_Travel_cost)

p = ggplot(catlen, aes(Sum_Travel_cost, fill = category)) + geom_density(alpha = 0.5)
p2 = p + ggtitle("Density Plot of Trade Cost") + xlab("Log Trade Cost") + ylab("")

plot_grid(p1, p2, labels = "AUTO")
#ggplot(catlen, aes(Mean_Travel_cost, fill = category)) + geom_density(alpha = 0.5)

###########################
#####Reduced_Reg Level#####
###########################

v_m_17 = log(as.numeric(as.character(cf_07_17_df$v_a)))
#v_m_17 = as.numeric(as.character(cf_07_17_df$v_a))

v_m_07 = log(as.numeric(as.character(cf_07_17_df$`data$trade_ratio_city_2007`)))
#v_m_07 = as.numeric(as.character(cf_07_17_df$`data$trade_ratio_city_2007`))

n_17 = log(as.numeric(as.character(cf_07_17_df$n)))
n_07 = log(as.numeric(as.character(cf_07_17_df$`data$density_city_2007`)))

v_n_17 = log(1-as.numeric(as.character(cf_07_17_df$v_a)))
v_n_07 = log(1-as.numeric(as.character(cf_07_17_df$`data$trade_ratio_city_2007`)))

city_name = as.character(cf_07_17_df$name)

city_cat = as.factor(data$category)

rownames(city_name) = NULL

b_17 = log(b_CRH)
#b_17 = b_CRH
b_07 = log(b_reg)
#b_07 = b_reg

sum_kappa_07 = log(sum_cost_before)
sum_kappa_17 = log(sum_cost_after)

mean_kappa_07 = log(mean_cost_before)
mean_kappa_17 = log(mean_cost_after)


before_df = data.frame(cbind(city_name, city_cat, b_07, sum_kappa_07, mean_kappa_07, v_m_07, v_n_07, n_07, N_L_0))
after_df = data.frame(cbind(city_name, city_cat, b_17, sum_kappa_17, mean_kappa_17, v_m_17, v_n_17, n_17, N_L_0))

colnames(before_df) = c("name", "category", "b", "sum_kappa", "mean_kappa", "v_m", "v_n", "n", "N_L_0")
colnames(after_df) = c("name","category", "b", "sum_kappa", "mean_kappa", "v_m", "v_n", "n", "N_L_0")

before_df$b = as.numeric(as.character(before_df$b))
before_df$sum_kappa = as.numeric(as.character(before_df$sum_kappa))
before_df$mean_kappa = as.numeric(as.character(before_df$mean_kappa))
before_df$v_m = as.numeric(as.character(before_df$v_m))
before_df$v_n = as.numeric(as.character(before_df$v_n))
before_df$n = as.numeric(as.character(before_df$n))
before_df$N_L_0 = as.numeric(as.character(before_df$N_L_0))

after_df$b = as.numeric(as.character(after_df$b))
after_df$sum_kappa = as.numeric(as.character(after_df$sum_kappa))
after_df$mean_kappa = as.numeric(as.character(after_df$mean_kappa))
after_df$v_m = as.numeric(as.character(after_df$v_m))
after_df$v_n = as.numeric(as.character(after_df$v_n))
after_df$n = as.numeric(as.character(after_df$n))
after_df$N_L_0 = as.numeric(as.character(after_df$N_L_0))

before_df$cat = 'Before CRH'
after_df$cat = 'After CRH'

rownames(before_df) = NULL
rownames(after_df) = NULL

library(sandwich)
library(plm)
library(lmtest)

lm_v_m_17_b = felm(v_m ~ b | category, data = after_df)
summary(lm_v_m_17_b)

lm_v_m_17_b = plm(v_m ~ b, data = after_df, index = c("category"), model = "within")
coeftest(lm_v_m_17_b, vcov. = vcovHC, type = "HC1")

lm_v_m_17_b = lm(v_m ~ b + category - 1, data = after_df)
summary(lm_v_m_17_b)

lm_v_m_17_b = lm(v_m ~ b , data = after_df)
summary(lm_v_m_17_b)



lm_v_m_07_b = felm(v_m ~ b | category, data = before_df)
summary(lm_v_m_07_b)

lm_v_m_07_b = plm(v_m ~ b, data = before_df, index = c("category"), model = "within")
coeftest(lm_v_m_07_b, vcov. = vcovHC, type = "HC1")

lm_v_m_07_b = lm(v_m ~ b + category - 1, data = before_df)
summary(lm_v_m_07_b)

lm_v_m_07_b = lm(v_m ~ b , data = before_df)
summary(lm_v_m_07_b)

lm_v_n_17_b = lm(v_n ~ b + category - 1, data = after_df)
summary(lm_v_n_17_b)

lm_v_n_07_b = lm(v_n ~ b + category - 1, data = before_df)
summary(lm_v_n_07_b)

lm_n_17_b = lm(n ~ b + category - 1, data = after_df)
summary(lm_n_17_b)

lm_n_07_b = lm(n ~ b + category - 1, data = before_df)
summary(lm_n_07_b)

lm_v_m_17_k = lm(v_m_17~sum_kappa_17)
lm_v_m_07_k = lm(v_m_07~sum_kappa_07)

lm_v_n_17_k = lm(v_n_17~sum_kappa_17)
lm_v_n_07_k = lm(v_n_07~sum_kappa_07)

lm_n_17_k = lm(n_17~sum_kappa_17)
lm_n_07_k = lm(n_07~sum_kappa_07)


cat_reg = rbind(before_df, after_df)

ggplot(cat_reg , aes(b, v_m, color = cat)) +
  geom_point() +
  geom_smooth(method = "lm")
#labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")

ggplot(cat_reg , aes(b, v_n, color = cat)) +
  geom_point() +
  geom_smooth(method = "lm")
#labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")

ggplot(cat_reg , aes(b, n, color = cat)) +
  geom_point() +
  geom_smooth(method = "lm")
#labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")


ggplot(cat_reg , aes(sum_kappa, v_m, color = cat)) +
  geom_point() +
  geom_smooth(method = "lm")
#labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")

ggplot(cat_reg , aes(sum_kappa, v_n, color = cat)) +
  geom_point() +
  geom_smooth(method = "lm")
#labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")

ggplot(cat_reg , aes(sum_kappa, n, color = cat)) +
  geom_point() +
  geom_smooth(method = "lm")
#labs(title = "Counterfactual, Population Density", x= "Log Difference of the Actual Population Density", y="Fitted Value")


###########################
######tsls b###############
###########################

before_df = cbind(before_df, zeta)
after_df = cbind(after_df, zeta)

tsls1_07_b = lm(formula = b~ log(zeta) + category - 1, data = before_df)
summary(tsls1_07_b )

tsls1_07_b = felm(b ~ log(zeta) | category, data = before_df)
summary(tsls1_07_b )

tsls1_07_b = plm(b ~ log(zeta), data = before_df, index = c("category"), model = "within")
coeftest(tsls1_07_b, vcov. = vcovHC, type = "HC1")


tsls1_17_b = lm(formula = b~ log(zeta) + category - 1, data = after_df)
summary(tsls1_17_b )

tsls1_17_b = felm(b ~ log(zeta) | category, data = after_df)
summary(tsls1_17_b )

tsls1_17_b = plm(b ~ log(zeta), data = after_df, index = c("category"), model = "within")
coeftest(tsls1_17_b, vcov. = vcovHC, type = "HC1")

library(AER)


d.hat_07_b = fitted.values(tsls1_07_b)
d.hat_17_b = fitted.values(tsls1_17_b)

before_df$d_hat = d.hat_07_b
after_df$d_hat = d.hat_17_b

tsls2_v_m_07_b = lm(v_m~d_hat + category - 1, data = before_df)

#tsls2_v_m_07_b = felm(v_m ~ d.hat_07_b | category, data = before_df)
#tsls2_v_n_07_b = lm(v_n_07~d.hat_07_b)
tsls2_n_07_b = lm(n_07~d.hat_07_b+ city_cat - 1)
#tsls2_n_07_b = felm(n ~ d.hat_07_b | category, data = before_df)

tsls2_v_m_17_b = lm(v_m_17~d.hat_17_b+ city_cat - 1)
#tsls2_v_n_17_b = lm(v_n_17~d.hat_17_b)
tsls2_n_17_b = lm(n_17~d.hat_17_b+ city_cat - 1)

tsls2_v_m_07_b = ivreg(formula = v_m ~ b + category -1 | category -1 + zeta, data = before_df)


summary(tsls2_v_m_07_b,vcov = sandwich, diagnostics = TRUE)

tsls2_v_m_17_b = ivreg(formula = v_m ~ b + category -1 | category -1 + zeta, data = after_df)


summary(tsls2_v_m_17_b,vcov = sandwich, diagnostics = TRUE)

tsls2_v_n_07_b = ivreg(formula = v_n ~ b + category -1 | category -1 + zeta, data = before_df)


summary(tsls2_v_n_07_b,vcov = sandwich, diagnostics = TRUE)

tsls2_v_n_17_b = ivreg(formula = v_n ~ b + category -1 | category -1 + zeta, data = after_df)


summary(tsls2_v_n_17_b,vcov = sandwich, diagnostics = TRUE)

the 

tsls2_n_17_b = ivreg(formula = n ~ b + category -1 | category -1 + zeta, data = after_df)


summary(tsls2_v_n_17_b,vcov = sandwich, diagnostics = TRUE)


###########################
######tsls kappa###########
###########################

tsls1_07_k = lm(mean_kappa_07~ zeta)
tsls1_17_k = lm(mean_kappa_17~ zeta)

d.hat_07_k = fitted.values(tsls1_07_k)
d.hat_17_k = fitted.values(tsls1_17_k)

tsls2_v_m_07_k = lm(v_m_07~d.hat_07_k)
tsls2_v_n_07_k = lm(v_n_07~d.hat_07_k)
tsls2_n_07_k = lm(n_07~d.hat_07_k)

tsls2_v_m_17_k = lm(v_m_17~d.hat_17_k)
tsls2_v_n_17_k = lm(v_n_17~d.hat_17_k)
tsls2_n_17_k = lm(n_17~d.hat_17_k)

