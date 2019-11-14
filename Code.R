
## Calibration

alpha_a = 0.4
alpha_n = 0.25
beta_t=0.3
theta=4
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
  z_a_diff = (z_a_new - z_a)/z_a
  z_n_diff = (z_n_new - z_n)/z_n
  
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
  return (c(u_new, v_a_new_predict, n_new_predict)) # we have the predicted value of new agri share and pop density
}

data = read.csv("Final_data.csv")


# 2007 vs 2017

data_0717 = data.frame(cbind(as.character(data$city_name), data$density_city_2007, data$trade_ratio_city_2007, data$density_city_2017, data$trade_ratio_city_2017))
colnames(data_0717) = c("name", "n_2007", "v_a_2007", "n_2017", "v_a_2017")
data_0717$n_2007 = as.numeric(as.character(data_0717$n_2007))
data_0717$n_2017 = as.numeric(as.character(data_0717$n_2017))
data_0717$v_a_2007 = as.numeric(as.character(data_0717$v_a_2007))
data_0717$v_a_2017 = as.numeric(as.character(data_0717$v_a_2017))

data_0717=data_0717[-c(121), ]

cf_0717 = counterfactual(data_0717[1, ]$v_a_2007,  data_0717[1, ]$n_2007, data_0717[1, ]$v_a_2017, data_0717[1, ]$n_2017)

for(i in 2: 285){
  row = counterfactual(data_0717[i, ]$v_a_2007,  data_0717[i, ]$n_2007, data_0717[i, ]$v_a_2017, data_0717[i, ]$n_2017)
  cf_0717=rbind(cf_0717, row)
}

cf_0717 = cbind(as.character(data_0717$name), cf_0717)
cf_0717 = cbind(cf_0717, data_0717$v_a_2017, data_0717$n_2017)
colnames(cf_0717)=c("name", "u", "v_a_hat", "n_hat", "v_a", "n")

cf_07_17_df = data.frame(cf_0717)

cf_07_17_df$Model_Perf_va = as.numeric(as.character(cf_07_17_df$v_a_hat)) - as.numeric(as.character(cf_07_17_df$v_a))

cf_07_17_df$Model_Perf_n = as.numeric(as.character(cf_07_17_df$n_hat)) - as.numeric(as.character(cf_07_17_df$n))

cf_07_17_df$Model_Perf=NULL

data=data[-c(121), ]

cf_07_17_df = cbind(cf_07_17_df, data$chr_dummy, data$year_chr, data$trade_accessible_dummy)

cf_07_17_df = cbind(cf_07_17_df, data$density_city_2007, data$trade_ratio_city_2007)

cf_07_17_df$v_a = as.numeric(as.character(cf_07_17_df$v_a))
cf_07_17_df$v_a_hat = as.numeric(as.character(cf_07_17_df$v_a_hat))
cf_07_17_df$n = as.numeric(as.character(cf_07_17_df$n))
cf_07_17_df$n_hat = as.numeric(as.character(cf_07_17_df$n_hat))

rownames(cf_07_17_df)=cf_07_17_df$name

#install.packages("car")
library(ggplot2)
library(ggrepel)
library(ggpubr)

p1 = ggplot(cf_07_17_df, aes(v_a, v_a_hat)) +
         geom_point(color = ifelse(abs(cf_07_17_df$Model_Perf_va)>0.08, "tomato2", "grey30"))+
         geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
         geom_text(data=data.frame(x=0.18, y=0.15), aes(x, y), label = "y=x", size=5)+
         geom_text_repel(data = subset(cf_07_17_df, abs(Model_Perf_va)>0.08), aes(label = name))+ labs(title = "Plot of Trade Sector Employment Shares, 2017", x="Actual Trade Sector Employment Shares", y="Predicted Trade Sector Employment Shares")

p2 = ggplot(cf_07_17_df, aes(n, n_hat)) +
  geom_point(color = ifelse(abs(cf_07_17_df$Model_Perf_n)>650, "tomato2", "grey30"))+
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=0, y=-150), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_07_17_df, abs(Model_Perf_n)>650), aes(label = name)) + labs(title = "Plot of Population Density, 2017", x="Actual Population Density(people/km^2)", y="Predicted Population Density(people/km^2)")

ggarrange(p1, p2 ,ncol = 2, nrow = 1)

write.csv(cf_07_17_df, file="counterfactual_07_17.csv")

cf_07_17_rmse_n = mean(abs(cf_07_17_df$Model_Perf_n))
cf_07_17_rmse_va = mean(abs(cf_07_17_df$Model_Perf_va))

# 2007 vs 2010

data_0710 = data.frame(cbind(as.character(data$city_name), data$density_city_2007, data$trade_ratio_city_2007, data$density_city_2010, data$trade_ratio_city_2010))
colnames(data_0710) = c("name", "n_2007", "v_a_2007", "n_2010", "v_a_2010")
data_0710$n_2007 = as.numeric(as.character(data_0710$n_2007))
data_0710$n_2010 = as.numeric(as.character(data_0710$n_2010))
data_0710$v_a_2007 = as.numeric(as.character(data_0710$v_a_2007))
data_0710$v_a_2010 = as.numeric(as.character(data_0710$v_a_2010))


#data_0710=data_0710[-c(121), ]# Lhasa got missing data

cf_0710 = counterfactual(data_0710[1, ]$v_a_2007,  data_0710[1, ]$n_2007, data_0710[1, ]$v_a_2010, data_0710[1, ]$n_2010)

for(i in 2: 285){
  row = counterfactual(data_0710[i, ]$v_a_2007,  data_0710[i, ]$n_2007, data_0710[i, ]$v_a_2010, data_0710[i, ]$n_2010)
  cf_0710=rbind(cf_0710, row)
}

cf_0710 = cbind(as.character(data_0710$name), cf_0710)
cf_0710 = cbind(cf_0710, data_0710$v_a_2010, data_0710$n_2010)
colnames(cf_0710)=c("name", "v_a_hat", "n_hat", "v_a", "n")

cf_07_10_df = data.frame(cf_0710)

cf_07_10_df$Model_Perf_va = as.numeric(as.character(cf_07_10_df$v_a_hat)) - as.numeric(as.character(cf_07_10_df$v_a))

cf_07_10_df$Model_Perf_n = as.numeric(as.character(cf_07_10_df$n_hat)) - as.numeric(as.character(cf_07_10_df$n))

cf_07_10_df = cbind(cf_07_10_df, data$chr_dummy, data$year_chr, data$trade_accessible_dummy)

cf_07_10_df = cbind(cf_07_10_df, data$density_city_2007, data$trade_ratio_city_2007)

cf_07_10_df$v_a = as.numeric(as.character(cf_07_10_df$v_a))
cf_07_10_df$v_a_hat = as.numeric(as.character(cf_07_10_df$v_a_hat))
cf_07_10_df$n = as.numeric(as.character(cf_07_10_df$n))
cf_07_10_df$n_hat = as.numeric(as.character(cf_07_10_df$n_hat))


p1 = ggplot(cf_07_10_df, aes(v_a, v_a_hat)) +
  geom_point(color = ifelse(abs(cf_07_10_df$Model_Perf_va)>0.02, "tomato2", "grey30"))+
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=0.18, y=0.15), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_07_10_df, abs(Model_Perf_va)>0.02), aes(label = name))+ labs(title = "Plot of Trade Sector Employment Shares, 2010", x="Actual Trade Sector Employment Shares", y="Predicted Trade Sector Employment Shares")

p2 = ggplot(cf_07_10_df, aes(n, n_hat)) +
  geom_point(color = ifelse(abs(cf_07_10_df$Model_Perf_n)>150, "tomato2", "grey30"))+
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=0, y=-150), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_07_10_df, abs(Model_Perf_n)>150), aes(label = name)) + labs(title = "Plot of Population Density, 2010", x="Actual Population Density(people/km^2)", y="Predicted Population Density(people/km^2)")

ggarrange(p1, p2 ,ncol = 2, nrow = 1)

write.csv(cf_07_10_df, file="counterfactual_07_10.csv")

cf_07_10_rmse_n = mean(abs(cf_07_10_df$Model_Perf_n))
cf_07_10_rmse_va = mean(abs(cf_07_10_df$Model_Perf_va))


# 2007 vs 2014

data_0714 = data.frame(cbind(as.character(data$city_name), data$density_city_2007, data$trade_ratio_city_2007, data$density_city_2014, data$trade_ratio_city_2014))
colnames(data_0714) = c("name", "n_2007", "v_a_2007", "n_2014", "v_a_2014")
data_0714$n_2007 = as.numeric(as.character(data_0714$n_2007))
data_0714$n_2014 = as.numeric(as.character(data_0714$n_2014))
data_0714$v_a_2007 = as.numeric(as.character(data_0714$v_a_2007))
data_0714$v_a_2014 = as.numeric(as.character(data_0714$v_a_2014))


#data_0714=data_0714[-c(121), ]# Lhasa got missing data

cf_0714 = counterfactual(data_0714[1, ]$v_a_2007,  data_0714[1, ]$n_2007, data_0714[1, ]$v_a_2014, data_0714[1, ]$n_2014)

for(i in 2: 285){
  row = counterfactual(data_0714[i, ]$v_a_2007,  data_0714[i, ]$n_2007, data_0714[i, ]$v_a_2014, data_0714[i, ]$n_2014)
  cf_0714=rbind(cf_0714, row)
}

cf_0714 = cbind(as.character(data_0714$name), cf_0714)
cf_0714 = cbind(cf_0714, data_0714$v_a_2014, data_0714$n_2014)
colnames(cf_0714)=c("name", "v_a_hat", "n_hat", "v_a", "n")

cf_07_14_df = data.frame(cf_0714)

cf_07_14_df$Model_Perf_va = as.numeric(as.character(cf_07_14_df$v_a_hat)) - as.numeric(as.character(cf_07_14_df$v_a))

cf_07_14_df$Model_Perf_n = as.numeric(as.character(cf_07_14_df$n_hat)) - as.numeric(as.character(cf_07_14_df$n))

cf_07_14_df = cbind(cf_07_14_df, data$chr_dummy, data$year_chr, data$trade_accessible_dummy)

cf_07_14_df = cbind(cf_07_14_df, data$density_city_2007, data$trade_ratio_city_2007)

cf_07_14_df$v_a = as.numeric(as.character(cf_07_14_df$v_a))
cf_07_14_df$v_a_hat = as.numeric(as.character(cf_07_14_df$v_a_hat))
cf_07_14_df$n = as.numeric(as.character(cf_07_14_df$n))
cf_07_14_df$n_hat = as.numeric(as.character(cf_07_14_df$n_hat))


p1 = ggplot(cf_07_14_df, aes(v_a, v_a_hat)) +
  geom_point(color = ifelse(abs(cf_07_14_df$Model_Perf_va)>0.06, "tomato2", "grey30"))+
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=0.18, y=0.15), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_07_14_df, abs(Model_Perf_va)>0.06), aes(label = name))+ labs(title = "Plot of Trade Sector Employment Shares, 2014", x="Actual Trade Sector Employment Shares", y="Predicted Trade Sector Employment Shares")

p2 = ggplot(cf_07_14_df, aes(n, n_hat)) +
  geom_point(color = ifelse(abs(cf_07_14_df$Model_Perf_n)>800, "tomato2", "grey30"))+
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=0, y=-150), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_07_14_df, abs(Model_Perf_n)>800), aes(label = name)) + labs(title = "Plot of Population Density, 2014", x="Actual Population Density(people/km^2)", y="Predicted Population Density(people/km^2)")

ggarrange(p1, p2 ,ncol = 2, nrow = 1)


write.csv(cf_07_14_df, file="counterfactual_07_14.csv")

cf_07_14_rmse_n = mean(abs(cf_07_14_df$Model_Perf_n))
cf_07_14_rmse_va = mean(abs(cf_07_14_df$Model_Perf_va))

#n_new = data_0717[i, ]$n_2017
#v_a = data_0717[i, ]$v_a_2007
#v_a_new = data_0717[i, ]$v_a_2017
#n = data_0717[i, ]$n_2007


u = as.numeric(as.character(cf_07_17_df$u))

N_L = as.numeric(as.character(data$N_2017))
N_L_pre = as.numeric(as.character(data$N_2007))

N_L = N_L[-121]
N_L_pre = N_L_pre[-121]

N = sum(N_L)
N_pre = sum(N_L_pre)



wma <- function(o, b){#o is the index of origin in kappa matrix
  result = 0
  for(k in 1: length(b)){
    if(o==k){
      result =result
    }else{
      result = result + (kappa[o, k]) ^(-theta) * b[k] * u[k]^theta
    }
  }
  return(result)
}


matrix_after = read.csv("Travel_Matrix_CRH.csv")
city_row = matrix_after$X 

matrix_after = matrix_after[matrix_after$X != "Lhasa", ]

matrix_after$X = NULL
matrix_after$Lhasa = NULL

kappa_after = data.matrix(matrix_after, rownames.force = NA)

kappa_after = kappa_after[-286:-287, ]

kappa = kappa_after

kappa[is.na(kappa)] <- 10000

colnames(kappa) = NULL

for (i in 1:length(b)) {
  for (j in 1:length(b)) {
    if(kappa[i, j]==0){
      if(i != j){
        kappa[i, j] = 10000
      }
    }
  }
  
}

b_in = u# initial guess on b

for (i in 1:length(b_in)) {
  s = 0
  for (j in 1:length(b_in)) {
    if(j==i){
      s=s
      #s[j] = 0
    }else{
      s = s+ (kappa[j, i]^(-theta) * u[i]^theta) / wma(j, b_in)* (N_L_pre[j]/N_pre)
      #s[j] = (kappa[j, i]^(-theta) * u[i]^theta) / wma(j, b_in)* (N_L_pre[j]/N_pre)
    }
  }
  b[i] = N_L[i]/N * (s)^(-1)
}

b_out = b

error =c()

count = 1

error[count] = mean((b_out - b_in)^2)

while (error[count]>1000) {
  b_in = b_out 
  for (i in 1:length(b_in)) {
    s = 0
    for (j in 1:length(b_in)) {
      if(j==i){
        s=s
        #s[j] = 0
      }else{
        s = s+ (kappa[j, i]^(-theta) * u[i]^theta) / wma(j, b_in)* (N_L_pre[j]/N_pre)
        #s[j] = (kappa[j, i]^(-theta) * u[i]^theta) / wma(j, b_in)* (N_L_pre[j]/N_pre)
      }
    }
    b[i] = N_L[i]/N * (s)^(-1)
  }
  b_out = b
  count = count+1
  error[count] = mean((b_out - b_in)^2)
}


### Location vs Location, same year

data2 = read.csv("Final_data.csv")

data2 = data2[with(data2, order(density_city_2017)), ]#order by 2017 pop density

data2$index = seq.int(nrow(data2))

data2_odd = data2[data2$index %% 2 ==1,  ]
data2_odd$density_city_2007 = NULL
data2_odd$density_city_2010 = NULL
data2_odd$density_city_2014 = NULL
data2_odd$trade_ratio_city_2007 = NULL
data2_odd$trade_ratio_city_2010 = NULL
data2_odd$trade_ratio_city_2014 = NULL
colnames(data2_odd) = c("city_name_odd", "v_a_2017_odd", "n_2017_odd", "chr_dummy_odd", "year_chr_odd", "trade_accessible_dummy_odd", "index_odd")



data2_even = data2[data2$index %% 2 ==0,  ]
data2_even$density_city_2007 = NULL
data2_even$density_city_2010 = NULL
data2_even$density_city_2014 = NULL
data2_even$trade_ratio_city_2007 = NULL
data2_even$trade_ratio_city_2010 = NULL
data2_even$trade_ratio_city_2014 = NULL
colnames(data2_even) = c("city_name_even", "v_a_2017_even", "n_2017_even", "chr_dummy_even", "year_chr_even", "trade_accessible_dummy_even", "index_even")


cf_loc = cbind(data2_odd, data2_even)

cf_loc$n_2017_odd = as.numeric(as.character(cf_loc$n_2017_odd))
cf_loc$n_2017_even = as.numeric(as.character(cf_loc$n_2017_even))
cf_loc$v_a_2017_odd = as.numeric(as.character(cf_loc$v_a_2017_odd))
cf_loc$v_a_2017_even = as.numeric(as.character(cf_loc$v_a_2017_even))

cf_location = counterfactual(cf_loc[1, ]$v_a_2017_odd,  cf_loc[1, ]$n_2017_odd, cf_loc[1, ]$v_a_2017_even, cf_loc[1, ]$n_2017_even)

for(i in 2: length(cf_loc$index_even)){
  row = counterfactual(cf_loc[i, ]$v_a_2017_odd,  cf_loc[i, ]$n_2017_odd, cf_loc[i, ]$v_a_2017_even, cf_loc[i, ]$n_2017_even)
  cf_location=rbind(cf_location, row)
}

cf_location = cbind(as.character(cf_loc$city_name_odd), as.character(cf_loc$city_name_even), cf_location)
cf_location = cbind(cf_location,  cf_loc$v_a_2017_odd, cf_loc$n_2017_odd,cf_loc$v_a_2017_even, cf_loc$n_2017_even)
colnames(cf_location)=c("name_odd","name_even", "v_a_hat", "n_hat", "v_a_odd", "n_odd","v_a_even", "n_even")


cf_location_df = data.frame(cf_location)

cf_location_df$Model_Perf_va = as.numeric(as.character(cf_location_df$v_a_hat)) - as.numeric(as.character(cf_location_df$v_a_even))

cf_location_df$Model_Perf_n = as.numeric(as.character(cf_location_df$n_hat)) - as.numeric(as.character(cf_location_df$n_even))

cf_location_df = cbind(cf_location_df, cf_loc$chr_dummy_odd, cf_loc$chr_dummy_even, cf_loc$year_chr_odd,cf_loc$year_chr_even, cf_loc$trade_accessible_dummy_odd,cf_loc$trade_accessible_dummy_even)

write.csv(cf_location_df, file="counterfactual_location.csv")

cf_location_rmse_n = mean(abs(cf_location_df$Model_Perf_n))
cf_location_rmse_va = mean(abs(cf_location_df$Model_Perf_va))

cf_location_df$v_a_even = as.numeric(as.character(cf_location_df$v_a_even))
cf_location_df$v_a_odd = as.numeric(as.character(cf_location_df$v_a_odd))
cf_location_df$v_a_hat = as.numeric(as.character(cf_location_df$v_a_hat))
cf_location_df$n_even = as.numeric(as.character(cf_location_df$n_even))
cf_location_df$n_odd = as.numeric(as.character(cf_location_df$n_odd))
cf_location_df$n_hat = as.numeric(as.character(cf_location_df$n_hat))

p1 = ggplot(cf_location_df, aes(v_a_even, v_a_hat)) +
  geom_point(color = ifelse(abs(cf_location_df$Model_Perf_va)>0.1, "tomato2", "grey30"))+
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=0.18, y=0.15), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_location_df, abs(Model_Perf_va)>0.1), aes(label = name_even))+ labs(title = "Plot of Trade Sector Employment Shares, Location", x="Actual Trade Sector Employment Shares", y="Predicted Trade Sector Employment Shares")

p2 = ggplot(cf_location_df, aes(n_even, n_hat)) +
  geom_point(color = ifelse(abs(cf_location_df$Model_Perf_n)>700, "tomato2", "grey30"))+
  geom_abline(intercept = 0, slope = 1, color = "royalblue2", size=0.75)+
  geom_text(data=data.frame(x=0, y=-150), aes(x, y), label = "y=x", size=5)+
  geom_text_repel(data = subset(cf_location_df, abs(Model_Perf_n)>700), aes(label = name_even)) + labs(title = "Plot of Population Density, Location", x="Actual Population Density(people/km^2)", y="Predicted Population Density(people/km^2)")

ggarrange(p1, p2 ,ncol = 2, nrow = 1)

## Further Analysis
demand <- function(v_a){
  rel_p_t=((1/beta_t)* (((1-alpha_n)*v_a)/ ((1-alpha_a)+(alpha_a-alpha_n)*v_a) ))^(1/(1-sigma))
  rel_p_n = ((1-beta_t*rel_p_t^(1-sigma))/(1-beta_t))^(1/(1-sigma))
  d_t = beta_t*rel_p_t^((-1)*sigma)
  d_n = (1-beta_t)*(rel_p_n)^((-1)*sigma)
  rel_d_t = d_t/(d_t+d_n) #from (4) (5) of Fajgelbaum and Redding
  return(rel_d_t)
  #return(d_t)
}

result =process(v_a, n)
z_a = result[1]
z_n = result[2]
u = result[3]

problem_cases = read.csv(file = "../code/problem_cases.csv")
problem_cases$v_a_2007 = as.numeric(as.character(problem_cases$v_a_2007))
problem_cases$v_a_2017 = as.numeric(as.character(problem_cases$v_a_2017))

cf_07_17_df$rel_d_t_2017 = demand(cf_07_17_df$v_a)
cf_07_17_df$rel_d_t_2007 = demand(cf_07_17_df$`data$trade_ratio_city_2007`)
cf_07_17_df$rel_d_t_diff = cf_07_17_df$rel_d_t_2017 - cf_07_17_df$rel_d_t_2007














problem_cases$rel_d_t_2017 = demand(problem_cases$v_a_2017)
problem_cases$rel_d_t_2007 = demand(problem_cases$v_a_2007)
problem_cases$rel_p_t_2007 =((1/beta_t)* (((1-alpha_n)*problem_cases$v_a_2007)/ ((1-alpha_a)+(alpha_a-alpha_n)*problem_cases$v_a_2007) ))^(1/(1-sigma))
problem_cases$rel_p_t_2017 =((1/beta_t)* (((1-alpha_n)*problem_cases$v_a_2017)/ ((1-alpha_a)+(alpha_a-alpha_n)*problem_cases$v_a_2017) ))^(1/(1-sigma))

#wage_rent_ratio=(1/n) * ( ((1-alpha_a)*(1-alpha_n)) /( alpha_n*(1-alpha_a) + (alpha_a-alpha_n)*v_a)) # n : population densities

#setwd("C:/Users/gorde/Dropbox/HSR/Data")

data_0717_problem = read.csv(file = "counterfactual_07_17.csv")


delta_z_a <-function (v_a, n, v_a_new, n_new){
  result =process(v_a, n)
  z_a = result[1]
  
  result_new =process(v_a_new, n_new)
  z_a_new = result_new[1]
  
  z_a_diff = (z_a_new - z_a)/z_a
  return (c(log(z_a_new/z_a), z_a_diff))
}

cf_0717_problem = delta_z_a(data_0717_problem[1, ]$v_a,  data_0717_problem[1, ]$n, data_0717_problem[1, ]$v_a_new, data_0717_problem[1, ]$n_new)

for(i in 2: 285){
  row = delta_z_a(data_0717_problem[i, ]$v_a,  data_0717_problem[i, ]$n, data_0717_problem[i, ]$v_a_new, data_0717_problem[i, ]$n_new)
  cf_0717_problem=rbind(cf_0717_problem, row)
}

cf_0717_problem = cbind(as.character(data_0717_problem$name), cf_0717_problem)
cf_0717_problem = cbind(cf_0717_problem, data_0717_problem$delta_landuse, data_0717_problem$ldelta_landuse)
colnames(cf_0717_problem)=c("name","ldelta_z_a" , "delta_z_a", "delta_landuse", "ldelta_landuse")
rownames(cf_0717_problem) = data_0717_problem$name
cf_0717_problem = data.frame(cf_0717_problem)
cf_0717_problem = cbind(cf_0717_problem, data_0717_problem$Problem_Case)
colnames(cf_0717_problem)=c("name","ldelta_z_a" , "delta_z_a", "delta_landuse", "ldelta_landuse", "problem_case")

make_numeric <-function(x){
  return(as.numeric(as.character(x)))
}

cf_0717_problem[2:5] = apply(cf_0717_problem[2:5], 2, make_numeric)


m <-lm(delta_z_a ~ delta_landuse + problem_case, data=cf_0717_problem)
#m2 <-lm(ldelta_z_a ~ ldelta_landuse, data=cf_0717_problem)
m2 <-lm(ldelta_z_a ~ ldelta_landuse + problem_case, data=cf_0717_problem)
library(lfe)
m2_prime = felm(ldelta_z_a ~ ldelta_landuse | problem_case | 0 | problem_case, cf_0717_problem) #problem_case factors to be projected out

library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lmtest)
library(sandwich)
coeftest(m, vcov = vcovHC(m, type = "HC1"))
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
