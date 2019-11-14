library(readxl)

data_2006=read_excel("Data_2006.xlsx")
colnames(data_2006)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", "Agri_Land_City", "Agri_Land_per_Person_City"
                      , "Density_City", "Density_Dist", "City_Planning_Area_Dist", "City_Planning_Ratio_Dist")
data_2006=data_2006[-1, ]
data_2006$city_name_cn=NULL

data_2007=read_excel("Data_2007.xlsx")
colnames(data_2007)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", "Agri_Land_City", "Agri_Land_per_Person_City"
                      , "Density_City", "Density_Dist", "City_Planning_Area_Dist", "City_Planning_Ratio_Dist")
data_2007=data_2007[-1, ]
data_2007$city_name_cn=NULL

data_2008=read_excel("Data_2008.xlsx")
colnames(data_2008)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", "Agri_Land_City", "Agri_Land_per_Person_City"
                      , "Density_City", "Density_Dist", "City_Planning_Area_Dist", "City_Planning_Ratio_Dist")
data_2008=data_2008[-1, ]
data_2008$city_name_cn=NULL


data_2009=read_excel("Data_2009.xlsx")
colnames(data_2009)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", "Agri_Land_City", "Agri_Land_per_Person_City"
                      , "Density_City", "Density_Dist", "City_Planning_Area_Dist", "City_Planning_Ratio_Dist")
data_2009=data_2009[-1, ]
data_2009$city_name_cn=NULL

data_2010=read_excel("Data_2010.xlsx")
colnames(data_2010)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", "Agri_Land_City", "Agri_Land_per_Person_City"
                      , "Density_City", "Density_Dist", "City_Planning_Area_Dist", "City_Planning_Ratio_Dist")
data_2010=data_2010[-1, ]
data_2010$city_name_cn=NULL

data_2011=read_excel("Data_2011.xlsx")

colnames(data_2011)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", "Agri_Land_City", "Agri_Land_per_Person_City"
                      , "Density_City", "Density_Dist", "City_Planning_Area_Dist", "City_Planning_Ratio_Dist")

data_2011=data_2011[-1, ]
data_2011$city_name_cn=NULL

data_2012=read_excel("Data_2012.xlsx")
colnames(data_2012)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", 
                      "Density_City", "Density_Dist", "City_Planning_Area_Dist", "Living_Land_Used_Dist", "City_Planning_Ratio_Dist")
data_2012=data_2012[-1, ]
data_2012$city_name_cn=NULL


data_2013=read_excel("Data_2013.xlsx")
colnames(data_2013)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", 
                      "Density_City", "Density_Dist", "City_Planning_Area_Dist", "Living_Land_Used_Dist", "City_Planning_Ratio_Dist")
data_2013=data_2013[-1, ]
data_2013$city_name_cn=NULL



data_2014=read_excel("Data_2014.xlsx")
colnames(data_2014)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", 
                      "Density_City", "Density_Dist", "City_Planning_Area_Dist", "Living_Land_Used_Dist", "City_Planning_Ratio_Dist")
data_2014=data_2014[-1, ]
data_2014$city_name_cn=NULL

data_2015=read_excel("Data_2015.xlsx")
colnames(data_2015)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", 
                      "Density_City", "Density_Dist", "City_Planning_Area_Dist", "Living_Land_Used_Dist", "City_Planning_Ratio_Dist")
data_2015=data_2015[-1, ]
data_2015$city_name_cn=NULL

data_2016=read_excel("Data_2016.xlsx")
colnames(data_2016)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", 
                      "Density_City", "Density_Dist", "City_Planning_Area_Dist", "Living_Land_Used_Dist", "City_Planning_Ratio_Dist")
data_2016=data_2016[-1, ]
data_2016$city_name_cn=NULL

data_2017=read_excel("Data_2017.xlsx")
colnames(data_2017)=c("city_name_cn","city_name_en", "pop_city", "pop_dist", "non_agri_pop_city","non_agri_pop_dist", "company_labor_city", "company_labor_dist"
                      , "indivi_labor_city","indivi_labor_dist", "unempoly_city","unempoly_dist", "Agri_City","Agri_Dist", "Mining_City", "Mining_Dist"
                      , "Manuf_City", "Manuf_Dist", "Energy_City", "Energy_Dist", "Constr_City", "Constr_Dist", "Transport_City", "Transport_Dist"
                      , "IT_City", "IT_Dist", "Retail_City", "Retail_Dist", "Hotel_Dining_City","Hotel_Dining_Dist", "Finance_City", "Finance_Dist"
                      , "Real_Estate_City", "Real_Estate_Dist", "Biz_Service_City", "Biz_Service_Dist", "S&T_City", "S&T_Dist", "Environment_City", "Environment_Dist"
                      , "Public_Service_City", "Public_Service_Dist", "Edu_City", "Edu_Dist", "Health_City", "Health_Dist", "Art_Sport_City", "Art_Sport_Dist"
                      , "Public_Manage_City", "Public_Manage_Dist", "Area_City","Area_Dist", "Constructed_Area_Dist", 
                      "Water_Resources", "City_Planning_Area_Dist", "Living_Land_Used_Dist", "City_Planning_Ratio_Dist")
data_2017=data_2017[-1, ]
data_2017$city_name_cn=NULL

data_2009 = data_2009[1:288, ]
data_2010 = data_2010[1:288, ]
data_2011 = data_2011[1:288, ]
data_2012 = data_2012[1:287, ]
data_2015 = data_2015[1:287, ]
data_2007 = data_2007[-106, ]
data_2008 = data_2008[-106, ]
data_2009 = data_2009[-106, ]
data_2010 = data_2010[-106, ]
data_2011 = data_2011[-106, ]

write.csv(data_2006, file = "csv/data_2006.csv")
write.csv(data_2007, file = "csv/data_2007.csv")
write.csv(data_2008, file = "csv/data_2008.csv")
write.csv(data_2009, file = "csv/data_2009.csv")
write.csv(data_2010, file = "csv/data_2010.csv")
write.csv(data_2011, file = "csv/data_2011.csv")
write.csv(data_2012, file = "csv/data_2012.csv")
write.csv(data_2013, file = "csv/data_2013.csv")
write.csv(data_2014, file = "csv/data_2014.csv")
write.csv(data_2015, file = "csv/data_2015.csv")
write.csv(data_2016, file = "csv/data_2016.csv")
write.csv(data_2017, file = "csv/data_2017.csv")

#No Chongqing in data_2016 and no total in data_2017

## Data Organize

#2007
data_2007[data_2007 == "Jiangshan"] = "Jiangmen"

data_2007[] <- lapply(data_2007, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

data_2007[is.na(data_2007)] = 0

all_labor_city_2007 = as.numeric(data_2007$company_labor_city)
all_labor_dist_2007 = as.numeric(data_2007$company_labor_dist)
trade_labor_city_2007 = as.numeric(data_2007$Agri_City)+as.numeric(data_2007$Mining_City)+as.numeric(data_2007$Manuf_City)+as.numeric(data_2007$Energy_City)+as.numeric(data_2007$Constr_City)
trade_labor_dist_2007 = as.numeric(data_2007$Agri_Dist)+as.numeric(data_2007$Mining_Dist)+as.numeric(data_2007$Manuf_Dist)+as.numeric(data_2007$Energy_Dist)+as.numeric(data_2007$Constr_Dist)

trade_ratio_city_2007 = trade_labor_city_2007/all_labor_city_2007
trade_ratio_dist_2007 = trade_labor_dist_2007/all_labor_dist_2007

density_city_2007 = data_2007$Density_City
density_dist_2007 = data_2007$Density_Dist

trimmed_data_2007 = data.frame(cbind(trade_ratio_city_2007, trade_ratio_dist_2007, density_city_2007, density_dist_2007))
trimmed_data_2007 = trimmed_data_2007[-1, ]

#2010
data_2010[data_2010 == "Jiangshan"] = "Jiangmen"

data_2010[] <- lapply(data_2010, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

data_2010[is.na(data_2010)] = 0

all_labor_city_2010 = as.numeric(data_2010$company_labor_city)
all_labor_dist_2010 = as.numeric(data_2010$company_labor_dist)
trade_labor_city_2010 = as.numeric(data_2010$Agri_City)+as.numeric(data_2010$Mining_City)+as.numeric(data_2010$Manuf_City)+as.numeric(data_2010$Energy_City)+as.numeric(data_2010$Constr_City)
trade_labor_dist_2010 = as.numeric(data_2010$Agri_Dist)+as.numeric(data_2010$Mining_Dist)+as.numeric(data_2010$Manuf_Dist)+as.numeric(data_2010$Energy_Dist)+as.numeric(data_2010$Constr_Dist)

trade_ratio_city_2010 = trade_labor_city_2010/all_labor_city_2010
trade_ratio_dist_2010 = trade_labor_dist_2010/all_labor_dist_2010

density_city_2010 = data_2010$Density_City
density_dist_2010 = data_2010$Density_Dist

trimmed_data_2010 = data.frame(cbind(trade_ratio_city_2010, trade_ratio_dist_2010, density_city_2010, density_dist_2010))
trimmed_data_2010 = trimmed_data_2010[-1, ]

#2014
data_2014[data_2014 == "Jiangshan"] = "Jiangmen"

data_2014[] <- lapply(data_2014, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

data_2014[is.na(data_2014)] = 0

all_labor_city_2014 = as.numeric(data_2014$company_labor_city)
all_labor_dist_2014 = as.numeric(data_2014$company_labor_dist)
trade_labor_city_2014 = as.numeric(data_2014$Agri_City)+as.numeric(data_2014$Mining_City)+as.numeric(data_2014$Manuf_City)+as.numeric(data_2014$Energy_City)+as.numeric(data_2014$Constr_City)
trade_labor_dist_2014 = as.numeric(data_2014$Agri_Dist)+as.numeric(data_2014$Mining_Dist)+as.numeric(data_2014$Manuf_Dist)+as.numeric(data_2014$Energy_Dist)+as.numeric(data_2014$Constr_Dist)

trade_ratio_city_2014 = trade_labor_city_2014/all_labor_city_2014
trade_ratio_dist_2014 = trade_labor_dist_2014/all_labor_dist_2014

density_city_2014 = data_2014$Density_City
density_dist_2014 = data_2014$Density_Dist

trimmed_data_2014 = data.frame(cbind(trade_ratio_city_2014, trade_ratio_dist_2014, density_city_2014, density_dist_2014))
trimmed_data_2014 = trimmed_data_2014[-1, ]

#2017
data_2017[data_2017 == "Jiangshan"] = "Jiangmen"

data_2017[] <- lapply(data_2017, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

data_2017[is.na(data_2017)] = 0

all_labor_city_2017 = as.numeric(data_2017$company_labor_city)
all_labor_dist_2017 = as.numeric(data_2017$company_labor_dist)
trade_labor_city_2017 = as.numeric(data_2017$Agri_City)+as.numeric(data_2017$Mining_City)+as.numeric(data_2017$Manuf_City)+as.numeric(data_2017$Energy_City)+as.numeric(data_2017$Constr_City)
trade_labor_dist_2017 = as.numeric(data_2017$Agri_Dist)+as.numeric(data_2017$Mining_Dist)+as.numeric(data_2017$Manuf_Dist)+as.numeric(data_2017$Energy_Dist)+as.numeric(data_2017$Constr_Dist)

trade_ratio_city_2017 = trade_labor_city_2017/all_labor_city_2017
trade_ratio_dist_2017 = trade_labor_dist_2017/all_labor_dist_2017

density_city_2017 = as.numeric(data_2017$pop_city)*10000/ as.numeric(data_2017$Area_City)
density_dist_2017 = as.numeric(data_2017$pop_dist)*10000/ as.numeric(data_2017$Area_Dist)

trimmed_data_2017 = data.frame(cbind(trade_ratio_city_2017, trade_ratio_dist_2017, density_city_2017, density_dist_2017))

trimmed_data = data.frame(cbind(data_2017$city_name_en, trimmed_data_2007, trimmed_data_2010, trimmed_data_2014, trimmed_data_2017))

write.csv(trimmed_data, file = "trimmed_data.csv")




#Wu-Guang

wu_guang_city=c("Wuhan", "Xianning", "Yueyang", "Changsha", "Zhuzhou", "Hengyang", "Chenzhou", "Shaoguan", "Qingyuan", "Guangzhou")

wu_guang_2006=data_2006[data_2006$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2006=rbind(wu_guang_2006, data_2006[data_2006$city_name_en==city, ])
}

wu_guang_2006$year=2006

wu_guang_2006$Agri_Land_City=NULL
wu_guang_2006$Agri_Land_per_Person_City=NULL

wu_guang_2007=data_2007[data_2007$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2007=rbind(wu_guang_2007, data_2007[data_2007$city_name_en==city, ])
}

wu_guang_2007$year=2007

wu_guang_2007$Agri_Land_City=NULL
wu_guang_2007$Agri_Land_per_Person_City=NULL

wu_guang_2008=data_2008[data_2008$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2008=rbind(wu_guang_2008, data_2008[data_2008$city_name_en==city, ])
}

wu_guang_2008$year=2008

wu_guang_2008$Agri_Land_City=NULL
wu_guang_2008$Agri_Land_per_Person_City=NULL

wu_guang_2009=data_2009[data_2009$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2009=rbind(wu_guang_2009, data_2009[data_2009$city_name_en==city, ])
}

wu_guang_2009$year=2009

wu_guang_2009$Agri_Land_City=NULL
wu_guang_2009$Agri_Land_per_Person_City=NULL

wu_guang_2010=data_2010[data_2010$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2010=rbind(wu_guang_2010, data_2010[data_2010$city_name_en==city, ])
}

wu_guang_2010$year=2010

wu_guang_2010$Agri_Land_City=NULL
wu_guang_2010$Agri_Land_per_Person_City=NULL

wu_guang_2011=data_2011[data_2011$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2011=rbind(wu_guang_2011, data_2011[data_2011$city_name_en==city, ])
}

wu_guang_2011$year=2011

wu_guang_2011$Agri_Land_City=NULL
wu_guang_2011$Agri_Land_per_Person_City=NULL

wu_guang_2012=data_2012[data_2012$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2012=rbind(wu_guang_2012, data_2012[data_2012$city_name_en==city, ])
}

wu_guang_2012$year=2012

wu_guang_2012$Living_Land_Used_Dist=NULL

wu_guang_2013=data_2013[data_2013$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2013=rbind(wu_guang_2013, data_2013[data_2013$city_name_en==city, ])
}

wu_guang_2013$year=2013

wu_guang_2013$Living_Land_Used_Dist=NULL

wu_guang_2014=data_2014[data_2014$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2014=rbind(wu_guang_2014, data_2014[data_2014$city_name_en==city, ])
}

wu_guang_2014$year=2014

wu_guang_2014$Living_Land_Used_Dist=NULL

wu_guang_2015=data_2015[data_2015$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2015=rbind(wu_guang_2015, data_2015[data_2015$city_name_en==city, ])
}

wu_guang_2015$year=2015

wu_guang_2015$Living_Land_Used_Dist=NULL

wu_guang_2016=data_2016[data_2016$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2016=rbind(wu_guang_2016, data_2016[data_2016$city_name_en==city, ])
}

wu_guang_2016$year=2016

wu_guang_2016$Living_Land_Used_Dist=NULL

wu_guang_2017=data_2017[data_2017$city_name_en=="Wuhan", ]

for (i in 2:length(wu_guang_city)) {
  city=wu_guang_city[i]
  wu_guang_2017=rbind(wu_guang_2017, data_2017[data_2017$city_name_en==city, ])
}

wu_guang_2017$year=2017

wu_guang_2017$Water_Resources=NULL

wu_guang_2017$Density_City = as.numeric(wu_guang_2017$pop_city)*10000/as.numeric(wu_guang_2017$Area_City)
wu_guang_2017$Density_Dist = as.numeric(wu_guang_2017$pop_dist)*10000/as.numeric(wu_guang_2017$Area_Dist)

wu_guang_2017$Living_Land_Used_Dist=NULL

wu_guang = rbind(wu_guang_2006, wu_guang_2007, wu_guang_2008, wu_guang_2009, wu_guang_2010, wu_guang_2011
                 , wu_guang_2012, wu_guang_2013, wu_guang_2014, wu_guang_2015, wu_guang_2016, wu_guang_2017)

wu_guang_wuhan=wu_guang[wu_guang$city_name_en=="Wuhan", ]

wu_guang_wuhan$Area_Dist=c(2718, 2718, 2718, 2718, 2718, 2718, 2718, 2718, 2718, 1738, 1738, 1738)

wu_guang_wuhan$Density_Dist=as.numeric(wu_guang_wuhan$pop_dist)*10000/wu_guang_wuhan$Area_Dist

wu_guang_wuhan$year=as.character(wu_guang_wuhan$year)

wu_guang_wuhan$Density_City=as.numeric(wu_guang_wuhan$Density_City)

library(ggplot2)

qplot(year, Density_City, data=wu_guang_wuhan, geom=c("auto"), ylim=c(925, 1000))
