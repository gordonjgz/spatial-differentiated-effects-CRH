
clear

cd "\\tsclient\C\Users\guozhenj\Dropbox\HSR"

import delimited "Data/trimmed_data.csv"

drop v1

rename data_2017city_name_en city_name

encode density_dist_2010, generate(density_dist_2010_2)
drop density_dist_2010
rename density_dist_2010_2 density_dist_2010

encode trade_ratio_city_2007, generate(trade_ratio_city_2007_2)
drop trade_ratio_city_2007
rename trade_ratio_city_2007_2 trade_ratio_city_2007

recast float density_dist_2010

recast float trade_ratio_city_2007

encode trade_ratio_dist_2007, generate(trade_ratio_dist_2007_2)
drop trade_ratio_dist_2007
rename trade_ratio_dist_2007_2 trade_ratio_dist_2007
recast float trade_ratio_dist_2007

encode trade_ratio_dist_2010, generate(trade_ratio_dist_2010_2)
drop trade_ratio_dist_2010
rename trade_ratio_dist_2010_2 trade_ratio_dist_2010
recast float trade_ratio_dist_2010

encode trade_ratio_dist_2014, generate(trade_ratio_dist_2014_2)
drop trade_ratio_dist_2014
rename trade_ratio_dist_2014_2 trade_ratio_dist_2014
recast float trade_ratio_dist_2014

encode trade_ratio_dist_2017, generate(trade_ratio_dist_2017_2)
drop trade_ratio_dist_2017
rename trade_ratio_dist_2017_2 trade_ratio_dist_2017
recast float trade_ratio_dist_2017


merge 1:1 city_name using "Data/City_CHR.dta"

drop _m

drop density_dist* trade_ratio_dist*

label variable city_name "Name of Perfecture City - China"

rename chr chr_dummy
rename year year_chr
rename tradeaccessible trade_accessible_dummy

label variable density_city_2007 "2007 Pop density for Perfecture City - China"
label variable density_city_2010 "2010 Pop density for Perfecture City - China"
label variable density_city_2014 "2014 Pop density for Perfecture City - China"
label variable density_city_2017 "2017 Pop density for Perfecture City - China"
label variable trade_ratio_city_2007 "2007 The proportion of labor in the Trade sector"
label variable trade_ratio_city_2010 "2010 The proportion of labor in the Trade sector"
label variable trade_ratio_city_2014 "2014 The proportion of labor in the Trade sector"
label variable trade_ratio_city_2017 "2017 The proportion of labor in the Trade sector"
label variable chr_dummy "Indicate if the city has High Speed Railway"
label variable year_chr "The year that city built High Speed Railway, NA if no railway"
label variable trade_accessible_dummy "Indicate if the city is accessible to the world market"

order trade_ratio_city_2007, after(density_city_2007)

save "Data\Final_data.dta", replace
