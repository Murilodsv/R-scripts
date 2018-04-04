#--- Sugarcane Logistic Problem
#--- Please refer to Lamsal (2014) thesis for problem intuition
#--- Sugarcane harvest logistic (phd Thesis) - Kamal Lamsal

avg_yi  = 75     #Average Brazilian Sugarcane Yield [ton ha-1]
raiz_a  = 850000 #Raizen Total sugarcane cropped area [ha]
tr_cap  = 62     #Maximum truck ("treminhão") capacity [ton]
harv_p  = 8 * 30 #Average Harvesting season assuming 8 months (March - October) [days]
cct  = 25        #Average cost of transportation of sugarcane (R$ ton-1) 

#How many payloads Raizen would need per year?
tr_year = avg_yi * raiz_a / tr_cap

#How many payloads Raizen would need to manage per day?
tr_day  = tr_year / harv_p

#Average cost of transportation
total_c = avg_yi * raiz_a * cct

#Average cost per ha
cct_ha  = cct * avg_yi

inc_y   = 1:10/10 + 1
avg_yinc = avg_yi * inc_y

#How many payloads Raizen would need per year?
tr_yearinc = avg_yinc * raiz_a / tr_cap

#How many payloads Raizen would need to manage per day?
tr_dayinc  = tr_yearinc / harv_p

plot(tr_dayinc~inc_y)

gap = 120 / 75
gap
