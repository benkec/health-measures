####  standardized categorization and score building for SOEP-CoV group ###
load(file = str_c(temp_path,"health_covid_soep_risk.RData"))


### categorical for anxiety/depression  
# Kroenke et al (2009) An Ultra Brief Screening Scale for Anxiety and Depression: The PHQ-4. Psychosomatics 50(6), 613-621
crisis <- crisis %>% mutate(depr_cat  = cut(depr, breaks = c(0,2,5,8,12))) 

#### lonely dichotmous variable

#Steptoe, A., Shankar, A., Demakakos, P., & Wardle, J. (2013). Social isolation, loneliness, and all-cause mortality in older men and women. Proceedings of the National Academy of Sciences of the United States of America, 110(15), 5797–5801. https://doi.org/10.1073/pnas.1219686110
crisis <- crisis %>% mutate(lonely  = if_else(lone >= 6, 1, 0, NA_real_))
table(crisis$lonely)


#### depression diagnosis
crisis <- crisis %>% mutate(diag_dep = if_else(is.na(diag_dep), 0, 1, NA_real_))

#pomp scores -----------------------

# POMP = [(observed - minimum)/(maximum - minimum)] x 100,
# where observed = the observed score for a single case,
# minimum = the minimum possible score on the scale (here = 3*0 = 0), and
# maximum = the maximum possible score on the scale (here = 3*4 = 12)

crisis <- crisis %>% mutate(lone_pmp = ((lone)/(12)) * 100) 




#save
save(crisis, file = paste0(c(temp_path,"B5Lone_long.RData"), collapse=""))

#hist(crisis$lone_pmp)

### (very) good self rated health

covid_analysis <- crisis %>% mutate(srh_good = if_else(srh <=2,1,0,NA_real_) ) 

covid_analysis <- covid_analysis %>% mutate(sample_covpop = if_else(psample %in% c(1:16,20,21,99),1,0,NA_real_)) 


#### worries about own health ###

covid_analysis <- covid_analysis %>% mutate(wry_hlt_high = if_else(wry_hlt == 1,1,0,NA_real_)) 



# save -----------------------------
save(covid_analysis, file = paste0(c(temp_path,"soep_covid_health_cat.RData"), collapse=""))


write.dta(covid_analysis, paste0(c(temp_path,"soep_covid_health_cat.dta"), collapse=""))

