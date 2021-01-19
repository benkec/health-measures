# load harmonized dataset -----------------------------
load(str_c(temp_path,"soep_core_health.RData"))

#load RG data set -------------------
load(str_c(temp_path,"soep_core_risk.RData"))


# load other CORE varibales data set
load(str_c(temp_path,"soep_core_other.RData"))

# load SES and weights CORE varibales data set
load(str_c(temp_path,"soep_core_ses.RData"))

#behavioral data
load(str_c(temp_path,"health_covid_behavior.RData"))

#append data set with time varying variables ----------------------


  
# risk groups --------------------
crisis <- merge(dat_harmonized, soep_rg, by = c("pid"), all.x = T)
# other variables --------------------
crisis <- merge(crisis, soep_other, by = c("pid"), all.x = T) %>% mutate(sample_covpop = if_else(psample %in% c(1:16,20,21,99),1,0,NA_real_)) %>%
  filter(sample_covpop==1)
## ses and weights, and behavior
crisis <- left_join(crisis, ses, by = c("pid", "syear")) %>% left_join(soep_behavior, by = c("pid", "syear"))

#### covid weights wide anmergen für sample_cov only analysen
#ses <- left_join(ses,covid_weights,by=c("pid","syear")) %>% mutate(phrf = if_else(syear==2020,phrf.y,phrf.x,NA_real_))

if (Sys.info()[["user"]] == "bootstrap"){
	crisis <- left_join(crisis, select(covid_weights,pid,phrf), by = c("pid")) %>% rename(phrf= phrf.x, phrf_cati=phrf.y)

	crisis <- left_join(crisis,covid_weights,by=c("pid","syear")) %>% mutate(phrf = if_else(syear==2020,phrf.y,phrf.x,NA_real_))


	crisis <- crisis %>% mutate(soep_cov_ind = if_else(is.na(phrf_cati) | phrf_cati ==0,0,1))
} else {

	crisis <- crisis %>% mutate(phrf_cati = 1) %>% select(-phrf) %>% mutate(phrf = 1)
}
#### set missing weights to zero ### 

crisis <- crisis %>% mutate(phrf = if_else(is.na(phrf),0,phrf),phrf_cati = if_else(is.na(phrf_cati),0,phrf_cati))


#crisis <- filter(crisis,!is.na(phrf))

save(crisis, file = paste0(c(temp_path,"health_covid_soep_risk.RData"), collapse=""))

  
  

