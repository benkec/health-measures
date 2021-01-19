#######################
## risk groups
#######################

# read data
# if (exists("pl_v35") != TRUE) {
#         pl_v35 <- read_dta(paste0(c(soep_long_path,"pl.dta"), collapse=""))
# }

if (exists("pl") != TRUE) {
        pl <- read_dta(str_c(soep_harmonized,harmonized_version))
}

if (exists("ppath") != TRUE) {
        ppath <- read_dta(paste0(c(soep_long_path,"ppath.dta"), collapse=""))
}

ppath_get <- c("pid", "gebjahr")
age <- select(ppath, one_of(ppath_get))

bmi_vars <- c("height","weight")

pl_get <- c("pid", "syear", "ple0010_h", "ple0012", 
            "ple0013", "ple0014", "ple0015", "ple0016", 
            "ple0018", "ple0020", "ple0021", "ple0025",bmi_vars) 
          



dat <- select(pl, one_of(pl_get)) %>% group_by(pid) %>% arrange(pid, syear) %>% 
        mutate_at(vars(all_of(bmi_vars)), ~if_else(syear == 2019 & is.na(.),lag(.),.,NA_real_)) %>%
        ungroup() %>% filter(syear == 2019) %>% merge(age, by="pid", all.x = T) %>%
        mutate_at(vars(c("pid", "syear", "ple0010_h", "ple0012", 
                         "ple0013", "ple0014", "ple0015", "ple0016", 
                         "ple0018", "ple0020", "ple0021", "ple0025",all_of(bmi_vars),"gebjahr")),
        list(~ ifelse( . %in% c(-8:-1), NA, .)))

names(dat) <- c("pid", "syear", "byear", "covid19_rg_diab", 
                 "covid19_rg_asth", "covid19_rg_cardio", "covid19_rg_canc", "covid19_rg_strok", 
                 "covid19_rg_hbp", "covid19_rg_deme", "covid19_rg_rheu", "covid19_rg_hand",bmi_vars,"gebjahr")



dat <- dat %>% 
        mutate(covid19_rg_sum = rowSums(select(.,starts_with("covid19_rg")),na.rm=T))  %>% 
       mutate(age_p = (2020 - byear)) %>% 
        mutate(age = (2020 - gebjahr)) %>% 
       mutate(age_cat = cut(age, breaks = c(0,30,40,50, 60, 70, 80, 110)))  %>%
        mutate(covid19_rg_any = if_else(covid19_rg_sum >=1,1,0,NA_real_)) %>% 
        mutate(bmi = round(weight/(height/100)^2,digits=1)) %>%
        mutate(bmi_cat = cut(bmi, breaks = c(0,18.5,24.9,29.9,34.9,39.9,100)))

soep_rg <- dat %>% mutate(pid = as.integer(pid)) %>% select(-syear)


#rm(dat)

#save ------------------------------------
save(soep_rg, file = paste0(c(temp_path,"soep_core_risk.RData"), collapse=""))



####################
#codebook
####################

#age -------------------------------------

#?ltere Personen (mit stetig steigendem Risiko f?r schweren Verlauf ab 
#etwa 50-60 Jahren; 86 % der in Deutschland an COVID-19 Verstorbenen waren 
#70 Jahre alt oder ?lter [Altersmedian: 82 Jahre])

# ple0010_h (birthyear hamonized)




#vorerkrankungen -------------------------

#des Herz-Kreislauf-Systems (z. B. koronare Herzerkrankung und Bluthochdruck) # ple0016 (Apoplectic Stroke) # ple0014 (Cardiopathy) # ple0018 (high blood pressure)
#chronische Erkrankungen der Lunge (z. B. COPD) # ple0013 (asthma)
#Patienten mit chronischen Lebererkrankungen  -- FEHLT --
#Patienten mit Diabetes mellitus (Zuckerkrankheit) # ple0012 (diabetes)
#Patienten mit einer Krebserkrankung # ple0015 (cancer)
#Patienten mit geschw?chtem Immunsystem  -- FEHLT --
#(z. B. aufgrund einer Erkrankung, die mit einer Immunschw?che einhergeht oder durch die regelm??ige Einnahme von Medikamenten, die die Immunabwehr beeinflussen und herabsetzen k?nnen, wie z.B. Cortison)

#pl
# ple0011 (sleep disturbances) -- ziehe ich nicht, kein risiko
# ple0012 (diabetes)
# ple0013 (asthma)
# ple0014 (Cardiopathy)
# ple0015 (cancer)
# ple0016 (Apoplectic Stroke)
# ple0017 (Megrim) --> ziehe ich nicht, kein risiko
# ple0018 (high blood pressure)
# ple0020 (dementia)
# ple0021 (joint disorder) --> h?ufig mit einnahme von cortison verbunden)
# ple0025 (handicaped)