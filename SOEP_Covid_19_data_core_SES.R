
#Alle bis auf die Hochvermögenden und effektiv ohne Q da es hier gar keine Gewichte gab.

# SES variables: education and HH-income  ------------------------------------------
if (exists("pl") != TRUE) {
  pl <- read_dta(str_c(soep_harmonized,harmonized_version))
}
pl_ses_get <- c("hid", "pid", "syear","tranche","pgcasmin","hlc0005_h",
                "pgegp08","hhgr","psample","phrf",
                "hgsize",
                "hgseval",
                "hgeqpter",
                "hgeqpgar",
                "hgtyp2hh",
                "hgtyp1hh",
                "hlf0021_h",
                "systemrelevant_nace", 
                "systemrel")

hh_vars <- c("hhgr",
             "hgsize",
"hgseval",
"hgeqpter",
"hgeqpgar",
"hgtyp2hh",
"hgtyp1hh",
"hlf0021_h")

#### HH composition ####

ses <- select(pl, all_of(pl_ses_get))



#### get weights from the previous year for 2019 ####
ses <- ses %>% group_by(pid) %>% arrange(pid, syear) %>% mutate(phrf_l = lag(phrf)) %>% mutate(phrf_l2 = lag(phrf_l)) %>% ungroup() %>%
       mutate(phrf= if_else(syear ==2019 & is.na(phrf),phrf_l,phrf,NA_real_)) %>% 
       mutate(phrf= if_else(syear ==2019 & is.na(phrf),phrf_l2,phrf,NA_real_))

### get weights ###

warning("Weights are set to 1 and need to be added!")

if (Sys.info()[["user"]] == "bootstrap"){

  covid_weights <- read_dta(str_c(soep_harmonized,weights_version)) %>% rename(phrf_normal = phrf) %>%
  rename(phrf=phrf_cati) %>% mutate(syear =2020) %>% select(pid,syear,phrf,hhrf,phrf_normal)
} 



cpi <- read.csv(str_c(script_path,"cpi.csv")) %>% mutate(syear=as.numeric(syear))


### var hhnetinc "Monthly hh net income" CPI weighted to be like 2015
ses <- left_join(ses,cpi,by="syear") %>%  mutate_at(vars(c(all_of(pl_ses_get))),
                                                    list(~ ifelse( . %in% c(-8:-1), NA, .))) %>% 
  mutate(hhnetinc=(hlc0005_h*100/cpi)/sqrt(hhgr))



#### create tertiles: 
#### 0) generate sample indicator
#### 1) winsowering 
#### 2) calculate tertiles cut-offs (weight) 
#### 3) build groups for 2019
#### 4) groups 2019 wide machen


### 0) sample indicator and NA recode
ses <- ses %>% mutate(sample_covpop = if_else(psample %in% c(1:16,20,21,99),1,0,NA_real_)) %>%
  filter(sample_covpop==1)  

### 1) winsowering 

percentiles <- ses %>% group_by(syear) %>% summarise(hhnetinc_p1 = quantile(hhnetinc, c(.01),na.rm=T),hhnetinc_p99 = quantile(hhnetinc, c(.99),na.rm=T))

ses <- left_join(ses,percentiles,by="syear") %>% mutate(hhnetinc = if_else(hhnetinc < hhnetinc_p1,hhnetinc_p1,hhnetinc,NA_real_)) %>% 
                                         mutate(hhnetinc = if_else(hhnetinc > hhnetinc_p99,hhnetinc_p99,hhnetinc,NA_real_))

#### 2) calculate tertiles cut-offs (weights) 

tertiles_cut_off <- filter(ses,syear<2020&syear>2014) %>% group_by(syear) %>% 
  summarise(hhnetinc_tert1 = wtd.quantile(hhnetinc, probs=c(.333),na.rm=T,weights=phrf),hhnetinc_tert2 = wtd.quantile(hhnetinc, probs=c(.666),na.rm=T,weights=phrf))


ses <- left_join(ses,tertiles_cut_off,by="syear") %>% 
  mutate(hhnetinc_tertiles = if_else(hhnetinc <= hhnetinc_tert1,1,if_else(hhnetinc <= hhnetinc_tert2,2,3,NA_real_),NA_real_))




#### education low/middle/high

ses <- ses %>% mutate(edu = cut(pgcasmin, breaks = c(0,3, 7, 9)))


#### get education category in 2019 from previous year ####
ses <- ses %>% group_by(pid) %>% arrange(pid, syear) %>% 
  mutate(edu = if_else(syear ==2019 & is.na(edu),lag(edu),edu)) %>% ungroup()
#### get education category in 2020 from previous year (indirectly from 2018) ####
ses <- ses %>% group_by(pid) %>% arrange(pid, syear) %>% 
  mutate(edu = if_else(syear ==2020 & is.na(edu),lag(edu),edu)) %>% ungroup()


filter(ses,!is.na(edu)) %>% group_by(syear) %>% summarise(n = n()) 

#### get income/education category from previous year ####
ses <- ses %>% group_by(pid) %>% arrange(pid, syear) %>% 
  mutate(hhnetinc_tertiles = if_else(syear >2019 & is.na(hhnetinc_tertiles),lag(hhnetinc_tertiles),hhnetinc_tertiles,NA_real_)) %>% ungroup()


#### get hh_vars category from 2 years previous ####
ses <- ses %>% group_by(pid) %>% arrange(pid, syear) %>% 
  mutate_at(vars(all_of(hh_vars)), ~if_else(syear == 2019 & is.na(.),lag(.),.,NA_real_)) %>% 
  mutate_at(vars(all_of(hh_vars)), ~if_else(syear == 2020 & is.na(.),lag(.),.,NA_real_)) %>% ungroup()
   

# mutate(vars(all_of(hh_vars)), if_else(is.na(.),lag(.),.,NA_real_))

#### HH TYP CODIEREN ####

ses <- ses %>% rec(hgtyp1hh, rec = "1=1; 2=2 ; 3=3 ; 4:6=4 ; 7:8=5") %>%  rename(hh_typ = hgtyp1hh_r)


#### crowding indicator ####


##sqm per person, persons per room
ses <- ses %>% mutate(sqm_pp=hgsize/hhgr) %>% mutate(persons_room = hhgr/hlf0021_h ) %>%
  rec(persons_room, rec = "0:0.99999=1; 1:1.999=2 ; 2:10000=3" ) %>%  rename(ppr_cat = persons_room_r) %>%
  rec(sqm_pp, rec = "0:19.99999=1; 20:29.999999=2 ; 30:10000=3" ) %>%  rename(sqmpp_cat = sqm_pp_r)

#%>% summarise(m=mean(sqm_pp,na.rm=T),min=min(sqm_pp,na.rm=T),max=max(sqm_pp,na.rm=T))

## 


ses <- select(ses,pid,syear,pgegp08,edu,hhnetinc_tertiles,phrf,all_of(hh_vars),ppr_cat,sqmpp_cat,hh_typ,sqm_pp,persons_room)

#save --------------------------------------------------------
save(ses, file = paste0(c(temp_path,"soep_core_ses.RData"), collapse=""))


     