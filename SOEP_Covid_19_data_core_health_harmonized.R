# basic demo
if (exists("ppathl") != TRUE) {
  ppathl <- read_dta(paste0(c(soep_long_path,"ppathl.dta"), collapse=""))
}

ppathl_get <- c("pid", "syear", "sampreg")
demo <- select(ppathl, one_of(ppathl_get)) %>%
        mutate_at(vars(c("pid", "syear", "sampreg")),
        list(~ ifelse( . %in% c(-8:-1), NA, .)))


if (exists("ppath") != TRUE) {
  ppath <- read_dta(paste0(c(soep_long_path,"ppath.dta"), collapse=""))
}

ppath_get <- c("pid", "loc1989", "sex","psample","migback")
ppath_base <- select(ppath, one_of(ppath_get)) %>%
               mutate_at(vars(c("pid", "loc1989", "sex","psample","migback")),
               list(~ ifelse( . %in% c(-8:-1), NA, .)))




# health pr? covid-19 ------------------------------------------
if (exists("pl") != TRUE) {
  pl <- read_dta(str_c(soep_harmonized,harmonized_version))
}
pl_get <- c("hid", "pid", "syear","tranche", "plh0204_v2", "ple0081_v2", "ple0008", "plh0171", 
            "plh0172", "plh0184", "plh0185", "plh0186", "plh0187", 
            "plj0587", "plj0588", "plj0589",
            "plh0339", "plh0340", 
            "plh0341", "plh0342", "plh0035", "plh0182", "plh0166", "pdatm", "pdatt"
            )


health_vars <- c("smoke", "srh", "sat_hlt", 
                 "sat_slp", "angr", "anx", "hpy", "sad", 
                 "lone1", "lone2", "lone3", "phq1", "phq2", "phq3", "phq4", 
                 "wry_hlt", "lifesat", "lifesat_f")

core <- select(pl, all_of(pl_get))
names(core) <- c("hid", "pid", "syear","tranche", "risk", health_vars, "pdatm", "pdatt")


core <- mutate_at(core, vars(c("hid", "pid", "syear","tranche", "risk", all_of(health_vars))),
                  list(~ ifelse( . %in% c(-8:-1), NA, .)))
                           

core <- core %>% mutate(pid = as.integer(pid), syear = as.integer(syear) )
ppath_base <- ppath_base %>% mutate(pid = as.integer(pid))
demo <- demo %>% mutate(pid = as.integer(pid),syear = as.integer(syear))

# merge it all together ---------------------------------------------------

dat <- left_join(core, ppath_base, by="pid") %>% filter(psample !=17 & psample!=18 & psample !=19) %>%
       left_join(demo, by = c("pid","syear")) %>% left_join(select(filter(demo,syear==2018),pid,sampreg),by=c("pid")) %>%
       mutate(sampreg.x = if_else(is.na(sampreg.x),sampreg.y,sampreg.x,NA_real_)) %>% select(-sampreg.y) %>% rename("sampreg" = "sampreg.x")


# recode -------------------------------------------------------------------

recode.scale <- function(x, na.rm = TRUE) (max(x, na.rm = T) - (x-1)) 
dat <- mutate_at(dat, vars(angr, anx, sad, lone1, lone2, lone3,
                          wry_hlt),
                  .funs = list("r"=~recode.scale(.)))



dat$pdatt2 <- ifelse(dat$pdatt < 10, str_c(0, dat$pdatt), dat$pdatt)
dat$pdatm2 <- ifelse(dat$pdatm < 10, str_c(0, dat$pdatm), dat$pdatm)
dat$date <- str_c(dat$pdatm2, dat$pdatt2, dat$syear)
dat$date <- as.Date(dat$date, format = "%m %d %y")
range(dat$date, na.rm = T)

dat$week <- strftime(dat$date, format = "%V")
range(dat$week,na.rm = T)

#create diff phases for analysis ---------------------------------------
dat <- dat %>%  mutate(phase = ifelse(syear <= 2019, 1,
                       ifelse(syear == 2020 & week <= 16, 2, 
                      ifelse(syear == 2020 & week >= 17 & week <= 23, 3, 
                      ifelse(syear ==2020 & week > 23, 4, NA))))) %>%
  mutate(phase1 = if_else(syear == 2020 & week <= 16, 1, 0),
                      phase2 = if_else(syear == 2020 & week >= 17 & week <= 23 , 2 , 0),
                      phase3 = if_else(syear ==2020 & week >= 24, 3, 0))



### indicator für corona 

dat_harmonized <- dat %>% mutate(wlb = rowSums(cbind(angr_r + anx_r + hpy + sad_r), na.rm = F)) %>%
  mutate(lone = rowSums(cbind(lone1_r + lone2_r + lone3_r)-3, na.rm = F)) %>%
  mutate(depr = phq1 + phq2 + phq3 + phq4 - 4)

#### syear variable including tranches

dat_harmonized <- dat_harmonized %>% mutate(syear_tranche = if_else(!is.na(as.integer(tranche)),as.integer(2019+tranche),as.integer(syear),NA_integer_))



#save --------------------------------------------------------
save(dat_harmonized, file = paste0(c(temp_path,"soep_core_health.RData"), collapse=""))


     