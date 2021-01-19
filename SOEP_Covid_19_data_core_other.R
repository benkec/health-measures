
# if (exists("pl_v35") != TRUE) {
#   pl_v35 <- read.dta(paste0(c(soep_long_path,"pl.dta"), collapse=""), convert.factors=F)
# }


if (exists("pl") != TRUE) {
  pl <- read_dta(str_c(soep_harmonized,harmonized_version))
}


pl_get <- c("pid","syear", "plh0215","plh0220","plh0225", "plh0255",
            "plh0212","plh0218","plh0222",
            "plh0213", "plh0219","plh0223",
            "plh0214", "plh0217","plh0224",
            "plh0216","plh0221","plh0226",  "ple0019") 

#

soep_other <- select(pl, one_of(pl_get))  %>% filter(syear==2019) 



ocean_ind <- c("opn01","opn02","opn03", "opn04",
              "cns01","cns02","cns03",
              "ext01", "ext02","ext03", 
              "agr01",  "agr02","agr03",
              "neu01", "neu02", "neu03",  "diag_dep")


#

names(soep_other) <- c("pid", "syear", ocean_ind)


soep_other <- soep_other %>%  mutate_at(vars(c(all_of(ocean_ind))),
          list(~ ifelse( . %in% c(-8:-1), NA, .)))

recode_scale <- function(x, na.rm = TRUE) (max(x, na.rm = T) - (x-1)) 
soep_other <- mutate_at(soep_other, vars(cns02, ext03, agr01, neu03),
                 .funs = list("r"=~recode_scale(.)))

soep_other <- soep_other %>% 
  mutate(neu = rowMeans(cbind(neu01, neu02, neu03_r), na.rm = T)) %>%
  mutate(opn = rowMeans(cbind(opn01, opn02, opn03), na.rm = T)) %>%
  mutate(cns = rowMeans(cbind(cns01, cns02_r, cns03), na.rm = T)) %>%
  mutate(ext = rowMeans(cbind(ext01, ext02, ext03_r), na.rm = T)) %>%
  mutate(agr = rowMeans(cbind(agr01_r, agr02, agr03), na.rm = T))


soep_other <- soep_other %>% mutate(pid = as.integer(pid)) %>% select(-syear)
#save ------------------------------------
save(soep_other, file = str_c(temp_path,"soep_core_other.RData"))

