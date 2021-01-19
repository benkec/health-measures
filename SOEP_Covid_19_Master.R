#############################
#### Analyses of SOEP-CoV####
#### study on            ####
#### p health measures   ####
#### and psych healt     ####
####                     ####
#### DIW  Marburg        ####
####                     ####
####                     ####
#### 18.12.2020          ####
#############################
rm(list = ls())

#############################
#### packages ####
#############################

loadpackage <- function(x){
  for(i in x){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require(i, character.only = TRUE )){
      #  If package was not able to be loaded then re-install
      install.packages(i, dependencies = TRUE)
    }
    #  Load package (after installing)
    library(i, character.only = TRUE)
  }
}

#  Then try/install packages...
loadpackage(c("tidyverse","rlang", "haven","lavaan","foreign", 
              "semTools", "knitr", "kableExtra","Hmisc",
              "srvyr", "scales","sjPlot","sjmisc","cowplot","extrafont"))



##################################
##### set paths ##################
##################################

if (Sys.info()[["user"]] == "tentri") {
  temp_path   <- c("/Users/bootstrap/documents/temp/")
  data_path   <- c("/Users/bootstrap/ownCloud/cov_daten/data/")
  script_path   <- c("/Users/bootstrap/documents/do/covid/mainz/")
  analysis_path   <- str_c(script_path,"analysis/")
  soep_long_path <- c("/Users/bootstrap/Documents/data/SOEP/SOEP35/Stata/")
  soep_harmonized <- c("/Users/bootstrap/ownCloud/cov_daten/data/")
  tables_path <- str_c(script_path,"tables/")
  ses_decomp <- str_c("/Users/bootstrap/ownCloud/[Projects]/Covid_19/Manuskripte/Mainz/")
  graphs_path <- str_c(ses_decomp,"graphs/")


harmonized_version <- c("soep_cov_20201126_statav13.dta")

weights_version <- c("2020-07-22_SOEP_CoV_Gewichte_v13.dta")


} else if (Sys.info()[["user"]] == "XXX NORAS BENUTZERNAME XXX") {



  harmonized_version <- c("DUMMY_soep_cov_20201203_statav13")
}



#############################
#### Data from Stata     ####
#############################

# soep_covid19_l <- read_dta(paste0(c(data_path,"Vorabdaten_CoV201_20200414.dta"),
#                                   collapse=""))
# soep_covid19 <- read_dta(paste0(c(data_path,"Vorabdaten_CoV201_20200414.dta"), collapse=""))




###### RUN DATA FILES -----------------------------------------------------------------------

### Harmomized Data  --------------------------------------------------------
source(str_c(script_path,"SOEP_Covid_19_data_core_health_harmonized.R"))

### SES and weights  --------------------------------------------------------
source(str_c(script_path,"SOEP_Covid_19_data_core_SES.R"))

#### Identify Covid_19 risk groups in CORE
source(str_c(script_path,"SOEP_Covid_19_data_risk_groups.R"))

#### Other CORE variables incl. personality
source(str_c(script_path,"SOEP_Covid_19_data_core_other.R"))

#### Behavioral variables
source(str_c(script_path,"SOEP_Covid_19_data_behavior.R"))

#### merge data sets
source(str_c(script_path,"SOEP_Covid_19_data_merge_core.R"))

#### define general health categories and codings for everyone 
source(str_c(script_path,"SOEP_Covid_19_data_health_categories.R"))



