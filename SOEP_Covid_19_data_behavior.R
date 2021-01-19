


if (exists("pl") != TRUE) {
  pl <- read_dta(str_c(soep_harmonized,harmonized_version))
}


pl_behavior_get <- c("pid","syear","pli0092_v3","pli0040","pli0043_v3",
            "pli0044_v3","pli0046","pli0051","pli0080",
            "pli0081","pli0083","pli0093_v2","pli0165",
            "pli0038_v4","pli0047_v1","pli0049_v3","pli0088",
            "pli0162","plh0032","plh0033",
            "plb0018","plb0021","plb0022_h",
            "hgeqpgar", "hgeqpter", "hgseval") 

#

soep_behavior <- select(pl, one_of(pl_behavior_get))  %>% mutate_at(vars(c(all_of(pl_behavior_get))),
                                                           list(~ ifelse( . %in% c(-8:-1), NA, .)))

names(soep_behavior) <- c("pid", "syear","freq_sports","time_chores","time_housework",
                       "time_children","time_care","time_hobbies","freq_friends",
                       "freq_family","freq_video","freq_arts","freq_social_network",
                       "time_job","time_training","time_repairs","freq_car",
                       "time_physical","wry_econ_gen","wry_econ_own",
                       "paid_work","unemp","emplst",
                       "garden","terasse","size_eval")




save(soep_behavior, file = paste0(c(temp_path,"health_covid_behavior.RData"), collapse=""))




# pli0038_v4      byte    %54.0g     pli0038_v4
# Beruf/Lehre Std./Werktag [1992-2018]
# pli0040         byte    %54.0g     pli0040    Besorgungen Std., Werktg.
# pli0043_v3      byte    %54.0g     pli0043_v3
# Hausarbeit Std./Werktag [1992-2018]
# pli0044_v3      byte    %54.0g     pli0044_v3
# Kinderbetreuung, Mo.-Fr., Stunden [1992-2018]
# pli0046         byte    %54.0g     pli0046    Versorgung Pflegebeduerftiger, Werktg.
# pli0047_v1      byte    %42.0g     pli0047_v1
# Aus- u. Weiterb., Lernen Std., Werktg. (erwerbstaetig) [1984-2018]
# pli0049_v3      byte    %54.0g     pli0049_v3
# Reparaturen Std./Werktag [1992-2018]
# pli0051         byte    %42.0g     pli0051    Hobbies, Freizeit Std., Werktg.
# pli0080         byte    %54.0g     pli0080    Besuche Nachbarn,Freunde
# pli0081         byte    %54.0g     pli0081    Besuche Familie,Verwandte
# pli0083         byte    %54.0g     pli0083    Fernsehen, Video
# pli0088         byte    %54.0g     pli0088    Fahrzeugpflege,-reparaturen
# pli0092_v3      byte    %54.0g     pli0092_v3
# Aktiver Sport (lang) (unregelmaessig) [1990-2018]
# pli0093_v2      byte    %54.0g     pli0093_v2
# Kuenstlerische und musische Taetigkeiten (lang) (unregelmaessig (unregelmaessi
#                                                                  pli0094_h       byte    %54.0g     pli0094_h
#                                                                  Kontakt zu Freunden, Verwandten im Ausland [harmonisiert]
#                                                                  pli0094_v3      byte    %54.0g     pli0094_v3
#                                                                  Kontakt zu Freunden, Verwandten im Ausland (lang) [2008,2013,2017-2018]
#                                                                  pli0095_v2      byte    %54.0g     pli0095_v2
#                                                                  Mithelfen bei Freund., Verwandt. (unregelmaessig) [1985-2017]
#                                                                  pli0162         byte    %54.0g     pli0162    Koerperl. Aktivitaeten Std., Werktg.
#                                                                  pli0165         byte    %54.0g     pli0165    Nutzen sozialer Online-Netzwerke
#                                                                  


#sport
#pli0092_h/pli0092_v3, pli0162