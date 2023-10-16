remove(list=ls()) #removes all objects from project

library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(readr)
library(data.table)
library(stringr)
library(lubridate)
library(tidyverse)


# Calibration Ranges --------------------------------------------------

#Manufacturer Defined Parameter Ranges c(Lower,Upper)    
Temp_Range <-c(-5,50)
Cond_Range <-c(0,200000)
SpCond_Range <-c(0,200000)
ODO_mg_Range <-c(0,50) 
pH_Range <-c(0,14)
Turbidity_Range <-c(0,4000)
fDOM_RFU_Range <-c(0,100)

#Calibration ranges for bracketing
SpCond_2000 <-c(1900,2100)
SpCond_200 <-c(190,210)
pH_4 <-c(3.7,4.3)
pH_7 <-c(6.7,7.3)
pH_10 <-c(9.7,10.3)
Turbidity_124<-c(111.6,136.4)
FDOM_RFU_0 <-c(0,2)
fDOM_RFU_100 <-c(90,110)


# Import Deployment Data -------------------------------------------------------------
#Deployment 1
Bare_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Bare_ECOTOPE_060921.csv") %>% mutate (Event="Event 1") #deployment 1
Chara_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Chara_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1
Typha_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Cattail_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1
Southern_N_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Southern_N_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1
Mixed_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Mixed_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1

#Deployment 2
Bare_082621 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/pdynSTA34A41_ecotopeBare - 082621 174830.csv") %>% mutate (Event="Event 2") %>%
slice(1993:2325) %>%  mutate(Site="STA3/4C2B_Ecotope_Bare")             #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1991 and collected at 2325

Cattail_082621 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Cattail - 083121 133214.csv") %>% mutate (Event="Event 2")  %>% mutate(`Site`="Typha") %>% slice(3875:4847)#file contains data from PDYNAMICS site and ECOTOPE site

Chara_082621 <-  read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Chara - 083121 133903.csv") %>% mutate (Event="Event 2") %>% slice(1994:2328) %>% #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1994 and collected at 2328
mutate(Site="STA3/4C2B_Ecotope_Chara") #file contains data from PDYNAMICS site and ECOTOPE site

Mixed_082621 <-  read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Mixed- 083121 134557.csv") %>% mutate (Event="Event 2") %>% #file contains data from PDYNAMICS site and ECOTOPE site
slice(1995:2326) %>%  mutate(Site="STA3/4C2B_Ecotope_Mixed") #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1995 and collected at 2326

Naiad_082621<- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Southern Naiad- 083121 135134.csv") %>% mutate (Event="Event 2") %>% #file contains data from PDYNAMICS site and ECOTOPE site
slice(1992:2325) %>%  mutate(Site="STA3/4C2B_Ecotope_Naiad") #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1992 and collected at 1995
 
#Deployment 3
Bare_091521 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20210915_bare.csv") %>% mutate (Event="Event 3")
Mixed_091521 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20210915_mixed.csv") %>% mutate (Event="Event 3")
Chara_091521 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20210915_chara.csv") %>% mutate (Event="Event 3")
Naiad_091521 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20210915_naiad.csv") %>% mutate (Event="Event 3")
Typha_091521 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20210915_typha.csv") %>% mutate (Event="Event 3")

#Deployment 4
Bare_112421 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220111_Bare.csv", skip = 8) %>% mutate (Event="Event 4")
Naiad_112421 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220111_Naiad.csv",skip = 8) %>% mutate (Event="Event 4")
Mixed_112421 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220111_Mixed.csv",skip = 8) %>% mutate (Event="Event 4")
Typha_112421 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220111_Typha.csv",skip = 8) %>% mutate (Event="Event 4")
Chara_112421 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220111_Chara.csv",skip = 8) %>% mutate (Event="Event 4")

#Deployment 5
chara_20220502 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220502_chara.csv",skip = 8) %>% mutate (Event="Event 5")
Bare_20220502 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220502_bare.csv",skip = 8) %>% mutate (Event="Event 5")
Typha_20220502 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220502_typha.csv",skip = 8) %>% mutate (Event="Event 5")
Mixed_20220502 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220502_mix.csv",skip = 8) %>% mutate (Event="Event 5")
Naiad_20220502 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220502_naiad.csv",skip = 8) %>% mutate (Event="Event 5")

#Deployment 6
chara_20220531 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220531_chara.csv") %>% mutate (Event="Event 6")
Bare_20220531 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220531_bare.csv") %>% mutate (Event="Event 6")
Typha_20220531 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220531_Typha.csv") %>% mutate (Event="Event 6")
Mixed_20220531 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220531_mix.csv") %>% mutate (Event="Event 6")
Naiad_20220531 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20220531_naiad.csv") %>% mutate (Event="Event 6")

#Deployment 7
chara_20230524 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20230524 Chara.csv") %>% mutate (Event="Event 7")
Bare_20230524 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20230524 Bare.csv") %>% mutate (Event="Event 7")
Typha_20230524 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20230524 Typha.csv") %>% mutate (Event="Event 7")
Mixed_20230524 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20230524 Mixed.csv") %>% mutate (Event="Event 7")

#Deployment 8
chara_20230823 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20230823 Chara.csv") %>% mutate (Event="Event 8")
Bare_20230823 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20230823 Bare.csv") %>% mutate (Event="Event 8")
Typha_20230823 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20230823 Typha.csv") %>% mutate (Event="Event 8")
Mixed_20230823 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/20230823 Mixed.csv") %>% mutate (Event="Event 8")



# Import Post-Calibration Data --------------------------------------------

Post_Cal_20210630 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20210630 Post-Cal.xlsx") %>% mutate (Event="Event 1")
Post_Cal_20210826 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20210826 Post-Cal.xlsx") %>% mutate (Event="Event 2")
Post_Cal_20211110 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20211110 Post-Cal.xlsx") %>% mutate (Event="Event 3")
#event 4 missing?
Post_Cal_20220503 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20220503 Post-Cal.xlsx") %>% mutate (Event="Event 5")
Post_Cal_20230524 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20230524 Post-Cal.xlsx") %>% mutate (Event="Event 6")
Post_Cal_20230606 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20230606 Post-Cal.xlsx") %>% mutate (Event="Event 7")
Post_Cal_20230823 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20230823 Post-Cal.xlsx") %>% mutate (Event="Event 8")


# Tidy Deployment Data ----------------------------------------------------

#join Data sets 
Deployment_Data <- bind_rows(Clean_Column_Headers(Bare_60921),Clean_Column_Headers(Southern_N_60921), Clean_Column_Headers(Typha_60921), Clean_Column_Headers(Mixed_60921), Clean_Column_Headers(Chara_60921)) %>% #Bind Event 1 Data
bind_rows(Clean_Column_Headers(Bare_082621), Clean_Column_Headers(Naiad_082621), Clean_Column_Headers(Cattail_082621), Clean_Column_Headers(Mixed_082621), Clean_Column_Headers(Chara_082621)) %>% #Bind Event 2 Data
bind_rows(Clean_Column_Headers(Bare_091521), Clean_Column_Headers(Naiad_091521), Clean_Column_Headers(Typha_091521), Clean_Column_Headers(Mixed_091521), Clean_Column_Headers(Chara_091521)) %>% #Bind Event 3 Data
bind_rows(Clean_Column_Headers(Bare_112421), Clean_Column_Headers(Naiad_112421), Clean_Column_Headers(Typha_112421), Clean_Column_Headers(Mixed_112421), Clean_Column_Headers(Chara_112421)) %>% #Bind Event 4 Data
bind_rows(Clean_Column_Headers(Bare_20220502), Clean_Column_Headers(Naiad_20220502), Clean_Column_Headers(Typha_20220502), Clean_Column_Headers(Mixed_20220502), Clean_Column_Headers(chara_20220502)) %>% #Bind Event 5 Data
bind_rows(Clean_Column_Headers(Bare_20220531), Clean_Column_Headers(Naiad_20220531), Clean_Column_Headers(Typha_20220531), Clean_Column_Headers(Mixed_20220531), Clean_Column_Headers(chara_20220531)) %>% #Bind Event 6 Data
bind_rows(Clean_Column_Headers(Bare_20230524), Clean_Column_Headers(Typha_20230524), Clean_Column_Headers(Mixed_20230524), Clean_Column_Headers(chara_20230524)) %>% #Bind Event 7 Data
bind_rows(Clean_Column_Headers(Bare_20230823), Clean_Column_Headers(Typha_20230823), Clean_Column_Headers(Mixed_20230823), Clean_Column_Headers(chara_20230823)) %>%    #Bind Event 8 Data
mutate(Date=mdy(Date),`Date Time`=ymd_hms(paste(Date," ",Time))) %>%  # Format date and time
mutate(STA=ifelse(str_detect(Site,"STA3"),"STA34",NA))    %>%         #get STA name from site  
mutate(Ecotope=case_when(str_detect(Site,"Bare")~"Bare",
                         str_detect(Site,"SouthernN")~"Southern Naiad",
                         str_detect(Site,"Mixed")~"Chara and Southern Naiad",
                         str_detect(Site,"Chara")~"Chara",
                         str_detect(Site,"Cattail")~"Typha",
                         .default = as.character("Missing site name"))) %>%
mutate(Sonde=substr(`Unit ID`,str_locate(`Unit ID`,"-")[1]+1,nchar(`Unit ID`)))
                         
                     
write.csv(Deployment_Data,"C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Ecotope_deployed_sonde_data_raw.csv",row.names=FALSE)  #Save combined data without any qualifiers

# Tidy Post Cal Data ------------------------------------------------------

#Checks to see if post-cal passed single calibration points
Post_Cal_Data <- bind_rows(Post_Cal_20210630,Post_Cal_20210826,Post_Cal_20211110,Post_Cal_20220503,Post_Cal_20230524,Post_Cal_20230606,Post_Cal_20230823) %>%    
pivot_longer(names_to="Parameter",values_to="Value",cols=c("SpCond 200", "SpCond 2000","DO", "pH 7", "pH 10", "FDOM RFU", "Turbidity","TAL RFU", "BGA RFU","pH 4")) %>% 
mutate(Calibration=case_when(Parameter=="SpCond 2000" & Value %between% SpCond_2000 ~ "Passed SpCond 2000",
                             Parameter=="SpCond 2000" & !Value %between% SpCond_2000 ~ "Failed SpCond 2000",
                             Parameter=="SpCond 200" & Value %between% SpCond_200 ~ "Passed SpCond 200",
                             Parameter=="SpCond 200" & !Value %between% SpCond_200 ~ "Failed SpCond 200",
                             Parameter=="pH 7" & Value %between% pH_7 ~ "Passed pH 7",
                             Parameter=="pH 7" & !Value %between% pH_7 ~ "Failed pH 7",
                             Parameter=="pH 10" & Value %between% pH_10 ~ "Passed pH 7",
                             Parameter=="pH 10" & !Value %between% pH_10 ~ "Failed pH 7",
                             Parameter=="pH 4" & Value %between% pH_4 ~ "Passed pH 4",
                             Parameter=="pH 4" & !Value %between% pH_4 ~ "Failed pH 4",
                             Parameter=="Turbidity" & Value %between% Turbidity_124 ~ "Passed Turbidity 124",
                             Parameter=="Turbidity" & !Value %between% Turbidity_124 ~ "Failed Turbidity 4",
                             Parameter=="FDOM RFU" & Value %between% fDOM_RFU_100 ~ "Passed RFU 100",
                             Parameter=="FDOM RFU" & !Value %between% fDOM_RFU_100 ~ "Failed RFU 100",
                            .default = as.character("Missing Calibration Data"))) %>%
mutate(Calibration=ifelse(is.finite(Value) & is.finite(`DO T-Val`) & Parameter=="DO",ifelse(abs(Value-`DO T-Val`)<=0.3,"Passed DO","Failed DO") ,Calibration))        #check if DO is within 0.3 mg/l to True value based on temp


#checks to see if post-cal was passed for the multiple calibrations points needed to bracket the data
Post_Cal_Summarized <- Post_Cal_Data %>%
select(Event,Sonde,Parameter,Calibration) %>%  
pivot_wider(names_from = "Parameter", values_from = "Calibration")  %>%
mutate(`DO (mg/L)`=case_when(str_detect(DO,"Passed")~"DO Passed Post-Calibration",                            
                             str_detect(DO,"Failed")~"DO Failed Post-Calibration",
                             str_detect(DO,"Missing")~"DO Missing Post-Calibration Data",
                             .default = as.character("DO Missing Post-Calibration Data"))) %>%
mutate(`SpCond (µS/cm)`=case_when(str_detect(`SpCond 2000`,"Passed") &  str_detect(`SpCond 200`,"Passed") ~"SpCond Passed Post-Calibration",
                               str_detect(`SpCond 2000`,"Failed") |  str_detect(`SpCond 200`,"Failed")~"SpCond Failed Post-Calibration",
                               str_detect(`SpCond 2000`,"Missing") |  str_detect(`SpCond 200`,"Missing")~"SpCond Missing Post-Calibration Data",
                               .default = as.character("SpCond Missing Post-Calibration Data"))) %>%
mutate(`pH`=case_when(str_detect(`pH 7`,"Passed") &  str_detect(`pH 10`,"Passed") ~"pH Passed Post-Calibration",
                               str_detect(`pH 7`,"Failed") |  str_detect(`pH 10`,"Failed")~"pH Failed Post-Calibration",
                               str_detect(`pH 7`,"Missing") |  str_detect(`pH 10`,"Missing")~"pH Missing Post-Calibration Data",
                               .default = as.character("SpCond Missing Post-Calibration Data"))) %>%
mutate(`Turbidity (FNU)`=case_when(str_detect(Turbidity,"Passed")~"Turbidity Passed Post-Calibration",                              
                               str_detect(Turbidity,"Failed")~"Turbidity Failed Post-Calibration",
                               str_detect(Turbidity,"Missing")~"Turbidity Missing Post-Calibration Data",
                               .default = as.character("Turbidity Missing Post-Calibration Data"))) %>%
mutate(`fDOM (RFU)`=case_when(str_detect(`FDOM RFU`,"Passed")~"FDOM Passed Post-Calibration",                              
                               str_detect(`FDOM RFU`,"Failed")~"FDOM Failed Post-Calibration",
                               str_detect(`FDOM RFU`,"Missing")~"FDOM Missing Post-Calibration Data",
                               .default = as.character("FDOM Missing Post-Calibration Data"))) %>%
pivot_longer(names_to = "Parameter",values_to ="Post-Calibration",cols = c("DO (mg/L)", "SpCond (µS/cm)","pH","Turbidity (FNU)", "fDOM (RFU)"))  %>%
select(Event,Sonde,Parameter,`Post-Calibration`)



# --------- Joins deployed data to post-cal data ----------------------------------------------------------------


Deployment_Data_Post_cal <- Deployment_Data %>%
pivot_longer(names_to = "Parameter",values_to ="Value",cols = c("DO (mg/L)","Temp (C°)", "SpCond (µS/cm)","pH","Turbidity (FNU)", "fDOM (RFU)","TAL (RFU)") )  %>%
left_join(Post_Cal_Summarized,by=c("Event","Sonde","Parameter"))




# checks to see if deployed data is within manufacturers range ------------

Deployment_Data_Post_cal_manufacurer_check <- Deployment_Data_Post_cal %>%
mutate(`Manufacturer Range`=case_when(Parameter=="Temp (C°)" & Value %between% Temp_Range =="FALSE"~ "Value outside sensor range",
                                      Parameter=="DO (mg/L)" & Value %between% ODO_mg_Range =="FALSE"~ "Value outside sensor range",  
                                      Parameter=="SpCond (µS/cm)" & Value %between% SpCond_Range =="FALSE"~ "Value outside sensor range",
                                      Parameter=="pH" & Value %between% pH_Range =="FALSE"~ "Value outside sensor range",
                                      Parameter=="Turbidity (FNU)" & Value %between% Turbidity_Range =="FALSE"~ "Value outside sensor range",
                                      Parameter=="fDOM (RFU)" & Value %between% fDOM_RFU_Range =="FALSE"~ "Value outside sensor range", 
                                     .default = as.character(""))) 



# Add qualifier code and remark notes with reason for qualification -------


Deployment_Data_Qualified  <- Deployment_Data_Post_cal_manufacurer_check %>%
mutate(`Remark Code`="",`Remark Note`="") %>%
mutate(`Remark Code`=case_when(`Manufacturer Range`=="Value outside sensor range"~"?",
                               str_detect(`Post-Calibration`,"Failed") | str_detect(`Post-Calibration`,"Missing") ~"J",
                               `Parameter` %in% c("fDOM (RFU)","TAL (RFU)","Turbidity (FNU)") ~"H",
                               .default = `Remark Code`)) %>%
mutate(`Remark Note`=ifelse(`Remark Code` =="H","Method has not been recognized by the FDEP as equivalent to laboratory methods",`Remark Note`)) %>% 
mutate(`Remark Note`=trimws(ifelse(`Remark Code` %in% c("H","J","?"),paste(`Remark Note`,`Manufacturer Range`,`Post-Calibration`),`Remark Note`))) 
  
                                
write.csv(Deployment_Data_Qualified,"C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Ecotope_deployed_sonde_data_qualified.csv",row.names=FALSE)  #Save combined data with qualifiers


# Function clean column headers -------------------------------------------

Clean_Column_Headers <-  function(df)
{
  ifelse(TRUE %in% str_detect(names(df),"DO mg/L"),setnames(df, old=grep("DO mg/L", names(df), value = TRUE),new="DO (mg/L)"),ifelse(TRUE %in% str_detect(names(df),"DO \\(mg/L\\)"),NA,df<-add_column(df,`DO (mg/L)` = NA))) 
  ifelse(TRUE %in% str_detect(names(df),"\u00B0"),setnames(df,old=grep("\u00B0", names(df), value = TRUE),new="Temp (C°)"),ifelse(TRUE %in% str_detect(names(df),"Temp \\(C°\\)"),NA,df<-add_column(df,`Temp (C°)` = NA)))       #Degree symbol is specified using unicode designation
  ifelse(TRUE %in% str_detect(names(df),"SPC-uS/cm"),setnames(df,old=grep("SPC-uS/cm", names(df), value = TRUE),new="SpCond (µS/cm)"),ifelse(TRUE %in% str_detect(names(df),"SpCond \\(µS/cm\\)"),NA,df<-add_column(df,`SpCond (µS/cm)` = NA)))    
  ifelse(TRUE %in% str_detect(names(df),"pH-"),setnames(df,old=grep("pH-", names(df), value = TRUE),new="pH"),ifelse(TRUE %in% str_detect(names(df),"pH"),NA,df<-add_column(df,`pH` = NA)))
  ifelse(TRUE %in% str_detect(names(df),"FNU"),setnames(df,old=grep("FNU", names(df), value = TRUE),new="Turbidity (FNU)"),ifelse(TRUE %in% str_detect(names(df),"Turbidity \\(FNU\\)"),NA,df<-add_column(df,`Turbidity (FNU)` = NA)))   
  ifelse(TRUE %in% str_detect(names(df),"fDOM RFU"), setnames(df,old=grep("fDOM RFU", names(df), value = TRUE),new="fDOM (RFU)"),ifelse(TRUE %in% str_detect(names(df),"fDOM \\(RFU\\)"),NA,df<-add_column(df,`fDOM (RFU)` = NA)))    
  ifelse(TRUE %in% str_detect(names(df),"Chl RFU"),setnames(df,old=grep("Chl RFU", names(df), value = TRUE),new="TAL (RFU)"),ifelse(TRUE %in% str_detect(names(df),"TAL \\(RFU\\)"),NA,df<-add_column(df,`TAL (RFU)` = NA)))  
  ifelse(TRUE %in% str_detect(names(df),"Site Name"),setnames(df,old=grep("Site Name", names(df), value = TRUE),new="Site"),ifelse(TRUE %in% str_detect(names(df),"Site"),NA,df<-add_column(df,`Site` = NA)))  
  ifelse(TRUE %in% str_detect(names(df),"Unit"),setnames(df,old=grep("Unit", names(df), value = TRUE),new="Unit ID"),ifelse(TRUE %in% str_detect(names(df),"Unit ID"),NA,df<-add_column(df,`Unit ID` = NA)))  
  ifelse(TRUE %in% str_detect(names(df),"Date \\(MM/DD/YYYY\\)"),setnames(df,old=grep("Date \\(MM/DD/YYYY\\)", names(df), value = TRUE),new="Date"),ifelse(TRUE %in% str_detect(names(df),"Date"),NA,df<-add_column(df,`Date` = NA)))  
  
  ifelse(TRUE %in% str_detect(names(df),"Fract. Sec"),df<- select(df,-"Time (Fract. Sec)"),NA)   #Remove fraction of second time column. 
  ifelse(TRUE %in% str_detect(names(df),"Time"),setnames(df,old=grep("Time", names(df), value = TRUE),new="Time"),ifelse(TRUE %in% str_detect(names(df),"Time"),NA,df<-add_column(df,`Time` = NA)))  
  
  select(df,Event,Site,`Unit ID`,Date,Time,`DO (mg/L)`,`Temp (C°)`,`SpCond (µS/cm)`,pH,`Turbidity (FNU)`,`fDOM (RFU)`,`TAL (RFU)`)
}











#Import Post-Calibration data and for each parameter check if value is within post-Calibration range for top and bottom standards. If value is within both top and bottom bracket a "pass" is assigned in calibration column  
Calibrations <- read_excel("//ad/DFSroot/data/RSSI/PFlux/Data/EXO_Sonde_Data/QC/Event_Calibrations2.xlsx") %>%
  mutate(`Calibration`=.33) %>%
  mutate(`Calibration`=ifelse(Parameter == "SpCond_2000", if_else(Value %between% SpCond_2000,1,5,missing=.33),`Calibration`)) %>% #checks parameter is within bracket. if not assigns value of 1 for fail or 5 for pass or 0.33 for parameter not post-calibrated
  mutate(`Calibration`=ifelse(Parameter == "SpCond_200", if_else(Value %between% SpCond_200,1,5,missing=.33),`Calibration`))  %>%
  mutate(`Calibration`=ifelse(Parameter == "pH_4", if_else(Value %between% pH_4,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "pH_7", if_else(Value %between% pH_7,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "pH_10", if_else(Value %between% pH_10,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "Turbidity_0", if_else(Value %between% Turbidity_0,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "Turbidity_124", if_else(Value %between% Turbidity_124,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "FDOM_QSU_0", if_else(Value %between% FDOM_QSU_0,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "FDOM_QSU_300", if_else(Value %between% fDOM_QSU_300,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "FDOM_QSU_0", if_else(Value %between% FDOM_QSU_0,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "FDOM_RFU_100", if_else(Value %between% fDOM_RFU_100,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "Chlorophyll_ug_L_0", if_else(Value %between% Chlorophyll_ug_L_0,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "Chlorophyll_ug_L_66", if_else(Value %between% Chlorophyll_ug_L_66,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "Chlorophyll_RFU_0", if_else(Value %between% Chlorophyll_RFU_0,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "Chlorophyll_RFU_16.4", if_else(Value %between% Chlorophyll_RFU_16.4,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "BGA_ug_L_16", if_else(Value %between% BGA_ug_L_16,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "BGA_ug_L_0", if_else(Value %between% BGA_ug_L_0,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "BGA_RFU_16", if_else(Value %between% BGA_RFU_16,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "BGA_RFU_0", if_else(Value %between% BGA_RFU_0,1,5,missing=.33),`Calibration`)) %>%
  mutate(`Calibration`=ifelse(Parameter == "DO", if_else( Value >= True_Value-.3 &  Value <= True_Value+.3,3,5,missing=.33),`Calibration`)) %>%
  group_by(Event,Deployed_Station,`Parameter Group`) %>%
  summarise(Calibration=sum(as.numeric(Calibration),na.rm=TRUE)) %>%
  mutate(Calibration=ifelse(Calibration%%1 >0,"No Post-Cal Data Available",ifelse(Calibration %between% c(1.99,3.01),"Pass","Post-Cal Failed"))) %>% #Summarises by parameter type. "Pass" range 1.99-3.01. "Missing cal data" any number with remainder when divided by 1. else "Fail"
  mutate(`Calibration`=ifelse(str_detect(`Parameter Group`,"Temp"),"Factory Calibrated" ,`Calibration`)) %>% 
  rename(`Site Name` = Deployed_Station,Parameter=`Parameter Group`)
Calibrations$Parameter <- str_replace(Calibrations$Parameter, "FDOM RFU", "fDOM RFU")



#Function to Select relevant parameters
Parameter_select <-  function(df)
{
  df1 <- select(df,import_parameters)
  return(df1)  
}

#Bind Event data and selects parameters in parameter list
All_data <- Parameter_select(Event_1) %>%
  bind_rows(Parameter_select(Event_3)) %>%
  bind_rows(Parameter_select(Event_4)) %>%
  bind_rows(Parameter_select(Event_5)) %>%
  bind_rows(Parameter_select(Event_6)) %>%
  bind_rows(Parameter_select(Event_7)) %>%
  bind_rows(Parameter_select(Event_8)) %>%
  bind_rows(Parameter_select(Event_9))   

#Fixing site names, add column with cell names, clean date and time columns
All_data_clean <- All_data %>%
  mutate(`Site Name`=ifelse(`Site Name` == "Inflow", "ST2C3Inflow",`Site Name`)) %>%
  mutate(`Site Name`=ifelse(`Site Name` == "ST2C3InFlowCanal", "ST2C3Inflow",`Site Name`))  %>%
  mutate(`Site Name`=ifelse(`Site Name` == "STA2C1A3", "ST2C1A3",`Site Name`)) %>%
  mutate(`Site Name`=ifelse(`Site Name` == "STA2C1B2", "ST2C1B2",`Site Name`)) %>%
  mutate(`Site Name`=ifelse(`Site Name` == "STA2C1F3", "ST2C1F3",`Site Name`)) %>% 
  mutate(`Site Name`=ifelse(`Site Name` == "STA2C1G3", "ST2C1G3",`Site Name`)) %>% 
  mutate(`Site Name`=ifelse(`Site Name` == "STA2C1H2", "ST2C1H2",`Site Name`)) %>% 
  mutate(`Site Name`=ifelse(`Site Name` == "STA2C1Out", "ST2C1OUT",`Site Name`)) %>%
  mutate(Cell=ifelse(`Site Name` %in% `Cell 1`,"Cell 1",NA)) %>%
  mutate(Cell=ifelse(`Site Name` %in% `Cell 3`,"Cell 3",Cell)) %>%
  mutate(Cell=ifelse(`Site Name` %in% `Cell 3A`,"Cell 3A",Cell)) %>%
  mutate(Cell=ifelse(`Site Name` %in% `Cell 3B`,"Cell 3B",Cell)) %>%
  mutate(`Date (MM/DD/YYYY)`=ISOdatetime(year(`Date (MM/DD/YYYY)`),month(`Date (MM/DD/YYYY)`),day(`Date (MM/DD/YYYY)`),hour(`Time (HH:MM:SS)`),minute(`Time (HH:MM:SS)`),second(`Time (HH:MM:SS)`),tz="")) %>%
  mutate(`Time (HH:MM:SS)`=format(`Time (HH:MM:SS)`,format="%H:%M:%S")) %>%
  rename(Time=`Time (HH:MM:SS)`,`DateTime`=`Date (MM/DD/YYYY)`)

#Qualify Data from when the Sonde was out of the water. SpCond less than 100 ms/L
All_data_clean <- All_data_clean %>%
  mutate(`Remark Code`=ifelse(`SpCond ?S/cm`<  100,"Sonde Out of Water, ",NA)) 

#checks to see if value is within manufacturers range
All_sites_QC_remarked <- All_data_clean  %>%
  gather("Parameter", "Value", 4:19) %>%
  mutate(`Remark Code`=if_else(Parameter == "Temp ?C", if_else(Value %between% Temp_Range,`Remark Code`,paste(`Remark Code`,"Temp ?, "),"Temp Missing, "), `Remark Code`)) %>%
  mutate(`Remark Code`=if_else(Parameter == "Cond ?S/cm", if_else(Value %between% Cond_Range,`Remark Code`,paste(`Remark Code`,"Cond ?, "),"Cond Missing, "), `Remark Code`)) %>%
  mutate(`Remark Code`=if_else(Parameter == "SpCond ?S/cm", if_else(Value %between% SpCond_Range,`Remark Code`,paste(`Remark Code`,"SpCond ?, "),"SpCond Missing, "), `Remark Code`)) %>%  
  mutate(`Remark Code`=if_else(Parameter == "Sal psu",if_else(Value %between% Sal_Range,`Remark Code`,paste(`Remark Code`,"Salinity PSU ?, "),"Salinity Missing, "), `Remark Code`)) %>%
  mutate(`Remark Code`=if_else(Parameter == "TDS mg/L",if_else(Value %between% TDS_Range ,`Remark Code`,paste(`Remark Code`,"TDS ?, "),"TDS Missing, "), `Remark Code`)) %>%
  mutate(`Remark Code`=if_else(Parameter == "ODO % sat",if_else(Value %between% ODO_percent_Range ,`Remark Code`,paste(`Remark Code`,"ODO% ?, "),"ODO% Missing, "), `Remark Code`)) %>%
  mutate(`Remark Code`=if_else(Parameter == "ODO mg/L",if_else(Value %between% ODO_mg_Range  ,`Remark Code`,paste(`Remark Code`,"ODO mg/l ?, "),"ODO mg/l Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "pH",if_else(Value %between% pH_Range  ,`Remark Code`,paste(`Remark Code`,"pH ?, "),"pH Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "Turbidity FNU",if_else(Value %between% Turbidity_Range  ,`Remark Code`,paste(`Remark Code`,"Turbidity ?, "),"Turbidity Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "Turbidity NTU",if_else(Value %between% Turbidity_Range_NTU  ,`Remark Code`,paste(`Remark Code`,"Turbidity NTU ?, "),"Turbidity NTU Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "TSS mg/L",if_else(Value %between% TSS_Range  ,`Remark Code`,paste(`Remark Code`,"TSS ?, "),"TSS Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "fDOM QSU",if_else(Value %between% fDOM_QSU_Range  ,`Remark Code`,paste(`Remark Code`,"FDOM QSU ?, "),"FDOM QSU Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "fDOM RFU",if_else(Value %between% fDOM_QSU_Range  ,`Remark Code`,paste(`Remark Code`,"FDOM QSU ?, "),"FDOM QSU Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "Chlorophyll RFU",if_else(Value %between% Chlorophyll_RFU_Range  ,`Remark Code`,paste(`Remark Code`,"Chlorophyll RFU ?, "),"Chlorophyll RFU Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "Chlorophyll ?g/L",if_else(Value %between% Chlorophyll_ugl_Range  ,`Remark Code`,paste(`Remark Code`,"Chlorophyll ug/L ?, "),"Chlorophyll ug/L Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "BGA-PC RFU",if_else(Value %between% BGA_PC_RFU_Range  ,`Remark Code`,paste(`Remark Code`,"BGA_PC_RFU ?, "),"BGA_PC_RFU Missing, "), `Remark Code`))%>%
  mutate(`Remark Code`=if_else(Parameter == "BGA-PC ?g/L",if_else(Value %between% BGA_PC_ug_L_Range  ,`Remark Code`,paste(`Remark Code`,"BGA_PC_?g/L ?, "),"BGA_PC_ug/L Missing, "), `Remark Code`)) 

#Joins calibration data to deployed data
All_sites_QC_remarked_calibrations <- All_sites_QC_remarked %>%
  merge(Pre_Calibrations,by=c("Event","Parameter","Site Name"), all.x = TRUE) %>%
  merge(Calibrations,by=c("Event","Parameter","Site Name"), all.x = TRUE)

#Saves Data 
All_sites_combined_QC_list <-split(All_sites_QC_remarked_calibrations, (as.numeric(rownames(All_sites_QC_remarked_calibrations))-1) %/% 1000000)
write.csv(All_sites_combined_QC_list[1],"QC_DATA_1.csv",row.names=FALSE,na="")
write.csv(All_sites_combined_QC_list[2],"QC_DATA_2.csv",row.names=FALSE,na="")
write.csv(All_sites_combined_QC_list[3],"QC_DATA_3.csv",row.names=FALSE,na="")


# Analysis ----------------------------------------------------------------

#missing data
Missing_data <- All_sites_QC_remarked_calibrations %>%
  mutate(Exists=ifelse(is.na(Value),"Missing","Exists")) %>%
  group_by(date(DateTime),Parameter,Exists) %>%
  count()
#plot missing data by date
ggplot(Missing_data, aes(`date(DateTime)`,n,fill=Exists))+geom_col(position="stack")+scale_x_date(labels = date_format("%m/%y"),breaks = date_breaks("months"))+  theme(axis.text.x=element_text(angle=90,hjust=1))+facet_grid(Parameter~.)


#Breakdown of qualifiers
Remark_Code_Breakdown <- All_sites_QC_remarked_calibrations %>%
  group_by(`Remark Code`) %>%
  count() 

#Breakdown of post-cal
Post_Cal_Breakdown <- All_sites_QC_remarked_calibrations %>%
  group_by(Calibration) %>%
  count() 

#qualified data by date
Qualified_data <- All_sites_QC_remarked_calibrations %>%
  group_by(date(DateTime),`Remark Code`) %>%
  count() 
#plot qualifiers by date
ggplot(Qualified_data, aes(`date(DateTime)`,n,fill=`Remark Code`))+geom_col(position="stack")+scale_x_date(labels = date_format("%m/%y"),breaks = date_breaks("months"))+  theme(axis.text.x=element_text(angle=90,hjust=1)) 

#Post-cal data by date
Post_Cal_data <- All_sites_QC_remarked_calibrations %>%
  group_by(date(DateTime),Parameter,Calibration) %>%
  count() 
#plot qualifiers by date
ggplot(Qualified_data, aes(`date(DateTime)`,n,fill=`Remark Code`))+geom_col(position="stack")+scale_x_date(labels = date_format("%m/%y"),breaks = date_breaks("months"))+  theme(axis.text.x=element_text(angle=90,hjust=1)) 







#------------------------Code that was replaced or removed below this line-----------------------------

#checks to see if value is within manufacturers range.  gives remark code if it is out of range or missing
All_sites_combined_QC_remarked <- All_data3 %>%
  mutate(`Remark Code`="") %>%
  mutate(`Remark Code`=if_else(`Temp ?C` %between% Temp_Range,`Remark Code`,paste(`Remark Code`,"Temp OMR, "),"Temp Missing, ")) %>%
  mutate(`Remark Code`=if_else(`Cond ?S/cm` %between% Cond_Range,`Remark Code`,paste(`Remark Code`,"Cond OMR, "),"Cond Missing, ")) %>%
  mutate(`Remark Code`=if_else(`SpCond ?S/cm` %between% SpCond_Range,`Remark Code`,paste(`Remark Code`,"SpCond OMR, "),"SpCond Missing, ")) %>%  
  mutate(`Remark Code`=if_else(`Sal psu` %between% Sal_Range,`Remark Code`,paste(`Remark Code`,"Salinity PSU OMR, "),"Salinity Missing, ")) %>%
  mutate(`Remark Code`=if_else(`TDS mg/L` %between% TDS_Range ,`Remark Code`,paste(`Remark Code`,"TDS OMR, "),"TDS Missing, ")) %>%
  mutate(`Remark Code`=if_else(`ODO % sat` %between% ODO_percent_Range ,`Remark Code`,paste(`Remark Code`,"ODO% OMR, "),"ODO% Missing, "))%>%
  mutate(`Remark Code`=if_else(`ODO mg/L` %between% ODO_mg_Range  ,`Remark Code`,paste(`Remark Code`,"ODO mg/l OMR, "),"ODO mg/l Missing, "))%>%
  mutate(`Remark Code`=if_else(`pH` %between% pH_Range  ,`Remark Code`,paste(`Remark Code`,"pH OMR, "),"pH Missing, "))%>%
  mutate(`Remark Code`=if_else(`Turbidity FNU` %between% Turbidity_Range  ,`Remark Code`,paste(`Remark Code`,"Turbidity OMR, "),"Turbidity Missing, "))%>%
  mutate(`Remark Code`=if_else(`TSS mg/L` %between% TSS_Range  ,`Remark Code`,paste(`Remark Code`,"TSS OMR, "),"TSS Missing, "))%>%
  #mutate(`Remark Code`=if_else(`fDOM RFU` %between% fDOM_RFU_Range  ,`Remark Code`,paste(`Remark Code`,"FDOM RFU OMR, "),"FDOM RFU Missing, "))%>%
  mutate(`Remark Code`=if_else(`fDOM QSU` %between% fDOM_QSU_Range  ,`Remark Code`,paste(`Remark Code`,"FDOM QSU OMR, "),"FDOM QSU Missing, "))%>%
  mutate(`Remark Code`=if_else(`Chlorophyll RFU` %between% Chlorophyll_RFU_Range  ,`Remark Code`,paste(`Remark Code`,"Chlorophyll RFU OMR, "),"Chlorophyll RFU Missing, "))%>%
  mutate(`Remark Code`=if_else(`Chlorophyll ?g/L` %between% Chlorophyll_ugl_Range  ,`Remark Code`,paste(`Remark Code`,"Chlorophyll ug/L OMR, "),"Chlorophyll ug/L Missing, "))%>%
  mutate(`Remark Code`=if_else(`BGA-PC RFU` %between% BGA_PC_RFU_Range  ,`Remark Code`,paste(`Remark Code`,"BGA_PC_RFU OMR, "),"BGA_PC_RFU Missing, "))%>%
  mutate(`Remark Code`=if_else(`BGA-PC ?g/L` %between% BGA_PC_ug_L_Range  ,`Remark Code`,paste(`Remark Code`,"BGA_PC_?g/L OMR, "),"BGA_PC_ug/L Missing, "))

#checks to see if value is within manufacturers and historically measured range
All_sites_combined_QC_remarked <- All_data3 %>%
  mutate(`Remark Code`="") %>%
  mutate(`Remark Code`=if_else(`Temp ?C` %between% Temp_Range,`Remark Code`,paste(`Remark Code`,"Temp ?, "),"Temp Missing, ")) %>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `Temp ?C` %between% Temp_Range_Hist_C1 | `Cell` == 3 & `Temp ?C` %between% Temp_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"Temp !, "),"Temp Missing, ")) %>%  
  
  mutate(`Remark Code`=if_else(`Cond ?S/cm` %between% Cond_Range,`Remark Code`,paste(`Remark Code`,"Cond ?, "),"Cond Missing, ")) %>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `Cond ?S/cm` %between% Cond_Range_Hist_C1 | `Cell` == 3 & `Cond ?S/cm` %between% Cond_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"Cond !, "),"Cond Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`SpCond ?S/cm` %between% SpCond_Range,`Remark Code`,paste(`Remark Code`,"SpCond ?, "),"SpCond Missing, ")) %>%  
  mutate(`Remark Code`=if_else(`Cell` == 1 & `SpCond ?S/cm` %between% SpCond_Range_Hist_C1 | `Cell` == 3 & `SpCond ?S/cm` %between% SpCond_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"SpCond !, "),"SpCond Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`Sal psu` %between% Sal_Range,`Remark Code`,paste(`Remark Code`,"Salinity PSU ?, "),"Salinity Missing, ")) %>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `Sal psu` %between% Sal_Range_Hist_C1 | `Cell` == 3 & `Sal psu` %between% Sal_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"Salinity !, "),"Salinity Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`TDS mg/L` %between% TDS_Range ,`Remark Code`,paste(`Remark Code`,"TDS ?, "),"TDS Missing, ")) %>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `TDS mg/L` %between% TDS_Range_Hist_C1 | `Cell` == 3 & `TDS mg/L` %between% TDS_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"TDS !, "),"TDS Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`ODO % sat` %between% ODO_percent_Range ,`Remark Code`,paste(`Remark Code`,"ODO% ?, "),"ODO% Missing, ")) %>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `ODO % sat` %between% ODO_percent_Range_Hist_C1 | `Cell` == 3 & `ODO % sat` %between% ODO_percent_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"ODO% !, "),"ODO% Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`ODO mg/L` %between% ODO_mg_Range  ,`Remark Code`,paste(`Remark Code`,"ODO mg/l ?, "),"ODO mg/l Missing, "))%>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `ODO mg/L` %between% ODO_mg_Range_Hist_C1 | `Cell` == 3 & `ODO mg/L` %between% ODO_mg_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"ODO mg/l !, "),"ODO mg/l Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`pH` %between% pH_Range  ,`Remark Code`,paste(`Remark Code`,"pH ?, "),"pH Missing, "))%>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `pH` %between% pH_Range_Hist_C1 | `Cell` == 3 & `pH` %between% pH_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"pH !, "),"pH Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`Turbidity FNU` %between% Turbidity_Range  ,`Remark Code`,paste(`Remark Code`,"Turbidity ?, "),"Turbidity Missing, "))%>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `Turbidity FNU` %between% Turbidity_Range_Hist_C1 | `Cell` == 3 & `Turbidity FNU` %between% Turbidity_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"Turbidity !, "),"Turbidity Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`Turbidity NTU` %between% Turbidity_Range_NTU  ,`Remark Code`,paste(`Remark Code`,"Turbidity NTU ?, "),"Turbidity NTU Missing, "))%>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `Turbidity NTU` %between% Turbidity_Range_NTU_Hist_C1 | `Cell` == 3 & `Turbidity NTU` %between% Turbidity_Range_NTU_Hist_C3,`Remark Code`,paste(`Remark Code`,"Turbidity NTU !, "),"Turbidity NTU Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`TSS mg/L` %between% TSS_Range  ,`Remark Code`,paste(`Remark Code`,"TSS ?, "),"TSS Missing, "))%>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `TSS mg/L` %between% TSS_Range_Hist_C1 | `Cell` == 3 & `TSS mg/L` %between% TSS_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"TSS !, "),"TSS Missing, ")) %>% 
  
  #mutate(`Remark Code`=if_else(`fDOM RFU` %between% fDOM_RFU_Range  ,`Remark Code`,paste(`Remark Code`,"FDOM RFU ?, "),"FDOM RFU Missing, "))%>%
  
  mutate(`Remark Code`=if_else(`fDOM QSU` %between% fDOM_QSU_Range  ,`Remark Code`,paste(`Remark Code`,"FDOM QSU ?, "),"FDOM QSU Missing, "))%>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `fDOM QSU` %between% fDOM_QSU_Range_Hist_C1 | `Cell` == 3 & `fDOM QSU` %between% fDOM_QSU_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"fDOM QSU !, "),"fDOM QSU Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`Chlorophyll RFU` %between% Chlorophyll_RFU_Range  ,`Remark Code`,paste(`Remark Code`,"Chlorophyll RFU ?, "),"Chlorophyll RFU Missing, "))%>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `Chlorophyll RFU` %between% Chlorophyll_RFU_Range_Hist_C1 | `Cell` == 3 & `Chlorophyll RFU` %between% Chlorophyll_RFU_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"Chlorophyll RFU !, "),"Chlorophyll RFU Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`Chlorophyll ?g/L` %between% Chlorophyll_ugl_Range  ,`Remark Code`,paste(`Remark Code`,"Chlorophyll ug/L ?, "),"Chlorophyll ug/L Missing, "))%>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `Chlorophyll ?g/L` %between% Chlorophyll_ugl_Range_Hist_C1 | `Cell` == 3 & `Chlorophyll ?g/L` %between% Chlorophyll_ugl_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"Chlorophyll ugl !, "),"Chlorophyll ugl Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`BGA-PC RFU` %between% BGA_PC_RFU_Range  ,`Remark Code`,paste(`Remark Code`,"BGA_PC_RFU ?, "),"BGA_PC_RFU Missing, "))%>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `BGA-PC RFU` %between% BGA_PC_RFU_Range_Hist_C1 | `Cell` == 3 & `BGA-PC RFU` %between% BGA_PC_RFU_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"BGA_PC_RFU !, "),"BGA_PC_RFU Missing, ")) %>% 
  
  mutate(`Remark Code`=if_else(`BGA-PC ?g/L` %between% BGA_PC_ug_L_Range  ,`Remark Code`,paste(`Remark Code`,"BGA_PC_?g/L ?, "),"BGA_PC_ug/L Missing, ")) %>%
  mutate(`Remark Code`=if_else(`Cell` == 1 & `BGA-PC ?g/L` %between% BGA_PC_ug_L_Range_Hist_C1 | `Cell` == 3 & `BGA-PC ?g/L` %between% BGA_PC_ug_L_Range_Hist_C3,`Remark Code`,paste(`Remark Code`,"BGA_PC_ug/L !, "),"BGA_PC_ug/L Missing, ")) 


#------------------------Data Exploration Code Below this Line-------------------


#Removes values outside manufacturers range
All_sites_combined_QC_screened_OMR <- All_sites_combined_QC_remarked %>%
  gather("Parameter","Value",4:19) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="Temp ?C",if_else(Value %between% Temp_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="Cond ?S/cm",if_else(Value %between% Cond_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="SpCond ?S/cm",if_else(Value %between% SpCond_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="Sal psu",if_else(Value %between% Sal_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="TDS mg/L",if_else(Value %between% TDS_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="ODO % sat",if_else(Value %between% ODO_percent_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="ODO mg/L",if_else(Value %between% ODO_mg_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="pH",if_else(Value %between% Cond_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="SpCond ?S/cm",if_else(Value %between% SpCond_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="Turbidity FNU",if_else(Value %between% Turbidity_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="TSS mg/L",if_else(Value %between% TSS_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="fDOM QSU",if_else(Value %between% fDOM_QSU_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="Chlorophyll RFU",if_else(Value %between% Chlorophyll_RFU_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="Chlorophyll ?g/L",if_else(Value %between% Chlorophyll_ugl_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="BGA-PC RFU",if_else(Value %between% BGA_PC_RFU_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Value=if_else(Parameter=="BGA-PC ?g/L",if_else(Value %between% BGA_PC_ug_L_Range,Value,NULL),Value)) %>%
  filter(!is.na(Value))

#creates summary statistics table by parameter for outlier detection
All_sites_combined_QC_summary <- All_sites_combined_QC_screened_OMR %>%
  group_by_at(vars(Cell, Parameter)) %>%
  summarise(n=n(),max=max(Value),min=min(Value),mean=mean(Value),median=median(Value),var=format(var(Value),scientific=FALSE),sd=format(sd(Value),scientific=FALSE),
            IQR=IQR(Value),Q1=median(Value)-IQR(Value)/2,Q3=median(Value)+IQR(Value)/2,lower_fence_1.5x=(median(Value)-IQR(Value)/2)-(1.5*IQR(Value)),upper_fence_1.5x=median(Value)+IQR(Value)/2+1.5*IQR(Value),
            lower_fence_3x=median(Value)-IQR(Value)/2-3*IQR(Value),upper_fence_3x=median(Value)+IQR(Value)/2+3*IQR(Value))

#Historical data Defined Parameter Ranges in Cell 1 c(Lower,Upper)    
Temp_Range_Hist_C1 <-c(13.465,39.967)
Cond_Range_Hist_C1 <-c(0,1884.8)
SpCond_Range_Hist_C1 <-c(0,1847.55)
Sal_Range_Hist_C1 <-c(0,0.92)
TDS_Range_Hist_C1 <-c(0,1201)
ODO_percent_Range_Hist_C1 <-c(0,72.15) 
ODO_mg_Range_Hist_C1 <-c(0,5.705) 
pH_Range_Hist_C1 <-c(6.685,8.155)
Turbidity_Range_Hist_C1 <-c(0,15.355)
Turbidity_Range_NTU_Hist_C1 <-c(0,5.81)
TSS_Range_Hist_C1 <-c(0,0)
fDOM_RFU_Range_Hist_C1 <-c(0,100)
fDOM_QSU_Range_Hist_C1 <-c(0,419)
Chlorophyll_RFU_Range_Hist_C1 <-c(0,26.33)
Chlorophyll_ugl_Range_Hist_C1 <-c(0,74.805)
BGA_PC_RFU_Range_Hist_C1 <-c(0,1.835)
BGA_PC_ug_L_Range_Hist_C1 <-c(0,1.82)

#Historical data Defined Parameter Ranges in Cell 3 c(Lower,Upper)    
Temp_Range_Hist_C3 <-c(4.3635,46.1325)
Cond_Range_Hist_C3 <-c(92.2,1801.6)
SpCond_Range_Hist_C3 <-c(173.75,1697.65)
Sal_Range_Hist_C3 <-c(0.075,0.845)
TDS_Range_Hist_C3 <-c(114.5,1101.5)
ODO_percent_Range_Hist_C3 <-c(0,338.05) 
ODO_mg_Range_Hist_C3 <-c(0,27.73) 
pH_Range_Hist_C3 <-c(5.965,10.655)
Turbidity_Range_Hist_C3 <-c(0,22.98)
Turbidity_Range_NTU_Hist_C3 <-c(0,400)
TSS_Range_Hist_C3 <-c(0,0)
fDOM_RFU_Range_Hist_C3 <-c(0,100)
fDOM_QSU_Range_Hist_C3 <-c(0,528.435)
Chlorophyll_RFU_Range_Hist_C3 <-c(0,14.445)
Chlorophyll_ugl_Range_Hist_C3 <-c(0,57.57)
BGA_PC_RFU_Range_Hist_C3 <-c(0,2.26)
BGA_PC_ug_L_Range_Hist_C3 <-c(0,2.165)


