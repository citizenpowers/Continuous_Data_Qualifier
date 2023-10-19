#The goal of this script is to qualify deployed sonde data based on whether the sensor passed post-calibration and measurements are within sensor range.

library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(lubridate)
library(stringr)
library(lubridate)
library(tidyverse)



# Step 1.) Run helper function that assists in cleaning column headers -------------------------------------------

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
  ifelse(TRUE %in% str_detect(names(df),"Date \\(MM\\/DD\\/YYYY\\)"),df<-rename(df,Date="Date (MM/DD/YYYY)"),ifelse(TRUE %in% str_detect(names(df),"Date"),NA,df<-add_column(df,`Date` = NA)))  
  
  ifelse(TRUE %in% str_detect(names(df),"Fract. Sec"),df<- select(df,-"Time (Fract. Sec)"),NA)   #Remove fraction of second time column. 
  ifelse(TRUE %in% str_detect(names(df),"Time"),setnames(df,old=grep("Time", names(df), value = TRUE),new="Time"),ifelse(TRUE %in% str_detect(names(df),"Time"),NA,df<-add_column(df,`Time` = NA)))  
  
  select(df,Event,Site,`Unit ID`,Date,Time,`DO (mg/L)`,`Temp (C°)`,`SpCond (µS/cm)`,pH,`Turbidity (FNU)`,`fDOM (RFU)`,`TAL (RFU)`)
}


# Step 2.) Import Calibration limits and sensor range provided from manufacturer --------------------------------------------------
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


# Step 3.) Import Deployment Data -------------------------------------------------------------
#Deployment 1
Bare_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Bare_ECOTOPE_060921.csv") %>% mutate (Event="Event 1") #deployment 1
Chara_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Chara_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1
Typha_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Cattail_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1
Southern_N_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Southern_N_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1
Mixed_60921 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Mixed_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1

#Deployment 2
Bare_082621 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/pdynSTA34A41_ecotopeBare - 082621 174830.csv") %>% mutate (Event="Event 2") %>%
slice(1993:2325) %>%  mutate(`Site Name`="STA3/4C2B_Ecotope_Bare")             #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1991 and collected at 2325

Cattail_082621 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Cattail - 083121 133214.csv") %>% mutate (Event="Event 2")  %>% mutate(`Site Name`="Typha") %>% slice(3875:4847)#file contains data from PDYNAMICS site and ECOTOPE site

Chara_082621 <-  read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Chara - 083121 133903.csv") %>% mutate (Event="Event 2") %>% slice(1994:2328) %>% #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1994 and collected at 2328
mutate(`Site Name`="STA3/4C2B_Ecotope_Chara") #file contains data from PDYNAMICS site and ECOTOPE site

Mixed_082621 <-  read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Mixed- 083121 134557.csv") %>% mutate (Event="Event 2") %>% #file contains data from PDYNAMICS site and ECOTOPE site
slice(1995:2326) %>%  mutate(`Site Name`="STA3/4C2B_Ecotope_Mixed") #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1995 and collected at 2326

Naiad_082621<- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Southern Naiad- 083121 135134.csv") %>% mutate (Event="Event 2") %>% #file contains data from PDYNAMICS site and ECOTOPE site
slice(1992:2325) %>%  mutate(`Site Name`="STA3/4C2B_Ecotope_Naiad") #sonde was switched from PDYNAMICS site to ECOTOPE site at  row 1992 and collected at 1995
 
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



# Step 4.) Import Post-Calibration Data --------------------------------------------

Post_Cal_20210630 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20210630 Post-Cal.xlsx") %>% mutate (Event="Event 1")
Post_Cal_20210826 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20210826 Post-Cal.xlsx") %>% mutate (Event="Event 2")
Post_Cal_20211110 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20211110 Post-Cal.xlsx") %>% mutate (Event="Event 3")
#event 4 missing?
Post_Cal_20220503 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20220503 Post-Cal.xlsx") %>% mutate (Event="Event 5")
Post_Cal_20230524 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20230524 Post-Cal.xlsx") %>% mutate (Event="Event 6")
Post_Cal_20230606 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20230606 Post-Cal.xlsx") %>% mutate (Event="Event 7")
Post_Cal_20230823 <- read_excel("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Calibrations/20230823 Post-Cal.xlsx") %>% mutate (Event="Event 8")


# Step 5.) Tidy Deployment Data ----------------------------------------------------

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

# Step 6.) Tidy Post Cal Data ------------------------------------------------------

#Checks to see if post-cal passed single calibration points
Post_Cal_Data <- bind_rows(Post_Cal_20210630,Post_Cal_20210826,Post_Cal_20211110,Post_Cal_20220503,Post_Cal_20230524,Post_Cal_20230606,Post_Cal_20230823) %>%    
pivot_longer(names_to="Parameter",values_to="Value",cols=c("SpCond 200", "SpCond 2000","DO", "pH 7", "pH 10", "FDOM RFU", "Turbidity", "BGA RFU","pH 4")) %>% 
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

write.csv(Post_Cal_Data,"C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Post_Cal_Data_tidy.csv",row.names=FALSE)  #Save combined data without any qualifiers


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

write.csv(Post_Cal_Summarized,"C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Post_Cal_Summarized.csv",row.names=FALSE)  #Save combined data without any qualifiers


# Step 7.) Joins deployed data to post-cal data ----------------------------------------------------------------


Deployment_Data_Post_cal <- Deployment_Data %>%
pivot_longer(names_to = "Parameter",values_to ="Value",cols = c("DO (mg/L)","Temp (C°)", "SpCond (µS/cm)","pH","Turbidity (FNU)", "fDOM (RFU)") )  %>%
left_join(Post_Cal_Summarized,by=c("Event","Sonde","Parameter"))




# Step 8.) Checks to see if deployed data is within manufacturers range ------------

Deployment_Data_Post_cal_manufacurer_check <- Deployment_Data_Post_cal %>%
mutate(`Manufacturer Range`=case_when(Parameter=="Temp (C°)" & Value %between% Temp_Range =="FALSE"~ "Value outside sensor range.",
                                      Parameter=="DO (mg/L)" & Value %between% ODO_mg_Range =="FALSE"~ "Value outside sensor range.",  
                                      Parameter=="SpCond (µS/cm)" & Value %between% SpCond_Range =="FALSE"~ "Value outside sensor range.",
                                      Parameter=="pH" & Value %between% pH_Range =="FALSE"~ "Value outside sensor range.",
                                      Parameter=="Turbidity (FNU)" & Value %between% Turbidity_Range =="FALSE"~ "Value outside sensor range.",
                                      Parameter=="fDOM (RFU)" & Value %between% fDOM_RFU_Range =="FALSE"~ "Value outside sensor range.", 
                                     .default = as.character(""))) 



# Step 9.) Add qualifier code and remark notes with reason for qualification -------


Deployment_Data_Qualified  <- Deployment_Data_Post_cal_manufacurer_check %>%
mutate(`Remark Code`="",`Remark Note`="") %>%
mutate(`Remark Code`=case_when(`Manufacturer Range`=="Value outside sensor range."~"?",
                               str_detect(`Post-Calibration`,"Failed") | str_detect(`Post-Calibration`,"Missing") ~"J",
                               `Parameter` %in% c("fDOM (RFU)","Turbidity (FNU)") ~"H",
                               .default = `Remark Code`)) %>%
mutate(`Remark Note`=ifelse(`Remark Code` =="H","Method has not been recognized by the FDEP as equivalent to laboratory methods",`Remark Note`)) %>% 
mutate(`Remark Note`=trimws(ifelse(`Remark Code` %in% c("H","J","?"),paste(`Remark Note`,`Manufacturer Range`,`Post-Calibration`),`Remark Note`))) 
  
                                
write.csv(Deployment_Data_Qualified,"C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Ecotope_deployed_sonde_data_qualified.csv",row.names=FALSE)  #Save combined data with qualifiers





