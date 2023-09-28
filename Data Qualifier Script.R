remove(list=ls()) #removes all objects from project

library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(xlsx) 
library(tidyr)
library(stringr)
library(rebus)
library(lubridate)
library(scales)
library(readr)


# Calibration Ranges --------------------------------------------------

#Manufacturer Defined Parameter Ranges c(Lower,Upper)    
Temp_Range <-c(-5,50)
Cond_Range <-c(0,200000)
SpCond_Range <-c(0,200000)
Sal_Range <-c(0,70)
TDS_Range <-c(0,100000)
ODO_percent_Range <-c(0,500) 
ODO_mg_Range <-c(0,50) 
pH_Range <-c(0,14)
Turbidity_Range <-c(0,4000)
Turbidity_Range_NTU <-c(0,4000)
TSS_Range<-c(0,1500)
fDOM_RFU_Range <-c(0,100)
fDOM_QSU_Range <-c(0,300)
Chlorophyll_RFU_Range <-c(0,60)
Chlorophyll_ugl_Range <-c(0,400)
BGA_PC_RFU_Range <-c(0,100)
BGA_PC_ug_L_Range <-c(0,100)
import_parameters <-c("Site Name","Date (MM/DD/YYYY)","Time (HH:MM:SS)","Temp ?C","Cond ?S/cm","SpCond ?S/cm","Sal psu","TDS mg/L",
                      "ODO % sat","ODO mg/L","pH","Turbidity NTU","Turbidity FNU","TSS mg/L","fDOM QSU","Chlorophyll ?g/L","Chlorophyll RFU","BGA-PC ?g/L","BGA-PC RFU","Event")

#Pre-calibration ranges for bracketing
pre_SpCond_2000 <-c(1980,2020)
pre_Cond_2000 <- c(1980,2020)
pre_SpCond_200 <-c(198,202)
pre_do <- c(99, 101)
pre_pH_4 <-c(3.9,4.1)
pre_pH_7 <-c(6.9,7.1)
pre_pH_10 <-c(9.9,10.1)
pre_Turbidity_0 <-c(0,1.24)
pre_Turbidity_124<-c(122.8,125.2)
pre_TSS_0<-c(0,2)
pre_TSS_1500<-c(1350,1650)
pre_TDS_0 <-c(0,2)
pre_TDS_100000<-c(0,100000)
pre_FDOM_QSU_0 <-c(0,3)
pre_fDOM_QSU_300 <-c(297,303)
pre_FDOM_RFU_0 <-c(0,1)
pre_fDOM_RFU_100 <-c(99,101)
pre_Chlorophyll_ug_L_0 <-c(0,0.6)
pre_Chlorophyll_ug_L_66 <-c(65.34,66.66)
pre_Chlorophyll_RFU_0 <-c(0,0.16)
pre_Chlorophyll_RFU_16.4 <-c(16.24,16.56)
pre_BGA_ug_L_0 <-c(0,0.16)
pre_BGA_ug_L_16 <-c(15.84,16.16)
pre_BGA_RFU_0 <-c(0,0.16)
pre_BGA_RFU_16 <-c(15.84,16.16)

#Calibration ranges for bracketing
SpCond_2000 <-c(1900,2100)
SpCond_200 <-c(190,210)
pH_4 <-c(3.7,4.3)
pH_7 <-c(6.7,7.3)
pH_10 <-c(9.7,10.3)
Turbidity_0 <-c(0,2)
Turbidity_124<-c(111.6,136.4)
TSS_0<-c(0,2)
TSS_1500<-c(1350,1650)
TDS_0 <-c(0,2)
TDS_100000<-c(0,100000)
FDOM_QSU_0 <-c(0,2)
fDOM_QSU_300 <-c(270,330)
FDOM_RFU_0 <-c(0,2)
fDOM_RFU_100 <-c(90,110)
Chlorophyll_ug_L_0 <-c(0,2)
Chlorophyll_ug_L_66 <-c(59.4,72.6)
Chlorophyll_RFU_0 <-c(0,2)
Chlorophyll_RFU_16.4 <-c(14.76,18.04)
BGA_ug_L_0 <-c(0,2)
BGA_ug_L_16 <-c(14.4,17.6)
BGA_RFU_0 <-c(0,2)
BGA_RFU_16 <-c(14.4,17.6)

#Stations in Cells
`Cell 3`<- c("ST2C3C20","ST2C3C56","ST2C3C92","ST2C3C128","ST2C3C164","ST2C3C200","ST2C3Inflow")
`Cell 1`<- c("ST2C1A3","ST2C1B2","ST2C1F3","ST2C1G3","ST2C1H2","ST2C1Out","ST2C1C3","ST2C1H3","ST2C1OUT")
`Cell 3B`<- c("ST34C3BB7","ST34C3BD7")
`Cell 3A`<- c("ST34C3AA56","ST34C3AA20")




# Import Deployment Data -------------------------------------------------------------
#Deployment 1
Bare_Sonde <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Bare_ECOTOPE_060921.csv") %>% mutate (Event="Event 1") #deployment 1
Chara_Sonde <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Chara_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1
Cattail_Sonde <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Cattail_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1
Southern_N_Sonde <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Southern_N_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1
Mixed_Sonde <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/EXOdata_Mixed_ECOTOPE_060921.csv") %>% mutate (Event="Event 1")  #deployment 1

#Deployment 2
Bare_Sonde_082621 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/pdynSTA34A41_ecotopeBare - 082621 174830.csv") %>% mutate (Event="Event 2")  #file contains data from PDYNAMICS site and ECOTOPE site
Cattail_Sonde_082621 <- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Cattail - 083121 133214.csv") %>% mutate (Event="Event 2")  #file contains data from PDYNAMICS site and ECOTOPE site
Chara_Sonde_082621 <-  read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Chara - 083121 133903.csv") %>% mutate (Event="Event 2") #file contains data from PDYNAMICS site and ECOTOPE site
Mixed_Sonde_082621 <-  read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Mixed- 083121 134557.csv") %>% mutate (Event="Event 2")  #file contains data from PDYNAMICS site and ECOTOPE site
Naiad_Sonde_082621<- read_csv("C:/Users/citiz/OneDrive - South Florida Water Management District/WQTT/ECOTOPE/Data/Sonde/Southern Naiad- 083121 135134.csv") %>% mutate (Event="Event 2") #file contains data from PDYNAMICS site and ECOTOPE site

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

#Import Sonde Data
Event_1 <- cbind(read_excel("//ad/DFSroot/data/RSSI/PFlux/Data/EXO_Sonde_Data/QC/2015 April to November PFLUX Event 1.xlsx"),Event=1)
Event_3 <- cbind(read_excel("//ad/DFSroot/data/RSSI/PFlux/Data/EXO_Sonde_Data/QC/2016 February to April PFLUX Event 3.xlsx"),Event=3)
Event_4 <- cbind(read_excel("//ad/DFSroot/data/RSSI/PFlux/Data/EXO_Sonde_Data/QC/2016 June to September PFLUX Event 4.xlsx"),Event=4)
Event_5 <- cbind(read_excel("//ad/DFSroot/data/RSSI/PFlux/Data/EXO_Sonde_Data/QC/2016 October to November PFLUX Event 5.xlsx"),Event=5)
Event_6 <- cbind(read_excel("//ad/DFSroot/data/RSSI/PFlux/Data/EXO_Sonde_Data/QC/2017 January to February PFLUX Event 6.xlsx"),Event=6)
Event_7 <- cbind(read_excel("//ad/DFSroot/data/RSSI/PFlux/Data/EXO_Sonde_Data/QC/2017 May to August PFLUX Event 7.xlsx"),Event=7)
Event_8 <- cbind(read_excel("//ad/DFSroot/data/RSSI/PFlux/Data/EXO_Sonde_Data/QC/2017 November to December PFLUX Event 8.xlsx"),Event=8)
Event_9 <- cbind(read_excel("//ad/DFSroot/data/RSSI/PFlux/Data/EXO_Sonde_Data/QC/2018 June to Spetember PFLUX Event 9.xlsx"),Event=9)

#Function to Select relevent parameters
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


