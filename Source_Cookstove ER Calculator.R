library(readxl)
library(dplyr)
library(tidyr)
library(devtools)
library(lubridate)
library(readr)


Ds1 <- read_excel("D:/Sync/2. Climate Change/Training/CDM Training#2_ER sheet_2.xlsx",
sheet = "CPA Distribution data")
Ds2 <- read_excel("D:/Sync/2. Climate Change/Training/CDM Training#2_ER sheet_2.xlsx",
                  sheet = "ER calculations")
View(Ds1)
View(Ds2)
Stove_Models<-unique(Ds1[3])

# Assumptions & Fixed Parameters
mr_st<-as.Date("2015-01-01")
mr_et<-as.Date("2016-12-31")
Q_biomass<-as.numeric(Ds2[5,2]) # Quantity of Biomass consumed annually by household in baseline (tons/yr)
f_NRB<-as.numeric(Ds2[7,2]) #Fraction of non-renewable biomass in the baseline woody biomass
NCV_biomass<-as.numeric(Ds2[8,2])
EF_bl<-as.numeric(Ds2[9,2])
Eff_bl<-as.numeric(Ds2[10,2])
LAF<-as.numeric(Ds2[12,2])
Eff_pp<-as.numeric(Ds2[16,2])

#Data Validation
print("Performing data Validation...")
CPA_check<-unique(Ds1[1])
l<-length(CPA_check)
#Return error if there are more than 1 CPA code reported
if (l>1) {print("There is inconsistency in reporting of CPA Code!")
          } else {print("No error in CPA code!")}  

SN_err<-anyDuplicated(Ds1[4]) #Duplicate entries in Serial Number
if (SN_err>1) {print("There are duplicate Serial Codes!")
              } else {print("No error in Serial Code!")}

Add_err<-anyDuplicated(Ds1[8]) #Duplicate entries in Address
if (Add_err>1) {print("There are duplicate Customer Addresses!")
} else {print("No error in Customer Addresses!")}

Mob_err<-anyDuplicated(Ds1[12]) #Duplicate entries in Mobile Numbers
if (Mob_err>1) {print("There are duplicate Mobile Numbers!")
} else {print("No error in Mobile Numbers!")}

#Calculating operating period of each cookstove
N_all<-as.numeric(table(Ds1[1]))
n<-500
st_mp<-rep(mr_st,n)
ed_mp<-rep(mr_et,n)
a<-ed_mp-st_mp
b<-ymd(z)-st_mp
e<-ed_mp-ymd(z)
i<-1
while(i<=500) {
    st_yr[i]<-if (ymd(z[i])<st_mp[i]) {a/365} else {e[i]/365}  
  print(st_yr[i])
  i<-i+1}

# Emission Reduction Calculation
#B_old = N_all*STove_Year*LAF*Q_biomass
#B_saving = B_old*(1-Eff_bl/Eff_pp)
#ER = B_savings*EF_bl*f_NRB*NCV_biomass

B_old<-N_all * as.numeric(mean(st_yr)) * LAF * Q_biomass
print(B_old)
B_savings<-B_old*(1-Eff_bl/Eff_pp)
print(B_savings)

ER<-B_savings*EF_bl*f_NRB*NCV_biomass
paste(ER,"tons CO2")