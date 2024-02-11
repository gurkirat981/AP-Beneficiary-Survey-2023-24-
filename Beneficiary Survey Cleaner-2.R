library(data.table)
library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)
library(MASS)
library(kableExtra)
library(foreign)
library (dplyr)

setwd("/Users/gurkirat/Desktop/Flags Drp")

#Main Files ####
AP_Beneficiary_Survey <- read_excel("/Users/gurkirat/Desktop/Flags Drp/Beneficiary_Survey_Aug1.xlsx")
AP_Beneficiary_Roster <- read_excel("/Users/gurkirat/Desktop/Flags Drp/Beneficiary_Survey_Aug1.xlsx", sheet = 2)
AP_Beneficiary_Survey_28April <- read_excel("/Users/gurkirat/Desktop/Flags Drp/Beneficiary_Survey_28thApril.xlsx")

AP_Beneficiary_Survey_28April <- AP_Beneficiary_Survey_28April %>% 
  filter (start > '2023-05-17') %>% 
  mutate ("If you have never been a beneficiary, then why not?" = NA, .after = 96) %>% 
  mutate ("Other, please specify...98" = NA, .after = 97) %>% 
  mutate ("If not eligible, what was the reason?" = NA, .after = 122) %>% 
  mutate ("Other, please specify...124" = NA, .after = 123) %>% 
  mutate ("If not eligible previously, what was the reason?" = NA, .after = 126) %>% 
  mutate ("Other, please specify...128" = NA, .after = 127)

#Codebooks ####
AP_Survey_CB <- read_excel("AP_Beneficiary_Survey_Codebook.xlsx")
colnames(AP_Beneficiary_Survey) <- AP_Survey_CB$Col_Name
attr(AP_Beneficiary_Survey, "variable.labels") <- AP_Survey_CB$Col_Description

colnames(AP_Beneficiary_Survey_28April) <- AP_Survey_CB$Col_Name
attr(AP_Beneficiary_Survey_28April, "variable.labels") <- AP_Survey_CB$Col_Description

AP_Roster_CB <- read_excel("AP_Beneficiary_Survey_Codebook.xlsx", sheet = 2)
colnames(AP_Beneficiary_Roster) <- AP_Roster_CB$Col_Name
attr(AP_Beneficiary_Roster, "variable.labels") <- AP_Roster_CB$Col_Description

AP_Beneficiary_Survey <- AP_Beneficiary_Survey %>% 
  add_row(AP_Beneficiary_Survey_28April)

AP_Beneficiary_Survey$Secretariat_Name <- toupper(AP_Beneficiary_Survey$Secretariat_Name); 
AP_Beneficiary_Survey$Secretariat_Name <- sub(" ", "", AP_Beneficiary_Survey$Secretariat_Name);
AP_Beneficiary_Survey$Secretariat_Name <- sub("-", "", AP_Beneficiary_Survey$Secretariat_Name)

#Read in the All Secretariat File 
All_Secretariats_File <- read.csv("allsecretariats_update.csv")

All_Secretariats_File$SECRETARIAT_NAME <- sub(" ", "", All_Secretariats_File$SECRETARIAT_NAME)
All_Secretariats_File$SECRETARIAT_NAME <- sub("-", "", All_Secretariats_File$SECRETARIAT_NAME)


a <- as.data.table(table(AP_Beneficiary_Survey$Secretariat_Name, AP_Beneficiary_Survey$Secretariat_Code))
a <- a[a$N > 0,]; a$V2 <- as.numeric(a$V2)

a$Check_No <- a$Correct_Name <-  a$District <- rep(NA, nrow(a))

for (i in 1:nrow(a)) {

    a$Check_No[i] = ifelse(a$V2[i] %in% All_Secretariats_File$SECRETARIAT_CODE, 1, 0)
  
  if(a$Check_No[i] == 1) {
    
    a$Correct_Name[i] <- All_Secretariats_File$SECRETARIAT_NAME[All_Secretariats_File$SECRETARIAT_CODE == a$V2[i]]
    a$District[i] <- All_Secretariats_File$NEW_DISTRICT_1[All_Secretariats_File$SECRETARIAT_CODE == a$V2[i]]
    
    
  } 
  
}

a$Perf_Match = ifelse(a$Check_No == 1 & a$V1 == a$Correct_Name, 1, 0)
a

Imperfect_Matches <- a[Perf_Match == 0 & Check_No == 1,]


Secretariat_Name <- AP_Beneficiary_Survey$Secretariat_Name
Secretariat_Code <- as.numeric(AP_Beneficiary_Survey$Secretariat_Code)

for (i in 1:nrow(AP_Beneficiary_Survey)) {

    if(Secretariat_Name[i] %in% Imperfect_Matches$V1 & Secretariat_Code[i] %in% Imperfect_Matches$V2) {
    
    Secretariat_Name[i] <- unique(Imperfect_Matches$Correct_Name[Imperfect_Matches$V2 == Secretariat_Code[i]])
    
    } 
  
}

AP_Beneficiary_Survey$Secretariat_Name_Rec <- Secretariat_Name



#### Insert code to correct remaining 6 secretariats here
Fuzzy_Matches <- a[Check_No == 0,]

Fin_Names <- Fin_Numbers <- rep(NA, nrow(Fuzzy_Matches))

#Manual correction when distance is too high

for (i in 1:nrow(Fuzzy_Matches)) {

  Names <- All_Secretariats_File$SECRETARIAT_NAME[agrepl(Fuzzy_Matches$V1[i], All_Secretariats_File$SECRETARIAT_NAME, max.distance = 3)]
  
  Dist <- adist(Fuzzy_Matches$V1[i], Names)

  
  if (length(Names) > 0){
  Fin_Names[i] <- Names[which.min(Dist)]
  Fin_Numbers[i] <- All_Secretariats_File$SECRETARIAT_CODE[All_Secretariats_File$SECRETARIAT_NAME == Fin_Names[i]]
  }
  
}

Fuzzy_Matches$Fin_Names <- Fin_Names; Fuzzy_Matches$Fin_Numbers <- Fin_Numbers


Secretariat_Name <- AP_Beneficiary_Survey$Secretariat_Name_Rec
Secretariat_Code <- as.numeric(AP_Beneficiary_Survey$Secretariat_Code)

Fin_Names[Fuzzy_Matches$V1 == "PADINARAYANA"] <- "B.AGRAHARAM"; Fin_Numbers[Fuzzy_Matches$V1 == "PADINARAYANA"] <- 11390574
Fin_Names[Fuzzy_Matches$V1 == 11090946] <- "YARRAIAHGARIPALLE"; Fin_Numbers[Fuzzy_Matches$V1 == "11090946"] <- 11090946


for (i in 1:nrow(AP_Beneficiary_Survey)) {
  
  if(Secretariat_Name[i] %in% Fuzzy_Matches$V1 & Secretariat_Code[i] %in% Fuzzy_Matches$V2) {
    
    Secretariat_Name[i] <- unique(Fuzzy_Matches$Fin_Names[Fuzzy_Matches$V2 == Secretariat_Code[i]])
    Secretariat_Code[i] <- unique(Fuzzy_Matches$Fin_Numbers[Fuzzy_Matches$Fin_Names == Secretariat_Name[i]])
    
  } 
  
}

AP_Beneficiary_Survey$Secretariat_Code <- Secretariat_Code; AP_Beneficiary_Survey$Secretariat_Name_Rec <- Secretariat_Name

#Run the check again to see whether the errors have beeen fixed
d <- as.data.table(table(AP_Beneficiary_Survey$Secretariat_Name_Rec, AP_Beneficiary_Survey$Secretariat_Code))
d <- d[d$N > 0,]; d$V2 <- as.numeric(d$V2)

Correct_Name <-  District <- rep(NA, nrow(d))

for (i in 1:nrow(d)) {
  
  d$Check_No[i] = ifelse(d$V2[i] %in% All_Secretariats_File$SECRETARIAT_CODE, 1, 0)
  
  if(d$Check_No[i] == 1) {
    
    Correct_Name[i] <- All_Secretariats_File$SECRETARIAT_NAME[All_Secretariats_File$SECRETARIAT_CODE == d$V2[i]]
    District[i] <- All_Secretariats_File$NEW_DISTRICT_1[All_Secretariats_File$SECRETARIAT_CODE == d$V2[i]]
    
    
  } 
  
}

d$Correct_Name <- Correct_Name; d$District <- District

d$Perf_Match = ifelse(d$Check_No == 1 & d$V1 == d$Correct_Name, 1, 0)

Imperfect_Matches <- d[Perf_Match == 0 & Check_No == 1,]

#Completed Secretariats Marked
#For now using criteria of 17+ surveys

Secretariat_Completed <- d$V2[d$N >= 17]

for (i in 1:nrow(All_Secretariats_File)) {
  
  All_Secretariats_File$Completed[i] <- ifelse(All_Secretariats_File$SECRETARIAT_CODE[i] %in% Secretariat_Completed, 1, 
                                               All_Secretariats_File$Completed[i])
  
}

table(All_Secretariats_File$Completed)

write_xlsx(All_Secretariats_File, paste("All_Secretariats_Update_", Sys.Date(),"_.xlsx"))

AB <- All_Secretariats_File %>% 
filter (NEW_DISTRICT == "ANANTHAPURAMU" | NEW_DISTRICT == "ANNAMAYYA" | NEW_DISTRICT == "CHITTOOR" | NEW_DISTRICT == "KURNOOL"
        | NEW_DISTRICT == "NANDYAL" | NEW_DISTRICT == "SRI POTTI SRIRAMULU NELLORE" | NEW_DISTRICT == "SRI SATHYA SAI" | NEW_DISTRICT == "TIRUPATI" 
        | NEW_DISTRICT == "YSR")



write_xlsx(secretariats_district, "Secretariats Completed by districts.xlsx")

ay <- as.data.frame(table(AB$NEW_DISTRICT)) %>% 
rename ("Districts" = Var1)

az <- as.data.frame(table(AB$NEW_DISTRICT[AB$Completed == 1])) %>% 
rename ("D" = Var1)

af <- cbind (ay,az) 

write_xlsx(af, "Secretariats Completed by districts.xlsx")

secretariat_progress <-  read_excel("/Users/gurkirat/Desktop/Flags Drp/secretariats sampled and completed.xlsx")
  
