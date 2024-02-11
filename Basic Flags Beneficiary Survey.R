library(data.table)
library(readxl)
library(writexl)
library(dplyr)
library(MASS)
library(kableExtra)
library(foreign)
library (dplyr)
library(tidyr)
library(data.table)
library(broom)
library(readxl)
library(writexl)
library(haven)
library(readr)
library(dplyr)
library(cowplot)
library(tidyverse)
library(ggplot2)
library(MASS)
library(ggrepel)
library(RColorBrewer)
library(stargazer)
library(DeclareDesign)
library(texreg)
library(car)
library(lubridate)
library(scales)
library(cowplot)
library(writexl)
library(janitor)
library(gridExtra)
library(grid)
library(ggpubr)
library(kableExtra)
library(data.table)
library(foreign)
library(readxl)
library(knitr)
library(tinytex)
library(stringr)

setwd("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp")

#Main Files ####
AP_Beneficiary_Survey <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/Beneficiary_Survey_Jan25.xlsx")
AP_Beneficiary_Roster <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/Beneficiary_Survey_Jan25.xlsx", sheet = 2)
AP_Beneficiary_Survey_28April <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/Beneficiary_Survey_28thApril.xlsx")
enum_names <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/Enumerator Details (Beneficiary Survey Phase II).xlsx")

AP_Beneficiary_Survey_28April <- AP_Beneficiary_Survey_28April %>% 
  filter (start > '2023-05-17') %>% 
  mutate ("If you have never been a beneficiary, then why not?" = NA, .after = 96) %>% 
  mutate ("Other, please specify...98" = NA, .after = 97) %>% 
  mutate ("If not eligible, what was the reason?" = NA, .after = 122) %>% 
  mutate ("Other, please specify...124" = NA, .after = 123) %>% 
  mutate ("If not eligible previously, what was the reason?" = NA, .after = 126) %>% 
  mutate ("Other, please specify...128" = NA, .after = 127)

xx <- as.data.frame(colnames(AP_Beneficiary_Roster))
yy <- read_excel("/Users/gurkirat/Desktop/Flags Drp/AP_Beneficiary_Survey_Codebook.xlsx")
write.csv (xx, "/Users/gurkirat/Desktop/Flags Drp/colnames.csv")

#Codebooks ####
AP_Survey_CB <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/AP_Beneficiary_Survey_Codebook.xlsx")
colnames(AP_Beneficiary_Survey) <- AP_Survey_CB$Col_Name
attr(AP_Beneficiary_Survey, "variable.labels") <- AP_Survey_CB$Col_Description

colnames(AP_Beneficiary_Survey_28April) <- AP_Survey_CB$Col_Name
attr(AP_Beneficiary_Survey_28April, "variable.labels") <- AP_Survey_CB$Col_Description

AP_Roster_CB <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/AP_Beneficiary_Survey_Codebook.xlsx", sheet = 2)
colnames(AP_Beneficiary_Roster) <- AP_Roster_CB$Col_Name
attr(AP_Beneficiary_Roster, "variable.labels") <- AP_Roster_CB$Col_Description

AP_Beneficiary_Survey <- AP_Beneficiary_Survey %>% 
  add_row(AP_Beneficiary_Survey_28April)

##Fixing District Names
AP_Beneficiary_Survey <- AP_Beneficiary_Survey %>% 
mutate (District_Name = ifelse(District_Name == "Ananthapur" | District_Name == "Ananthapuram" | District_Name == "Ananthapuramu" | District_Name == "Anantaouramu" | 
District_Name == "Ananthapuam" | District_Name == "Anantapuramu" | District_Name == "Ananthanpuram" | District_Name == "Anathapuram"|
District_Name == "Abanthapuram"|District_Name == "Anantapuram"|District_Name == "Ananthaouram"|District_Name == "Anatapuram"|
District_Name == "ananthapuram"| District_Name == "Anathapuaram"| District_Name == "అనంతపురం", "Anantapur",
ifelse(District_Name == "Chittore" | District_Name == "Chittoor in" | District_Name == "CHITTOOR" | District_Name == "Chowdepalle" | District_Name == "Chittoor" |
District_Name == "Chittor" | District_Name == "చిత్తూరు" | District_Name == "CHITTOR" | District_Name == "Chittot", "Chittoor",
ifelse(District_Name == "KURNOOL" | District_Name == "Kurnol" | District_Name == "P Adinarayana" | District_Name == "Koduy" | District_Name == "Kurnool C-BELAGAL", "Kurnool",
ifelse(District_Name == "Nadyal" | District_Name == "Nandual" | District_Name == "Nandyala" | District_Name == "Nandayal" |
District_Name == "Nanayal" | District_Name == "Nsndyal" | District_Name == "Nandal" | District_Name == "Andayal" | District_Name == "\nNandyal", "Nandyal",
ifelse(District_Name == "Ysr kadapa" |District_Name == "Ysr kadapa" | District_Name == "Yar" | District_Name == "Ysr kadapa" | District_Name == "YSR"| 
District_Name == "Kadapa" | District_Name == "YDR Kadapa" | District_Name == "Ysr" , "YSR Kadapa",
ifelse(District_Name == "అన్నమయ్య" | District_Name == "Annamyya", "Annamayya",
ifelse(District_Name == "Thirupathi"|District_Name == "Tirupathi" | District_Name == "Tirupati rural" | District_Name == "Turupathi", "Tirupati",
ifelse(District_Name == "Nelloor" |District_Name == "Nelloore" |District_Name == "Ravi Kumar" |District_Name == "S P S Nellore" |
District_Name == "S.P.S.Nellire" |District_Name == "S.P.S.Nellore" |District_Name == "S.P.S.R.Nellore" |District_Name == "SPS NELLORE" |
District_Name == "Spsr nelloor" |District_Name == "Spsr nelloore" | District_Name == "SPSR NELLORR" | District_Name == "Spsr nellore" |
District_Name == "SPSR Nellore" |District_Name == "SPSR NELLzoRE" |District_Name == "SPSR.Nellore" | District_Name == "SPSRNellore" |
  District_Name == "S PSR NELLORE" |District_Name == "SPSR  NELLORE" |District_Name == "Spsr 5" | District_Name == "Sri porti sriramulu nellore" |
  District_Name == "Sri potti sriramulu" |  District_Name == "Sri pottisriramulu nellore" | District_Name == "Nelloorr" |District_Name == "S P S R NELLORE" |District_Name == "SpSR Nellore" |
District_Name == "SPSR NELLORE" |District_Name == "SPSRNELLORE" |District_Name == "Sri potti sruramulu nellore" |
District_Name == "Sri potti sriramulu nellore" |District_Name == "Sri potti sriramulu Nellore", "Nellore",
ifelse (District_Name == "Sri Athya Sai" | District_Name == "Sri sathya sai" | District_Name == "Sri sathya sai"
| District_Name == "Sri Sathya sai"| District_Name == "Sri Sathya Sai" | District_Name == 5366
| District_Name == "Sri sathyasai" | District_Name == "Sri Sathya" | District_Name == "SRI Satya" | District_Name == "Sri. Sathyasai" | District_Name == "Srisahyasai"
| District_Name == "Sri satya sai"| District_Name == "Sri Satya sai" | District_Name == "Sti satya sai" |  District_Name == "SRI satya sai"
| District_Name == "Sri Satya Sai"| District_Name == "SRI Satya sai" |  District_Name == "Sri ssthya sai"
| District_Name == "SRI Satya Sai"| District_Name == "Srisathyasai"  |  District_Name == "Sri sthya sai"
| District_Name == "Sir Sathya Sai"| District_Name == "Srisatbyasai"  |  District_Name == "Sri sthya sai"
| District_Name == "Srisathayasai"| District_Name == "Srisathy sai"
| District_Name == "SRI Sathya sai", "Sri Sathya Sai", District_Name))))))))))

#Aggregate function
z <- aggregate(AP_Beneficiary_Roster$SR_1 == "Self", by = list(AP_Beneficiary_Roster$`_submission__uuid`), sum)
table(z$x) #Households with no. of selfs

#Dropping HH with multiple and no selves
u <- z$Group.1[z$x < 1]
w <- z$Group.1[z$x > 1]

#Merging Roster with Survey
AP_Beneficiary_Survey_Merged <- merge(AP_Beneficiary_Survey, AP_Beneficiary_Roster[AP_Beneficiary_Roster$SR_1 == "Self",], by.x = c(162), by.y = c(57))
AP_Beneficiary_Survey_Merged <- AP_Beneficiary_Survey_Merged[(AP_Beneficiary_Survey_Merged$`_uuid` %in% c(u,w)) == F,]

AP_Roster_Merged <- merge(AP_Beneficiary_Roster, AP_Beneficiary_Survey, by.x = c(57), by.y = c(162))

##Average household members by enumerator codes
ar <- AP_Roster_Merged %>% 
group_by(Enumerator_Code, `_submission__id`) %>% 
summarise (member_households = max(SNo)) %>% 
ungroup()

am <- AP_Roster_Merged %>% 
  group_by(District_Name, `_submission__id`) %>% 
  summarise (member_households = max(SNo)) %>% 
  ungroup()

#Surveyors going too fast
v <- as.data.frame(difftime(AP_Beneficiary_Survey$end, AP_Beneficiary_Survey$start))
e <- table(AP_Beneficiary_Survey$Enumerator_Code
           [as.numeric(difftime(AP_Beneficiary_Survey$end, AP_Beneficiary_Survey$start)) < 15])

b <- AP_Beneficiary_Survey %>% 
filter (Enumerator_Code == 54439)

#Key Households flagged
#Household with way too much spending
a <- AP_Beneficiary_Survey[AP_Beneficiary_Survey$S_D_65 == 55000,]
b <- AP_Beneficiary_Roster[AP_Beneficiary_Roster$`_submission__uuid` == "f217c337-313e-4df9-91ae-208be2566006",]

#Household with no spending at all
c <- AP_Beneficiary_Survey[AP_Beneficiary_Survey$S_D_64 == 0,]
d <- AP_Beneficiary_Roster[AP_Beneficiary_Roster$`_submission__uuid` == "25354a5f-b8ec-46ae-bf09-76230ca71168",]


#Cheyutha age bounds are between 45-59
table(AP_Beneficiary_Survey_Merged$Age, AP_Beneficiary_Survey_Merged$S_G_98)

table(AP_Beneficiary_Survey$S_D_64)

secretariats <- AP_Beneficiary_Survey %>%
mutate (`Secretariat Name` == ifelse(`Secretariat Name` = ))

bb < as.data.frame (table(AP_Beneficiary_Survey$S_E_81))

#Suspicious IDs, Correct IDs: 
AP_Beneficiary_Survey <- AP_Beneficiary_Survey %>% 
dplyr::mutate (Enumerator_Code = ifelse (Enumerator_Code == -54391, 54391,
ifelse (Enumerator_Code ==  511, 54861,
ifelse (Enumerator_Code ==  5473, 54723,
ifelse (Enumerator_Code ==  51136, 54136,
ifelse (Enumerator_Code ==  51314, 54314,
ifelse (Enumerator_Code ==  54165, 54162,
ifelse (Enumerator_Code ==  54362, 54632,
ifelse (Enumerator_Code ==  54536, 54836,
ifelse (Enumerator_Code ==  54569, 54659,
ifelse (Enumerator_Code ==  54584, 54586,
ifelse (Enumerator_Code ==  54856, 54826,
ifelse (Enumerator_Code ==  54526, 54625,
ifelse (Enumerator_Code ==  54761, 54671,
ifelse (Enumerator_Code ==  514516, 54516,
ifelse (Enumerator_Code ==  57515, 54515,
ifelse (Enumerator_Code ==  504, 54637,
ifelse (Enumerator_Code ==  753, 54812,
ifelse (Enumerator_Code ==  51842, 54812,
ifelse (Enumerator_Code ==  54546, 54516,
ifelse (Enumerator_Code ==  53943, 54943,
ifelse (Enumerator_Code ==  503, 54287,
ifelse (Enumerator_Code ==  546632, 54632,
ifelse (Enumerator_Code ==  54273, 54723,
ifelse (Enumerator_Code ==  1100, 54632,
ifelse (Enumerator_Code ==  54731, 54713,
ifelse (Enumerator_Code ==  554713, 54713,
ifelse (Enumerator_Code ==  54378, 54783,
ifelse (Enumerator_Code ==  600, 54136, Enumerator_Code)))))))))))))))))))))))))))))

#Enumerator Attendance and Productivity; Cleaning Enumerator IDs
AB <- AP_Beneficiary_Survey %>% 
  filter(start < '2023-08-15') %>% 
  separate(col=start, into=c('date','time'), sep= ' ', remove = FALSE) 

DRP <- as.data.frame.matrix(table(AB$Enumerator_Code, AB$date))
DRP <- rownames_to_column(DRP)
DRP$total <- rowSums(DRP[,c(2:55)])
DRP[, "max"] <- apply(DRP[, c(2:55)], 1, max)

ARP <- as.data.frame.matrix(table(AB$Enumerator_Code, AB$date))
ARP[ARP > 0] <- 1
ARP$total <- rowSums(ARP[,c(1:54)])
#ARP[ARP == 1] <- "P"
#ARP[ARP == 0] <- "A"
ARP <- rownames_to_column(ARP)

write_xlsx(ARP, "Phase I Attendance.xlsx")
write_xlsx(DRP, "Phase I Daily Submissions.xlsx")

AC <- AB %>% 
filter (Enumerator_Code %in% DRP$rowname[DRP$total > 6])

week1 <- DRP[ ,c(1,6:10)] 
week2 <- DRP[ ,c(1, 13:17)]
week3 <- DRP[, c(1, 20:24)]
week4 <- DRP[, c(1, 27:31)]
week5 <- DRP[, c(1, 34:38)]

max_surveys <- DRP[, c(1, 22:23)] %>% 
rename("Enumerator_Code" = rowname)

enum_check <- merge(max_surveys, enum_names, by.x = c(1), by.y = c(2)) %>% 
  rename("max_surveys" = max)

write_xlsx(week1, "productivity in week 1.xlsx")
write_xlsx(week2, "productivity in week 2.xlsx")
write_xlsx(week3, "productivity in week 3.xlsx")
write_xlsx(week4, "productivity in week 4.xlsx")
write_xlsx(week5, "productivity in week 5.xlsx")
write_xlsx(ARP, "Progress by date.xlsx")

AP_Roster_Merged <- merge(AP_Beneficiary_Roster, AC, by.x = c(57), by.y = c(162))

#Submissions by Enumerators, District Name, and Date
AS <- AC %>% 
mutate (S = 1) %>% 
group_by(Enumerator_Code, District_Name) %>% 
summarise (n= sum(S)) %>% 
ungroup ()

BS <- AB %>% 
mutate (S =1) %>% 
group_by(date) %>% 
summarise (n = sum(S)) %>% 
ungroup ()

write_xlsx(BS, "Phase I submissions by date.xlsx")

#Enumerators by districts
enum_districts <- AB %>% 
group_by(Enumerator_Code) %>% 
summarise(District = unique(District_Name)) %>% 
ungroup()

##Checks on over productive enumerators (Submission time & Maximum Surveys Completed)
AC <- AC %>% 
mutate (submission_time = difftime(AC$end, AC$start, units = "mins")) 

submission_time <- AC %>% 
filter (submission_time < ) %>% 
group_by(Enumerator_Code) %>% 
summarise (Avg_time = mean(submission_time)) %>% 
ungroup()

enum_check <- merge(max_surveys, submission_time, by.x = c(1), by.y = c(1)) %>% 
rename("max_surveys" = max)

enum_check <- merge(enum_check, enum_districts, by.x = c(1), by.y = c(1))

underperforming <- enum_check %>% 
filter(max_surveys > 12 & Avg_time < 15)

highperforming <- enum_check %>% 
filter(max_surveys < 12 & Avg_time > 15)

underperforming_enum <- merge(enum_names, underperforming, by.x = c(2), by.y = c(1)) %>% 
slice(1:5, 7:16)

highperforming_enum <- merge(enum_names, highperforming, by.x = c(2), by.y = c(1))

underperforming_enum %>% 
  kbl(caption = "Underperforming Enumerators: Surveys completed on a given day > 12 & Average Submission Time < 15 minutes", align = "l")%>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15)

highperforming_enum %>% 
  kbl(caption = "High-performing Enumerators: Surveys completed on a given day < 12 & Average Submission Time is between 15 and 30 minutes", align = "l")%>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15)

write_xlsx(submission_time, "Average Submission Time by Enumerator Code.xlsx")

#For enumerators with ids 54124, 54713, 54631,54859
ap <- submission_time %>%
filter (Enumerator_Code == 54124 |Enumerator_Code == 54713 | Enumerator_Code == 54631 | Enumerator_Code == 54859)

submissions <- as.data.frame(table(AP_Beneficiary_Survey$Enumerator_Code)) %>% 
filter (Enumerator_Code == 54124 |Enumerator_Code == 54713 | Enumerator_Code == 54631 | Enumerator_Code == 54859)

##No of household members by enumerator codes
hh_members1 <- ar %>% 
filter (ar$Enumerator_Code %in% AC$Enumerator_Code)
hh_members <- as.data.frame.matrix (table(hh_members1$Enumerator_Code, hh_members1$member_households))
hh_members <- rownames_to_column(hh_members)
write_xlsx(hh_members, "No of household members by enumerator codes")

##surveys by date
bb <- as.data.frame(table(AB$date))
write_xlsx(bb, "daywise progress.xlsx")

##Estimating Replacement Households
x <- c(100, 101, 102, 103, 104, 105, 200, 201, 202, 203, 204, 205, 300, 301, 302, 303, 304, 305, 400, 401, 402, 403, 404, 405, 
       500, 501, 502, 503, 504, 505, 600, 601, 602, 603, 604, 605, 700, 701, 702, 703, 704, 705, 800, 801, 802, 803, 804, 805,
       900, 901, 902, 903, 904, 905, 1000, 1001, 1002, 1003, 1004, 1005, 1100, 1101, 1102, 1103, 1104, 1105, 1200, 1201, 1202, 1203, 1204, 1205,
       1300, 1301, 1302, 1303, 1304, 1305, 1400, 1401, 1402, 1403, 1404, 1405, 1500, 1501, 1502, 1503, 1504, 1505, 1600, 1601, 1602, 1603, 1604, 1605,
       1700, 1701, 1702, 1703, 1704, 1705, 1800, 1801, 1802, 1803, 1804, 1805, 1900, 1901, 1902, 1903, 1904, 1905, 2000, 2001, 2002, 2003, 2004, 2005)

xr <- c(101, 102, 103, 104, 105, 201, 202, 203, 204, 205, 301, 302, 303, 304, 305, 401, 402, 403, 404, 405, 
       501, 502, 503, 504, 505, 601, 602, 603, 604, 605, 701, 702, 703, 704, 705, 801, 802, 803, 804, 805,
       901, 902, 903, 904, 905, 1001, 1002, 1003, 1004, 1005, 1101, 1102, 1103, 1104, 1105, 1201, 1202, 1203, 1204, 1205,
       1301, 1302, 1303, 1304, 1305, 1401, 1402, 1403, 1404, 1405, 1501, 1502, 1503, 1504, 1505, 1601, 1602, 1603, 1604, 1605,
       1701, 1702, 1703, 1704, 1705, 1801, 1802, 1803, 1804, 1805, 1901, 1902, 1903, 1904, 1905, 2001, 2002, 2003, 2004, 2005)

rep_hh <- AC %>% 
filter (Respondent_ID %in% x) %>% 
mutate (rep_hh = ifelse(Respondent_ID %in% xr, 1,0))

rep_hh$rep_rate <- str_extract(rep_hh$Respondent_ID, "\\d$")

#District-wise dis-aggregation
round(prop.table(table(rep_hh$District_Name, rep_hh$rep_rate),1)*100,2)

rep_district <- rep_hh %>% 
group_by(District_Name) %>% 
summarise(Avg_Rep_Rate = mean(as.numeric(rep_rate))) %>% 
ungroup()

#Dis-aggregating by enumerators
rep_enum <- rep_hh %>% 
group_by(Enumerator_Code) %>% 
summarise (Avg_Rep_Rate = mean(as.numeric(rep_rate)), prop_rep = sum(rep_hh)*100/length(rep_hh)) %>% 
ungroup()

##Ordering secretariats from coastal districts
All_Secretariats_File <- read.csv("allsecretariats_update.csv")

secretariats_coastal <- All_Secretariats_File %>% 
group_by(NEW_DISTRICT) %>% 
ungroup()

secretariats_coastal <- secretariats_coastal %>% 
filter (NEW_DISTRICT == "PRAKASAM" | NEW_DISTRICT == "PALNADU" | NEW_DISTRICT == "BAPATLA" |
          NEW_DISTRICT == "GUNTUR" |NEW_DISTRICT == "KRISHNA" |NEW_DISTRICT == "EAST GODAVARI" |
          NEW_DISTRICT == "WEST GODAVARI" |NEW_DISTRICT == "KONASEEMA" |NEW_DISTRICT == "KAKINADA" |
          NEW_DISTRICT == "NTR" |NEW_DISTRICT == "ELURU")

write_xlsx(secretariats_coastal, "Coastal Secretariats.xlsx")





