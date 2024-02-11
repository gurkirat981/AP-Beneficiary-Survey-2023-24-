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
library(stringdist)

setwd("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp")

All_Secretariats_File <- read.csv("allsecretariats_update.csv")

secretariats_coastal <- All_Secretariats_File %>% 
  group_by(NEW_DISTRICT) %>% 
  ungroup()

secretariats_coastal_northern <- secretariats_coastal %>% 
  filter (NEW_DISTRICT == "ALLURI SITHARAMA RAJU" | NEW_DISTRICT == "ANAKAPALLI" | NEW_DISTRICT == "PARVATHIPURAM MANYAM" |
            NEW_DISTRICT == "SRIKAKULAM" |NEW_DISTRICT == "VISAKHAPATNAM" |NEW_DISTRICT == "VIZIANAGARAM")

write_xlsx(secretariats_coastal_northern, "Coastal Northern Secretariats.xlsx")

a <- as.data.frame(table(All_Secretariats_File$MANDAL_NAME[All_Secretariats_File$R_U == "U"]))
write_xlsx(a, "urban secretariats by ulbs.xlsx")

#Main Files
AP_Beneficiary_Survey <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/Beneficiary_Survey_Feb7.xlsx")
AP_Beneficiary_Roster <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/Beneficiary_Survey_Feb7.xlsx", sheet = 2)
AP_Beneficiary_Survey_28April <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/Beneficiary_Survey_28thApril.xlsx")
matchednames_phase3 <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/Enumerator Codes (Phase III).xlsx")
all_secretariats <- read_excel("/Users/gurkirat/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Flags Drp/allsecretariats_update.xlsx")

AP_Beneficiary_Survey_28April <- AP_Beneficiary_Survey_28April %>% 
  filter (start > '2023-05-17') %>% 
  mutate ("If you have never been a beneficiary, then why not?" = NA, .after = 96) %>% 
  mutate ("Other, please specify...98" = NA, .after = 97) %>% 
  mutate ("If not eligible, what was the reason?" = NA, .after = 122) %>% 
  mutate ("Other, please specify...124" = NA, .after = 123) %>% 
  mutate ("If not eligible previously, what was the reason?" = NA, .after = 126) %>% 
  mutate ("Other, please specify...128" = NA, .after = 127)

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

AB <- AP_Beneficiary_Survey %>% 
  separate(col=start, into=c('date','time'), sep= ' ', remove = FALSE) 

AB <- merge(AB, matchednames_phase3, by.x = c(8), by.y = c(3))

AB <- AB %>% 
 filter (start > '2024-01-01 00:00:00')

#Fuzzy-matching secretariat names
all_secretariats <- all_secretariats %>% 
  rename("Secretariat_Name" = SECRETARIAT_NAME)

official_names_id <- all_secretariats %>% 
  dplyr::select(Secretariat_Name, SECRETARIAT_CODE, NEW_DISTRICT)

AC <- merge(official_names_id, AP_Beneficiary_Survey, by.x = c(2), by.y = c(12), all.y = TRUE) %>% 
  rename("Actual_Sec_Name" = Secretariat_Name.x) %>% 
  rename("Reported_Sec_Name" = Secretariat_Name.y)

AC$Actual_Sec_Name <- as.character(AC$Actual_Sec_Name)

##imperfect matches
imperfect_matches <- AC %>% 
  filter (is.na(Actual_Sec_Name) == TRUE)

official_names = data.frame(all_secretariats$Secretariat_Name)
used_names = data.frame(imperfect_matches$Reported_Sec_Name)
used_names = unique(used_names)

used_names$new_names <- "" 

used_names <- used_names %>% 
  rename("Reported_Sec_Name" = imperfect_matches.Reported_Sec_Name)

for(i in 1:dim(used_names)[1]) {
  x <- agrep(used_names$Reported_Sec_Name[i], official_names$all_secretariats.Secretariat_Name,
             ignore.case=TRUE, value=TRUE,
             max.distance = 0.08, useBytes = TRUE)
  x <- paste0(x,"")
  used_names$new_names[i] <- x
} 

used_names[used_names == ''] <- NA

matches <- merge (imperfect_matches, used_names, by.x = c(15), by.y = c(1))

matches <- matches %>% 
  dplyr::select (-Actual_Sec_Name) %>% 
  mutate ("Actual_Sec_Names" = new_names, .after = 2)

matches_imperfect <- matches %>% 
  filter(is.na(Actual_Sec_Names) == FALSE)

matches_imperfect <- matches_imperfect %>% 
  dplyr::select(-new_names) %>% 
  rename("Reported_Sec_Names" = Reported_Sec_Name)

##Perfect matches
matches_perfect <- AC %>% 
  filter (is.na(Actual_Sec_Name) == FALSE)

matches_perfect <- matches_perfect %>% 
  rename("Actual_Sec_Names" = Actual_Sec_Name) %>% 
  mutate("Reported_Sec_Names" = Reported_Sec_Name, .before = 1) 

matches_perfect <- matches_perfect %>% 
  dplyr::select(-Reported_Sec_Name)

sec_names <- rbind(matches_perfect,matches_imperfect)

sec_names2 <- merge(official_names_id, sec_names, by.x = c(1), by.y = c(3))

sec_names_phase2 <-sec_names2 %>% 
  group_by(NEW_DISTRICT.x, Secretariat_Name) %>% 
  summarise (No_Responses = length(Secretariat_Name)) %>% 
  ungroup ()

a <- as.data.frame(table(sec_names_phase2$NEW_DISTRICT.x[sec_names_phase2$No_Responses > 1])) %>% 
  rename("District" = Var1) %>% 
  rename("Secretariats Completed" = Freq)

districts <- as.data.frame(table(all_secretariats$NEW_DISTRICT)) %>% 
  rename("District" = Var1) %>% 
  rename("Secretariats Sampled" = Freq)

table_secretariats <- merge (a, districts, by.x = c(1), by.y = c(1)) 

remaining_srikakulam <- all_secretariats %>% 
  filter(all_secretariats$NEW_DISTRICT == "SRIKAKULAM")

srikakulam_remaining <- remaining_srikakulam %>% 
filter (!(Secretariat_Name %in% sec_names_phase2$Secretariat_Name))

write_xlsx(table_secretariats, "Srikakulam Progress by Districts.xlsx")
write_xlsx(srikakulam_remaining, "Remaining Secretariats Srikakulam.xlsx")

table_secretariats %>% 
  kbl(caption = "District-wise Survey Progress", align = "l")%>%
  kable_classic(full_width = F, html_font = "Times New Roman", font_size = 15)

phase_3 <- all_secretariats %>% 
  filter (NEW_DISTRICT == "ANAKAPALLI" | NEW_DISTRICT == "ALLURI SITHARAMA RAJU" | NEW_DISTRICT == "PARVATHIPURAM MANYAM" | NEW_DISTRICT == "SRIKAKULAM" | NEW_DISTRICT == "VISAKHAPATNAM" |
            NEW_DISTRICT == "VIZIANAGARAM")

write_xlsx (phase_3, "Phase III Secretariats.xlsx")

##Note: Konaseema, Krishna and West Godavari have been fully assigned

#Merging Roster with Survey
AP_Beneficiary_Survey_Merged <- merge(AP_Beneficiary_Survey, AP_Beneficiary_Roster[AP_Beneficiary_Roster$SR_1 == "Self",], by.x = c(162), by.y = c(57))
AP_Beneficiary_Survey_Merged <- AP_Beneficiary_Survey_Merged[(AP_Beneficiary_Survey_Merged$`_uuid` %in% c(u,w)) == F,]

AP_Roster_Merged <- merge(AP_Beneficiary_Roster, AP_Beneficiary_Survey, by.x = c(57), by.y = c(162))
APCoast_Roster_Merged <- merge(AP_Beneficiary_Roster, AB, by.x = c(57), by.y = c(164))

#Submissions by Enumerators, District Name, and Date
AS <- AB %>% 
  mutate (S = 1) %>% 
  group_by(Enumerator_Code, Enumerator_name, District_name) %>% 
  summarise (n= sum(S)) %>% 
  ungroup ()

BS <- AB %>% 
  mutate (S =1) %>% 
  group_by(date) %>% 
  summarise (Submissions = sum(S)) %>% 
  ungroup ()

write_xlsx(BS, "Phase III Week III,IV Submissions.xlsx")

SB <- BS %>% 
mutate (Week = ifelse (date > '2023-11-23' & date < '2023-12-25', "Week I", 
                       ifelse(date > '2023-12-24' & date < '2024-01-01', "Week II",
                            ifelse(date > '2023-12-31' & date < '2024-01-07', "Week III", "Week IV"))))
                                                                             
Weekly_Progress <- SB %>% 
group_by(Week) %>% 
summarise (Progress = 100*round(sum(Submissions)/3660,2)) %>% 
ungroup ()

#Weekly Progress
Weekly_Progress <- Weekly_Progress %>% 
mutate (Cumalative_Progress = cumsum(Progress))

Weekly_Progress$Week <- factor(Weekly_Progress$Week, levels = c("Week I", "Week II", "Week III", "Week IV"))

weekly_progress_plot <- ggplot(Weekly_Progress, aes(x= Week, y= Cumalative_Progress, fill = Cumalative_Progress)) +
  geom_bar(stat="identity", width= 0.5, show.legend = FALSE) + 
  geom_text (aes(label = Cumalative_Progress ), position = position_dodge(0.1), vjust = -1) +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_discrete(labels = function(`Week`) str_wrap(`Week`, width = 10)) +
  #scale_fill_brewer(palette="YlGnBu") +
  labs(x="",y="Percentage", subtitle = "Phase III: Weekly Progress (Cumalative; %)", caption = "Note: Progress is based on submissions recieved at the end of the week") + newtheme

Y_E_80_plot

#Aggregate function

z <- aggregate(APCoast_Roster_Merged$SR_1 == "Self", by = list(APCoast_Roster_Merged$`_submission__id`), sum) %>% 
  rename ("No_Self" = x)
table(z$No_Self) #Households with no. of selfs

AB <- AB %>% 
  mutate(self = ifelse(`_uuid` %in% z$Group.1[z$No_Self == 0], "Self Missing", 
                       ifelse(`_uuid` %in% z$Group.1[z$No_Self == 1], "Self Correctly Reported", "Multiple Self")))

enums_incorrectself <- AB %>% 
  filter (self == "Self Missing" | self == "Multiple Self") 

a <- as.data.frame.matrix(table(enums_incorrectself$Enumerator_Code, enums_incorrectself$self))
a <- rownames_to_column(a) %>% 
  rename("Enumerator_Code" = rowname)

self_responses <- merge (AS, a, by.x= c(1), by.y= c(1)) %>% 
  rename("Number_of_Submissions" = n)

write_xlsx(self_responses, "Incorrect Responses on Self.xlsx")

#Dropping HH with multiple and no selves
u <- z$Group.1[z$x < 1]
w <- z$Group.1[z$x > 1]

##Average household members by enumerator codes
ar <- APCoast_Roster_Merged %>% 
  group_by(Enumerator_Code, `_submission__id`) %>% 
  summarise (member_households = max(SNo)) %>% 
  ungroup()

am <- APCoast_Roster_Merged %>% 
  group_by(District_name, `_submission__id`) %>% 
  summarise (member_households = max(SNo)) %>% 
  ungroup()

one_member_hhs <- am %>% 
  filter(member_households == 1)

surveys_onememberhhs <- APCoast_Roster_Merged %>% 
  filter(`_submission__id` %in% one_member_hhs$`_submission__id`)

#Surveyors going too fast
v <- as.data.frame(difftime(AP_Beneficiary_Survey$end, AP_Beneficiary_Survey$start))
e <- table(AP_Beneficiary_Survey$Enumerator_Code
           [as.numeric(difftime(AP_Beneficiary_Survey$end, AP_Beneficiary_Survey$start)) < 15])

b <- AP_Beneficiary_Survey %>% 
  filter (Enumerator_Code == 54439)


#Cheyutha age bounds are between 45-59
table(AP_Beneficiary_Survey_Merged$Age, AP_Beneficiary_Survey_Merged$S_G_98)

table(AP_Beneficiary_Survey$S_D_64)

secretariats <- AP_Beneficiary_Survey %>%
  mutate (`Secretariat Name` == ifelse(`Secretariat Name` = ))

bb < as.data.frame (table(AP_Beneficiary_Survey$S_E_81))

#Enumerator Attendance and Productivity; Cleaning Enumerator IDs
DRP <- as.data.frame.matrix(table(AB$Enumerator_Code, AB$date))
DRP <- rownames_to_column(DRP) 
DRP$total <- rowSums(DRP[,c(2:14)])
DRP[, "max"] <- apply(DRP[, c(2:14)], 1, max)

ARP <- as.data.frame.matrix(table(AB$Enumerator_Code, AB$date))
ARP[ARP > 0] <- 1
ARP$total <- rowSums(ARP[,c(1:13)])
#ARP[ARP == 1] <- "P"
#ARP[ARP == 0] <- "A"
ARP <- rownames_to_column(ARP)

attendance_sheet <- merge(matchednames_phase3, ARP, by.x = c(3), by.y = c(1)) %>% 
  rename ("Phase III, Week III & IV Attendance" = total)

progress_sheet <- merge(matchednames, DRP, by.x = c(1), by.y = c(1)) %>% 
  rename ("Phase II Total submissions" = total)

write_xlsx(attendance_sheet, "Phase III Week III & IV Attendance Sheet.xlsx")
write_xlsx(progress_sheet, "Phase II total submimssions.xlsx")

#Week V: Enumerator Productivity by Districts
max_surveys <- DRP[, c(1, 15:16)] %>% 
  rename("Enumerator_Code" = rowname) 

max_days <- ARP[, c(1, 15:15)] %>% 
  rename("Enumerator_Code" = rowname) %>% 
  rename("total days" = total)

max_days_surveys <- merge (max_surveys, max_days, by.x = c(1), by.y = c(1))

max_days_surveys <- merge(matchednames_phase3, max_days_surveys, by.x = c(3), by.y = c(1))

district_productivity <- max_days_surveys %>% 
  group_by(District_name) %>% 
  summarise(No_Enum = length(Enumerator_Code), No_days = sum(`total days`), No_Surveys = sum(total)) %>% 
  ungroup()

write_xlsx(district_productivity, "Week III & IV Phase III Enumerator Productivity by Districts.xlsx")


#Enumerators by districts
enum_districts <- AB %>% 
  group_by(Enumerator_Code) %>% 
  summarise(District = unique(District_Name)) %>% 
  ungroup()

enumerators <- merge(enum_check, enum_districts,by.x = c(1), by.y = c(1))

write_xlsx(enumerators, "enumerators details (coastal districts).xlsx")

##Checks on over productive enumerators (Submission time & Maximum Surveys Completed)
AB <- AB %>% 
  mutate (submission_time = difftime(AB$end, AB$start, units = "mins")) 

submission_time <- AB %>% 
  filter (submission_time < 45 & submission_time > 0) %>% 
  group_by(Enumerator_Code, Enumerator_name, District_name) %>% 
  summarise (Avg_time = round(mean(submission_time),2)) %>% 
  ungroup()

AB$submission_time <- round(as.numeric(AB$submission_time),2)

b <- as.data.frame(table(AB$Enumerator_Code[AB$submission_time > 0 & AB$submission_time < 10])) %>% 
  rename("Enumerator Code" = Var1) %>% 
  rename ("Number of Surveys" = Freq)

surveys_under10 <- merge (AS, b, by.x = c(1), by.y = c(1))

write_xlsx(surveys_under10, "Week III & IV Phase III Surveys under 10 minutes.xlsx")


par(family = "Times New Roman", font = 2, font.lab = 2, font.axis = 2)
hist_bh <- hist(AB$submission_time[AB$submission_time < 45 & AB$submission_time > 0], xlim = c(0,50), ylim = c(0,300), breaks = c(10),
                xlab = "Submission Time (in minutes)", main = "Week III & IV: Distribution of surveys by submission time", 
                ylab = "Number of Surveys", col = "skyblue2", border = "white", labels = TRUE)

#High-performing and Under-Performing Enumerators
days_surveys <- max_days_surveys %>% 
dplyr::select(-District_name, -Enumerator_name) %>% 
rename ("Total Surveys" = total) %>% 
rename("Maximum survyes in one day" = max) %>% 
rename ("Total Working Days" = `total days`)

enum_check <- merge(submission_time, days_surveys, by.x = c(1), by.y = c(1)) %>% 
  rename("Average Submission Time" = Avg_time) %>% 
  mutate ("Productivity (Submissions/Working days)" = round(`Total Surveys`/`Total Working Days`,2))

enum_check <- enum_check %>% 
mutate ("Productivity Category" = ifelse (`Productivity (Submissions/Working days)` < 5, "Under Productive", "Appropriately Productive"))

write_xlsx(enum_check, "Week III & IV Enumerator Productivity.xlsx")

underperforming <- enum_check %>% 
  filter(max_surveys > 10) %>% 
  rename ("Total Submissions" = total)

highperforming <- enum_check %>% 
  filter(max_surveys < 10 & Avg_time > 17) %>% 
  rename ("Total Submissions" = total)

iefficient_enum <- AS %>% 
  filter (n < 10)

write_xlsx(submission_time, "Average Submission Time by Enumerator Code.xlsx")
write_xlsx(underperforming, "Week VIII underperforming enumerators.xlsx")
write_xlsx(highperforming, "Week VIII highperforming enumerators.xlsx")
write_xlsx(iefficient_enum, "Week VII inefficient enumerators.xlsx")

##No of household members by enumerator codes
hh_members1 <- ar %>% 
  filter (ar$Enumerator_Code %in% AB$Enumerator_Code)
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

rep_hh <- AB %>% 
  filter (Respondent_ID %in% x) %>% 
  mutate (rep_hh = ifelse(Respondent_ID %in% xr, 1,0))

rep_hh$rep_rate <- str_extract(rep_hh$Respondent_ID, "\\d$")

#District-wise dis-aggregation
round(prop.table(table(rep_hh$District, rep_hh$rep_rate),1)*100,2)

rep_district <- rep_hh %>% 
  group_by(District) %>% 
  summarise(Avg_Rep_Rate = mean(as.numeric(rep_rate))) %>% 
  ungroup()

#Dis-aggregating by enumerators
rep_enum <- rep_hh %>% 
  group_by(Enumerator_Code, District) %>% 
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

submissions_enum <- as.data.frame(table(AB$Enumerator_Code)) %>% 
  rename("Enumerator_Code" = Var1) %>% 
  rename ("Number of Surveys" = Freq)

write_xlsx(submissions_enum, "Submissions by Enumerator Code.xlsx")

#Gender of the Self marked Female
responses_male <- APCoast_Roster_Merged %>% 
  filter (APCoast_Roster_Merged$SR_1 == "Self" & APCoast_Roster_Merged$Gender == "Male")

incorrect_gender <- AB %>% 
  filter (`_uuid` %in% responses_male$`_submission__id`)

male_responses <- as.data.frame(table(incorrect_gender$Enumerator_Code))

male_responses <- merge(matchednames_phase3, male_responses, by.x = c(3), by.y = c(1)) %>% 
  rename("Surveys with Self Gender = Male" = Freq)

write_xlsx(male_responses, "gender reported male.xlsx")

##Comparing HSN's and GSTIN 

