library("stringr")

load("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BoulderHousingPartnersData/Data/combined-New-March22-ver2-2017.RData")
#grep("PH_or_S8", names(final), value = TRUE)
ph2017 <- read.csv("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BHP-New/BHP-2017-roundData/BHP2017.csv")
#phadmin2017 <- read.csv("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/")



#for(i in 3:length(names(phadmin2017))) names(phadmin2017)[i] <- paste0(names(phadmin2017)[i], "_17", sep="")

ph2017$From <- "ph2017"
ph2017$From <- as.factor(ph2017$From)
#phadmin2017$From <- "phadmin2017"
#phadmin2017$TCode <- tolower(as.character(phadmin2017$Tenant))

ph2017$TCode <- tolower(as.character(ph2017$TCode))
ph2017$TCode.1 <- tolower(as.character(ph2017$TCode.1))
### Now check if all the TCodes were correctly entered.
#ph2017$TCode[i] <- as.character(as.factor(ph2017$TCode[i]))
#ph2017$TCode.1[i] <- as.character(ph2017$TCode.1[i])
for(i in 1:length(ph2017$TCode)){
  if(!is.na(ph2017$TCode[i]) & tolower(as.character(ph2017$TCode[i])) == tolower(as.character(ph2017$TCode.1[i]))) ph2017$TCodeCorrect[i] <- 1 else ph2017$TCodeCorrect[i] <- 0
}

ph2017$TCodeCorrect<-as.factor(ph2017$TCodeCorrect)
#summary(as.factor(ph2017$TCodeCorrect)) 
incorrect <- subset(ph2017, TCodeCorrect==0)
incorrect <- data.frame(incorrect$TCode, incorrect$TCode.1)
#incorrect ## There are five TCodes where the TCode does not match the TCode.1.  
### Correct the one TCode by replacing too14800 by t0014800. 
for(i in 1:length(ph2017$TCode)) if(!is.na(ph2017$TCode[i])) if(as.character(ph2017$TCode[i])=="too14800") ph2017$TCode[i]<-"t0014800"

#summary(as.factor(ph2017$TCode)) # To identify duplicates. 
ph2017 <- ph2017[-grep("t0000869", ph2017$TCode)[1], ]


#setdiff(phadmin2017$TCode, intersect(phadmin2017$TCode, ph2017$TCode)) ## TCodes that are there in phadmin2017 but not in ph2017.
#setdiff(ph2017$TCode, intersect(phadmin2017$TCode, ph2017$TCode)) ## TCodes that are there in ph2017 but not in phadmin2017. 


##############################################

for(i in 1:length(ph2017))
  if(names(ph2017)[i] == "Program") names(ph2017)[i] <- "PH_or_S8"
#summary(as.factor(ph2017$PH_or_S8))


## Change the names of the variables that start with "Q" and remove the part of the variable name that starts with "." so that they are similar to 2014 data.

for(i in 1:length(ph2017)) {
  if(substr(names(ph2017)[i], 1, 1) == 'Q') {
    b <-as.vector(strsplit(names(ph2017)[i], ".", fixed=TRUE))
    names(ph2017)[i]<-b[[1]][1]
  }
}

### Missing values need to be assigned correctly.
for(i in 1:length(ph2017))
  for(j in 1:length(ph2017$TCode))
    if(is.na(ph2017[j, i]) | (ph2017[j, i])=='No response' | (ph2017[j, i])=="") ph2017[j, i]<-NA

## Check for duplicate TCodes.
#summary(as.factor(ph2017$TCode))

### No duplicates found.


## Remove "No response" as one of the factors in all responses. ### Need to correct this code.

for(i in 1:length(names(ph2017))) {
  if(is.factor(ph2017[[i]])) {
    if("No response" %in% levels(ph2017[[i]]) ) {
      #levels(ph2017[[i]]) <- levels(ph2017[[i]])[levels(ph2017[[i]]) != "No response"]
      ph2017[[i]] = ordered(ph2017[[i]], levels=levels(ph2017[[i]])[levels(ph2017[[i]]) != "No response"])
      #print( levels(ph2017[[i]]) )
      #print(i)
      #print(levels(ph2017[[i]]))
    }
  }
}    

for(i in 1:length(names(ph2017))) {
  if(is.factor(ph2017[[i]])) {
    if("" %in% levels(ph2017[[i]]) ) {
      #levels(ph2017[[i]]) <- levels(ph2017[[i]])[levels(ph2017[[i]]) != "No response"]
      ph2017[[i]] = ordered(ph2017[[i]], levels=levels(ph2017[[i]])[levels(ph2017[[i]]) != ""])
      #print( levels(ph2017[[i]]) )
      #print(i)
      #print(levels(ph2017[[i]]))
    }
  }
}    

for(i in 4:length(ph2017))
  names(ph2017)[i]<-paste0(names(ph2017)[i], "_17", sep="")

###### There are two variables in ph2017 with the name "Q35_12_17" (variable 163 and 164). The second one needs to be renamed as "Q35_12_17.1"  
names(ph2017)[164] <- "Q35_12_17.1"

#############################################################################
##################   MERGING DATA   #########################################

final1 <- merge(final, ph2017, by = intersect(names(final), names(ph2017)), sort = TRUE, all = TRUE)
final <- final1
final$From_14 <- as.factor(final$From_14)
final$From_15 <- as.factor(final$From_15)
final$From_16 <- as.factor(final$From_16)
final$From_17 <- as.factor(final$From_17)
final$From <- as.factor(final$From)

#for(i in 1:length(names(final))) 
#  if(names(final)[i] == "PH_or_S8.y") names(final)[i] <- "PH_or_S8"


#summary(as.factor(final$TCode)) 
#temp <- subset(final, TCode=="t0008313")
###########################################
#for(i in 1:length(names(temp))) {
#  if(!is.na(temp[1, i]) & !is.na(temp[2, i])) if(temp[1, i] != temp[2, i]) print(c(names(temp)[i], temp[1, i], temp[2, i]))
#}

#nacount1 <- 0
#nacount2 <- 0
#for(i in 1:length(names(temp))) {
#  if(is.na(temp[1, i])) nacount1 <- nacount1 + 1
#  if(is.na(temp[2, i])) nacount2 <- nacount2 + 1
#}
#print(c(nacount1, nacount2))
##########################################
#for(i in 1:length(names(temp))) {
#  if(!is.na(temp[1, i])) if(!is.na(temp[2, i]) & (temp[1, i] != temp[2, i])) temp[1, i] <- temp[2, i]
#}
#for(i in 1:length(names(temp))) {
#  if(is.na(temp[1, i])) if(!is.na(temp[2, i])) temp[1, i] <- temp[2, i]
#}
#final <- subset(final, TCode !="t0008313")
#final <- merge(final, temp[1,], by = intersect(names(final), names(temp)), all = TRUE)
#summary(as.factor(final$TCode))


########################## Deal with the other dublicates in the same way as above. ##########################

############### Merging ADMIN data ########################################################################
final11 <- merge(final, phadmin2017, by = "TCode", all = TRUE)
final <- final11

#### This section below was added on March 22, 2017 version 2.
summary(as.factor(final$PH_or_S8))
revisedAdmin16 <- read_csv("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BoulderHousingPartnersData/Data/BHP-Admin_records/2017_HC_Admin_revisedMarch06_2017.csv", skip = 1)
for(i in 1:length(final$TCode)) {
  if(final$TCode[i] %in% revisedAdmin16$Resident) final$PH_or_S8[i] <- "PH"
}
final$PH_or_S8 <- as.factor(final$PH_or_S8)
summary(final$PH_or_S8)
##### Section below was commented out on March 22, 2017 version 2.
#for(i in 1:length(final$TCode)) {
#  if(!is.na(final$From[i]) & final$From[i]=="phadmin2017") final$PH_or_S8[i] <- NA
#}

#setdiff(final$TCode, intersect(final$TCode, ph2017$TCode)) ## TCodes that are there in final but not in ph2017.
#setdiff(ph2017$TCode, intersect(final$TCode, ph2017$TCode)) ## TCodes that are there in ph2017 but not in final.


temp <- c("X",
          "SSHousing_14",
          "SSEmployment_14",
          "SSIncome_14",
          "SSFood_14",
          "SSTransportation_14",
          "SSChildcare_14",
          "SSSupportNetwork_14",
          "SSLegal_14",
          "SSHealth_14",
          "SSEducation_14",
          "SSWhole_14",
          "BarrierHousing_14",
          "BarrierToBecomingEmployed_14",
          "BarrierToWorkingMoreHours_14",
          "BarrierFood_14",
          "BarrierTransportation_14",
          "BarrierChildcare_14",
          "BarrierSupportNetwork_14",
          "BarrierLegal_14",
          "BarrierEducation_14",
          "Surveyed_14",
          "SSLanguage_14",
          "Recd.14",
          "Scanned14",
          "Sent.to.CU14",
          "Status14",
          "Household.Member14",
          "Mailing.Zip14",
          "Move.In14",
          "Move.Out14",
          "Member.Age...as.of.date14",
          "Effective.Date14",
          "Admission.Date14",
          "Total.Annual.Income14",
          "Special.Program.Setting14",
          "Member.Citizen14",
          "Member.Sex14",
          "Tenant.Rent14",
          "TTP14",
          "Member.Ethnicity14",
          "Bedrooms14",
          "Family.Members14",
          "Eligible.Members14",
          "Dependants14",
          "HH.Members.under.age.1314",
          "Student.Houshold.Members14",
          "Non.Students.Ages.18.14",
          "Single.Parent.HH14",
          "From14",
          "Property14",
          "First.Request14",
          "Second.Request14",
          "Final.Request14",
          "Comments14",
          "Property",
          "First.Request15",
          "Second.Request15",
          "Final.Request15",
          "Recd.15",
          "Scanned15",
          "Sent.to.CU15",
          "Comments15",
          "Status15",
          "Household.Member15",
          "Mailing.Zip15",
          "Move.In15",
          "Move.Out15",
          "Member.Age...as.of.date15",
          "Effective.Date15",
          "Admission.Date15",
          "Total.Annual.Income15",
          "Special.Program.Setting15",
          "Member.Citizen15",
          "Member.Sex15",
          "Tenant.Rent15",
          "TTP15",
          "Member.Ethnicity15",
          "Bedrooms15",
          "Family.Members15",
          "Eligible.Members15",
          "Dependants15",
          "HH.Members.under.age.1315",
          "Student.Household.Members15",
          "Non.Students.Ages.18.15",
          "Single.Parent.HH15",
          "Spouse15",
          "X.115")

## The following lines of code prints to see if any of the above list of column names can be deleted or not.
for(i in 1:length(temp)) {
  print(paste0("final11$", temp[i], sep=""))
  print(summary(as.factor(eval(parse(text = paste0("final11$", temp[i], sep=""))))))
  print("**********************************************")
}


out <- function(a, b) {
  if(is.na(a)) {
    if(is.na(b)) temp <- NA
    else temp <- b
  }
  else {
    if(is.na(b)) {
      temp <- a
    }
    else {
      if(a %in% c("Yes", 2) | b %in% c("Yes", 2)) {
        temp <- "Yes"
      }
      else temp <- "No"
    }
  }
  return(temp)
}

################################# 2014  ######################################################
for(i in 1:length(final$TCode)) {
  final$Q8_1_14[i] <- out(as.character(final$Q8_1_1_14[i]), as.character(final$Q8_2_1_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_2_14[i] <- out(as.character(final$Q8_1_2_14[i]), as.character(final$Q8_2_2_14[i]))
} 
for(i in 1:length(final$TCode)) {
  final$Q8_3_14[i] <- out(as.character(final$Q8_1_3_14[i]), as.character(final$Q8_2_3_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_4_14[i] <- out(as.character(final$Q8_1_4_14[i]), as.character(final$Q8_2_4_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_5_14[i] <- out(as.character(final$Q8_1_5_14[i]), as.character(final$Q8_2_5_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_6_14[i] <- out(as.character(final$Q8_1_6_14[i]), as.character(final$Q8_2_6_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_7_14[i] <- out(as.character(final$Q8_1_7_14[i]), as.character(final$Q8_2_7_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_8_14[i] <- out(as.character(final$Q8_1_8_14[i]), as.character(final$Q8_2_8_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_9_14[i] <- out(as.character(final$Q8_1_9_14[i]), as.character(final$Q8_2_9_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_10_14[i] <- out(as.character(final$Q8_1_10_14[i]), as.character(final$Q8_2_10_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_11_14[i] <- out(as.character(final$Q8_1_11_14[i]), as.character(final$Q8_2_11_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_12_14[i] <- out(as.character(final$Q8_1_12_14[i]), as.character(final$Q8_2_12_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_13_14[i] <- out(as.character(final$Q8_1_13_14[i]), as.character(final$Q8_2_13_14[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_14_14[i] <- out(as.character(final$Q8_1_14_14[i]), as.character(final$Q8_2_14_14[i]))
}

################################# 2015  ######################################################
for(i in 1:length(final$TCode)) {
  final$Q8_1_15[i] <- out(as.character(final$Q8_1_1_15[i]), as.character(final$Q8_2_1_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_2_15[i] <- out(as.character(final$Q8_1_2_15[i]), as.character(final$Q8_2_2_15[i]))
} 
for(i in 1:length(final$TCode)) {
  final$Q8_3_15[i] <- out(as.character(final$Q8_1_3_15[i]), as.character(final$Q8_2_3_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_4_15[i] <- out(as.character(final$Q8_1_4_15[i]), as.character(final$Q8_2_4_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_5_15[i] <- out(as.character(final$Q8_1_5_15[i]), as.character(final$Q8_2_5_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_6_15[i] <- out(as.character(final$Q8_1_6_15[i]), as.character(final$Q8_2_6_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_7_15[i] <- out(as.character(final$Q8_1_7_15[i]), as.character(final$Q8_2_7_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_8_15[i] <- out(as.character(final$Q8_1_8_15[i]), as.character(final$Q8_2_8_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_9_15[i] <- out(as.character(final$Q8_1_9_15[i]), as.character(final$Q8_2_9_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_10_15[i] <- out(as.character(final$Q8_1_10_15[i]), as.character(final$Q8_2_10_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_11_15[i] <- out(as.character(final$Q8_1_11_15[i]), as.character(final$Q8_2_11_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_12_15[i] <- out(as.character(final$Q8_1_12_15[i]), as.character(final$Q8_2_12_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_13_15[i] <- out(as.character(final$Q8_1_13_15[i]), as.character(final$Q8_2_13_15[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_14_15[i] <- out(as.character(final$Q8_1_14_15[i]), as.character(final$Q8_2_14_15[i]))
}

################################# 2017 ######################################################
for(i in 1:length(final$TCode)) {
  final$Q8_1_17[i] <- out(as.character(final$Q8_1_1_17[i]), as.character(final$Q8_2_1_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_2_17[i] <- out(as.character(final$Q8_1_2_17[i]), as.character(final$Q8_2_2_17[i]))
} 
for(i in 1:length(final$TCode)) {
  final$Q8_3_17[i] <- out(as.character(final$Q8_1_3_17[i]), as.character(final$Q8_2_3_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_4_17[i] <- out(as.character(final$Q8_1_4_17[i]), as.character(final$Q8_2_4_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_5_17[i] <- out(as.character(final$Q8_1_5_17[i]), as.character(final$Q8_2_5_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_6_17[i] <- out(as.character(final$Q8_1_6_17[i]), as.character(final$Q8_2_6_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_7_17[i] <- out(as.character(final$Q8_1_7_17[i]), as.character(final$Q8_2_7_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_8_17[i] <- out(as.character(final$Q8_1_8_17[i]), as.character(final$Q8_2_8_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_9_17[i] <- out(as.character(final$Q8_1_9_17[i]), as.character(final$Q8_2_9_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_10_17[i] <- out(as.character(final$Q8_1_10_17[i]), as.character(final$Q8_2_10_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_11_17[i] <- out(as.character(final$Q8_1_11_17[i]), as.character(final$Q8_2_11_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_12_17[i] <- out(as.character(final$Q8_1_12_17[i]), as.character(final$Q8_2_12_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_13_17[i] <- out(as.character(final$Q8_1_13_17[i]), as.character(final$Q8_2_13_17[i]))
}
for(i in 1:length(final$TCode)) {
  final$Q8_14_17[i] <- out(as.character(final$Q8_1_14_17[i]), as.character(final$Q8_2_14_17[i]))
}

makefactor <- function(q, year) {
  x <- paste0("final$Q8_", as.character(q), year, sep = "")
  x1 <- eval(parse(text = x))
  x2 <- as.factor(x1)
  return(x2)
}

for(year in c("_14", "_15", "_17")) {
  for(q in 1:14) {
    x <- paste0("Q8_", as.character(q), year, sep = "")
    final[[x]] <- makefactor(q, year)
  }
}

summary(final$Q8_2_17)
summary(final$Q8_1_2_17)
summary(final$Q8_2_2_17)
#####################################################################################################
#####################################################################################################
#####################################################################################################

#### Make sure the admin data for income and Rent do not have commas within the digits.
#################################################################################################

final$Total.Annual.Income_17 <- gsub(",", "", final$Total.Annual.Income_17)
final$Total.Annual.Income_17 <- as.numeric(final$Total.Annual.Income_17)

final$Tenant.Rent_17 <- gsub(",", "", final$Tenant.Rent_17)
final$Tenant.Rent_17 <- as.numeric(final$Tenant.Rent_17)



for(i in 1:length(final)) {
  if(is.factor(final[[i]])) {
    if("Not applicable" %in% levels(final[[i]])) {
      final[[i]] <- ordered(final[[i]],
                            levels = levels(final[[i]])[levels(final[[i]]) != "Not applicable"])
    }
    if("I don't use" %in% levels(final[[i]])) {
      final[[i]] <- ordered(final[[i]],
                            levels = levels(final[[i]])[levels(final[[i]]) != "I don't use"])
    }
  }
}


########################################################################################
##### Remove the TCodes that have not participated in either rounds of the survey  #####
########################################################################################

for(n in 1:length(final$TCode)){
  final$total.answered[n] <- 0
  for(i in 1:length(names(final))) {
    if(grepl("^Q", names(final)[i], ignore.case = FALSE, perl = TRUE) & !is.na(final[n, i])) final$total.answered[n] <- final$total.answered[n] + 1
  }
}

for(n in 1:length(final$TCode)){
  final$total.answered_15[n] <- 0
  for(i in 1:length(names(final))) {
    if(grepl("^Q", names(final)[i], ignore.case = FALSE, perl = TRUE) & grepl("_15$", names(final)[i], ignore.case = FALSE, perl = TRUE) & !is.na(final[n, i])) final$total.answered_15[n] <- final$total.answered_15[n] + 1
  }
}


for(n in 1:length(final$TCode)){
  final$total.answered_14[n] <- 0
  for(i in 1:length(names(final))) {
    if(grepl("^Q", names(final)[i], ignore.case = FALSE, perl = TRUE) & grepl("_14$", names(final)[i], ignore.case = FALSE, perl = TRUE) & !is.na(final[n, i])) final$total.answered_14[n] <- final$total.answered_14[n] + 1
  }
}





for(n in 1:length(final$TCode)){
  final$total.answered_17[n] <- 0
  for(i in 1:length(names(final))) {
    if(grepl("^Q", names(final)[i], ignore.case = FALSE, perl = TRUE) & grepl("_17$", names(final)[i], ignore.case = FALSE, perl = TRUE) & !is.na(final[n, i])) final$total.answered_17[n] <- final$total.answered_17[n] + 1
  }
}



temp1 <- subset(final, final$total.answered==0)
temp1 <- subset(temp1, !is.na(temp1$PH_or_S8))

toRemove <- temp1$TCode

## Lets assign final$PH_or_S8 to NA for those that do not have any data for survey questions. This section commented out on march 21, 2017.

#for(i in 1:length(final$TCode)) {
#  if(final$TCode[i] %in% toRemove) final$PH_or_S8[i] <- NA
#}

summary(final$Surveyed_14)

final$Surveyed_17 <- NA
final$Surveyed_17[final$total.answered_17>0] <- "Yes"
final$Surveyed_14 <- NA
final$Surveyed_14[final$total.answered_14>0] <- "Yes"
final$Surveyed_15 <- NA
final$Surveyed_15[final$total.answered_15>0] <- "Yes"

summary(as.factor(final$Surveyed_14))
summary(as.factor(final$Surveyed_15))
summary(as.factor(final$Surveyed_17))
N_core_group <- subset(final, Surveyed_14=="Yes" & Surveyed_17=="Yes" & PH_or_S8 == "PH")

ggplot(final, aes(total.answered_14)) + geom_histogram(color="white", binwidth = 1) + coord_cartesian(xlim=c(1,20))
ggplot(final, aes(total.answered_15)) + geom_histogram(color="white", binwidth = 10)
ggplot(final, aes(total.answered_17)) + geom_histogram(color="white", binwidth = 10)
ggplot(final, aes(total.answered)) + geom_histogram(color="white", binwidth = 10)

summary(final$total.answered_17)

#temp <- subset(final, (total.answered_15>0 & total.answered_15<10)) # | (total.answered_14>0 & total.answered_14<17))
#for(n in 1:length(temp$TCode)) {
#  for(i in 1:length(names(temp))) {
#    if(
#      !is.na(temp[n, i]) & 
#      grepl("^Q", names(temp)[i], ignore.case = FALSE, perl = TRUE) & 
#      grepl("_15$", names(temp)[i], ignore.case = FALSE, perl = TRUE)) print(c(temp$TCode[n], names(temp)[i], temp[n, i]))
#  }
#}



xtabs(formula = ~ final$Surveyed_14 + final$PH_or_S8)
xtabs(formula = ~ final$Surveyed_17 + final$PH_or_S8)
xtabs(formula = ~ final$Surveyed_17 + final$From_17)
xtabs(formula = ~ final$Surveyed_17 + final$From_17, drop.unused.levels = FALSE)

final$PH_or_S8[final$TCode=="t0009249"] <- "PH"

##### For those TCodes who had zero annual income in 2014 based on original admin data but had non-zero income in the revised data, correct the income.

updatedIncome <- read_csv("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BoulderHousingPartnersData/Data/BHP-Admin_records/outMovers2014IncomeRevised.csv")
updatedIncome$old <- NA

for(i in 1:length(updatedIncome$X1)) {
  updatedIncome$old[i] <- final$Total.Annual.Income_14[final$TCode==updatedIncome$X1[i]]
}


for(i in 1:length(final$TCode)) {
  if(!is.na(final$Total.Annual.Income14[i]) & final$Total.Annual.Income14[i]==0) {
    if(final$TCode[i] %in% updatedIncome$X1) final$Total.Annual.Income14[i] <- updatedIncome$`Annual income`[updatedIncome$X1==final$TCode[i]]
  }
}
final$Total.Annual.Income14[final$TCode=="t0000771"] <- 0

for(i in 1:length(final$TCode)) {
  if(!is.na(final$Total.Annual.Income_14[i]) & final$Total.Annual.Income_14[i]==0) {
    if(final$TCode[i] %in% updatedIncome$X1) final$Total.Annual.Income_14[i] <- updatedIncome$`Annual income`[updatedIncome$X1==final$TCode[i]]
  }
}
final$Total.Annual.Income_14[final$TCode=="t0000771"] <- 0



ques <- read.csv("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BHP-New/BHP-2015-roundData/ListOfQues&Ns.csv", stringsAsFactors=FALSE)
names(ques)[1] <- "Q"
ques$Q <- gsub(" ", "", ques$Q, fixed = TRUE)
names(final) <- gsub(" ", "", names(final), fixed = TRUE)

for(j in 1:length(ques$Q)) {
  x <- grep(paste0(ques$Q[j], "_", sep = ""), names(final))
  for(i in 1:length(x)) attr(final[ ,x[i]], "description") <- (ques$description[j])
}

temp <- data.frame(names(final), d=c(1:length(names(final))))
temp$d <- NA
for(i in 1:length(names(final))) {
  if(length(attr(final[,i], "description"))!=0) temp$d[i] <- attr(final[,i], "description")
}

#save(final, file="/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BHP-New/BHP-2017-roundData/combined-Sept8-2017.RData")

subset(temp, is.na(temp$d))



final1 <- subset(final, From_17 == "ph2017" | From_14 == "all2014.RData")
summary(final1$PH_or_S8)




