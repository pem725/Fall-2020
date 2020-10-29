### Analysis script for "Prosociality in an economic game is associated with less parochialism and greater willingness to vote for intergroup compromise"
### Mohsen Mosleh1, Alexander J. Stewart2, Joshua B. Plotkin3, and David G. Rand1,4

##  1Sloan School of Management, Massachusetts Institute of Technology, Cambridge, MA, USA, 2Department of Biology, 
##  University of Houston, Houston, TX, USA, 2Department of Biology, University of Houston, Houston, TX, USA, 
##  3Department of Biology, University of Pennsylvania, Philadelphia, PA, USA, 4Department of Brain and Cognitive Science, Massachusetts Institute of Technology, Cambridge, MA, USA

library(estimatr)
rm(list=ls()) 
data <- read.csv('data.csv')
names(data)

data$age <-as.numeric(as.character(data$age))

data$treatment<-factor( data$treatment)

data$income <- factor(data$income)

data$education <- factor(data$education)

data$country <- factor(data$country)

data$otherTeam_distance_before<-as.numeric(data$otherTeam_distance_before)
data$difi_otherTeam<-scale(data$otherTeam_distance_before)
data$ownTeam_distance_before <- as.numeric(data$ownTeam_distance_before)
data$difi_ownTeam<-scale(data$ownTeam_distance_before)
data$difi_wholeGroup <- data$wholeGroup_distance_before

data$difi_team <-data$ownTeam_distance_before-data$otherTeam_distance_before

data$difi_team_nonScale <-data$difi_team
data$difi_team <-scale(data$difi_team)

data$difi_wholeGroup<-as.numeric(data$difi_wholeGroup)
data$difi_wholeGroup_nonScale <- data$difi_wholeGroup
data$difi_wholeGroup <-scale(data$difi_wholeGroup)


data$dgOffer<-as.numeric(data$dgOffer)
data$dgOffer_nonScale<-as.numeric(data$dgOffer)

data$dgOffer <-scale(data$dgOffer)



data$P_OTwin_early<-as.numeric(data$P_OTwin_early)
## the orginial data is probablity of voting for own team, so we transform it to probalbity of voting for the other team
data$P_OTwin_early<-1 - data$P_OTwin_early


data$P_OTwin_early_nonScale <- data$P_OTwin_early

data$P_OTwin_early <- scale(data$P_OTwin_early )

data$P_OTwin_late <- as.numeric(data$P_OTwin_late)
## the orginial data is probablity of voting for own team, so we transform it to probalbity of voting for the other team

data$P_OTwin_late <- 1-data$P_OTwin_late

data$P_OTwin_late_nonScale <- data$P_OTwin_late
data$P_OTwin_late <- scale(data$P_OTwin_late)


#############################################################################################
# Table 1:   The relationship between parachialism DIFI measure and compromise              #
#                                                                                           #
#############################################################################################

# ----------------------------
# Compromise in early stage  |
#-----------------------------

# model 1
lm_robust(P_OTwin_early~difi_team, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(P_OTwin_early~difi_team+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(P_OTwin_early~difi_team+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(P_OTwin_early~difi_team+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")


# ----------------------------
# Compromise in late stage  |
#-----------------------------

# model 1
lm_robust(P_OTwin_late~difi_team, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(P_OTwin_late~difi_team+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(P_OTwin_late~difi_team+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(P_OTwin_late~difi_team+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")


#############################################################################################
# Table 2:   The relationship between universalism DIFI measure and compromise             #
#                                                                                           #
#############################################################################################

# ----------------------------
# Compromise in early stage  |
#-----------------------------

# model 1
lm_robust(P_OTwin_early~difi_wholeGroup, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(P_OTwin_early~difi_wholeGroup+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(P_OTwin_early~difi_wholeGroup+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(P_OTwin_early~difi_wholeGroup+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")


# ----------------------------
# Compromise in late stage  |
#-----------------------------

# model 1
lm_robust(P_OTwin_late~difi_wholeGroup, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(P_OTwin_late~difi_wholeGroup+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(P_OTwin_late~difi_wholeGroup+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(P_OTwin_late~difi_wholeGroup+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")

#############################################################################################
# Table 3:The relationship between DG giving and parochialism and universalism DIFI measures#
#                                                                                           #
#############################################################################################

# ----------------------------
# Parochialism DIFI measure  |
#-----------------------------

# model 1
lm_robust(difi_team~dgOffer, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(difi_team~dgOffer+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(difi_team~dgOffer+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(difi_team~dgOffer+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")


# ----------------------------
# Universalism DIFI measure  |
#-----------------------------

# model 1
lm_robust(difi_wholeGroup~dgOffer, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(difi_wholeGroup~dgOffer+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(difi_wholeGroup~dgOffer+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(difi_wholeGroup~dgOffer+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")


#############################################################################################
# Table 4:The relationship between DG giving and compromise                                 #
#                                                                                           #
#############################################################################################

# ----------------------------
# Compromise in early stage  |
#-----------------------------

# model 1
lm_robust(P_OTwin_early~dgOffer, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(P_OTwin_early~dgOffer+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(P_OTwin_early~dgOffer+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(P_OTwin_early~dgOffer+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")


# ----------------------------
# Compromise in late stage  |
#-----------------------------

# model 1
lm_robust(P_OTwin_late~dgOffer, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(P_OTwin_late~dgOffer+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(P_OTwin_late~dgOffer+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(P_OTwin_late~dgOffer+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")



#############################################################################################
# Table 5:The relationship between DIFI measures of own team/other team and compromise      #
#                                                                                           #
#############################################################################################

# ----------------------------
# Compromise in early stage  |
#-----------------------------

# model 1
lm_robust(P_OTwin_early~difi_ownTeam*difi_otherTeam, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(P_OTwin_early~difi_ownTeam*difi_otherTeam+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(P_OTwin_early~difi_ownTeam*difi_otherTeam+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(P_OTwin_early~difi_ownTeam*difi_otherTeam+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")


# ----------------------------
# Compromise in late stage  |
#-----------------------------

# model 1
lm_robust(P_OTwin_late~difi_ownTeam*difi_otherTeam, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(P_OTwin_late~difi_ownTeam*difi_otherTeam+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(P_OTwin_late~difi_ownTeam*difi_otherTeam+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(P_OTwin_late~difi_ownTeam*difi_otherTeam+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")



#############################################################################################
# Table 6:The relationship between DIFI measures of own team/other team and giving in DG    #
#                                                                                           #
#############################################################################################

# ----------------------------
# DIFI Own Team              |
#-----------------------------

# model 1
lm_robust(difi_ownTeam~dgOffer, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(difi_ownTeam~dgOffer+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(difi_ownTeam~dgOffer+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(difi_ownTeam~dgOffer+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")


# ----------------------------
# DIFI other Team          |
#-----------------------------

# model 1
lm_robust(difi_otherTeam~dgOffer, data=data, clusters = session,se_type = "stata")

# model 2
lm_robust(difi_otherTeam~dgOffer+gender+age+education+country+income, data=data, clusters = session,se_type = "stata")

# model 3
lm_robust(difi_otherTeam~dgOffer+gender+age+education+country+income+winnerN, data=data, clusters = session,se_type = "stata")

# model 4
lm_robust(difi_otherTeam~dgOffer+gender+age+education+country+income+winnerN+treatment, data=data, clusters = session,se_type = "stata")

