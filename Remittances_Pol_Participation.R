#Project: International Remittances and Political Participation
#Authors: Anaman, Allor and Kuffuor (2022)

#------------------------------------
# Preliminaries                   ####
set.seed(1234)
setwd("~/")
library(pacman)
library(pacman)
library(lmtest)
library(rbounds)
library(rgenoud)
library(survey)
library(tableone)
library(dplyr)
library(Matching)
library(cobalt)
p_load(mediation)
p_load(haven, janitor, here, tidyverse, readxl, tableone,Hmisc, MatchIt, cobalt, Zelig, rbounds, EValue, sandwich)


if(FALSE) {
round6_7 <- read_dta("/Users/preciousallor/Desktop/george/new_final.dta")
read.delim("/Users/preciousallor/Desktop/george/new_final.dta")
head(round6_7)
newdata <-write.csv(round6_7, file="/Users/preciousallor/Desktop/george/new_data.csv")
head(newdata)
datanew <- read.csv("/Users/preciousallor/Desktop/george/new_data.csv")
head(datanew)
Datanew1=datanew[,-1]
head(Datanew1)
}



#STEP 1i:Creating Unmatched and Matched Sample using the Genetic Matching Prodecure 
datanew <- read.csv("Desktop/Media & Remittances/Data/R codes/datanew2.csv")
head(datanew)
Datanew1=datanew[,-1]
head(Datanew1)
Data.complete3 <-subset(a_lot, select = -c(CampaignRally, Vote, PolContact, Protest, JoinOthers))
head(Data.complete3)

Data.complete2 <- na.omit(Data.complete3)
Data.complete2$employstat =as.factor(Data.complete2$employstat)
Data.complete2$remittance =as.factor(Data.complete2$remittance)
Data.complete2$REGION=as.factor(Data.complete2$REGION)
Data.complete2$commengage =as.factor(Data.complete2$commengage)
Data.complete2$survey =as.factor(Data.complete2$survey)

Tr2 <- Data.complete2$a_lot

table(Data.complete2$a_lot)
table(Data.complete1$somewhat)
table(a_little$a_little_bit)
#Running Logit Model as the baseline Regression
#mylogit <- glm(MediaContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
               #+commengage+REGION, data =Data.complete, family = "binomial", weights= ~withinwt)

#summary(mylogit)

#Creating Table 1: Baseline Characteristics of Respondents before and after Matching
gen.match <- matchit(Tr2 ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey, data=Data.complete2, distance="glm", link="logit", method ="genetic",estimand = "ATT", pop.size=120, s.weights= ~withinwt)
gen.match
summary(gen.match)
bal.tab(gen.match, m.threshold = 0.25, un = TRUE) 


#The VR is the ratio of variance in the treatment and control groups. A VR close to 1.0 indicates a balance in the covariate, whereas a VR <0.5 or >2.0 are considered “too extreme” (57).
bal.tab(gen.match, v.threshold = 2)
plot(gen.match, type = "jitter", interactive = FALSE)
plot(gen.match, type = 'hist')
#love.plot(bal.tab(gen.match, m.threshold=0.25),stat = "mean.diffs", grid=TRUE, stars="raw", abs = F)
#love.plot(bal.tab(gen.match, v.threshold=2),stat = "variance.ratios", grid=TRUE, stars="raw", abs = F)


V <- data.frame(old=c("REGION", "AdultHH", "Age","commengage", "Male", "Urban","Edulevel", "employstat","OwnPhone",
                      "AssetIndex", "EvalPolSyst", "PolAffinity","lpi", "MediaExp_Radio_TV_News", "PolInterest","survey"),
                new=c("Region", "Adult Household Size(persons)", "Age", "Community group", "Male", "Urban", "Education",
                      "Employment status", "Own Mobile Phone", "Asset Index", "Evaluation of Political System", "Affinity to Political Party",
                     "Lived Poverty Index", "Media Exposure", "Discuss Political Matters", "Survey"))

love.plot((gen.match), m.threshold=0.25,stat = "mean.diffs", grid=TRUE, binary="std", abs = F
          , title = "Figure 4: Covariate Balance for Genetic Matching", var.names = V, sample.names = c("Pre-match", "Post-match"))

data.matched <- match.data(gen.match)
pre.balance <- summary(gen.match)$sum.all
post.balance <- summary(gen.match)$sum.match
gen.data <- match.data(gen.match)
head(gen.data)


############################USING MATCHIT FOR NNM MATCHING with sample weight; Propensity Score without replacement#################################
nearest.match2 <- matchit(Tr2 ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey, data=Data.complete2, distance="glm", link="logit", method ="nearest",estimand = "ATT", s.weights= ~withinwt, replace=FALSE)

nearest.match2
summary(nearest.match2)

bal.tab(nearest.match, m.threshold = 0.25, un = TRUE) #0.25
#The VR is the ratio of variance in the treatment and control groups. A VR close to 1.0 indicates a balance in the covariate, whereas a VR <0.5 or >2.0 are considered “too extreme” (57).
bal.tab(nearest.match, v.threshold = 2)
plot(nearest.match, type = "jitter", interactive = FALSE)
plot(nearest.match, type = 'hist')

love.plot(bal.tab(nearest.match, m.threshold=0.25),stat = "mean.diffs", grid=TRUE,stars="raw", abs = F
          , labels = TRUE,var.names = V, sample.names = c("Pre-match", "Post-match"))

data.matched2 <- match.data(nearest.match2)
pre.balance2 <- summary(nearest.match2)$sum.all
post.balance2 <- summary(nearest.match2)$sum.match
nearest.data2 <- match.data(nearest.match2)
head(nearest.data2)


############################USING MATCHIT FOR OPTIMAL (1:2 matching) with sample weight; Propensity Score without replacement#################################
optimal.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="optimal",estimand = "ATT", s.weights= ~withinwt, ratio=2)

summary(optimal.match)
bal.tab(optimal.match, m.threshold = 0.25, un = TRUE) 
#The VR is the ratio of variance in the treatment and control groups. A VR close to 1.0 indicates a balance in the covariate, whereas a VR <0.5 or >2.0 are considered “too extreme” (57).
bal.tab(optimal.match, v.threshold = 2)
plot(optimal.match, type = "jitter", interactive = FALSE)
plot(optimal.match, type = 'hist')

#Optimal results shows one variable is not balanced with 1:1 ratio
love.plot(bal.tab(optimal.match, m.threshold=0.25),binary="std", grid=TRUE, abs = F
          , labels = TRUE, drop.distance=TRUE, var.names = V, sample.names = c("Pre-match", "Post-match"))

optimal.data <- match.data(optimal.match)



############################USING MATCHIT FOR Full matching (1:1 matching) with sample weight; Propensity Score#################################
Full.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="full",estimand = "ATT", s.weights= ~withinwt)
summary(Full.match)
bal.tab(Full.match, m.threshold = 0.25, un = TRUE) 
#The VR is the ratio of variance in the treatment and control groups. A VR close to 1.0 indicates a balance in the covariate, whereas a VR <0.5 or >2.0 are considered “too extreme” (57).
bal.tab(Full.match, v.threshold = 2)
plot(Full.match, type = "jitter", interactive = FALSE)
plot(Full.match, type = 'hist')
#love.plot(bal.tab(Full.match, m.threshold=0.25),stat = "mean.diffs", grid=TRUE, stars="raw", abs = F)
#love.plot(bal.tab(Full.match, v.threshold=2),stat = "variance.ratios", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(Full.match, m.threshold=0.25),stat = "mean.diffs", grid=TRUE, stars="raw", abs = F
          , labels = TRUE,  var.names = V, sample.names = c("Pre-match", "Post-match"))

love.plot((Full.match),binary="std",m.threshold=0.25, grid=TRUE, abs = F
          , labels = TRUE,  var.names = V, sample.names = c("Pre-match", "Post-match"))

Full.data <- match.data(Full.match)



#####Unifying all the Matching Methods
bal.tab(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey, data=Data.complete, weights=list(nn=nearest.match, optimal=optimal.match, full=Full.match, genetic=gen.match))
        
love.plot(gen.match, stats = "m", 
          weights = list(nn = nearest.match, optimal=optimal.match, Full.match),
          drop.distance = TRUE, thresholds = c(m = .25),
          binary = "std",
          shapes = c("triangle", "square","circle", "diamond", "star"), 
          colors = c("red","darkgreen", "black", "purple"),
          sample.names = c("Pre-match", "Genetic Matching", "Nearest Neighbor Matching", "Optimal Matching","Full Matching"),
          position = "right", var.names = V, grid = TRUE, stars="raw", title="Figure 5: Covariate Balance | Comparing GenMatch to Other Matching Methods")



###### A model-based estimate of the ATT for Genetic Matching
gen.model <- glm(MediaContact ~ a_lot+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                 +commengage+REGION+survey,data = gen.data, weights=weights )

summary(gen.model)

###### A model-based estimate of the ATT for NNM Matching
nearest.model2 <- glm(MediaContact ~ a_lot +Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey,data = nearest.data2, weights=weights )
  
summary(nearest.model2)


###### A model-based estimate of the ATT for Optimal Matching
optimal.model <- glm(MediaContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = optimal.data, weights=weights )

summary(optimal.model)



###### A model-based estimate of the ATT for Full Matching
Full.model <- glm(MediaContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                  +commengage+REGION+survey,data = Full.data, weights=weights )
summary(Full.model)


####---------------------------------------------------
###############Table for Appendix: Using the Other Measures of Political Participation - Voting
head(Datanew1)
Data.complete1 <-subset(Datanew1, select = -c(CampaignRally, MediaContact, PolContact, Protest, JoinOthers))
head(Data.complete1)

Data.complete <- na.omit(Data.complete1)
Data.complete$employstat =as.factor(Data.complete$employstat)
Data.complete$remittance =as.factor(Data.complete$remittance)
Data.complete$REGION=as.factor(Data.complete$REGION)
Data.complete$commengage =as.factor(Data.complete$commengage)
Data.complete$survey =as.factor(Data.complete$survey)

Tr <- Data.complete$remittance

gen.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="genetic",estimand = "ATT", pop.size=120, s.weights= ~withinwt)
gen.match
summary(gen.match)
bal.tab(gen.match, m.threshold = 0.25, un = TRUE) 


#The VR is the ratio of variance in the treatment and control groups. A VR close to 1.0 indicates a balance in the covariate, whereas a VR <0.5 or >2.0 are considered “too extreme” (57).
bal.tab(gen.match, v.threshold = 2)
plot(gen.match, type = "jitter", interactive = FALSE)
plot(gen.match, type = 'hist')

V <- data.frame(old=c("REGION", "AdultHH", "Age","commengage", "Male", "Urban","Edulevel", "employstat","OwnPhone",
                      "AssetIndex", "EvalPolSyst", "PolAffinity","lpi", "MediaExp_Radio_TV_News", "PolInterest","survey"),
                new=c("Region", "Adult Household Size(persons)", "Age", "Community group", "Male", "Urban", "Education",
                      "Employment status", "Own Mobile Phone", "Asset Index", "Evaluation of Political System", "Affinity to Political Party",
                      "Lived Poverty Index", "Media Exposure", "Discuss Political Matters", "Survey"))

love.plot((gen.match), m.threshold=0.25,stat = "mean.diffs", grid=TRUE, binary="std", abs = F
          , title = "Figure 4: Covariate Balance for Genetic Matching", var.names = V, sample.names = c("Pre-match", "Post-match"))

data.matched <- match.data(gen.match)
pre.balance <- summary(gen.match)$sum.all
post.balance <- summary(gen.match)$sum.match
gen.data <- match.data(gen.match)
head(gen.data)


############################USING MATCHIT FOR NNM MATCHING with sample weight; Propensity Score without replacement#################################
nearest.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="nearest",estimand = "ATT", s.weights= ~withinwt, replace=FALSE)
nearest.match
summary(nearest.match)

bal.tab(nearest.match, m.threshold = 0.25, un = TRUE) #0.25
#The VR is the ratio of variance in the treatment and control groups. A VR close to 1.0 indicates a balance in the covariate, whereas a VR <0.5 or >2.0 are considered “too extreme” (57).
bal.tab(nearest.match, v.threshold = 2)
plot(nearest.match, type = "jitter", interactive = FALSE)
plot(nearest.match, type = 'hist')

love.plot(bal.tab(nearest.match, m.threshold=0.25),stat = "mean.diffs", grid=TRUE,stars="raw", abs = F
          , labels = TRUE,var.names = V, sample.names = c("Pre-match", "Post-match"))

data.matched <- match.data(nearest.match)
pre.balance <- summary(nearest.match)$sum.all
post.balance <- summary(nearest.match)$sum.match
nearest.data <- match.data(nearest.match)
head(nearest.data)


############################USING MATCHIT FOR OPTIMAL (1:2 matching) with sample weight; Propensity Score without replacement#################################
optimal.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="optimal",estimand = "ATT", s.weights= ~withinwt, ratio=2)

summary(optimal.match)
bal.tab(optimal.match, m.threshold = 0.25, un = TRUE) 
#The VR is the ratio of variance in the treatment and control groups. A VR close to 1.0 indicates a balance in the covariate, whereas a VR <0.5 or >2.0 are considered “too extreme” (57).
bal.tab(optimal.match, v.threshold = 2)
plot(optimal.match, type = "jitter", interactive = FALSE)
plot(optimal.match, type = 'hist')

#Optimal results shows one variable is not balanced with 1:1 ratio
love.plot(bal.tab(optimal.match, m.threshold=0.25),binary="std", grid=TRUE, abs = F
          , labels = TRUE, drop.distance=TRUE, var.names = V, sample.names = c("Pre-match", "Post-match"))

optimal.data <- match.data(optimal.match)



############################USING MATCHIT FOR Full matching (1:1 matching) with sample weight; Propensity Score#################################
Full.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="full",estimand = "ATT", s.weights= ~withinwt)
summary(Full.match)
bal.tab(Full.match, m.threshold = 0.25, un = TRUE) 
#The VR is the ratio of variance in the treatment and control groups. A VR close to 1.0 indicates a balance in the covariate, whereas a VR <0.5 or >2.0 are considered “too extreme” (57).
bal.tab(Full.match, v.threshold = 2)
plot(Full.match, type = "jitter", interactive = FALSE)
plot(Full.match, type = 'hist')
#love.plot(bal.tab(Full.match, m.threshold=0.25),stat = "mean.diffs", grid=TRUE, stars="raw", abs = F)
#love.plot(bal.tab(Full.match, v.threshold=2),stat = "variance.ratios", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(Full.match, m.threshold=0.25),stat = "mean.diffs", grid=TRUE, stars="raw", abs = F
          , labels = TRUE,  var.names = V, sample.names = c("Pre-match", "Post-match"))

love.plot((Full.match),binary="std",m.threshold=0.25, grid=TRUE, abs = F
          , labels = TRUE,  var.names = V, sample.names = c("Pre-match", "Post-match"))

Full.data <- match.data(Full.match)


###### A model-based estimate of the ATT for Genetic Matching
gen.model <- glm(Vote ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                 +commengage+REGION+survey,data = gen.data, weights=weights )

summary(gen.model)

###### A model-based estimate of the ATT for NNM Matching
nearest.model <- glm(Vote ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = nearest.data, weights=weights )

summary(nearest.model)


###### A model-based estimate of the ATT for Optimal Matching
optimal.model <- glm(Vote ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = optimal.data, weights=weights )

summary(optimal.model)



###### A model-based estimate of the ATT for Full Matching
Full.model <- glm(Vote ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                  +commengage+REGION+survey,data = Full.data, weights=weights )
summary(Full.model)






####---------------------------------------------------
###############Table for Appendix: Using the Other Measures of Political Participation - Protest
datanew <- read.csv("Desktop/Media & Remittances/Data/R codes/datanew2.csv")
head(datanew)
Datanew1=datanew[,-1]
head(Datanew1)
Data.complete1 <-subset(Datanew1, select = -c(CampaignRally, Vote, PolContact, MediaContact, JoinOthers))
head(Data.complete1)

Data.complete <- na.omit(Data.complete1)
Data.complete$employstat =as.factor(Data.complete$employstat)
Data.complete$remittance =as.factor(Data.complete$remittance)
Data.complete$REGION=as.factor(Data.complete$REGION)
Data.complete$commengage =as.factor(Data.complete$commengage)
Data.complete$survey =as.factor(Data.complete$survey)

Tr <- Data.complete$remittance


gen.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="genetic",estimand = "ATT", pop.size=120, s.weights= ~withinwt)
gen.match
summary(gen.match)
gen.data <- match.data(gen.match)
head(gen.data)


############################USING MATCHIT FOR NNM MATCHING with sample weight; Propensity Score without replacement#################################
nearest.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="nearest",estimand = "ATT", s.weights= ~withinwt, replace=FALSE)
nearest.match

nearest.data <- match.data(nearest.match)
head(nearest.data)


############################USING MATCHIT FOR OPTIMAL (1:2 matching) with sample weight; Propensity Score without replacement#################################
optimal.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="optimal",estimand = "ATT", s.weights= ~withinwt, ratio=2)

summary(optimal.match)

optimal.data <- match.data(optimal.match)



############################USING MATCHIT FOR Full matching (1:1 matching) with sample weight; Propensity Score#################################
Full.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="full",estimand = "ATT", s.weights= ~withinwt)
summary(Full.match)
Full.data <- match.data(Full.match)



#A Model-based estimate for ATT for GenMatch
gen.model <- glm(Protest ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                 +commengage+REGION+survey,data = gen.data, weights=weights )

summary(gen.model)



###### A model-based estimate of the ATT for NNM Matching
nearest.model <- glm(Protest ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = nearest.data, weights=weights )

summary(nearest.model)


###### A model-based estimate of the ATT for Optimal Matching
optimal.model <- glm(Protest ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = optimal.data, weights=weights )

summary(optimal.model)


Full.model <- glm(Protest ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                  +commengage+REGION+survey,data = Full.data, weights=weights )
summary(Full.model)



















####---------------------------------------------------
###############Table for Appendix: Using the Other Measures of Political Participation - Join Others
datanew <- read.csv("Desktop/Media & Remittances/Data/R codes/datanew2.csv")
head(datanew)
Datanew1=datanew[,-1]
head(Datanew1)
Data.complete1 <-subset(Datanew1, select = -c(CampaignRally, Vote, PolContact, MediaContact, Protest))
head(Data.complete1)

Data.complete <- na.omit(Data.complete1)
Data.complete$employstat =as.factor(Data.complete$employstat)
Data.complete$remittance =as.factor(Data.complete$remittance)
Data.complete$REGION=as.factor(Data.complete$REGION)
Data.complete$commengage =as.factor(Data.complete$commengage)
Data.complete$survey =as.factor(Data.complete$survey)

Tr <- Data.complete$remittance


gen.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="genetic",estimand = "ATT", pop.size=120, s.weights= ~withinwt)
gen.match
summary(gen.match)
gen.data <- match.data(gen.match)
head(gen.data)


############################USING MATCHIT FOR NNM MATCHING with sample weight; Propensity Score without replacement#################################
nearest.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="nearest",estimand = "ATT", s.weights= ~withinwt, replace=FALSE)
nearest.match

nearest.data <- match.data(nearest.match)
head(nearest.data)


############################USING MATCHIT FOR OPTIMAL (1:2 matching) with sample weight; Propensity Score without replacement#################################
optimal.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="optimal",estimand = "ATT", s.weights= ~withinwt, ratio=2)

summary(optimal.match)

optimal.data <- match.data(optimal.match)



############################USING MATCHIT FOR Full matching (1:1 matching) with sample weight; Propensity Score#################################
Full.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="full",estimand = "ATT", s.weights= ~withinwt)
summary(Full.match)
Full.data <- match.data(Full.match)



#A Model-based estimate for ATT for GenMatch
gen.model <- glm(JoinOthers ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                 +commengage+REGION+survey,data = gen.data, weights=weights )

summary(gen.model)



###### A model-based estimate of the ATT for NNM Matching
nearest.model <- glm(JoinOthers ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = nearest.data, weights=weights )

summary(nearest.model)


###### A model-based estimate of the ATT for Optimal Matching
optimal.model <- glm(JoinOthers ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = optimal.data, weights=weights )

summary(optimal.model)


Full.model <- glm(JoinOthers ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                  +commengage+REGION+survey,data = Full.data, weights=weights )
summary(Full.model)
































































































































############################USING MATCHIT FOR NNM MATCHING with sample weight; Propensity Score without replacement#################################
nearest.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="nearest",estimand = "ATT", s.weights= ~withinwt, replace=FALSE)
nearest.match

nearest.data <- match.data(nearest.match)
head(nearest.data)


############################USING MATCHIT FOR OPTIMAL (1:2 matching) with sample weight; Propensity Score without replacement#################################
optimal.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                         +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="optimal",estimand = "ATT", s.weights= ~withinwt, ratio=2)

summary(optimal.match)

optimal.data <- match.data(optimal.match)



############################USING MATCHIT FOR Full matching (1:1 matching) with sample weight; Propensity Score#################################
Full.match <- matchit(Tr ~ Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey, data=Data.complete, distance="glm", link="logit", method ="full",estimand = "ATT", s.weights= ~withinwt)
summary(Full.match)
Full.data <- match.data(Full.match)



#A Model-based estimate for ATT for GenMatch
gen.model <- glm(PolContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                 +commengage+REGION+survey,data = gen.data, weights=weights )

summary(gen.model)



###### A model-based estimate of the ATT for NNM Matching
nearest.model <- glm(PolContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = nearest.data, weights=weights )

summary(nearest.model)


###### A model-based estimate of the ATT for Optimal Matching
optimal.model <- glm(PolContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = optimal.data, weights=weights )

summary(optimal.model)


Full.model <- glm(PolContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                  +commengage+REGION+survey,data = Full.data, weights=weights )
summary(Full.model)



































































































































































gen.model1 <- glm(Vote ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                 +commengage+REGION+survey,data = gen.data, weights=weights )

summary(gen.model1)

gen.model2 <- glm(PolContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                  +commengage+REGION+survey,data = gen.data, weights=weights )

summary(gen.model2)


gen.model3 <- glm(Protest ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                  +commengage+REGION+survey,data = gen.data, weights=weights )

summary(gen.model3)


gen.model4 <- glm(JoinOthers ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                  +commengage+REGION+survey,data = gen.data, weights=weights )

summary(gen.model4)



#NNM Matching
nearest.model1 <- glm(Vote ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = nearest.data, weights=weights )

summary(nearest.model1)


nearest.model2 <- glm(PolContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey,data = nearest.data, weights=weights )

summary(nearest.model2)


nearest.model3 <- glm(Protest ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey,data = nearest.data, weights=weights )

summary(nearest.model3)


nearest.model4 <- glm(JoinOthers ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey,data = nearest.data, weights=weights )

summary(nearest.model4)


#Optimal Matching
optimal.model1 <- glm(Vote ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                     +commengage+REGION+survey,data = optimal.data, weights=weights )

summary(optimal.model1)


optimal.model2 <- glm(PolContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey,data = optimal.data, weights=weights )

summary(optimal.model2)

optimal.model3 <- glm(Protest ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey,data = optimal.data, weights=weights )

summary(optimal.model3)


optimal.model4 <- glm(JoinOthers ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                      +commengage+REGION+survey,data = optimal.data, weights=weights )

summary(optimal.model4)


#Full Matching
Full.model1 <- glm(Vote ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                  +commengage+REGION+survey,data = Full.data, weights=weights )
summary(Full.model1)

Full.model2 <- glm(PolContact ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                   +commengage+REGION+survey,data = Full.data, weights=weights )
summary(Full.model2)


Full.model3 <- glm(Protest ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                   +commengage+REGION+survey,data = Full.data, weights=weights )
summary(Full.model3)


Full.model4 <- glm(JoinOthers ~ remittance+Age+Male+Edulevel+Urban+AdultHH+employstat+lpi+AssetIndex+OwnPhone+PolAffinity+EvalPolSyst+PolInterest+MediaExp_Radio_TV_News
                   +commengage+REGION+survey,data = Full.data, weights=weights )
summary(Full.model4)





######## SENSITIVITY ANALYSIS FOR THE IMPACT OF UNMEASURED COUNFOUNDERS -WE DO FOR GENETIC MATCHING RESULTS[Sensitivity analysis is conducted on Optimal Match, Nearest Neighbor, and genetic]
#Note: The SensitivityR5 is not yet on CRAN repository.
library(devtools) 
devtools::install_github("Ngendahimana/SensitivityR5") 
library(SensitivityR5)
browseVignettes("SensitivityR5")
library(Matching);library(MatchIt);library(kableExtra)


#Sensitivity Analysis for GenMatch
binObject <- binarysens2(x=gen.match,y ="MediaContact", Gamma=1.7, GammaInc=0.01)
binObject

binObject1 <- binarysens2(x=nearest.match,y ="MediaContact", Gamma=2, GammaInc=0.01)
binObject1


#Sensitivity Analysis for Optimal Matching
binObject2 <- binarysens2(x=optimal.match,y ="MediaContact", Gamma=2, GammaInc=0.01)
binObject2


#Sensitivity Analysis for Full Matching
binObject3 <- binarysens2(x=Full.match,y ="MediaContact", Gamma=1.2, GammaInc=0.01)
binObject3

par(mfrow=c(1,2))
















































##Mediation Analysis
med.fit <-  glm(lpi ~remittance+PolAffinity+commengage+EvalPolSyst+PolInterest+Age+Urban+Male+AssetIndex+ employstat+Edulevel
                +AdultHH+OwnPhone+MediaExp_Radio_TV_News+REGION+survey, data = gen.data, weights=weights )
  
out.fit <- glm(MediaContact ~ remittance+PolAffinity+commengage+EvalPolSyst+PolInterest+Age+Urban+Male+AssetIndex+ employstat+Edulevel
               +AdultHH+OwnPhone+lpi+MediaExp_Radio_TV_News+REGION+survey, data = gen.data, weights=weights )
                 
                 
med.out <- mediate(med.fit, out.fit, treat = "remittance", mediator = "lpi", sims=1000 )
summary(med.out)
plot(med.out)

  
  
  glm(MediaContact ~ remittance*as.factor(Male) + PolAffinity+commengage+CampaignRally+EvalPolSyst+PolInterest+Age+Urban+Male+AssetIndex+ employstat+EduLevel
               +AdultHH+OwnPhone+REGION+survey, family = binomial, data = data.matched2, weights=withinwt)
summary(med.fit)


library(devtools) 
