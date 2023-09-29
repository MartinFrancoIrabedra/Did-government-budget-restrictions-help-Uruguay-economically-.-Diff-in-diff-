### Final Paper regressions



### Government_Budget_Value
library(readr)
Government_Budget_Value <- read_csv("~/Desktop/ASemester/Thesis/2Data/2Data_Government_Budget_Value.csv")
View(Government_Budget_Value)

Government_Budget_Value_dates <-as.Date(Government_Budget_Value$DateTime)
Government_Budget_Value_dates
Government_Budget_Value_time <- seq(1,76)
Government_Budget_Value_time
Government_Budget_Value_value <-as.numeric(Government_Budget_Value$Value)
Government_Budget_Value_value
Government_Budget_Value_before_boolean <- Government_Budget_Value$DateTime>="2020-07-31"
Government_Budget_Value_before_boolean
Government_Budget_Value_after_intervention <- ifelse(Government_Budget_Value$DateTime>="2020-07-31",Government_Budget_Value_time-54,0)
Government_Budget_Value_after_intervention


Government_Budget_Value_dataframe<-data.frame(Time = Government_Budget_Value_time,
                                              Value = Government_Budget_Value_value,
                                              Date = Government_Budget_Value_dates,
                                              before = Government_Budget_Value_before_boolean,
                                              after = Government_Budget_Value_after_intervention)

Government_Budget_Value_dataframe



lm_mod_Government_Budget_Value <- lm(Government_Budget_Value_value ~ Government_Budget_Value_time + Government_Budget_Value_before_boolean + Government_Budget_Value_after_intervention, data = Government_Budget_Value_dataframe)
lm_mod_Government_Budget_Value
summary(lm_mod_Government_Budget_Value)

library(tidyverse)
ggplot(data = Government_Budget_Value_dataframe, mapping = aes(x = Government_Budget_Value_dates, y = Government_Budget_Value_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), linetype=1, colour="blue" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1)





### CPI
library(readr)
CPI <- read_csv("~/Desktop/ASemester/Thesis/2Data/2Data_CPI.csv")
View(CPI)

CPI_dates <-as.Date(CPI$DateTime)
CPI_dates
CPI_time <- seq(1,76)
CPI_time
CPI_value <-as.numeric(CPI$Value)
CPI_value
CPI_before_boolean <- CPI$DateTime>="2020-07-31"
CPI_before_boolean
CPI_after_intervention <- ifelse(CPI$DateTime>="2020-07-31",CPI_time-54,0)
CPI_after_intervention


CPI_dataframe<-data.frame(Time = CPI_time,
                          Value = CPI_value,
                          Date = CPI_dates,
                          before = CPI_before_boolean,
                          after = CPI_after_intervention)
CPI_dataframe



lm_mod <- lm(Value ~ Time + before + CPI_after_intervention, data = CPI_dataframe)
lm_mod
summary(lm_mod)

library(ggplot2)
library(tidyverse)
ggplot(data = CPI_dataframe, mapping = aes(x = CPI_dates, y = CPI_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("CPI") + theme_bw()


library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm_mod, pred.labels = c("Intercept", "Time", "Law", "Time after Law"),dv.labels = "CPI", file="CPI.doc")





### Government Spending
library(readr)
Government_Spending <- read_csv("~/Desktop/ASemester/Thesis/2Data/2Data_Government_Spending.csv")
View(Government_Spending)

Government_Spending_dates <-as.Date(Government_Spending$DateTime)
Government_Spending_dates
Government_Spending_time <- seq(1,25)
Government_Spending_time
Government_Spending_value <-as.numeric(Government_Spending$Value)
Government_Spending_value
Government_Spending_before_boolean <- Government_Spending$DateTime>="2020-07-31"
Government_Spending_before_boolean
Government_Spending_after_intervention <- ifelse(Government_Spending$DateTime>"2020-07-31",Government_Spending_time-18,0)
Government_Spending_after_intervention


Government_Spending_dataframe<-data.frame(Time = Government_Spending_time,
                                          Value = Government_Spending_value,
                                          Date = Government_Spending_dates,
                                          before = Government_Spending_before_boolean,
                                          after = Government_Spending_after_intervention)
Government_Spending_dataframe



lm_mod_Government_Spending <- lm(Value ~ Government_Spending_time + Government_Spending_before_boolean + Government_Spending_after_intervention, data = Government_Spending_dataframe)
lm_mod_Government_Spending
summary(lm_mod_Government_Spending)

library(ggplot2)
library(tidyverse)
ggplot(data = Government_Spending_dataframe, mapping = aes(x = Government_Spending_dates, y = Government_Spending_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-06-30"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("Government Spending") + theme_bw()


library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm_mod_Government_Spending, pred.labels = c("Intercept", "Time", "Law", "Time after Law"),dv.labels = "Government Spending", file="lm_mod_Government_Spending.doc")







### GDP growth rate
library(readr)
GDP <- read_csv("~/Desktop/ASemester/Thesis/2Data/2Data_GDP_Growth_from_2016.csv")
View(GDP)

GDP_dates <-as.Date(GDP$DateTime)
GDP_dates
GDP_time <- seq(1,24)
GDP_time
GDP_value <-as.numeric(GDP$Value)
GDP_value
GDP_before_boolean <- GDP$DateTime>="2020-07-31"
GDP_before_boolean
GDP_after_intervention <- ifelse(GDP$DateTime>="2020-07-31",GDP_time-18,0)
GDP_after_intervention


GDP_dataframe<-data.frame(Time = GDP_time,
                          Value = GDP_value,
                          Date = GDP_dates,
                          before = GDP_before_boolean,
                          after = GDP_after_intervention)
GDP_dataframe



lm_mod_GDP <- lm(Value ~ GDP_time + GDP_before_boolean + GDP_after_intervention, data = GDP_dataframe)
lm_mod_GDP
summary(lm_mod_GDP)

library(ggplot2)
library(tidyverse)
ggplot(data = GDP_dataframe, mapping = aes(x = GDP_dates, y = GDP_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("GDP Growth") + theme_bw()


library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm_mod_GDP, pred.labels = c("Intercept", "Time", "Law", "Time after Law"),dv.labels = "GDP Growth", file="lm_mod_GDP.doc")





###  Fiscal Expenditure
library(readr)
Fiscal_Expenditure <- read_csv("Desktop/ASemester/Thesis/2Data/2Data_Fiscal_Expenditure.csv")
View(Fiscal_Expenditure)

Fiscal_Expenditure_dates <-as.Date(Fiscal_Expenditure$DateTime)
Fiscal_Expenditure_dates
Fiscal_Expenditure_time <- seq(1,76)
Fiscal_Expenditure_time
Fiscal_Expenditure_value <-as.numeric(Fiscal_Expenditure$Value)
Fiscal_Expenditure_value
Fiscal_Expenditure_before_boolean <- Fiscal_Expenditure$DateTime>="2020-07-31"
Fiscal_Expenditure_before_boolean
Fiscal_Expenditure_after_intervention <- ifelse(Fiscal_Expenditure$DateTime>="2020-07-31",Fiscal_Expenditure_time-54,0)
Fiscal_Expenditure_after_intervention


Fiscal_Expenditure_dataframe<-data.frame(Time = Fiscal_Expenditure_time,
                                         Value = Fiscal_Expenditure_value,
                                         Date = Fiscal_Expenditure_dates,
                                         before = Fiscal_Expenditure_before_boolean,
                                         after = Fiscal_Expenditure_after_intervention)
Fiscal_Expenditure_dataframe



lm_mod_Fiscal_Expenditure <- lm(Value ~ Fiscal_Expenditure_time + Fiscal_Expenditure_before_boolean + Fiscal_Expenditure_after_intervention, data = Fiscal_Expenditure_dataframe)
lm_mod_GDP
summary(lm_mod_Fiscal_Expenditure)

library(tidyverse)
ggplot(data = Fiscal_Expenditure_dataframe, mapping = aes(x = Fiscal_Expenditure_dates, y = Fiscal_Expenditure_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), linetype=1, colour="blue" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1)







### Inflation
library(readr)
Inflation <- read_csv("Desktop/ASemester/Thesis/2Data/2Data_Inflation_rate.csv")
View(Inflation)

Inflation_dates <-as.Date(Inflation$DateTime)
Inflation_dates
Inflation_time <- seq(1,76)
Inflation_time
Inflation_value <-as.numeric(Inflation$Value)
Inflation_value
Inflation_before_boolean <- Inflation$DateTime>="2020-07-31"
Inflation_before_boolean
Inflation_after_intervention <- ifelse(Inflation$DateTime>"2020-07-31",Inflation_time-54,0)
Inflation_after_intervention


Inflation_dataframe<-data.frame(Time = Inflation_time,
                                Value = Inflation_value,
                                Date = Inflation_dates,
                                before = Inflation_before_boolean,
                                after = Inflation_after_intervention)
Inflation_dataframe



lm_mod_Inflation <- lm(Value ~ Inflation_time + Inflation_before_boolean + Inflation_after_intervention, data = Inflation_dataframe)
lm_mod_Inflation
summary(lm_mod_Inflation)

library(tidyverse)
ggplot(data = Inflation_dataframe, mapping = aes(x = Inflation_dates, y = Inflation_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), linetype=1, colour="blue" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1)








### Unemployment rate
library(readr)
Unemployment <- read_csv("~/Desktop/ASemester/Thesis/2Data/2Data_Unemployment_rate.csv")
View(Unemployment)

Unemployment_dates <-as.Date(Unemployment$DateTime)
Unemployment_dates
Unemployment_time <- seq(1,75)
Unemployment_time
Unemployment_value <-as.numeric(Unemployment$Value)
Unemployment_value
Unemployment_before_boolean <- Unemployment$DateTime>="2020-07-31"
Unemployment_before_boolean
Unemployment_after_intervention <- ifelse(Unemployment$DateTime>"2020-07-31",Unemployment_time-54,0)
Unemployment_after_intervention


Unemployment_dataframe<-data.frame(Time = Unemployment_time,
                                   Value = Unemployment_value,
                                   Date = Unemployment_dates,
                                   before = Unemployment_before_boolean,
                                   after = Unemployment_after_intervention)
Unemployment_dataframe



lm_mod_Unemployment <- lm(Value ~ Unemployment_time + Unemployment_before_boolean + Unemployment_after_intervention, data = Unemployment_dataframe)
lm_mod_Unemployment
summary(lm_mod_Unemployment)

library(ggplot2)
library(tidyverse)
ggplot(data = Unemployment_dataframe, mapping = aes(x = Unemployment_dates, y = Unemployment_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-06-30"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("Unemployment") + theme_bw()


library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm_mod_Unemployment, pred.labels = c("Intercept", "Time", "Law", "Time after Law"),dv.labels = "Unemployment", file="lm_mod_Unemployment.doc")








### M0
library(readr)
M0 <- read_csv("~/Desktop/ASemester/Thesis/2Data/2Data_M0.csv")
View(M0)

M0_dates <-as.Date(M0$DateTime)
M0_dates
M0_time <- seq(1,76)
M0_time
M0_Value <-as.numeric(M0$Value)
M0_Value
M0_before_boolean <- M0$DateTime>="2020-07-31"
M0_before_boolean
M0_after_intervention <- ifelse(M0$DateTime>"2020-07-31",M0_time-54,0)
M0_after_intervention


M0_dataframe<-data.frame(Time = M0_time,
                         Value = M0_value,
                         Date = M0_dates,
                         before = M0_before_boolean,
                         after = M0_after_intervention)
M0_dataframe



lm_mod_M0 <- lm(M0_Value ~ M0_time + M0_before_boolean + M0_after_intervention, data = M0_dataframe)
lm_mod_M0
summary(lm_mod_M0)

library(ggplot2)
library(tidyverse)
ggplot(data = M0_dataframe, mapping = aes(x = M0_dates, y = M0_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-06-30"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("M0") + theme_bw()


library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm_mod_M0, pred.labels = c("Intercept", "Time", "Law", "Time after Law"),dv.labels = "M0", file="lm_mod_M0.doc")








### M1
library(readr)
M1 <- read_csv("~/Desktop/ASemester/Thesis/2Data/2Data_M1.csv")
View(M1)

M1_dates <-as.Date(M1$DateTime)
M1_dates
M1_time <- seq(1,76)
M1_time
M1_value <-as.numeric(M1$Value)
M1_value
M1_before_boolean <- M1$DateTime>="2020-07-31"
M1_before_boolean
M1_after_intervention <- ifelse(M1$DateTime>"2020-07-31",M1_time-54,0)
M1_after_intervention


M1_dataframe<-data.frame(Time = M1_time,
                         Value = M1_value,
                         Date = M1_dates,
                         before = M1_before_boolean,
                         after = M1_after_intervention)
M1_dataframe



lm_mod_M1 <- lm(M1_value ~ M1_time + M1_before_boolean + M1_after_intervention, data = M1_dataframe)
lm_mod_M1
summary(lm_mod_M1)

library(ggplot2)
library(tidyverse)
ggplot(data = M1_dataframe, mapping = aes(x = M1_dates, y = M1_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-06-30"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("M1") + theme_bw()

library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm_mod_M1, pred.labels = c("Intercept", "Time", "Law", "Time after Law"),dv.labels = "M1", file="lm_mod_M11.doc")







### M2
library(readr)
M2 <- read_csv("~/Desktop/ASemester/Thesis/2Data/2Data_M2.csv")
View(M2)

M2_dates <-as.Date(M2$DateTime)
M2_dates
M2_time <- seq(1,76)
M2_time
M2_value <-as.numeric(M2$Value)
M2_value
M2_before_boolean <- M2$DateTime>="2020-07-31"
M2_before_boolean
M2_after_intervention <- ifelse(M2$DateTime>"2020-07-31",M2_time-54,0)
M2_after_intervention


M2_dataframe<-data.frame(Time = M2_time,
                         Value = M2_value,
                         Date = M2_dates,
                         before = M2_before_boolean,
                         after = M2_after_intervention)
M2_dataframe



lm_mod_M2 <- lm(M2_value ~ M2_time + M2_before_boolean + M2_after_intervention, data = M2_dataframe)
lm_mod_M2
summary(lm_mod_M2)

library(ggplot2)
library(tidyverse)
ggplot(data = M2_dataframe, mapping = aes(x = M2_dates, y = M2_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-06-30"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("M2") + theme_bw()


library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm_mod_M2, pred.labels = c("Intercept", "Time", "Law", "Time after Law"),dv.labels = "M2", file="lm_mod_M2.doc")





### Diff in Diff Uruguay vs Argentina

library(readr)
Diff_in_diff <- read_csv("~/Desktop/Diff_in_diff_arg_vs_uru_GBV_Placebo.csv")
View(Diff_in_diff)

Diff_in_diff_dates <-as.Date(Diff_in_diff$DateTime)
Diff_in_diff_dates
Diff_in_diff_time <- seq(1,152)
Diff_in_diff_time
Diff_in_diff_value <-as.numeric(Diff_in_diff$Value)
Diff_in_diff_value
Diff_in_diff_subject <-as.numeric(Diff_in_diff$Treatment_Subject)
Diff_in_diff_subject
Diff_in_diff_boolean <- as.numeric(Diff_in_diff$AfterIntervention)
Diff_in_diff_boolean
Diff_in_diff_trearmXafter <- as.numeric(Diff_in_diff$`Treatm*AftIntv`)
Diff_in_diff_trearmXafter

Diff_in_diff_dataframe<-data.frame(Time = Diff_in_diff_time,
                                   Value = Diff_in_diff_value,
                                   Date = Diff_in_diff_dates,
                                   Intervention = Diff_in_diff_boolean,
                                   TreatmentXafter = Diff_in_diff_trearmXafter,
                                   Subject = Diff_in_diff_subject)

Diff_in_diff_dataframe

lm_mod_Diff_in_diff <- lm(Diff_in_diff_value ~ Diff_in_diff_subject + Diff_in_diff_boolean + Diff_in_diff_trearmXafter, data = Diff_in_diff_dataframe)
lm_mod_Diff_in_diff
summary(lm_mod_Diff_in_diff)

library(tidyverse)
ggplot(data = Diff_in_diff_dataframe, mapping = aes(x = Diff_in_diff_dates, y = Diff_in_diff_value, color = Diff_in_diff$Country)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), linetype=1, colour="blue" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) 

library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm_mod_Diff_in_diff, pred.labels = c("Intercept", "Subject", "After Intervention", "Subject * After Intervention"),dv.labels = "Difference in Differences", file="lm_mod_Diff_in_diff_arg_vs_uru.doc")






summary(Diff_in_diff, robust = T)
install.packages("stargazer")
require("stargazer")

# save robust standard errors
robust_se <- as.vector(summary(Diff_in_diff,robust = T)$predictors[,"Std. Error"])

# print stargazer output with robust standard errors
stargazer(Diff_in_diff,type = "text",se = list(robust_se))

# the last command prints the stargazer output (in this case as text)
# with robust standard errors. 





### Diff in diff Government Spending


library(readr)
Diff_in_diff_GS <- read_csv("~/Desktop/2Data_diffindiff_Government_Spending_arg_vs_uru_Placebo.csv")
View(Diff_in_diff_GS)

Diff_in_diff_GS_dates <-as.Date(Diff_in_diff_GS$DateTime)
Diff_in_diff_GS_dates
Diff_in_diff_GS_time <- seq(1,50)
Diff_in_diff_GS_time
Diff_in_diff_GS_value <-as.numeric(Diff_in_diff_GS$Value)
Diff_in_diff_GS_value
Diff_in_diff_GS_subject <-as.numeric(Diff_in_diff_GS$Treatment_Subject)
Diff_in_diff_GS_subject
Diff_in_diff_GS_boolean <- as.numeric(Diff_in_diff_GS$AfterIntervention)
Diff_in_diff_GS_boolean
Diff_in_diff_GS_trearmXafter <- as.numeric(Diff_in_diff_GS$`Treatm*AftIntv`)
Diff_in_diff_GS_trearmXafter

Diff_in_diff_GS_dataframe<-data.frame(Time = Diff_in_diff_GS_time,
                                   Value = Diff_in_diff_GS_value,
                                   Date = Diff_in_diff_GS_dates,
                                   Intervention = Diff_in_diff_GS_boolean,
                                   TreatmentXafter = Diff_in_diff_GS_trearmXafter,
                                   Subject = Diff_in_diff_GS_subject)

Diff_in_diff_GS_dataframe

lm_mod_Diff_in_diff_GS <- lm(Diff_in_diff_GS_value ~ Diff_in_diff_GS_subject + Diff_in_diff_GS_boolean + Diff_in_diff_GS_trearmXafter, data = Diff_in_diff_GS_dataframe)
lm_mod_Diff_in_diff_GS
summary(lm_mod_Diff_in_diff_GS)

Country <- Diff_in_diff_GS$Country

library(tidyverse)
ggplot(data = Diff_in_diff_GS_dataframe, mapping = aes(x = Diff_in_diff_GS_dates, y = Diff_in_diff_GS_value, color = Country)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-06-30"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("Government Spending") + theme_bw() + scale_color_manual(breaks = c("Uruguay", "Argentina"), values=c("black", "grey"))





#### CPI diff in diff


library(readr)
Diff_in_diff_CPI <- read_csv("~/Desktop/2Data_Diffindiff_CPI_arg_vs_uru_Placebo.csv")
View(Diff_in_diff_CPI)

Diff_in_diff_CPI_dates <-as.Date(Diff_in_diff_CPI$DateTime)
Diff_in_diff_CPI_dates
Diff_in_diff_CPI_time <- seq(1,130)
Diff_in_diff_CPI_time
Diff_in_diff_CPI_value <-as.numeric(Diff_in_diff_CPI$Value)
Diff_in_diff_CPI_value
Diff_in_diff_CPI_subject <-as.numeric(Diff_in_diff_CPI$Treatment_Subject)
Diff_in_diff_CPI_subject
Diff_in_diff_CPI_boolean <- as.numeric(Diff_in_diff_CPI$AfterIntervention)
Diff_in_diff_CPI_boolean
Diff_in_diff_CPI_trearmXafter <- as.numeric(Diff_in_diff_CPI$`Treatm*AftIntv`)
Diff_in_diff_CPI_trearmXafter

Diff_in_diff_CPI_dataframe<-data.frame(Time = Diff_in_diff_CPI_time,
                                      Value = Diff_in_diff_CPI_value,
                                      Date = Diff_in_diff_CPI_dates,
                                      Intervention = Diff_in_diff_CPI_boolean,
                                      TreatmentXafter = Diff_in_diff_CPI_trearmXafter,
                                      Subject = Diff_in_diff_CPI_subject)

Diff_in_diff_CPI_dataframe

lm_mod_Diff_in_diff_CPI <- lm(Diff_in_diff_CPI_value ~ Diff_in_diff_CPI_subject + Diff_in_diff_CPI_boolean + Diff_in_diff_CPI_trearmXafter, data = Diff_in_diff_CPI_dataframe)
lm_mod_Diff_in_diff_CPI
summary(lm_mod_Diff_in_diff_CPI)

Country <- Diff_in_diff_CPI$Country

library(tidyverse)
ggplot(data = Diff_in_diff_CPI_dataframe, mapping = aes(x = Diff_in_diff_CPI_dates, y = Diff_in_diff_CPI_value, color = Country)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-06-30"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("CPI") + theme_bw() + scale_color_manual(breaks = c("Uruguay", "Argentina"), values=c("black", "grey"))


Diff_in_diff_GDP_Growth_from_2016



### GDP Growth

library(readr)
Diff_in_diff_GDP <- read_csv("~/Desktop/Diff_in_diff_GDP_Growth_from_2016_Placebo.csv")
View(Diff_in_diff_GDP)

Diff_in_diff_GDP_dates <-as.Date(Diff_in_diff_GDP$DateTime)
Diff_in_diff_GDP_dates
Diff_in_diff_GDP_time <- seq(1,48)
Diff_in_diff_GDP_time
Diff_in_diff_GDP_value <-as.numeric(Diff_in_diff_GDP$Value)
Diff_in_diff_GDP_value
Diff_in_diff_GDP_subject <-as.numeric(Diff_in_diff_GDP$TreatmentSubject)
Diff_in_diff_GDP_subject
Diff_in_diff_GDP_boolean <- as.numeric(Diff_in_diff_GDP$AfterIntervention)
Diff_in_diff_GDP_boolean
Diff_in_diff_GDP_trearmXafter <- as.numeric(Diff_in_diff_GDP$`Treatm*AftIntv`)
Diff_in_diff_GDP_trearmXafter

Diff_in_diff_GDP_dataframe<-data.frame(Time = Diff_in_diff_GDP_time,
                                       Value = Diff_in_diff_GDP_value,
                                       Date = Diff_in_diff_GDP_dates,
                                       Intervention = Diff_in_diff_GDP_boolean,
                                       TreatmentXafter = Diff_in_diff_GDP_trearmXafter,
                                       Subject = Diff_in_diff_GDP_subject)

Diff_in_diff_GDP_dataframe

lm_mod_Diff_in_diff_GDP <- lm(Diff_in_diff_GDP_value ~ Diff_in_diff_GDP_subject + Diff_in_diff_GDP_boolean + Diff_in_diff_GDP_trearmXafter, data = Diff_in_diff_GDP_dataframe)
lm_mod_Diff_in_diff_GDP
summary(lm_mod_Diff_in_diff_GDP)

Country <- Diff_in_diff_GDP$Country

library(tidyverse)
ggplot(data = Diff_in_diff_GDP_dataframe, mapping = aes(x = Diff_in_diff_GDP_dates, y = Diff_in_diff_GDP_value, color = Country)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-06-30"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("GDP Growth") + theme_bw() + scale_color_manual(breaks = c("Uruguay", "Argentina"), values=c("black", "grey"))



####

library(readr)
Diff_in_diff_GDP <- read_csv("~/Desktop/Diff_in_diff_GDP_Growth_from_2016_Placebo.csv")
View(Diff_in_diff_GDP)

Diff_in_diff_GDP_dates <-as.Date(Diff_in_diff_GDP$DateTime)
Diff_in_diff_GDP_dates
Diff_in_diff_GDP_time <- seq(1,48)
Diff_in_diff_GDP_time
Diff_in_diff_GDP_value <-as.numeric(Diff_in_diff_GDP$Value)
Diff_in_diff_GDP_value
Diff_in_diff_GDP_subject <-as.numeric(Diff_in_diff_GDP$TreatmentSubject)
Diff_in_diff_GDP_subject
Diff_in_diff_GDP_boolean <- as.numeric(Diff_in_diff_GDP$AfterIntervention)
Diff_in_diff_GDP_boolean
Diff_in_diff_GDP_trearmXafter <- as.numeric(Diff_in_diff_GDP$`Treatm*AftIntv`)
Diff_in_diff_GDP_trearmXafter

Diff_in_diff_GDP_dataframe<-data.frame(Time = Diff_in_diff_GDP_time,
                                       Value = Diff_in_diff_GDP_value,
                                       Date = Diff_in_diff_GDP_dates,
                                       Intervention = Diff_in_diff_GDP_boolean,
                                       TreatmentXafter = Diff_in_diff_GDP_trearmXafter,
                                       Subject = Diff_in_diff_GDP_subject)

Diff_in_diff_GDP_dataframe

lm_mod_Diff_in_diff_GDP <- lm(Diff_in_diff_GDP_value ~ Diff_in_diff_GDP_subject + Diff_in_diff_GDP_boolean + Diff_in_diff_GDP_trearmXafter, data = Diff_in_diff_GDP_dataframe)
lm_mod_Diff_in_diff_GDP
summary(lm_mod_Diff_in_diff_GDP)

Country <- Diff_in_diff_GDP$Country

library(tidyverse)
ggplot(data = Diff_in_diff_GDP_dataframe, mapping = aes(x = Diff_in_diff_GDP_dates, y = Diff_in_diff_GDP_value, color = Country)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-06-30"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("GDP Growth") + theme_bw() + scale_color_manual(breaks = c("Uruguay", "Argentina"), values=c("black", "grey"))




### Inflation
library(readr)
Inflation <- read_csv("Desktop/ASemester/Thesis/2Data/2Data_Inflation_rate.csv")
View(Inflation)

Inflation_dates <-as.Date(Inflation$DateTime)
Inflation_dates
Inflation_time <- seq(1,76)
Inflation_time
Inflation_value <-as.numeric(Inflation$Value)
Inflation_value
Inflation_before_boolean <- Inflation$DateTime>="2020-07-31"
Inflation_before_boolean
Inflation_after_intervention <- ifelse(Inflation$DateTime>"2020-07-31",Inflation_time-54,0)
Inflation_after_intervention


Inflation_dataframe<-data.frame(Time = Inflation_time,
                                Value = Inflation_value,
                                Date = Inflation_dates,
                                before = Inflation_before_boolean,
                                after = Inflation_after_intervention)
Inflation_dataframe



lm_mod_Inflation <- lm(Value ~ Inflation_time + Inflation_before_boolean + Inflation_after_intervention, data = Inflation_dataframe)
lm_mod_Inflation
summary(lm_mod_Inflation)

library(ggplot2)
library(tidyverse)
ggplot(data = Inflation_dataframe, mapping = aes(x = Inflation_dates, y = Inflation_value)) + geom_point() +  geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), linetype=1, colour="black" )  +  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) + xlab("Dates") + ylab("Inflation rate") + theme_bw()














































































































