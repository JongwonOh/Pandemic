library(haven)
library(dplyr)
library(tidyverse)
library(stargazer)
library(sandwich)
library(clubSandwich)
library(ggplot2)
library(margins)
options(scipen=999)

CES <- read_dta("CES20_Common_OUTPUT_vv.dta")
View(CES)

head(CES$CC20_309a_5)
table(CES$CC20_309a_5)
CES <- CES %>% mutate(covid = case_when(CC20_309a_5 == 1 ~ 0,
                                        CC20_309a_5 == 2 ~ 1))
table(CES$covid)

head(CES$CC20_305_2)
table(CES$CC20_305_2)
CES <- CES %>% mutate(unemployment = ifelse(CC20_305_2 == 1, 1, 0))
summary(CES$unemployment)
table(CES$unemployment)

head(CES$faminc_new)
summary(CES$faminc_new)
table(CES$faminc_new)
CES <- CES %>% mutate(faminc = case_when(faminc_new == 1 ~ 5000,
                                         faminc_new == 2 ~ 15000,
                                         faminc_new == 3 ~ 25000,
                                         faminc_new == 4 ~ 35000,
                                         faminc_new == 5 ~ 45000,
                                         faminc_new == 6 ~ 55000,
                                         faminc_new == 7 ~ 65000,
                                         faminc_new == 8 ~ 75000,
                                         faminc_new == 9 ~ 90000,
                                         faminc_new == 10 ~ 110000,
                                         faminc_new == 11 ~ 135000,
                                         faminc_new == 12 ~ 175000,
                                         faminc_new == 13 ~ 225000,
                                         faminc_new == 14 ~ 300000,
                                         faminc_new == 15 ~ 400000,
                                         faminc_new == 16 ~ 500000,
                                         faminc_new == 97 ~ NA_real_))
table(CES$faminc)

head(CES$educ)
CES <- CES %>% mutate(degree = case_when(educ == 5 ~ 1,
                                         educ == 6 ~ 1,
                                         educ == 1 ~ 0,
                                         educ == 2 ~ 0,
                                         educ == 3 ~ 0,
                                         educ == 4 ~ 0,
                                         educ == 8 ~ NA_real_,
                                         educ == 9 ~ NA_real_))
summary(CES$degree)
table(CES$degree)

CES <- CES %>% mutate(insur = case_when(healthins_6 == 1 ~ 0,
                                        healthins_5 == 1 ~ NA_real_,
                                        healthins_1 == 1 | healthins_2 == 1 | healthins_3 == 1 | healthins_4 == 1 ~ 1))
table(CES$insur)

head(CES$CC20_320a)
table(CES$CC20_320a)
CES <- CES %>% mutate(trump = case_when(CC20_320a == 1 ~ 1,
                                        CC20_320a == 2 ~ 1,
                                        CC20_320a == 3 ~ 0,
                                        CC20_320a == 4 ~ 0,
                                        CC20_320a == 5 ~ NA_real_))
summary(CES$trump)
table(CES$trump)

head(CES$pew_religimp)
table(CES$pew_religimp)
CES <- CES %>% mutate(faith = case_when(pew_religimp == 4 ~ 1,
                                        pew_religimp == 3 ~ 2,
                                        pew_religimp == 2 ~ 3,
                                        pew_religimp == 1 ~ 4))
table(CES$faith)
CES$faith <- as.factor(CES$faith)

head(CES$marstat)
table(CES$marstat)
CES <- CES %>% mutate(marry = ifelse(marstat == 1, 1, 0))
summary(CES$marry)
table(CES$marry)

head(CES$CC20_443_1)
table(CES$CC20_443_1)
CES <- CES %>% mutate(welfare = case_when(CC20_443_1 == 1 ~ 1,
                                          CC20_443_1 == 2 ~ 1,
                                          CC20_443_1 == 3 ~ 0,
                                          CC20_443_1 == 4 ~ 0,
                                          CC20_443_1 == 5 ~ 0))
table(CES$welfare)

CES <- CES %>% mutate(welfare2 = case_when(CC20_443_1 == 5 ~ 1,
                                           CC20_443_1 == 4 ~ 2,
                                           CC20_443_1 == 3 ~ 3,
                                           CC20_443_1 == 2 ~ 4,
                                           CC20_443_1 == 1 ~ 5))
table(CES$welfare2)
CES$welfare2 <- as.factor(CES$welfare2)

head(CES$CC20_327a)
CES <- CES %>% mutate(health = case_when(CC20_327a == 1 ~ 1,
                                         CC20_327a == 2 ~ 0,
                                         CC20_327a == 8 ~ NA_real_,
                                         CC20_327a == 9 ~ NA_real_))
table(CES$health)

head(CES$CC20_443_2)
table(CES$CC20_443_2)
CES <- CES %>% mutate(health2 = case_when(CC20_443_2 == 5 ~ 1,
                                          CC20_443_2 == 4 ~ 2,
                                          CC20_443_2 == 3 ~ 3,
                                          CC20_443_2 == 2 ~ 4,
                                          CC20_443_2 == 1 ~ 5))
table(CES$health2)
CES$health2 <- as.factor(CES$health2)

library(fixest)
Model1 <- feglm(data = CES, welfare ~ covid, family = binomial(link = 'probit'))
Model2 <- feglm(data = CES, welfare ~ covid + unemployment + log(faminc) + insur + degree
                + trump, family = binomial(link = 'probit'))
Model3 <- feglm(data = CES, welfare ~ covid + unemployment + log(faminc) + insur + degree
                + trump + gender + marry 
                + faith | inputstate, family = binomial(link = 'probit'))
Model4 <- feglm(data = CES, health ~ covid, family = binomial(link = 'probit'))
Model5 <- feglm(data = CES, health ~ covid + unemployment + log(faminc) + insur + degree
                + trump, family = binomial(link = 'probit'))
Model6 <- feglm(data = CES, health ~ covid + unemployment + log(faminc) + insur + degree
                + trump + gender + marry 
                + faith | inputstate, family = binomial(link = 'probit'))
etable(Model1, Model2, Model3, Model4, Model5, Model6, se = 'hetero', digits = 3, tex = TRUE)

CES <- CES %>% mutate(pred1 = predict(Model3, newdata = CES)) %>% mutate(pred2 = predict(Model6, newdata = CES))

a <- ggplot(data = CES, aes(x = covid, y = pred1)) + geom_boxplot(aes(fill = factor(covid)), width = 0.5)
plot1 <- a + coord_cartesian(xlim = c(-0.5, 1.5), ylim = c(0, 1)) + ggtitle("COVID-19 & Attitude toward welfare expansion")
plot1 + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + labs(x="Suffering from COVID-19", y="Agree(1) / Disagree(0) with welfare expansion") + theme(axis.title = element_text(face = "bold", size = 9, color = "black")) + theme(legend.position = "none") + scale_x_continuous(breaks = c(0, 1)) + stat_summary(fun.y="mean", geom="point", shape=22, size=3, fill="blue")

b <- ggplot(data = CES, aes(x = covid, y = pred2)) + geom_boxplot(aes(fill = factor(covid)), width = 0.5)
plot2 <- b + coord_cartesian(xlim = c(-0.5, 1.5), ylim = c(0, 1)) + ggtitle("COVID-19 & Attitude toward health care expansion")
plot2 + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkred")) + labs(x="Suffering from COVID-19", y="Agree(1) / Disagree(0) with health care expansion") + theme(axis.title = element_text(face = "bold", size = 9, color = "black")) + theme(legend.position = "none") + scale_x_continuous(breaks = c(0, 1)) + stat_summary(fun.y="mean", geom="point", shape=22, size=3, fill="blue")

library(MASS)
CES <- CES %>% mutate(covid2 = case_when(CES$CC20_309a_5 == 1 ~ 1,
                                         CES$CC20_309a_4 == 1 ~ 2,
                                         CES$CC20_309a_3 == 1 ~ 3,
                                         CES$CC20_309a_2 == 1 ~ 4,
                                         CES$CC20_309a_1 == 1 ~ 5))
table(CES$covid2)
CES$covid2 <- as.factor(CES$covid2)

CES$inputstate <- as.factor(CES$inputstate)
opwelfare <- polr(data = CES, welfare2 ~ covid2 + unemployment + log(faminc) + insur
                  + degree + trump + gender + marry + faith
                  + inputstate, method = 'probit', Hess = T)
vopwelfare <- vcovCL(opwelfare, type = 'HC1')
opwelfare$zeta

ophealth <- polr(data = CES, health2 ~ covid2 + unemployment + log(faminc) + insur
                 + degree + trump + gender + marry + faith
                 + inputstate, method = 'probit', Hess = T)
vophealth <- vcovCL(ophealth, type = 'HC1')
ophealth$zeta

CES <- CES %>% mutate(cdeath = case_when(CES$CC20_309a_4 == 1 ~ 1,
                                         CES$CC20_309a_3 == 1 ~ 2,
                                         CES$CC20_309a_2 == 1 ~ 3,
                                         CES$CC20_309a_1 == 1 ~ 4,))
table(CES$cdeath)
opwelfare2 <- polr(data = CES, welfare2 ~ cdeath + unemployment + log(faminc) + insur
                   + degree + trump + gender + marry + faith
                   + inputstate, method = 'probit', Hess = T)
vopwelfare2 <- vcovCL(opwelfare2, type = 'HC1')
opwelfare2$zeta

ophealth2 <- polr(data = CES, health2 ~ cdeath + unemployment + log(faminc) + insur
                  + degree + trump + gender + marry + faith
                  + inputstate, method = 'probit', Hess = T)
vophealth2 <- vcovCL(ophealth2, type = 'HC1')
ophealth2$zeta

stargazer(opwelfare2, ophealth2, type = 'latex', keep = c(1:11), digits = 3, se = list(sqrt(diag(vopwelfare2)), sqrt(diag(vophealth2))), add.lines = list(c('State Fixed Effects', 'Yes', 'Yes'), c("$\tau$ 1", "-4.092", "-4.506"), c("$\tau$ 2", "-3.424", "-3.988"), c("$\tau$ 3", "-2.228", "-2.810"), c("$\tau$ 4", "-1.444", "-1.974")))

CES$welfare2 <- as.numeric(CES$welfare2)
CES$health2 <- as.numeric(CES$health2)
lrwelfare <- lm(data = CES, welfare2 ~ covid2 + unemployment + log(faminc) + insur
                + degree + trump + gender + marry + faith + inputstate)
vlrwelfare <- vcovHC(lrwelfare, type = 'HC1')
lrhealth <- lm(data = CES, health2 ~ covid2 + unemployment + log(faminc) + insur
               + degree + trump + gender + marry + faith + inputstate)
vlrhealth <- vcovHC(lrhealth, type = 'HC1')

stargazer(opwelfare, ophealth, lrwelfare, lrhealth, type = 'latex', keep = c(1:15), digits = 3, se = list(sqrt(diag(vopwelfare)), sqrt(diag(vophealth))), add.lines = list(c('State Fixed Effects', 'Yes', 'Yes', 'Yes', 'Yes'), c("$\tau$ 1", "-4.210", "-4.291"), c("$\tau$ 2", "-3.577", "-3.818"), c("$\tau$ 3", "-2.369", "-2.574"), c("$\tau$ 4", "-1.624", "-1.769")))