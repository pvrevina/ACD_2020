#### ACD ESSAY; REVINA POLINA 

### DATA PREPARATION 
library(dplyr)
library(ggplot2)
library(readxl)

## NAVCO 
navco <- readxl::read_excel("navco.xlsx")
colnames(navco)[1] <- "Country"
navco_used <- navco %>% dplyr::select("Country", "year", "lccode", "prim_method", 'camp_size', "camp_size_est", 
                                      "navco1designation", 'camp_conf_intensity', "sec_defect", "in_media")
colnames(navco_used)[3] <- "ccode"

navco_used$in_media[navco_used$in_media == -99] <- NA
navco_used$sec_defect[navco_used$sec_defect == -99] <- NA
navco_used$repression[navco_used$repression == -99] <- NA
navco_used$camp_size[navco_used$camp_size == -99] <- NA
navco_used$camp_size_est[navco_used$camp_size_est == -99] <- NA

navco_used$sec_defect_logic <- as.logical(navco_used$sec_defect)
navco_used$sec_defect <- as.factor(navco_used$sec_defect)
navco_used$camp_size <- as.factor(navco_used$camp_size)
navco_used$navco1designation <- as.factor(navco_used$navco1designation)
navco_used$prim_method <- as.factor(navco_used$prim_method)
navco_used$in_media <- as.factor(navco_used$in_media)
navco_used$repression <- as.factor(navco_used$repression)

navco_used <- navco_used %>% dplyr::filter(!is.na(sec_defect))

summary(navco_used$year)

## Sudduth: military purges 
library(haven)
library(Hmisc)
sudduth1 <- read_dta("CPS_Elitepurge_Sudduth.dta")
sudduth1$rankofficer[sudduth1$rankofficer == 1] <- 2
#summary(sudduth1$rankofficer)
sudduth1$rankofficer <- as.factor(sudduth1$rankofficer)
summary(sudduth1$rankofficer)

sudduth1$purge4 <- as.factor(sudduth1$purge4)

purges <- sudduth1 %>% dplyr::select(ccode, year, milper, milex, gwf_military, rankofficer, purge4)

navco_purges <- merge(navco_used, purges, by = c("ccode", "year")) %>% filter(!is.na(sec_defect))

summary(navco_purges$year)

## GDP 
gdp <- read.csv("gdp2.csv")
colnames(gdp)[1] <- "Country"
colnames(gdp)[2] <- "ccode"
colnames(gdp)[3] <- "year"

navco_purges_2 <- merge(navco_purges, gdp, by = c("Country", "year"), all.x = TRUE)
navco_purges_2$log_gdp <- log(navco_purges_2$Value)

## Filter data 
summary(navco_purges)
navco_purges3 <- navco_purges_2 %>% filter(year >= 1984)
navco_purges3 <- navco_purges3 %>% filter(year <= 1994)
summary(navco_purges3$year)

#navco_purges3 <- navco_purges3 %>% dplyr::filter(Country != "West Papua")
#navco_purges3 <- navco_purges3 %>% dplyr::filter(Country != "East Timor")
#navco_purges3 <- navco_purges3 %>% dplyr::filter(Country != "Tibet")
#navco_purges3 <- navco_purges3 %>% dplyr::filter(Country != "Burundi")
#navco_purges3 <- navco_purges3 %>% dplyr::filter(Country != "China")
#navco_purges3 <- navco_purges3 %>% dplyr::filter(Country != "Kyrgyzstan")
#navco_purges3 <- navco_purges3 %>% dplyr::filter(Country != "Rwanda")

length(unique(navco_purges3$Country)) # 49 cuntries 
length(unique(navco_purges3$year)) # 1984-1994 period

navco_purges3$log_milex <- log(navco_purges3$milex)
navco_purges3$log_milper <- log(navco_purges3$milper)
navco_purges3$gwf_military <- as.factor(navco_purges3$gwf_military)

navco_purges3 <- navco_purges3 %>% dplyr::select(year, Country, sec_defect, camp_size, navco1designation, 
                                                 in_media, log_gdp, sec_defect_logic,
                                                 gwf_military, rankofficer, purge4, log_milex, log_milper)

summary(navco_purges3)

library(naniar)
vis_miss(navco_purges3)

library(stargazer)
stargazer(navco_purges3)
#write.csv(navco_purges3, "ACD_essay.csv")

### vis 
library(ggplot2)
ggplot(data=navco_purges3, aes(x = factor(sec_defect))) +
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  ggtitle("Military defection") + 
  xlab("security defection levels")

ggplot(data=navco_purges3, aes(x = factor(purge4))) +
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  ggtitle("Any military purge") + 
  xlab("military purge levels")

ggplot(data=na.omit(navco_purges3), aes(x = factor(rankofficer))) +
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  ggtitle("Rank of eliminated officers") + 
  xlab("officers' ranks levels")

ggplot(data=na.omit(navco_purges3), aes(x = factor(camp_size))) +
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  ggtitle("Campaign size") + 
  xlab("campaign size levels")

ggplot(data=na.omit(navco_purges3), aes(x = factor(navco1designation))) +
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  ggtitle("The type of resistance") + 
  xlab("resistance type levels")

plot(navco_purges3$log_milex)
plot(navco_purges3$log_gdp)
plot(navco_purges3$log_milper)

GGally::ggcorr(navco_purges3)



############ Models
## pooled regression 
pooled.logit <- glm(sec_defect ~ purge4 + log_milper + log_milex + log_gdp + gwf_military +
                      camp_size + navco1designation, family=binomial(link="logit"), data=navco_purges3)
summary(pooled.logit)

## FE + logistic
# glmmML package 
install.packages("glmmML")
library(glmmML)


### FE + logistic
## pglm package 
install.packages("pglm")
library(pglm)
model_pglm_1_0 <- pglm(sec_defect ~ purge4 + log_milper + log_milex + log_gdp, 
                       index = c("Country", "year"), 
                       model = "within", 
                       start = NULL,
                       family = binomial("logit"),
                       data = navco_purges3)
summary(model_pglm_1_0)
## unknown errror: argument "start" is missing, with no default
## so I skipped to bife package 
## however, it made F-test and other estimations impossible 

## bife package 
model_1_00 <- bife(sec_defect ~ purge4 | Country, data = navco_purges3, 
                  model = "logit")
summary(model_1_00)

model_1_0 <- bife(sec_defect ~ purge4 + log_gdp| Country, data = navco_purges3, 
                  model = "logit")
summary(model_1_0)

model_1_1 <- bife(sec_defect ~ purge4 + log_milper + log_milex + log_gdp | Country, data = navco_purges3, 
                  model = "logit")
summary(model_1_1)

model_1_2 <- bife(sec_defect ~ purge4 + log_milper + log_milex + log_gdp + gwf_military| Country, 
                  data = navco_purges3, 
                  model = "logit")
summary(model_1_2)

model_1_3 <- bife(sec_defect ~ purge4 + log_milper + log_milex + log_gdp + gwf_military +
                    + navco1designation| Country, data = navco_purges3, 
                  model = "logit")
summary(model_1_3)



## second model 
model_2_100 <- bife(as.factor(sec_defect) ~ rankofficer | Country, data = navco_purges3, 
                  model = "logit")
summary(model_2_100)

model_2_10 <- bife(as.factor(sec_defect) ~ rankofficer + log_gdp | Country, data = navco_purges3, 
                    model = "logit")
summary(model_2_10)

model_2_1 <- bife(as.factor(sec_defect) ~ rankofficer + log_milper + log_milex + log_gdp | Country, data = navco_purges3, 
                  model = "logit")
summary(model_2_1)

model_2_2 <- bife(sec_defect ~ rankofficer + log_milper + log_milex + log_gdp + gwf_military| Country, data = navco_purges3, 
                  model = "logit")
summary(model_2_2)

model_2_3 <- bife(sec_defect ~ rankofficer + log_milper + log_milex + log_gdp + gwf_military +
                    navco1designation| Country, data = navco_purges3, 
                  model = "logit")
summary(model_2_3)

##### To latex 
extract.bife <- function(model,
                         include.loglik = TRUE,
                         include.deviance = TRUE,
                         include.nobs = TRUE,
                         ...) {
  s <- summary(model)
  coefficient.names <- rownames(s$cm)
  co <- s$cm[, 1]
  se <- s$cm[, 2]
  pval <- s$cm[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    lik <- logLik(model)
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "Log Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.deviance == TRUE) {
    gof <- c(gof, deviance(model))
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- s$nobs["nobs"]
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}
library(texreg)
tr00 <- extract.bife(model_1_00)
tr0 <- extract.bife(model_1_0)
tr1 <- extract.bife(model_1_1)
tr2 <- extract.bife(model_1_2)
tr3 <- extract.bife(model_1_3)

texreg(c(tr00, tr0, tr1, tr2, tr3))

tr200 <- extract.bife(model_2_100)
tr20 <- extract.bife(model_2_10)
tr21 <- extract.bife(model_2_1)
tr22 <- extract.bife(model_2_2)
tr23 <- extract.bife(model_2_3)

texreg(c(tr200, tr20, tr21, tr22, tr23))


# Conditional model
navco_purges3 <- navco_purges3 %>% dplyr::select(-sec_defect_logical)
library(survival)
cond_1000 <- clogit(sec_defect_logic ~ purge4 + 
                    strata(Country), data = navco_purges3)
summary(cond_1000)

cond_100 <- clogit(sec_defect_logic ~ purge4 + log_gdp + 
                      strata(Country), data = navco_purges3)
summary(cond_100)

cond_10 <- clogit(sec_defect_logic ~ purge4 + log_milper + log_milex + log_gdp + 
                    strata(Country), data = navco_purges3)
summary(cond_10)

cond_11 <- clogit(sec_defect_logic ~ purge4 + log_milper + log_milex + log_gdp + gwf_military + 
                    strata(Country), data = navco_purges3)
summary(cond_11)

cond_12 <- clogit(sec_defect_logic ~ purge4 + log_milper + log_milex + log_gdp + gwf_military + 
                    navco1designation +
                    strata(Country), data = navco_purges3)
summary(cond_12)

cond_12_r <- clogit(sec_defect_logic ~ purge4 + log_milper + log_milex + log_gdp + gwf_military + 
                    navco1designation +
                    strata(Country), 
                    method="efron",
                    robust=TRUE,
                    data = navco_purges3)
summary(cond_12_r)

stargazer(cond_1000, cond_10, cond_11, cond_12, column.sep.width = "-15pt")
stargazer(exp(coef(cond_12)))

# wald 
wald.test(b = cond_12$coefficients, Sigma = vcov(cond_12), Terms = NULL) 

# AIC 
AIC(cond_12, cond_11)
AIC(cond_11, cond_10)

# lrtest 
library(lmtest)
lrtest(cond_12, cond_11)

# car::outlierTest(cond_12) # studentized residuals 


# influential observations 
# could not find a package for working with class "c('clogit', 'coxph')" 


## 2nd H 

cond_2000 <- clogit(sec_defect_logic ~ rankofficer + 
                      strata(Country), data = navco_purges3)
summary(cond_2000)

cond_200 <- clogit(sec_defect_logic ~ rankofficer + log_gdp + 
                     strata(Country), data = navco_purges3)
summary(cond_200)

cond_20 <- clogit(sec_defect_logic ~ rankofficer + log_milper + log_milex + log_gdp + 
                    strata(Country), data = navco_purges3)
summary(cond_20)

cond_21 <- clogit(sec_defect_logic ~ rankofficer + log_milper + log_milex + log_gdp + gwf_military + 
                    strata(Country), data = navco_purges3)
summary(cond_21)

cond_22 <- clogit(sec_defect_logic ~ rankofficer + log_milper + log_milex + log_gdp + gwf_military + 
                    navco1designation +
                    strata(Country), data = navco_purges3, robust = TRUE)
summary(cond_22)

cond_22_r <- clogit(sec_defect_logic ~ rankofficer + log_milper + log_milex + log_gdp + gwf_military + 
                      navco1designation +
                      strata(Country), 
                    method="efron",
                    robust=TRUE,
                    data = navco_purges3)
summary(cond_22_r)

stargazer(cond_12_r, cond_22_r, column.sep.width = "-15pt")

#install.packages("pubh")
#library(pubh)
#require(dplyr)
#require(sjlabelled)
#sjlabelled::get_term_labels(cond_22)
#pubh::glm_coef(cond_22)

stargazer(cond_2000, cond_20, cond_21, cond_22, column.sep.width = "-15pt")

stargazer(exp(coef(cond_22)))

pander(glm_coef(model_clogit, labels = c("Seek Help", "Benefits")), 
       caption = "Parameter estimates from conditional logistic regression.")

# heteroskedasticity-consistent (HC) standard errors


# AIC 
AIC(cond_21, cond_22)
AIC(cond_20, cond_21)
AIC(cond_200, cond_20)

# lrtest 
library(lmtest)
lrtest(cond_21, cond_22)
lrtest(cond_20, cond_21)

# Robustness checks 
# 
navco1 <- navco_purges3 %>% dplyr::filter((camp_size == 0)|(camp_size == 1)|(camp_size == 2))
navco2 <- navco_purges3 %>% dplyr::filter((camp_size == 3)|(camp_size == 4)|(camp_size == 5))


cond_12_rob <- clogit(sec_defect_logic ~ purge4 + log_milper + log_milex + log_gdp + gwf_military + 
                        navco1designation + 
                        strata(Country), data = navco1)
summary(cond_12_rob)

cond_22_rob <- clogit(sec_defect_logic ~ rankofficer + log_milper + log_milex + log_gdp + gwf_military +
                        navco1designation + 
                        strata(Country), data = navco1)
summary(cond_22_rob)

stargazer(cond_12_rob, cond_22_rob)
