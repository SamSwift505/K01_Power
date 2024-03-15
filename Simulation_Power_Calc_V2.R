#https://cran.r-project.org/web/packages/SimEngine/vignettes/example_1.html
#https://cran.r-project.org/web/packages/simstudy/vignettes/simstudy.html


#install.packages("SimEngine")
#install.packages("simstudy")
library(simstudy)
library(SimEngine)
library(tidyverse)
library(fixest)

j<-20000

  def <- defData(varname = "Y2", dist = "binary", formula =0,
                 link="identity")
  def <- defData(def,varname = "Y4", dist = "binary", formula =0,
                 link="identity")
  def <- defData(def,varname = "Y6", dist = "binary", formula =0,
                 link="identity")
  def <- defData(def,varname = "Y8", dist = "binary", formula =0,
                 link="identity")
  def <- defData(def,varname = "Y10", dist = "binary", formula =0,
                 link="identity")
  def <- defData(def,varname = "Y12", dist = "binary", formula =0.01,
                 link="identity")
  def <- defData(def,varname = "Y14", dist = "binary", formula =0.05,
                 link="identity")
  def <- defData(def,varname = "Y16", dist = "binary", formula =0.16,
                 link="identity")
  def <- defData(def,varname = "Y18", dist = "binary", formula =0.20,
                 link="identity")
  set.seed(16)
  dd <- genData(j,def)
 
  
 deftime<-addPeriods(dd, nPeriods=9,idvars="id", timevars=c("Y2","Y4",
  "Y6","Y8","Y10","Y12","Y14","Y16","Y18"), timevarName="Treatment") 
  #
### Conditions- outcome is always dependent on previous outcome
### Policy is always dependent on previous policy 
### Once the policies start outcomes are dependent on policy
# YT <- defCondition(condition="period > 5 & Treatment=1", dist = "binary", formula = "1",
#                    link="identity")
# YT <- defCondition(condition="period == 5 & Treatment=0", dist = "binary", formula = "1",
#                    link="identity")
# dd <- addCondition(YT, dd, newvar = "Year_Treated") 
paneldat<-as.data.frame(deftime) 

##This is the code I need to fix
#group and then create a variable to identify the first year a group is treated 
#and use the max function to make the year populate for all rows of a group
paneldat<-paneldat%>%
  group_by(id)%>%
  mutate(
    first_treated = max(ifelse(Treatment == 1, period, NA),na.rm = T)
         )%>%
  ungroup()

#this creates -inf values, so I replace those with NA
paneldat<-paneldat%>%
  mutate(
    first_treated = ifelse(first_treated == -Inf, 10000, first_treated)
  )

#create variable that makes the Treatment == 1 if the period is greater than or equal to the first treated
paneldat<-paneldat%>%
  group_by(id)%>%
  mutate(
    year_treated = ifelse( period >= first_treated, 1, Treatment)
    , year_treated = ifelse(is.na(first_treated), 0, year_treated)
    )%>%
  ungroup()


#make a data frame of unique IDS and whetehr they were treated, clean a bit
unique_panel <- distinct(paneldat, id, .keep_all = T)%>%
  select(
    id
    ,first_treated
  )%>%
  mutate(
    treated = ifelse(is.na(first_treated), 0, 1)
  )%>%
  select(
    -first_treated
  )

#create the random state assignment for each person 
unique_panel <- unique_panel %>% 
  mutate(cluster = ifelse(treated==1,sample(1:12,size=j*9, replace=TRUE),NA)
         ,cluster = ifelse(treated == 0,sample(13:50, size=j*9, replace=TRUE),cluster)
  )

#merge back to the larger data set  



treat_panel_dat <- merge(paneldat, unique_panel, by = "id")

treat_panel_dat <- treat_panel_dat %>% 
  mutate(outcome = case_when(year_treated==1~rbinom(n=j*9,size=1,prob=0.095)
         ,year_treated == 0~rbinom(n=j*9,size=1,prob=0.12)
                              ))

treat_panel_dat<-treat_panel_dat%>%
  group_by(id)%>%
  mutate(
    first_outcome = max(ifelse(outcome == 1, period, NA),na.rm = T)
  )%>%
  ungroup()

#this creates -inf values, so I replace those with NA
treat_panel_dat<-treat_panel_dat%>%
  mutate(
    first_outcome = ifelse(first_outcome == -Inf, NA, first_outcome)
  )


#create variable that makes the Treatment == 1 if the period is greater than or equal to the first treated
finalpaneldat<-treat_panel_dat%>%
  group_by(id)%>%
  mutate(
    year_outcome = ifelse( period >= first_outcome, 1, outcome)
    , year_outcome = ifelse(is.na(first_outcome), 0, year_outcome)
  )%>%
  ungroup()


# Test our data-generating function

xtabs(~ year_treated + year_outcome, finalpaneldat)


#### Test the model!!! MAKE THIS A BINARY MODEL 
mod_sa = feglm(year_outcome ~ sunab(first_treated, period)|
                 cluster + period,
               cluster = ~cluster, family = "binomial",
               data = finalpaneldat)

summary(mod_sa)



exp(-0.04)

















##Power
run_test <- function(data) {
  test_result <- t.test(outcome~group, data=data)
  return(as.integer(test_result$p.value<0.05))
}

sim %<>% set_script(function() {
  data <- create_rct_data(n=L$n)
  reject <- run_test(data)
  return (list("reject"=reject))
})

sim %<>% set_levels(n=c(20,40,60,80))
sim %<>% set_config(num_sim=1000)

sim %<>% run()

power_sim <- sim %>% summarize(
  list(stat="mean", name="power", x="reject")
)
print(power_sim)

power_sim <- sim %>% summarize(
  list(stat="mean", name="power", x="reject")
)
print(power_sim)

power_formula <- sapply(c(20,40,60,80), function(n) {
  pnorm(sqrt((n*(17-18)^2)/(2^2+2^2)) - qnorm(0.025, lower.tail=F))
})

library(ggplot2)
ggplot(data.frame(
  n = rep(c(20,40,60,80), 2),
  power = c(power_sim$power, power_formula),
  which = rep(c("Simulation","Formula"), each=4)
), aes(x=n, y=power, color=factor(which))) +
  geom_line() +
  labs(color="Method", y="Power", x="Sample size (per group)")
