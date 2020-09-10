
# Lag from onset to seeking care ------------------------------------------
### Biggerstaff et al/CDC data:
# ≤2 days: 35%
# 3–7 days: 47%
# ≥8 days: 18%

ln_fit <- function(pars){
  qs <- plnorm(c(2,8),meanlog=pars[1],sdlog = pars[2])
  dst <- sum(log(qs/c(.35,.82))^2)
  return(dst)
}
ln_mean <- function(fit) exp(fit$par[1]+fit$par[2]^2/2)

fit <- optim(par=c(log(4),.2),ln_fit)
ln_mean(fit)
# 5.314948

# Lag from symptom onset to death -------------------------------------------------
### CDC data:
# 18-49 years: 15 (9, 23) days
# 50-64 years: 15 (9, 25) days
# ≥65 years: 12 (7, 19) days

all_ages <- 175866
under_18 <- 88
age_group_min_ages <- c(18,50,65)
age_group_deaths <- c(765+8024,
                      27980)
age_group_deaths <- c(age_group_deaths,all_ages-under_18-sum(age_group_deaths))
w_age <- age_group_deaths/sum(age_group_deaths)

young_fit <- function(pars){
    qs <- plnorm(c(9,23),meanlog = pars[1],sdlog = pars[2])
    dst <- sum(log(qs/c(.25,.75))^2)
}
mid_fit <- function(pars){
  qs <- plnorm(c(9,25),meanlog = pars[1],sdlog = pars[2])
  dst <- sum(log(qs/c(.25,.75))^2)
}
old_fit <- function(pars){
  qs <- plnorm(c(7,19),meanlog = pars[1],sdlog = pars[2])
  dst <- sum(log(qs/c(.25,.75))^2)
}
young <- optim(c(log(15),1),young_fit)
mid <- optim(c(log(15),1),mid_fit)
old <- optim(c(log(15),1),old_fit)

avg_onset_to_death <- c(ln_mean(young),
                        ln_mean(mid),
                        ln_mean(old))

weighted.mean(avg_onset_to_death,age_group_deaths)
# 16.09126



# Lag from death to reporting ---------------------------------------------
### CDC data:
# 18-49 years: 7 (3, 18) days
# 50-64 years: 7 (2, 19) days
# ≥65 years: 6 (2, 18) days
