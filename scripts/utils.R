library(data.table)
library(deSolve)
library(magrittr)
library(zoo)
library(KFAS)
library(parallel)
library(tidyverse)
library(tsoutliers)
library(progress)
library(lubridate)
library(EpiEstim)


get_cori <- function(df.in,
                     icol_name,
                     out_name = 'Cori',
                     window = 1,
                     SI_mean=4,
                     SI_var=4.75^2,
                     wend = TRUE){
  
  nas <- which(is.na(getElement(df.in,icol_name)))
  df.in[nas,(icol_name):=0]
  
  idat <- df.in %>%
    #filter(get(icol_name) > 0 & !is.na(get(icol_name))) %>%
    complete(time = 2:max(df.in$time)) %>%
    mutate_all(.funs = function(xx){ifelse(is.na(xx), 0, xx)}) %>%
    arrange(time)
  
  ts <- idat$time
  ts <- ts[ts > 1 & ts <= (max(ts)-window+1)]
  te <- ts+(window-1)
  
  estimate_R(
    incid = pull(idat, eval(icol_name)),
    method = "uncertain_si",
    config = make_config(
      list(
        mean_si = SI_mean,
        min_mean_si = SI_mean -1,
        max_mean_si = SI_mean + 1,
        std_mean_si = 1.5,
        std_std_si = 1.5,
        std_si = sqrt(SI_var),
        min_std_si = sqrt(SI_var)*.8,
        max_std_si = sqrt(SI_var)*1.2,
        n1 = 50,
        n2 = 100,
        t_start=ts,
        t_end=te
      )
    )
  ) -> outs
  
  outs$R %>%
    mutate(time = if(wend == TRUE) t_end else ceiling((t_end+t_start)/2) ) %>%
    select(time, `Mean(R)`, `Quantile.0.025(R)`, `Quantile.0.975(R)`) %>%
    setNames(c('time', paste0(out_name, '.mean'), paste0(out_name, '.025'), paste0(out_name, '.975'))) %>%
    return()
}

# Requires to get sockets parallization to work on os x. 
library(rstudioapi)
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
  if(versionInfo()$version < "1.3.1056"){
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
  }  
}


nbss <- function(x,remove_outliers=TRUE,filtering=TRUE){
  nb_model <- function(x, pars){
    model_nb <- SSModel(x ~ SSMtrend(2, Q=list(0, NA),
                                     P1=diag(c(10, 1)),
                                     a1=c(0, 0),
                                     state_names=c("level", "trend"))+
                          SSMseasonal(7),
                        u=rep(exp(pars[1]), length(x)), distribution="negative binomial")
    fit <- fitSSM(model_nb, c(0), method="L-BFGS-B", control=list(maxit=200))
    return(fit)
  }
  logLik_nb <- function(x, pars){
    fit <- nb_model(x, pars)
    ll <- logLik(fit$model, marginal = TRUE)
    return(-ll)
  }
  if (remove_outliers==TRUE){
    x <- outlier_detection(x)
  }
  res <- optim(c(-1), function(y) logLik_nb(x, y), method="Brent", lower=-2, upper=2)   
  fit <- nb_model(x, res$par)
  if (filtering==TRUE){
    sm_signal <- KFS(fit$model, filtering="signal")
    sm_state <- KFS(fit$model, filtering="state")
    
    out <- data.frame(p2.5_position = c(qnorm(0.025, sm_state$alphahat[,1], (sqrt(sm_state$V[1,1,])))), 
                      p97.5_position = c(qnorm(0.975, sm_state$alphahat[,1],(sqrt(sm_state$V[1,1,])))), 
                      mean_position = (c(sm_state$alphahat[,1])),
                      p2.5_growth_rate = c(qnorm(0.025, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      p97.5_growth_rate = c(qnorm(0.975, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))),
                      p25_growth_rate = c(qnorm(0.25, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      p75_growth_rate = c(qnorm(0.75, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      growth_rate = (c(sm_state$alphahat[,2])),
                      percentile_0_growth_rate =c(pnorm(0, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))),
                      growth_rate = (c(sm_state$alphahat[,2])),
                      dispersion=res$par[1], 
                      z_score_growth_rate = c(sm_state$alphahat[,2]/sqrt(sm_state$V[2,2,])))
  } else {
    sm_signal <- KFS(fit$model, smoothing="signal")
    sm_state <- KFS(fit$model, smoothing="state")
    out <- data.frame(p2.5_signal = exp(qnorm(0.025, sm_signal$thetahat, sqrt(c(sm_signal$V_theta)))), 
                      p97.5_signal = exp(qnorm(0.975, sm_signal$thetahat, sqrt(c(sm_signal$V_theta)))), 
                      mean_signal = exp(sm_signal$thetahat), 
                      p2.5_position = c(qnorm(0.025, sm_state$alphahat[,1], (sqrt(sm_state$V[1,1,])))), 
                      p97.5_position = c(qnorm(0.975, sm_state$alphahat[,1],(sqrt(sm_state$V[1,1,])))), 
                      mean_position = (c(sm_state$alphahat[,1])),
                      p2.5_growth_rate = c(qnorm(0.025, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      p97.5_growth_rate = c(qnorm(0.975, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))),
                      p25_growth_rate = c(qnorm(0.25, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      p75_growth_rate = c(qnorm(0.75, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      growth_rate = (c(sm_state$alphahat[,2])),
                      percentile_0_growth_rate =c(pnorm(0, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))),
                      growth_rate = (c(sm_state$alphahat[,2])),
                      dispersion=res$par[1], 
                      z_score_growth_rate = c(sm_state$alphahat[,2]/sqrt(sm_state$V[2,2,])))
  }
  return(out)
}

# Main Model function -- CALL THIS 
# dat should be a data.frame for a single region to be modeled 
#    with column "new_confirmed" which is count data
#    arranged by date (increasing)
#
# return_fit=TRUE is for debugging mostly. 
#
# any other columns in dat are included in output of this function. 
# 
# 
# in addition the following key variables are included as model output
#   growth_rate == exponential growth rate
#   pX_Y is quantile X for the quantitly Y
#   mean_ is the mean of the quantity Y
#   z_score_growth_rate = growth_rate/sd_growth_rate
fit_covid_ssm <- function(dat, series="new_confirmed", precomputed_dispersions=NULL, return_fit=FALSE,maxiter=200){
  
  # Helper functions
  nb_model <- function(dat, pars){
    model_nb <- SSModel(dat[,series] ~ SSMtrend(2, Q=list(0, NA),
                                                     P1=diag(c(10, 1)),
                                                     a1=c(0, 0),
                                                     state_names=c("level", "trend"))+
                          SSMseasonal(7),
                        u=rep(exp(pars[1]), nrow(dat)), distribution="negative binomial")
    fit <- fitSSM(model_nb, c(0), method="L-BFGS-B", control=list(maxit=200, lower=-2, max=5))
    return(fit)
  }
  logLik_nb <- function(dat, pars){
    fit <- nb_model(dat, pars)
    ll <- logLik(fit$model, marginal = TRUE)
    return(-ll)
  }
  
  # Remove preceding zeros
  dat <- dat %>% 
    arrange(date)
  dat$cs = cumsum(dat[,series]) 
  dat <- dat %>% filter(cs > 0)
  if (nrow(dat) < 10) return(NULL)
  
  # outlier detection for early outbreak
  dat <- custom_processors(dat)
  if (sum(dat[,series]!=0) < 10) return(NULL)
  tryCatch({
    outlier_filtered_ts <- getElement(dat,series) %>% outlier_detection
    dat <- transmute(dat, series=outlier_filtered_ts)    
  },  error = function(err){
    return(NULL)
  })
  
  if (length(dat[,series][!is.na(dat[,series])]) < 10) return(NULL)
  
  
  if (!is.null(precomputed_dispersions)){
    res <- list()
    res$par <- dplyr::filter(precomputed_dispersions, id==unique(dat$id))$dispersion
    if (length(res$par)==0) return(NULL) # no precomputed dispersion (previously was not able to fit likely)
  } else {
    res <- optim(c(-1), function(x) logLik_nb(dat, x), method="Brent", lower=-2, upper=2)    
    if (res$convergence != 0) return(NULL)
  }
  
  # now fit the model with the optimized dispersion parameters
  fit <- nb_model(dat, res$par)
  if(return_fit) return(fit)
  if (fit$optim.out$convergence != 0) return(NULL)
  sm_signal <- KFS(fit$model, smoothing="signal")
  sm_state <- KFS(fit$model, smoothing="state")
  
  if (series=="new_confirmed"){
    out <- data.frame(p2.5_signal = exp(qnorm(0.025, sm_signal$thetahat, sqrt(c(sm_signal$V_theta)))), 
                      p97.5_signal = exp(qnorm(0.975, sm_signal$thetahat, sqrt(c(sm_signal$V_theta)))), 
                      mean_signal = exp(sm_signal$thetahat), 
                      p2.5_position = c(qnorm(0.025, sm_state$alphahat[,1], (sqrt(sm_state$V[1,1,])))), 
                      p97.5_position = c(qnorm(0.975, sm_state$alphahat[,1],(sqrt(sm_state$V[1,1,])))), 
                      mean_position = (c(sm_state$alphahat[,1])),
                      p2.5_growth_rate = c(qnorm(0.025, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      p97.5_growth_rate = c(qnorm(0.975, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))),
                      p25_growth_rate = c(qnorm(0.25, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      p75_growth_rate = c(qnorm(0.75, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      growth_rate = (c(sm_state$alphahat[,2])),
                      percentile_0_growth_rate =c(pnorm(0, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))),
                      dispersion=res$par[1], 
                      z_score_growth_rate = c(sm_state$alphahat[,2]/sqrt(sm_state$V[2,2,])))
  } else if (series == "new_deaths"){
    out <- data.frame(p2.5_signal_deaths = exp(qnorm(0.025, sm_signal$thetahat, sqrt(c(sm_signal$V_theta)))), 
                      p97.5_signal_deaths = exp(qnorm(0.975, sm_signal$thetahat, sqrt(c(sm_signal$V_theta)))), 
                      mean_signal_deaths = exp(sm_signal$thetahat), 
                      p2.5_position_deaths = c(qnorm(0.025, sm_state$alphahat[,1], (sqrt(sm_state$V[1,1,])))), 
                      p97.5_position_deaths = c(qnorm(0.975, sm_state$alphahat[,1],(sqrt(sm_state$V[1,1,])))), 
                      mean_position_deaths = (c(sm_state$alphahat[,1])),
                      p2.5_growth_rate_deaths = c(qnorm(0.025, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      p97.5_growth_rate_deaths = c(qnorm(0.975, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))),
                      p25_growth_rate_deaths = c(qnorm(0.25, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      p75_growth_rate_deaths = c(qnorm(0.75, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))), 
                      growth_rate_deaths = (c(sm_state$alphahat[,2])),
                      percentile_0_growth_rate_deaths =c(pnorm(0, sm_state$alphahat[,2], (sqrt(sm_state$V[2,2,])))),
                      dispersion_deaths=res$par[1], 
                      z_score_growth_rate_deaths = c(sm_state$alphahat[,2]/sqrt(sm_state$V[2,2,])))
  }
  
  if (any(grepl('administrative_area',colnames(dat)))){
    if (any(out$p97.5_signal > 1e6)) return(NULL)
  }
  if (quantile(abs(out$z_score_growth_rate), probs=0.75) < 0.4) return(NULL)
  return(cbind(dat, out))
}
covid19_nbss <- function(dat,series="new_confirmed", level='all',mc.cores=1, precomputed_dispersions=NULL){
  if (level=="country"){
    tmp <- filter(dat, administrative_area_level==1)
  } else if (level=="state"){
    tmp <- filter(dat, administrative_area_level==2)
  } else if (level=="all") {
    tmp <- dat
  } else {
    stop("only level variables that are supported are all, country, and state")
  }
  tmp <- dat %>% 
    as.data.frame() %>% 
    split(.$id)
  if (mc.cores == 1){
    fits <- list()
    pb <- progress_bar$new(total = length(tmp), format=" [:bar] :percent eta: :eta")
    for (i in 1:length(tmp)){
      pb$tick()
      fits[[i]] <- fit_nbss(tmp[[i]], series, precomputed_dispersions)
    }  
  } else {
    cl <- parallel::makeCluster(mc.cores)
    parallel::clusterEvalQ(cl, {
      library(tidyverse)
      library(lubridate)
      library(KFAS)
      library(tsoutliers)
      library(data.table)
    })
    parallel::clusterExport(cl,c("custom_processors", "outlier_detection", "fit_nbss"))
    fits <- parLapply(cl, tmp,  function(x) fit_nbss(x, series, precomputed_dispersions))
    stopCluster(cl)
    rm('cl')
  }
  fits <- bind_rows(fits)
  return(fits)
}


custom_processors <- function(dat){
  # Iowa and Indiana
  # if (unique(dat$administrative_area_level_2)%in%c("Iowa", "Indiana", "Kentucky")){
  if (!"I" %in% colnames(dat)){
    if (unique(dat$administrative_area_level_1)=="United States"){
      if (!is.na(unique(dat$administrative_area_level_2))){
        if (unique(dat$administrative_area_level_2 != "Washington")){
          dat <- dat %>% 
            filter(date > ymd("2020-02-28") )
        }
      }
    }
  } 
  return(dat)
}


outlier_detection <- function(x){
  tryCatch({
    res <- tso(ts(x),
               type="TC", delta=0.1, maxit.iloop = 100, maxit.oloop = 10, 
               #tsmethod = "auto.arima", args.tsmethod = list(allowdrift = FALSE, ic = "bic", stationary=TRUE),
               tsmethod="arima", args.tsmethod=list(order=c(1,1,2), method="ML", transform.pars=TRUE),
               cval=4)
  }, error = function(err){
    res <- tso(ts(x),
               type="TC", delta=0.1, maxit.iloop = 100, maxit.oloop = 10, 
               #tsmethod = "auto.arima", args.tsmethod = list(allowdrift = FALSE, ic = "bic", stationary=TRUE),
               tsmethod="arima", args.tsmethod=list(order=c(1,1,2), method="ML", transform.pars=FALSE),
               cval=4)
  })
  res <- res$outliers %>% 
    filter(tstat > 10)
  x[res$ind] <- NA
  return(x)
}

PoissonFit <- function(n,new_confirmed,date,half_life=NULL,day_of_week,z_score=FALSE){
  dd <- data.table('new_confirmed'=new_confirmed,
                   'date'=date,
                   'day_of_week'=day_of_week)
  
  dd <- dd[n]
  if (!is.null(half_life)){
    dd[,weight:=exp(as.numeric(date-min(date))*log(2)/half_life)]
  } else {
    dd[,weight:=1/.N]
  }
  
  fit <- glm(new_confirmed~date+day_of_week,family=poisson,weights=weight,data=dd)
  # fit <- mgcv::gam(new_confirmed~date+day_of_week,family='nb',data=dd[n])
  if (z_score){
    if (!'gam' %in% class(fit)){
      
      summary(fit)$coefficients['date','z value'] %>%
        return
    } else {
      summary(fit)$p.t['date'] %>% return
    }
  } else {
    return(coef(fit)['date'])
  }
}

seird_solve <- function(t,state,parameters){
  with(as.list(c(t,state,parameters)),{
    if (t>lag_onset_to_death){
      Dlag=lagvalue(t-lag_onset_to_death)[5]
    } else {
      Dlag=0
    }
    dS=lambda-(as.numeric(Dlag<=D_intervention)*beta_pre+
                 as.numeric(Dlag>D_intervention & Dlag<=D_relaxation)*(beta_intervention)+
                 as.numeric(Dlag>D_relaxation)*beta_relaxation)*S*I-m*S
    dE=(as.numeric(Dlag<=D_intervention)*beta_pre+
          as.numeric(Dlag>D_intervention & Dlag<=D_relaxation)*(beta_intervention)+
          as.numeric(Dlag>D_relaxation)*beta_relaxation)*S*I-a*E-m*E
    dI=a*E-m_inf*I-gamma*I
    dR=gamma*I-m*R
    dD=m_inf*I
    
    list(c(dS, dE, dI, dR, dD))
  })
}

seird <- function(r,cfr=0.004,S0=3.27e8,start_date=as.Date('2020-01-15'),days=200,
                  lag_onset_to_death=16,case_detection=0.01,window_size=21,half_life=NULL,
                  intervention_efficacy=0,relaxation=1,intervention_deaths=Inf,relaxation_deaths=Inf,
                  gamma=1/9,a=1/3,gr_estimation='nbss',
                  day_of_week_effects=data.table('day_of_week'=c('Saturday','Sunday',
                                                                 'Monday','Tuesday',
                                                                 'Wednesday','Thursday','Friday'),
                                                 'const'=c(0.6,0.4,1,1,1,1,.9))){
  
  state <- c('S'=S0,'E'=0,'I'=1,'R'=0,'D'=0)
  times <- seq(0, days, by = 0.01)
  m=8.685/100000/365
  
  m_inf <- cfr*gamma/(1-cfr)
  alpha <- m_inf/m
  
  # alpha=1.1
  c=a/(r+gamma+m_inf)   # the ratio of E/I during exponential phase
  beta_pre=(r+a+m)/(c*S0)
  beta_intervention=beta_pre*(1-intervention_efficacy)
  beta_relaxation=beta_pre*relaxation
  lambda=11.8/1000/365
  parameters <- c('m'=m,
                  'lambda'=lambda,
                  'gamma'=gamma,
                  'a'=a,
                  'beta_pre'=beta_pre,
                  'beta_intervention'=beta_intervention,
                  'beta_relaxation'=beta_relaxation,
                  'm_inf'=m_inf,
                  'D_intervention'=intervention_deaths,
                  'D_relaxation'=relaxation_deaths,
                  'r'=r,
                  'lag_onset_to_death'=lag_onset_to_death)
  
  out <- dede(y = state, times = times, func = seird_solve, parms = parameters) %>% as.data.table
  out[,day:=ceiling(time)]
  
  out <- out[,list(S=S[.N],
                   E=E[.N],
                   I=I[.N],
                   R=R[.N],
                   D=D[.N]),by=day]
  out[,date:=seq(start_date,start_date+days,by='day')]
  out[,day_of_week:=weekdays(date)]
  setkey(out,day_of_week)
  setkey(day_of_week_effects,day_of_week)
  
  out <- day_of_week_effects[out]
  setkey(out,date)
  
  
  out[,rt:=a*E/I-m_inf-gamma]
  out[,D:=shift(D,lag_onset_to_death)]
  out[,beta:=(as.numeric(D<=intervention_deaths)*beta_pre+
                as.numeric(D>intervention_deaths & D<relaxation_deaths)*(beta_intervention)+
                as.numeric(D>=relaxation_deaths)*beta_relaxation)]
  out[,new_infections:=beta*S*I]
  
  out[is.na(D),D:=0]
  out[,new_deaths:=c(0,diff(D))]
  
  out[,case_detection:=case_detection]
  
  out[,new_confirmed:=rpois(.N,const*case_detection*I)]
  out[,n:=1:.N]
  if (gr_estimation=='nbss'){
    out <- cbind(out,nbss(out$new_confirmed))
    out[,growth_rate:=shift(growth_rate,lag_onset_to_death)]
  } else {
    out[,growth_rate:=rollapply(n,width=window_size,FUN=PoissonFit,fill=NA,
                              new_confirmed=new_confirmed,date=date,half_life=half_life,
                              day_of_week=day_of_week,align='right')]
  }
  out$n <- NULL
  out[,r:=r]
  out[,cfr:=cfr]
  out[,lag_onset_to_death:=lag_onset_to_death]
  out[,intervention_efficacy:=intervention_efficacy]
  out[,intervention_deaths:=intervention_deaths]
  out[,relaxation:=relaxation]
  out[,relaxation_deaths:=relaxation_deaths]
  out[,deaths_pc:=D/S0]
  
  return(out)
}
