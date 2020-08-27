rm(list=ls())
gc()
library(magrittr)
library(ggpubr)
library(data.table)
library(COVID19)
library(zoo)
library(mgcv)
library(EpiEstim)
source('scripts/utils.R')








# R_e ---------------------------------------------------------------------

# 
# x <- seird(log(2)/2.6)
# 
# # https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article SI: mean SI = 3.96, sd=4.75
# 
# ### following Gostic et al.'s implementation of Cori et al.
# out <- estimate_R(x$new_confirmed,
#            method='parametric_si',
#            config = make_config(
#              list(
#                mean_si = 3.96,
#                std_si = 4.75
#              )
#            ))
# 
# 
# plot(out$dates[8:nrow(x)],log(out$R$`Mean(R)`))
# lines(x$day[1:(nrow(x)-7)],x$rt[1:(nrow(x)-7)])
# out$R %>%
#   mutate(time = if(wend == TRUE) t_end else ceiling((t_end+t_start)/2) ) %>%
#   select(time, `Mean(R)`, `Quantile.0.025(R)`, `Quantile.0.975(R)`) %>%
#   setNames(c('time', paste0(out_name, '.mean'), paste0(out_name, '.025'), paste0(out_name, '.975')))

# transmission rate -------------------------------------------------------

S0=3.27e8
growth_rates=log(2)/c(2,5,10)
x <- NULL
for (r in growth_rates){
  x <- rbind(x,seird(r,days=400))
}

x[,doubling_time:=factor(round(log(2)/r))]

g1=ggplot(x,aes(deaths_pc,growth_rate,by=doubling_time,color=doubling_time))+
  geom_line(lwd=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:1),limits=c(1e-8,0.1))+
  scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,1.2*max(x$r)))+
  ggtitle('Epidemics over Deaths per-capita')+
  geom_vline(lty=2,xintercept = 0.004)+
  theme_bw()

g2=ggplot(x,aes(date,new_confirmed/S0,by=doubling_time,color=doubling_time))+
  geom_line(lwd=2)+
  scale_y_continuous('Cases per capita')+
  ggtitle('Epidemics over Time')+
  theme_bw()

ggarrange(g2,g1,nrow=2)
ggsave('figures/epidemics_over_time_and_deaths.png',height=10,width=11,units='in')




ggplot(x[date>as.Date('2020-02-01')],aes(date,shift(rt,16),color=doubling_time))+
  geom_line(lwd=2)+
  geom_line(aes(date,growth_rate),lwd=2,lty=2,col='black')+
  facet_wrap(.~doubling_time)+
  scale_y_continuous(limits=c(-0.3,1))+
  ggtitle('Suitability of rolling Poisson glm to estimate r(t)')+
  theme_bw()
ggsave('figures/suitability_of_nbss_estimates_of_rt.png',height=8,width=11,units='in')


mx <- x[,list(day=day[which.max(I)],
              deaths_pc=deaths_pc[which.max(I)],
              rt=rt[which.max(I)],
              date=date[which.max(I)]),by=doubling_time]

g_dt=ggplot(x,aes(day,deaths_pc,by=doubling_time,color=doubling_time))+
  geom_line(lwd=2)+
  geom_point(data=mx,cex=7,pch=19)
g_dt_log <- g_dt+scale_y_continuous(trans='log',breaks=10^(-8:0))
ggarrange(g_dt,g_dt_log,nrow=2)+
  theme_bw()
ggsave('figures/death_time_scaling_with_peak_days.png',height=10,width=11,units='in')

 
# x[,doubling_time:=factor(doubling_time,levels =rev(c(2,3,5,7,10,14)))]
# g_i=ggplot(x[date>as.Date('2020-02-15') & date<as.Date('2020-11-01')],
#            aes(date,new_confirmed/S0,by=doubling_time,color=doubling_time))+
#   geom_line(lwd=2)+
#   scale_y_continuous('Cases per capita')+
#   ggtitle('Epidemics over Time')
# g_rt=ggplot(x[date>as.Date('2020-02-15') & date<as.Date('2020-11-01')],
#             aes(date,growth_rate,by=doubling_time,color=doubling_time))+
#   geom_hline(lwd=2,col='black',yintercept = 0)+
#   geom_line(lwd=2)+
#   scale_y_continuous('Estimated Exponential Growth Rate, r(t)',limits = c(-1/7,log(2)))+
#   ggtitle('Growth Rate over Time')
# 
# 
# 
# ggarrange(g_i,g_rt,nrow=2)
# 
# 
# x[abs(growth_rate)>1,growth_rate:=NA]



# NY analysis -------------------------------------------------------------
US <- COVID19::covid19(country='United States',level=2) %>% as.data.table
US[,state:=administrative_area_level_2]
NY <- US[state=='New York']
setkey(NY,date)

NY[,new_deaths:=c(deaths[1],diff(deaths))]

ggplot(NY[date<as.Date('2020-05-01')],aes(date,new_deaths))+
  geom_point()+
  scale_y_continuous(trans='log')

gr <- glm(new_deaths~date,family=poisson,data=NY[date>as.Date('2020-03-15') & date<as.Date('2020-04-02')])

NY[,new_confirmed:=c(confirmed[1],diff(confirmed))]
NY <- nbss(NY)
NY <- as.data.table(NY)
NY[,intervention_efficacy:='NY State']
NY[,deaths_pc:=deaths/population]

# interventions -----------------------------------------------------------
NY[date>as.Date('2020-03-15') & date<as.Date('2020-04-02'),growth_rate]

S0=19.54e6
# intervention_efficacies=seq(0,1,by=0.2)
intervention_efficacies <- c(0,0.25,0.5,0.75,1)
intervention_deaths=1
y <- NULL

r <- 0.2585 ## New York value obtained above
for (a in intervention_efficacies){
  y <- rbind(y,seird(r,intervention_efficacy = a,
                     intervention_deaths = intervention_deaths,days=400,
                     S0=S0,gamma=1/4))
}


y[,doubling_time:=log(2)/(r*intervention_efficacy)]
y[,intervention_efficacy:=factor(intervention_efficacy)]

ggplot(y,aes(deaths_pc,growth_rate,by=intervention_efficacy,color=intervention_efficacy))+
  geom_line(lwd=2)+
  geom_line(aes(deaths_pc,shift(rt,16)),lty=2)+
  geom_vline(xintercept = intervention_deaths/S0,lwd=2,lty=2)+
  geom_line(data=NY,aes(deaths_pc,shift(growth_rate,10)),col='black')+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:0),limits=c(1e-6,3e-3))+
  scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,0.5))+
  ggtitle('Early Intervention')+
  theme_bw()

ggsave('figures/nbss_inference_under_instantaneous_intervention.png',height=8,width=11)

ggplot(y,aes(deaths_pc,beta,by=intervention_efficacy,color=intervention_efficacy))+
  geom_line(lwd=2)+
  geom_vline(xintercept = intervention_deaths/S0)+
  scale_x_continuous(trans='log')

g_early=ggplot(y,aes(deaths_pc,growth_rate,by=intervention_efficacy,color=intervention_efficacy))+
  geom_line(lwd=2)+
  geom_vline(xintercept = intervention_deaths/S0,lwd=2,lty=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:0),limits=c(1e-8,1.1*max(y$deaths_pc)))+
  scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,1.2*max(y$r)))+
  ggtitle('Early Intervention')+
  theme_bw()
g_early


y <- NULL
for (a in intervention_efficacies){
  y <- rbind(y,seird(r,intervention_efficacy = a,
                     intervention_deaths = 114,
                     days=400,S0=S0))
}


y[,intervention_efficacy:=factor(intervention_efficacy)]


g_late=ggplot(y,aes(deaths_pc,growth_rate,by=intervention_efficacy,color=intervention_efficacy))+
  geom_line(lwd=2)+
  geom_vline(xintercept=114/S0,lty=3,lwd=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:0),limits=c(1e-8,1.1*max(y$deaths_pc)))+
  scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,1.2*max(y$r)))+
  ggtitle('Late Intervention')+
  theme_bw()


ggarrange(g_early,g_late,nrow=2)
ggsave('figures/early_vs_late_intervention.png',height=10,width=12,units='in')


ggarrange(g_early+
            geom_line(aes(deaths_pc,shift(rt,16)),lty=2),
          g_late+
            geom_line(aes(deaths_pc,shift(rt,16)),lty=2),nrow=2)
ggsave('figures/early_vs_late_intervention_w_seir.png',height=10,width=12,units='in')


# Relaxation --------------------------------------------------------------
S0=19.54e6
intervention_efficacy <- 0.8
intervention_deaths=1
relaxation_deaths=c(2,100,5000)

r=0.2585
y <- seird(r,days=400,S0=S0)
y[,relaxation_deaths:=0]
for (b in relaxation_deaths){
  y <- rbind(y,seird(r,intervention_efficacy = intervention_efficacy,
                     intervention_deaths = intervention_deaths,
                     relaxation_deaths=b,
                     days=400,S0=S0))
}

z <- seird(r,S0=S0,intervention_deaths = 50,intervention_efficacy = 1)
z$relaxation_deaths <- NULL
z$relaxation_deaths='Successful Intervention'

y <- rbind(y,z)

y[,relaxation_deaths:=factor(relaxation_deaths)]
ggplot(y,aes(deaths_pc,growth_rate,by=relaxation_deaths,color=relaxation_deaths))+
  geom_line(data=z,lwd=2,col='black')+
  geom_line(lwd=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:0),limits=c(1e-8,1.1*max(y$deaths_pc)))+
  scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,1.2*max(y$r)))+
  # geom_line(data=z,col='black',lty=2,lwd=2)+
  ggtitle('Relaxation of Interventions')+
  theme_bw()+
  geom_vline(xintercept = 4e-3)+
  scale_color_manual(values=c(viridis::viridis(4),'black'),labels=c(0,relaxation_deaths,'Successful Intervention'))

ggsave('figures/relaxation_of_interventions_70prc_effective_to_complete_relax.png',height=6,width=11,units='in')


ggplot(y,aes(deaths_pc,growth_rate,by=relaxation_deaths,color=relaxation_deaths))+
  geom_line(lwd=2)+
  # geom_vline(xintercept=intervention_deaths/S0,lty=3,lwd=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:0),limits=c(1e-8,1.1*max(y$deaths_pc)))+
  scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,1.2*max(y$r)))+
  # geom_line(data=z,col='black',lty=2,lwd=2)+
  ggtitle('Relaxation of Interventions')+
  theme_bw()+
  # geom_vline(xintercept = 4e-3)+
  geom_line(aes(deaths_pc,shift(rt,16)),lty=2)+
  scale_color_manual(values=c(viridis::viridis(4),'black'),labels=c(0,relaxation_deaths,'Successful Intervention'))
ggsave('figures/relaxation_of_interventions_70prc_effective_to_complete_relax_w_seir.png',height=6,width=11,units='in')

