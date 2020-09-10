rm(list=ls())
gc()
library(magrittr)
library(ggpubr)
library(data.table)
library(COVID19)
library(zoo)
library(mgcv)
library(EpiEstim)
library(viridis)
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

S0=19.54e6
growth_rates=log(2)/c(2,5,10)
x <- NULL
for (r in growth_rates){
  x <- rbind(x,seird(r,days=400))
}

x[,doubling_time:=factor(round(log(2)/r))]


g1=ggplot(x,aes(date,new_confirmed/S0,by=doubling_time,color=doubling_time))+
  geom_line(lwd=2)+
  scale_y_continuous('Cases per capita')+
  ggtitle('Epidemics over Time')+
  theme_bw()+
  theme(legend.position = c(0.8,0.8))
g2=ggplot(x,aes(deaths_pc,growth_rate,by=doubling_time,color=doubling_time))+
  geom_line(lwd=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:1),limits=c(1e-7,4e-3))+
  scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,1.2*max(x$r)))+
  ggtitle('Epidemics over Deaths per-capita')+
  geom_vline(lty=2,xintercept = 0.0036,lwd=2)+
  theme_bw()+
  theme(legend.position = 'none')



ggarrange(g1,g2,nrow=2)
ggsave('figures/epidemics_over_time_and_deaths.png',height=10,width=11,units='in')




ggplot(x[date>as.Date('2020-02-01')],aes(date,shift(rt,16),color=doubling_time))+
  geom_line(lwd=2)+
  geom_line(aes(date,growth_rate),lwd=1,lty=1,col='black')+
  facet_wrap(.~doubling_time,nrow=3)+
  scale_y_continuous(limits=c(-0.2,0.375))+
  ggtitle('Suitability of rolling Poisson glm to estimate r(t)')+
  theme_bw()+
  theme(legend.position=c(0.8,0.88))
ggsave('figures/suitability_of_nbss_estimates_of_rt.png',height=11,width=6,units='in')


mx <- x[,list(day=day[which.max(I)],
              deaths_pc=deaths_pc[which.max(I)],
              rt=rt[which.max(I)],
              date=date[which.max(I)]),by=doubling_time]

g_dt=ggplot(x,aes(day,deaths_pc,by=doubling_time,color=doubling_time))+
  geom_line(lwd=2)+
  geom_point(data=mx,cex=7,pch=19)+
  theme_bw()
g_dt_log <- g_dt+scale_y_continuous(trans='log',breaks=10^(-8:0))
ggarrange(g_dt,g_dt_log,nrow=2)
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
NY <- cbind(NY,nbss(NY$new_confirmed))
NY <- as.data.table(NY)
NY[,intervention_efficacy:='NY State']
NY[,deaths_pc:=deaths/population]
NY[,deaths_pc:=shift(deaths_pc,8,type='lead')]

Deaths_ny <- cbind(NY[date>as.Date('2020-02-28'),c('date','deaths_pc')],nbss(NY[date>as.Date('2020-02-28')]$new_deaths))

ggplot(dd,aes(deaths_pc,growth_rate))+
  geom_line(data=Deaths_ny,lwd=2,col='black')+
  theme_bw()+
  geom_vline(xintercept = 114/S0,lty=2,lwd=2)
# interventions -----------------------------------------------------------
NY[date>as.Date('2020-03-15') & date<as.Date('2020-04-02'),growth_rate]

S0=19.54e6
# intervention_efficacies=seq(0,1,by=0.2)
intervention_efficacies <- c(0,0.25,0.5,0.75,1)
intervention_deaths=1
y <- NULL

r <- 0.2585 ## New York value obtained above from more carefully curated analysis, 2.7 day doubling time
# r <- 0.375 ## the approximate y-intercept from the nbss fit to NY; 1.8 day doubling time
g_ny <- -min(NY$growth_rate)
for (a in intervention_efficacies){
  y <- rbind(y,seird(r,intervention_efficacy = a,
                     intervention_deaths = intervention_deaths,days=400,
                     S0=S0,gamma=g_ny))
}


y[,doubling_time:=log(2)/(r*intervention_efficacy)]
y[,intervention_efficacy:=factor(intervention_efficacy)]


y_thresh <- y[intervention_efficacy==0]
y_thresh <- rbind(y_thresh,y_thresh[.N])
y_thresh[.N,deaths_pc:=1]



ggplot(y,aes(deaths_pc,growth_rate,by=intervention_efficacy,color=intervention_efficacy))+
  geom_line(lwd=2)+
  geom_vline(xintercept = intervention_deaths/S0,lwd=2,lty=2)+
  geom_line(data=NY,aes(deaths_pc,shift(growth_rate,10)),col='black',lwd=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:0),limits=c(1e-7,4e-3))+
  # scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,0.5))+
  ggtitle('Early Intervention')+
  geom_ribbon(data=y_thresh,aes(ymin=growth_rate,ymax=0.6),alpha=0.3)+
  coord_cartesian(xlim = c(1e-7,4e-3),ylim = c(-0.2,0.5))+
  theme_bw()

ggsave('figures/nbss_inference_under_instantaneous_intervention.png',height=8,width=11)

ggplot(y,aes(deaths_pc,beta,by=intervention_efficacy,color=intervention_efficacy))+
  geom_line(lwd=2)+
  geom_vline(xintercept = intervention_deaths/S0)+
  scale_x_continuous(trans='log')

g_early=ggplot(y,aes(deaths_pc,growth_rate,by=intervention_efficacy,color=intervention_efficacy))+
  geom_line(lwd=2)+
  geom_vline(xintercept = 1/S0,lwd=2,lty=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:0),limits=c(1e-9,4e-3))+
  # scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,1.2*max(y$r)))+
  ggtitle('Early Intervention')+
  geom_ribbon(data=y_thresh,aes(ymin=growth_rate,ymax=0.6),alpha=0.3)+
  coord_cartesian(xlim = c(1e-8,4e-3),ylim = c(-0.2,0.5))+
  theme_bw()
g_early


y_mid <- NULL
for (a in intervention_efficacies){
  y_mid <- rbind(y_mid,seird(r,intervention_efficacy = a,
                               intervention_deaths = 10,
                               days=400,S0=S0))
}


y_mid[,intervention_efficacy:=factor(intervention_efficacy)]


g_mid=ggplot(y_mid,aes(deaths_pc,growth_rate,by=intervention_efficacy,color=intervention_efficacy))+
  geom_line(lwd=2)+
  geom_vline(xintercept=114/S0,lty=3,lwd=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:0),limits=c(1e-8,4e-3))+
  geom_ribbon(data=y_thresh,aes(ymin=growth_rate,ymax=0.6),alpha=0.3)+
  coord_cartesian(xlim = c(1e-8,4e-3),ylim = c(-0.2,0.5))+
  # scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,1.2*max(y$r)))+
  ggtitle('Intermediate Intervention')+
  theme_bw()


y_late <- NULL
for (a in intervention_efficacies){
  y_late <- rbind(y_late,seird(r,intervention_efficacy = a,
                     intervention_deaths = 114,
                     days=400,S0=S0))
}


y_late[,intervention_efficacy:=factor(intervention_efficacy)]


g_late=ggplot(y_late,aes(deaths_pc,growth_rate,by=intervention_efficacy,color=intervention_efficacy))+
  geom_line(lwd=2)+
  geom_vline(xintercept=114/S0,lty=3,lwd=2)+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-8:0),limits=c(1e-8,4e-3))+
  geom_ribbon(data=y_thresh,aes(ymin=growth_rate,ymax=0.6),alpha=0.3)+
  coord_cartesian(xlim = c(1e-8,4e-3),ylim = c(-0.2,0.5))+
  # scale_y_continuous('Inferred Growth Rate',limits=c(-0.2,1.2*max(y$r)))+
  ggtitle('Late Intervention')+
  theme_bw()


ggarrange(g_early,g_late,nrow=2,align='v')
ggsave('figures/early_vs_late_intervention.png',height=10,width=12,units='in')


ggarrange(g_early+
            geom_line(aes(deaths_pc,shift(rt,16)),lty=2),
          g_late+
            geom_line(aes(deaths_pc,shift(rt,16)),lty=2),nrow=2)
ggsave('figures/early_vs_late_intervention_w_seir.png',height=10,width=12,units='in')

save(list=ls(),file='data/sims_workspace')


#  ------------------------------------------------------------------------

## and what intervention-deaths, assuming 100% effective, do we get closest to NY's final deaths per-capita?
NY[,max(deaths_pc,na.rm=T)]
ny_dpc <- 0.001072073

# ds <- 1:5 ##
# dd <- NULL
# for (d in ds){
#   dd <- rbind(dd,seird(0.375,intervention_efficacy = 1,
#                         intervention_deaths = d,
#                         days=200,S0=S0,gamma=g_ny))
# }
# 
# dd[,list('dpc'=max(deaths_pc,na.rm=T)),by=intervention_deaths]
# ix=which.min(abs(dd[,list('dpc'=max(deaths_pc,na.rm=T)),by=intervention_deaths]$dpc-ny_dpc)) ##intervention_deaths=3 wins
# 
# ny_d <- dd[,list('dpc'=max(deaths_pc,na.rm=T)),by=intervention_deaths][ix,intervention_deaths]



# Relaxation --------------------------------------------------------------

y_thresh <- seird(0.375,S0=S0,gamma=g_ny)
y_thresh <- rbind(y_thresh,y_thresh[.N])
y_thresh[.N,deaths_pc:=1]
y_low_S <- seird(0.375,S0=S0*(ny_dpc/0.004),gamma=g_ny)
y_low_S[,deaths_pc:=D/S0]
y_low_ifr <- seird(0.375,cfr=ny_dpc,gamma=g_ny,S0=S0)
### Here, we'll simulate two scenarios: 
# (1) a relaxation with S0=100%, compare to 100% effective intervention ending at NY's dpc of 0.00107
# (2) relaxation with S0=25%

intervention_efficacy <- 0.85
intervention_deaths=0.1
relaxation_deaths=c(2,100,5000)

# r=0.2585
rr_full <- NULL
rr_low_ifr <- NULL
rr_low_S0 <- NULL
low_susceptible_fraction <- ny_dpc/0.004
for (b in relaxation_deaths){
  rr_full <- rbind(rr_full,seird(r=0.375,intervention_efficacy = intervention_efficacy,
                     intervention_deaths = intervention_deaths,
                     relaxation_deaths=b,gamma=g_ny,
                     days=400,S0=S0))
  rr_low_ifr <- rbind(rr_low_ifr,seird(r=0.375,cfr=ny_dpc,intervention_efficacy = intervention_efficacy,
                            intervention_deaths = intervention_deaths,
                            relaxation_deaths=b,gamma=g_ny,
                            days=400,S0=S0))
  rr_low_S0 <- rbind(rr_low_S0,seird(r=0.375,intervention_efficacy = intervention_efficacy,
                            intervention_deaths = intervention_deaths,
                            relaxation_deaths=b,gamma=g_ny,
                            days=400,S0=S0*low_susceptible_fraction))
}
rr_low_S0[,deaths_pc:=D/S0]

##upper boundary true
g_upper <- ggplot(y_thresh,aes(deaths_pc,growth_rate))+
  theme_bw()+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-7:0),limits=c(1e-9,4e-3))+
  geom_ribbon(data=NY,aes(deaths_pc,ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),alpha=0.1)+
  geom_line(data=NY,aes(deaths_pc,growth_rate),lwd=2,col='black')+
  geom_ribbon(data=y_thresh,aes(deaths_pc,growth_rate,ymin=growth_rate,ymax=0.6),alpha=0.3)+
  geom_line(data=rr_full,aes(deaths_pc,growth_rate,by=factor(relaxation_deaths),color=factor(relaxation_deaths)),lwd=2)+
  coord_cartesian(xlim = c(1e-7,4e-3),ylim = c(-0.1,0.5))+
  scale_color_manual(values=viridis(3))+
  theme(legend.position='none')+
  ggtitle('NE Corridor is not an Epidemic Resistance Line')


##lower boundary true
g_lower <- ggplot(y_thresh,aes(deaths_pc,growth_rate))+
  theme_bw()+
  scale_x_continuous('Deaths per capita',trans='log',breaks=10^(-7:0),limits=c(1e-9,4e-3))+
  geom_ribbon(data=NY,aes(deaths_pc,ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),alpha=0.1)+
  geom_line(data=NY,aes(deaths_pc,growth_rate),lwd=2,col='black')+
  geom_ribbon(data=y_thresh,aes(deaths_pc,growth_rate,ymin=growth_rate,ymax=0.6),alpha=0.3)+
  geom_line(data=rr_low_S0,aes(deaths_pc,growth_rate,by=factor(relaxation_deaths),color=factor(relaxation_deaths)),lwd=2)+
  coord_cartesian(xlim = c(1e-7,4e-3),ylim = c(-0.1,0.5))+
  scale_color_manual(values=viridis(3))+
  theme(legend.position='none')+
  ggtitle('NE Corridor is universal Epidemic Resistance Line')



ggarrange(g_upper,g_lower,nrow=2,align='v')
ggsave('figures/sims_upper_lower_bound.png',height=11,width=7)


yD <- seird(r=0.4,S0=S0*low_susceptible_fraction,growth_rate_deaths = TRUE,gamma=g_ny)
yD[,deaths_pc:=D/S0]

png(filename = 'figures/Lag_effect_growth_rate_from_deaths.png',height = 3.5,width=15,units = 'in',res=421)
par(mfrow=c(1,5))
for (ll in c(-10,-4,0,4,10)){
  plot(shift(Deaths_ny$deaths_pc,ll,type='lead'),Deaths_ny$growth_rate,type='l',lwd=2,
       xlab='D(t+tau)',ylab='r(t)',main=paste('tau=',ll,sep=''))
  lines(shift(yD$deaths_pc,ll,type='lead'),yD$growth_rate,col='orange',lwd=2,lty=2)
  lines(shift(rr_low_S0[relaxation_deaths==100]$deaths_pc,ll,type='lead'),
        rr_low_S0[relaxation_deaths==100]$growth_rate,lwd=2,col=viridis(2)[1],lty=3)
  points(shift(Deaths_ny$deaths_pc,ll,type='lead'),Deaths_ny$growth_rate)
  points(shift(Deaths_ny$deaths_pc,ll,type='lead')[Deaths_ny$date==as.Date('2020-03-22')],
         Deaths_ny[date==as.Date('2020-03-22')]$growth_rate,cex=2,pch=16,col='red')
  if (ll==4){
    legend('topright',lty=c(1,2,3,NA),pch=c(1,NA,NA,16),col=c('black','orange',viridis(2)[1],'red'),
            legend = c('NY State','No intervention','Intervention/Relaxation','Shelter-in-Place'))
  }
}
dev.off()



# relation between D, Z ---------------------------------------------------

x <- seird(0.4,gamma=g_ny,S0=S0)


x[,Z:=E+I+R]
x[,D_inst:=shift(D,19,type='lead')]


ll=19
ggplot(x,aes(shift(D,ll,type='lead'),I+E,color=rt))+
  geom_line(lwd=5)+
  theme_bw()+
  scale_x_continuous('D(t+tau+1/a)',limits=c(1,4e4))+
  geom_vline(xintercept = shift(x[day>0][rt<0,]$D,ll,type='lead')[1])
