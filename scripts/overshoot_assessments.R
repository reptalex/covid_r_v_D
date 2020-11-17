library(data.table)
library(magrittr)
library(ggplot2)
library(ggpubr)

rDlag=11

World <- read.csv('data/nbss_countries.csv') %>%as.data.table
setkey(World,country,date)
World[,deaths:=shift(deaths,n=rDlag,type='lead'),by=country]
World[,date:=as.Date(date)]

US_states <- read.csv('data/nbss_us_states.csv') %>% as.data.table
setkey(US_states,state,date)
US_states[,deaths:=shift(deaths,n=rDlag,type='lead'),by=state]
US_states[,date:=as.Date(date)]

US_counties <- read.csv('data/nbss_us_counties.csv') %>% as.data.table
setkey(US_counties,state,county,date)
US_counties[,deaths:=shift(deaths,n=rDlag,type='lead'),by=c('state','county')]
US_counties[,date:=as.Date(date)]

# wght_avg <- function(mean_position,growth_rate,deaths_pc){
#   weighted.mean(deaths_pc,exp(mean_position+growth_rate))
# }
# 
# # thts <- US[,list(theta=get_coef(growth_rate,deaths_pc,new_confirmed)),by=date]
# avg_dpc_la <- US_counties[growth_rate>0 & !is.na(deaths_pc) & state=='Louisiana',
#                           list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]
# 
# avg_dpc_world <- World[growth_rate>0 & !is.na(deaths_pc),
#                        list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]
# avg_dpc_states <- US_states[growth_rate>0 & !is.na(deaths_pc),
#                             list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]
# avg_dpc_counties <- US_counties[growth_rate>0 & !is.na(deaths_pc),
#                                 list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]
# 
# cols <- viridis::viridis(4)
# g1=ggplot(avg_dpc_la,aes(date,theta))+
#   geom_line(lwd=2,col=cols[4])+
#   geom_point(data=World[growth_rate>0 & !is.na(deaths_pc)],aes(date,deaths_pc),col=cols[1],pch=16,alpha=0.02)+
#   geom_point(data=US_counties[growth_rate>0 & !is.na(deaths_pc)],aes(date,deaths_pc),col=cols[3],pch=16,alpha=0.02)+
#   geom_point(data=US_states[growth_rate>0 & !is.na(deaths_pc)],aes(date,deaths_pc),col=cols[2],pch=16,alpha=0.02)+
#   geom_line(data=US_counties[growth_rate>0 & !is.na(deaths_pc) & state=='Louisiana'],
#              aes(date,deaths_pc,by=county),col=cols[4],alpha=0.2,lwd=2)+
#   geom_line(lwd=1)+
#   geom_line(data=avg_dpc_world,lwd=2,col=cols[1])+
#   geom_line(data=avg_dpc_states,lwd=2,col=cols[2])+
#   geom_line(data=avg_dpc_counties,lwd=2,col=cols[3])+
#   geom_hline(yintercept = 5e-4,lty=2,lwd=2)+
#   geom_hline(yintercept=3.6e-3,lty=3,lwd=2)+
#   theme_bw()+
#   scale_y_continuous('Average deaths-per-capita of a growth case',
#                      trans='log',breaks=10^(-7:0),limits = c(1e-7,4e-3))
# 
# W <- World[,list(confirmed=sum(confirmed,na.rm=T)),by=date]
# setkey(W,date)
# W[,new_confirmed:=c(confirmed[1],diff(confirmed))]
# W[new_confirmed<0,new_confirmed:=NA]
# W <- cbind(W,nbss(W$new_confirmed))
# 
# g1
# ggsave('figures/LA_US_world_growth-case_experienced_dpc.png',height=8,width=11)
# 
# g2=ggplot(World[country=='United States'],aes(date,growth_rate))+
#   geom_line(lwd=2,col=cols[2])+
#   geom_line(data=W,lwd=2,col=cols[1])+
#   geom_line(data=US_states[state=='Louisiana'],lwd=2,col=cols[4])+
#   scale_y_continuous(limits=c(-0.2,0.5))+
#   theme_bw()+
#   geom_hline(yintercept=0)
# 
# 
# ggarrange(g1,g2,nrow=2,align = 'v')
# 
# 
# 
# ggplot(US_states[state!='Guam'],aes(date,new_confirmed,color=state,fill=state))+
#   geom_bar(stat='identity')+
#   geom_bar(aes(date,new_deaths),stat='identity',color='grey',fill='grey')+
#   facet_wrap(.~state)+
#   scale_y_continuous(trans='log',breaks = 10^(0:4))+
#   theme_bw()+
#   theme(legend.position='none')
# ggsave('figures/US_states_cases_deaths.png',height=8,width=11)
# 
# 
# x <- US[country=='Peru']
# plot(x$date,x$new_confirmed)
# lines(x$date,exp(x$mean_position))



# All US states: ----------------------------------------------------------
US_states[,deaths_pc:=deaths/population]
World[,deaths_pc:=deaths/population]
US_counties[,deaths_pc:=deaths/population]
US <- World[country=='United States']
setkey(US,date)
setkey(US_states,date)

US[,us_total:=deaths_pc]

US_states <- US[,c('date','us_total')][US_states]
wght_avg <- function(mean_position,growth_rate,deaths_pc,population){
  weighted.mean(deaths_pc,exp(mean_position+growth_rate))
  # weighted.mean(deaths_pc,exp(growth_rate))
}
avg_dpc <- US_counties[growth_rate>0 & !is.na(deaths_pc),
                          list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=c('state','date')]

sts <- setdiff(unique(US_counties$state),c('Puerto Rico','Guam','Northern Mariana Islands',
                                           'District of Columbia','Virgin Islands'))
ggplot(US_counties[state %in% sts],aes(date,deaths_pc))+
  geom_point(aes(color=factor(sign(growth_rate))),pch=16,alpha=0.05)+
  geom_line(data=US_states[state %in% sts],lwd=2,col=rgb(1,2/3,0))+
  geom_line(data=US_states[state %in% sts],aes(date,us_total),col=rgb(0.2,0,0.6),lwd=1.5)+
  geom_line(data=avg_dpc[state %in% sts],aes(date,theta),col='black')+
  geom_hline(yintercept = 5e-4,lty=3)+
  geom_hline(yintercept = 3.6e-3,lty=2)+
  scale_y_continuous(trans='log',limits=c(1e-6,4e-3),breaks = c(1e-6,1e-5,1e-4,1e-3))+
  facet_wrap(.~state)+
  theme_bw()+
  theme(legend.position='none')
ggsave('figures/US_state_experienced_dpc_11_08.png',height=8,width=11,units='in')


us_sts <- US_states[growth_rate>0 & !is.na(deaths_pc),
           list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]

ggplot(US_states[state %in% sts],aes(date,deaths_pc))+
  geom_point(aes(color=factor(sign(growth_rate))),alpha=0.1)+
  geom_line(data=US,col=rgb(0.2,0,0.6),lwd=2)+
  geom_line(data=us_sts,aes(date,theta),col='black')+
  geom_hline(yintercept = 1.2e-3,lty=3,lwd=2)+
  geom_hline(yintercept = 3.6e-3,lty=2,lwd=2)+
  scale_y_continuous(trans='log',limits=c(1e-6,4e-3),breaks = c(1e-6,1e-5,1e-4,1e-3))+
  theme_bw()+
  theme(legend.position='none')

w <- World[growth_rate>0 & !is.na(deaths_pc),
           list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]
Tot <- World[,list(deaths_pc=sum(deaths)/7.8e9),by=date]

ggplot(World,aes(date,deaths_pc))+
  geom_point(aes(color=factor(sign(growth_rate))),alpha=0.1)+
  geom_line(data=Tot,col=rgb(0.2,0.5,0),lwd=2)+
  geom_line(data=w,aes(date,theta),col='black')+
  geom_line(data=US,col=rgb(0.2,0,.6),lwd=2)+
  geom_hline(yintercept = 5e-4,lty=2)+
  geom_hline(yintercept = 3.6e-3,lty=3)+
  scale_y_continuous(trans='log',limits=c(1e-8,4e-3),breaks = c(1e-6,1e-5,1e-4,1e-3))+
  theme_bw()+
  theme(legend.position='none')

World[,list(n=length(unique(country))),by=date] %>%
  ggplot(aes(date,n))+geom_line()+theme_bw()
