library(data.table)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(ggarrange)


World <- read.csv('data/nbss_countries.csv') %>%as.data.table
US_states <- read.csv('data/nbss_us_states.csv') %>% as.data.table
US_counties <- read.csv('data/nbss_us_counties.csv') %>% as.data.table
World[,date:=as.Date(date)]
US_states[,date:=as.Date(date)]
US_counties[,date:=as.Date(date)]

wght_avg <- function(mean_position,growth_rate,deaths_pc){
  weighted.mean(deaths_pc,exp(mean_position+growth_rate))
}

# thts <- US[,list(theta=get_coef(growth_rate,deaths_pc,new_confirmed)),by=date]
avg_dpc_la <- US_counties[growth_rate>0 & !is.na(deaths_pc) & state=='Louisiana' & county!='East Feliciana',
                          list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]

avg_dpc_world <- World[growth_rate>0 & !is.na(deaths_pc),
                       list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]
avg_dpc_states <- US_states[growth_rate>0 & !is.na(deaths_pc),
                            list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]
avg_dpc_counties <- US_counties[growth_rate>0 & !is.na(deaths_pc),
                                list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]

cols <- viridis::viridis(4)
g1=ggplot(avg_dpc_la,aes(date,theta))+
  geom_line(lwd=2,col=cols[4])+
  geom_line(data=avg_dpc_world,lwd=2,col=cols[1])+
  geom_line(data=avg_dpc_states,lwd=2,col=cols[2])+
  geom_line(data=avg_dpc_counties,lwd=2,col=cols[3])+
  geom_hline(yintercept = 5e-4,lty=2,lwd=2)+
  geom_hline(yintercept=3.6e-3,lty=3,lwd=2)+
  theme_bw()+
  scale_y_continuous('Average deaths-per-capita of a growth case',
                     trans='log',breaks=10^(-7:0),limits = c(1e-7,4e-3))

W <- World[,list(confirmed=sum(confirmed,na.rm=T)),by=date]
setkey(W,date)
W[,new_confirmed:=c(confirmed[1],diff(confirmed))]
W[new_confirmed<0,new_confirmed:=NA]
W <- cbind(W,nbss(W$new_confirmed))

g2=ggplot(World[country=='United States'],aes(date,growth_rate))+
  geom_line(lwd=2,col=cols[2])+
  geom_line(data=W,lwd=2,col=cols[1])+
  geom_line(data=US_states[state=='Louisiana'],lwd=2,col=cols[4])+
  scale_y_continuous(limits=c(-0.2,0.5))+
  theme_bw()+
  geom_hline(yintercept=0)


ggarrange(g1,g2,nrow=2,align = 'v')



x <- US[country=='Peru']
plot(x$date,x$new_confirmed)
lines(x$date,exp(x$mean_position))