rm(list=ls())
gc()
library(magrittr)
library(ggpubr)
library(data.table)
library(COVID19)
library(zoo)
library(gganimate)
window_size=21
# growth_rate vs. cumulative incidence ------------------------------------------------------------------------
getGrowthRate <- function(n,new_confirmed,date,day_of_week,z_score=FALSE){
  dd <- data.table('new_confirmed'=new_confirmed,
                   'date'=date,
                   'day_of_week'=day_of_week)
  
  fit <- glm(new_confirmed~date+day_of_week,family=poisson,data=dd[n])
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



# USA ---------------------------------------------------------------------

USA <- covid19(country='United States',level=2) %>% as.data.table
colnames(USA)[colnames(USA)=='administrative_area_level_2'] <- 'state'
setkey(USA,state,date)

max(USA$date)

USA[,new_confirmed:=c(confirmed[1],diff(confirmed)),by=state]
USA[new_confirmed<0,new_confirmed:=0]
USA[,cumulative_pc_cases:=confirmed/population]
USA[,gr:=(new_confirmed+shift(new_confirmed)+shift(new_confirmed,n=3))/
      (shift(new_confirmed,7)+shift(new_confirmed,8)+shift(new_confirmed,9)),by=state]

USA[,day_of_week:=weekdays(date)]
USA[,n:=1:.N,by=state]
USA[,new_deaths:=c(deaths[1],diff(deaths)),by=state]
USA[,growth_rate:=rollapply(n,width=window_size,FUN=getGrowthRate,fill=NA,
                            new_confirmed=new_confirmed,date=date,
                            day_of_week=day_of_week,align='right'),
    by=state]
USA[,deaths_pc:=deaths/population]
x <- USA[,list(growth_rate=growth_rate[max(which(!is.na(growth_rate)))],
               deaths_pc=deaths_pc[max(which(!is.na(growth_rate)))],
               date=max(date),
               id=unique(key_alpha_2)),by=state]

ggplot(USA,aes(deaths_pc,growth_rate,by=state,color=date))+
  geom_line(lwd=2,alpha=0.2)+
  geom_hline(yintercept = log(2)/7,lty=2)+
  geom_hline(yintercept=0,lty=1)+
  geom_point(data=x,cex=2,color='red')+
  geom_text(data=x,aes(label=id),hjust=-0.3, vjust=-0.3,color='red')+
  scale_y_continuous('Exponential (Poisson) Growth Rate',limits=c(-0.25,0.5))+
  theme_bw()+
  ggtitle(paste('Incidence vs Growth rate as of',max(X$date)))+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  theme(legend.position = 'none')+
  scale_x_continuous('Cumulative deaths per Capita',trans='log',limit=c(1e-7,4e-3),breaks=10^(-8:-1))

# world -------------------------------------------------------------------
X <- covid19(level=1) %>% as.data.table
colnames(X)[colnames(X)=='administrative_area_level_1'] <- 'country'
setkey(X,country,date)

countries_of_interest <- c('United States','Belgium','Italy','Switzerland',
                           'Sweden','Norway','Netherlands','France','Spain',
                           'Brazil','India','Iran','Denmark','Finland')
X <- X[country %in% countries_of_interest]
X[country=='China',population:=5.85e7] #Assume China's sampling reflected Wuhan epidemic


X[,new_confirmed:=c(confirmed[1],diff(confirmed)),by=country]
X[new_confirmed<0,new_confirmed:=0]
X[,new_deaths:=c(deaths[1],diff(deaths)),by=country]
X[,cumulative_pc_cases:=confirmed/population]
X[,deaths_pc:=deaths/population]
X[,day_of_week:=weekdays(date)]
X[,n:=1:.N,by=country]
window_size=21
X[,growth_rate:=rollapply(n,width=window_size,FUN=getGrowthRate,fill=NA,
                          new_confirmed=new_confirmed,date=date,
                          day_of_week=day_of_week,align='right'),
  by=country]



st <- USA[state %in% c('New York','New Jersey','Louisiana')]
st[,country:=state]
st[,id:=key_alpha_2]
st[,deaths_pc:=deaths/population]
cols <- c('date','deaths_pc','growth_rate','country','id')
X <- rbind(X[,cols,with=F],
           st[,cols,with=F])


x.c <- X[,list(growth_rate=growth_rate[max(which(!is.na(growth_rate)))],
               deaths_pc=deaths_pc[max(which(!is.na(growth_rate)))],
               date=max(date),
               id=unique(id)),by=country]



xm <- X[,list(growth_rate=max(growth_rate),
              country='MAX'),by=deaths_pc]

focal_countries <- c('New York','Louisiana','Sweden','Spain')
g_world=X %>%
  ggplot(aes(deaths_pc,growth_rate,by=country))+
  geom_line(lwd=2,alpha=0.8,color='grey')+
  # geom_line(data=X[country %in% c('New York','Norway','Sweden','Denmark','Finland')],lwd=2,alpha=0.2)+
  geom_hline(yintercept = log(2)/7,lty=2)+
  geom_hline(yintercept=0,lty=1)+
  geom_line(data=X[country %in% focal_countries],aes(color=country),lwd=2)+
  geom_point(data=x.c,cex=2,color='red')+
  geom_text(data=x.c,aes(label=id),hjust=-0.3, vjust=-0.3,color='grey')+
  geom_text(data=x.c[country %in% focal_countries],aes(color=country,label=id),hjust=-0.3, vjust=-0.3)+
  scale_y_continuous('Exponential (Poisson) Growth Rate',limits=c(-0.25,0.5))+
  theme_bw()+
  ggtitle(paste('Incidence vs Growth rate as of',max(X$date)))+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  # theme(legend.position = 'none')+
  scale_colour_manual(name='Region',
                      breaks=focal_countries,
                      labels=focal_countries,
                      values=viridis::viridis(length(focal_countries)))+
  theme(legend.position=c(0.8,0.8),
        legend.background = element_rect(fill=rgb(0.9,0.9,0.9),size=0.5,linetype='solid'))+
  # geom_line(data=X[country %in% c('New York','Norway','Sweden','Denmark','Finland')],lty=1,col='black')+
  scale_x_continuous('Cumulative deaths per Capita',trans='log',limit=c(1e-7,4e-3),breaks=10^(-8:-1))

g_world



X %>% as.data.frame %>%
  ggplot(aes(deaths_pc,growth_rate,by=country,color=country))+
  geom_point(alpha=0.5,cex=2)+
  geom_point(data=as.data.frame(X[country %in% c('United States','New York','China','Sweden')]),cex=6)+
  scale_y_continuous('Exponential (Poisson) Growth Rate',limits=c(-0.25,0.5))+
  theme_bw()+
  geom_hline(yintercept = log(2)/2.6,lty=3)+
  geom_hline(yintercept = log(2)/7,lty=2)+
  geom_hline(yintercept = 0)+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  theme(legend.position = 'none')+
  scale_x_continuous('Cumulative deaths per Capita',trans='log',limit=c(1e-7,4e-3),breaks=10^(-8:-1))+
  transition_time(date)+
  labs(title = 'Date : {frame_time}')+
  ease_aes('linear')





# counties ---------------------------------------------------------------

Y <- covid19(country='United States',level=3) %>% as.data.table
colnames(Y)[colnames(Y)=='administrative_area_level_2'] <- 'state'
colnames(Y)[colnames(Y)=='administrative_area_level_3'] <- 'county'
setkey(Y,state,county,date)

max(Y$date)

Y[,new_confirmed:=c(confirmed[1],diff(confirmed)),by=c('state','county')]
Y[new_confirmed<0,new_confirmed:=0]
Y[,day_of_week:=weekdays(date)]
Y[,n:=1:.N,by=c('state','county')]
Y[,new_deaths:=c(deaths[1],diff(deaths)),by=c('state','county')]
Y[,growth_rate:=rollapply(n,width=window_size,FUN=getGrowthRate,fill=NA,
                            new_confirmed=new_confirmed,date=date,
                            day_of_week=day_of_week,align='right'),
    by=c('state','county')]
Y[,deaths_pc:=deaths/population]


Y[,scounty:=paste(key_alpha_2,county,sep='_')]
yy <- Y[,list(growth_rate=growth_rate[max(which(!is.na(growth_rate)))],
              deaths_pc=deaths_pc[max(which(!is.na(growth_rate)))],
              date=max(date),
              id=unique(key_alpha_2)),by=scounty]

ggplot(Y,aes(deaths_pc,growth_rate,by=scounty))+
  geom_line(lwd=2,alpha=0.2)+
  geom_hline(yintercept = log(2)/7,lty=2)+
  geom_hline(yintercept=0,lty=1)+
  geom_point(data=yy,cex=2,color='red')+
  geom_text(data=yy,aes(label=id),hjust=-0.3, vjust=-0.3,color='red')+
  scale_y_continuous('Exponential (Poisson) Growth Rate',limits=c(-0.25,0.5))+
  theme_bw()+
  ggtitle(paste('Incidence vs Growth rate as of',max(X$date)))+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  theme(legend.position = 'none')+
  scale_x_continuous('Cumulative deaths per Capita',trans='log',limit=c(1e-7,4e-3),breaks=10^(-8:-1))

save(list=ls(),file='data/all_growth_rates_countries_states_UScounties.Rd')
