rm(list=ls())
gc()
library(magrittr)
library(ggpubr)
library(data.table)
library(COVID19)
library(zoo)
library(gganimate)
library(viridis)
library(ggplot2)
library(mgcv)
source('~/COVID/covid_r_v_D/scripts/utils.R')

rDlag <- 11
focal_states <- c('Montana','Idaho','North Dakota','South Dakota','New Mexico','Indiana','Illinois','Florida','Texas')

fill_dpc <- function(deaths_pc){
  dd <- data.table('y'=deaths_pc)
  dd[,x:=1:.N]
  fit <- gam(y~s(x),data=dd)
  
  dpc <- c(dd[!is.na(y),y],predict(fit,newdata = dd[is.na(y)]))
}

# growth_rate vs. cumulative incidence ------------------------------------------------------------------------
nbs <- function(x) nbss(x)$growth_rate


USA <- covid19(country='United States',level=2) %>% as.data.table
USA[,state:=administrative_area_level_2]
setkey(USA,state,date)
USA <- USA[state %in% c('New Jersey','New York',focal_states)]

USA[,new_confirmed:=c(confirmed[1],diff(confirmed)),by=state]
USA[new_confirmed<0,new_confirmed:=0]
USA[,growth_rate:=nbs(new_confirmed),by=state]
USA[,deaths_pc:=shift(deaths,n=rDlag,type='lead')/population,by=state]
max_obs_date <- max(USA[!is.na(deaths_pc),date])
USA[,country:=state]
USA[,new_deaths:=c(deaths[1],diff(deaths)),by=state]

USA[,deaths_pc:=fill_dpc(deaths_pc),by=state]

# Countries ---------------------------------------------------------------

X <- covid19() %>% as.data.table
X[,country:=administrative_area_level_1]
X <- X[country =='Sweden']
setkey(X,country,date)


X[,new_confirmed:=c(confirmed[1],diff(confirmed)),by=country]
X[new_confirmed<0,new_confirmed:=0]
X[,growth_rate:=nbs(new_confirmed),by=country]
X[,deaths_pc:=shift(deaths,n=rDlag,type='lead')/population,by=country]
X[,new_deaths:=c(deaths[1],diff(deaths)),by=country]

relevant_columns <- c('new_confirmed','new_deaths','deaths_pc','growth_rate','country','date')
X <- rbind(X[,relevant_columns,with=F],USA[state %in% focal_states,relevant_columns,with=F])

swy <- rgb(1,205/255,0)
# ny <- rgb(0,0.1,1,0.5)
# nj <- rgb(0,0.1,0.8,0.5)
ny <- rgb(0,0,0)
nj <- rgb(0.2,0.2,0.2)
cls <- rainbow(length(focal_states))

col_plot <- function(cntry,cl,X.=X,USA.=USA,swy.=swy,ny.=ny,nj.=nj,death_col=rgb(0.5,0.5,0.5),max_obs_date.=max_obs_date){
  case_label <- data.frame('deaths_pc'=exp((log(2e-4)+log(6e-3))/2),
                           'growth_rate'=0.5,
                           'country'='Cases')
  Nt <- data.frame('deaths_pc'=1.5e-4,
                   'growth_rate'=0.35,
                   'country'='log(N(t))')
  tm <- data.frame('deaths_pc'=1.2e-3,
                   'growth_rate'=0.2,
                   'country'='t')
  g1=ggplot(X,aes(deaths_pc,growth_rate,by=country))+
    geom_line(lwd=2,col='grey')+
    geom_line(data=X[country=='Sweden'],lwd=2,col=swy)+
    geom_line(data=USA[state=='New York'],lwd=2,col=ny)+
    geom_line(data=USA[state=='New Jersey'],lwd=2,col=nj)+
    geom_line(data=X[country==cntry & date<=max_obs_date],lwd=3,col=cl)+
    geom_line(data=X[country==cntry & date<=max_obs_date],col='black')+
    geom_point(data=X[country==cntry & date>max_obs_date],col=cl,pch=16,cex=3)+
    geom_point(data=X[country==cntry & date>max_obs_date],col='black',pch=16,cex=1)+
    theme_bw()+
    theme(legend.position='none')+
    ggtitle(cntry)+
    geom_hline(yintercept = 0)+
    scale_x_continuous(limits=c(1e-6,4e-3),trans='log',breaks=10^(-6:0))+
    scale_y_continuous(limits=c(-0.2,0.5))+
    geom_text(data=case_label,aes(label=country))+
    geom_label(data=case_label,aes(label=country))+
    geom_text(data=Nt,aes(label=country))+
    geom_text(data=tm,aes(label=country))
  g2=ggplot(X[country==cntry & date>as.Date('2020-02-15')],
            aes(date,new_confirmed))+
    geom_bar(stat='identity',fill=cl,color=cl)+
    theme(legend.position='none')+
    scale_y_continuous('log(N(t))',trans='log')+
    scale_x_continuous(name='t',breaks=NULL)+
    geom_bar(aes(date,new_deaths),stat='identity',fill=death_col,color=death_col)+
    geom_hline(aes(yintercept=10),lty=1)+
    geom_hline(aes(yintercept=100),lty=2)+
    geom_hline(aes(yintercept=1000),lty=3)+
    theme_transparent()
  g1+annotation_custom(ggplotGrob(g2),xmin=log(2e-4),xmax=log(6e-3),ymin=0.2,ymax=0.5) %>%
    return()
}

pps = mapply(col_plot,cntry=focal_states,cl=cls,SIMPLIFY = FALSE)
ggarrange(plotlist=pps,nrow=3,ncol=3)
ggsave('figures/focal_states_11_05.png',height=8,width=11,units='in')
