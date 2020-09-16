rm(list=ls())
gc()
library(data.table)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
source('scripts/utils.R')

# import data --------------------------------------------------------

USA <- read.csv('data/nbss_us_states.csv') %>% as.data.table
USA[,date:=as.Date(date)]
setkey(USA,state,date)
X <- read.csv('data/nbss_countries.csv') %>% as.data.table

SWE <- X[country=='Sweden']
swy <- rgb(1,205/255,0)
SWE[,date:=as.Date(date)]

# states by confirmed cases -----------------------------------------------

USA[date>=as.Date('2020-06-01') & date<as.Date('2020-09-01'),
    list(N=sum(new_confirmed,na.rm=T),D=sum(deaths[.N]-deaths[1])),by=state][order(N,decreasing = T)]
#                   state      N    D
# 1:           California 553086 7883
# 2:              Florida 544408 7919
# 3:                Texas 513250 9692
# 4:              Georgia 206963 3058
# 5:              Arizona 178167 3854
# 6:       North Carolina 126524 1633
# 7:            Tennessee 120931 1200
# 8:            Louisiana 103146 1945
# 9:             Illinois 101335 2677
# 10:       South Carolina 100127 2004
# 11:              Alabama  94210 1367
# 12:                 Ohio  79289 1772
# 13:             Virginia  68359 1075
# 14:          Mississippi  62393 1501
# 15:             Missouri  59353  653
# 16:             New York  58967 1329
# 17:               Nevada  57008  752
# 18:         Pennsylvania  56502 2011
# 19:            Wisconsin  54641  494
# 20:              Indiana  51358 1078
# 21:             Maryland  51324 1066
# 22:             Arkansas  49721  554
# 23:           Washington  47594  739
# 24:             Oklahoma  46659  392
# 25:            Minnesota  43262  753
# 26:             Michigan  43240  816
# 27:                 Utah  39318  272
# 28:                 Iowa  36723  485
# 29:             Kentucky  33825  442
# 30:           New Jersey  29080 2454
# 31:             Colorado  28785  634
# 32:               Kansas  27137  202
# 33:                Idaho  26823  224
# 34:          Puerto Rico  26120  254
# 35:        Massachusetts  24555 1886
# 36:               Oregon  20694  263
# 37:             Nebraska  17875  206
# 38:           New Mexico  16678  387
# 39:          Connecticut   9330  496
# 40:             Delaware   7424  129
# 41:         North Dakota   7299   62
# 42:        West Virginia   7262  103
# 43:         South Dakota   6283   99
# 44:         Rhode Island   6275  315
# 45:              Montana   5915   73
# 46:               Hawaii   5705   30
# 47: District of Columbia   4789  136
# 48:               Alaska   4723   22
# 49:              Wyoming   2684   20
# 50:        New Hampshire   2562  184
# 51:                Maine   2010   42
# 52:              Vermont    577    3
# 53:                 Guam    542    2


# State interventions -----------------------------------------------------

intervention_cols <- data.frame('intervention'=c('pre-stay-at-home','stay-at-home',
                                                 'reopening1','reopening2','masks','reversal'),
                                'color'=brewer.pal(6,'Paired'))
dispersions <- read.csv("data/precomputed_dispersion.csv")

USA[,new_confirmed:=new_deaths]
SWE[,new_confirmed:=new_deaths]
nms <- colnames(USA)
USA <- USA[,!(grepl('signal',nms) | grepl('position',nms) | grepl('growth_rate',nms)),with=F] %>% covid19_nbss(precomputed_dispersions = dispersions,filtering=TRUE)
nms <- colnames(SWE)
SWE <- SWE[,!(grepl('signal',nms) | grepl('position',nms) | grepl('growth_rate',nms)),with=F] %>% covid19_nbss(precomputed_dispersions = dispersions,filtering=TRUE)
USA <- as.data.table(USA)
SWE <- as.data.table(SWE)
setkey(USA,state,date)
setkey(SWE,state,date)

USA[,deaths_pc:=shift(deaths_pc,rDlag,type='lead'),by=state]
SWE[,deaths_pc:=shift(deaths_pc,rDlag,type='lead')]

AZ <- USA[state=='Arizona',]
NY <- USA[state=='New York']
FL <- USA[state=='Florida']
TX <- USA[state=='Texas']
MN <- USA[state=='Minnesota']
CA <- USA[state=='California']
LA <- USA[state=='Louisiana']


# 
# xx <- covid19(country = 'United States',level=2) %>% as.data.table
# xx[,state:=administrative_area_level_2]
# NY <- xx[state=='New York']
# NY[,new_confirmed:=c(confirmed[1],diff(confirmed))]
# NY <- nbss(NY,level='SEIR')
# NY[,deaths_pc:=deaths/population]
# NY[,growth_rate:=shift(growth_rate,10)]

# Arizona -----------------------------------------------------------------

AZ[date<as.Date('2020-03-30'),intervention:='pre-stay-at-home']
AZ[date>=as.Date('2020-03-30') & date<as.Date('2020-05-15'),intervention:='stay-at-home']
AZ[date>=as.Date('2020-05-15') & date<as.Date('2020-06-17'),intervention:='reopening']
AZ[date>=as.Date('2020-06-17'),intervention:='local mask-wearing mandate']
AZ[date>as.Date('2020-06-29'),intervention:='reopening_reversal']
AZ[,intervention:=factor(intervention,levels=unique(intervention))]

lbl <- data.frame('deaths_pc'=c(1e-5,3e-6),
                  'growth_rate'=c(0.37,0.1),
                  'label'=c('New York','Sweden'))

az <- ggplot(AZ,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color=swy,lwd=2)+
  geom_line(lwd=1.5)+
  # geom_line(data=AZ[intervention=='reopening_reversal'],color='red',lwd=2)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-6:0))+
  scale_y_continuous(limits=c(-0.1,0.4))+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1:3,5,6)])+
  geom_text(data=lbl,aes(label=label,color=NULL))+
  geom_label(data=lbl,aes(label=label,color=NULL),col=c('black',swy),size=10)+
  geom_hline(yintercept = 0)+
  ggtitle('Arizona')

az.rt <- ggplot(AZ[date>as.Date('2020-03-01')],
                aes(date,growth_rate,color=intervention))+
  geom_line(lwd=1.5)+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1:3,5,6)])+
  theme(legend.position='none')+
  geom_vline(xintercept = AZ[grepl('local',intervention),min(date)])+
  scale_y_continuous('r(t)')+  
  scale_x_continuous(limits = as.Date(c('2020-03-01','2020-09-01')),
                     breaks=as.Date(c('2020-03-01','2020-05-01','2020-07-01','2020-09-01')),
                     labels = c('Mar','May','July','Sep'))

az.cs <- ggplot(AZ[date>as.Date('2020-03-01')],
                aes(date,new_confirmed,fill=intervention))+
  geom_bar(stat='identity')+
  theme_bw()+
  scale_fill_manual(values=intervention_cols$color[c(1:3,5,6)])+
  theme(legend.position='none')+
  scale_y_continuous('N(t)')+
  scale_x_continuous(name=NULL,breaks=NULL)

az2=az+annotation_custom(ggplotGrob(az.rt),
                         xmin=log(3e-4),xmax=log(3e-3),
                         ymin=0.07,ymax=0.25)+
  annotation_custom(ggplotGrob(az.cs),
                    xmin=log(2.8e-4),xmax=log(3e-3),
                    ymin=0.25,ymax=0.4)


# Texas -------------------------------------------------------------------

TX[date<as.Date('2020-03-31'),intervention:='pre-stay-at-home']
TX[date>=as.Date('2020-03-31') & date<as.Date('2020-05-01'),intervention:='stay-at-home']
TX[date>=as.Date('2020-05-01') & date<as.Date('2020-05-18'),intervention:='Phase 1']
# TX[date>=as.Date('2020-05-18') & date<as.Date('2020-06-03'),intervention:='Phase 2']
# TX[date>=as.Date('2020-06-03') & date<as.Date('2020-06-26'),intervention:='Phase 3']
TX[date>=as.Date('2020-05-18') & date<as.Date('2020-06-26'),intervention:='Phase 2-3']
TX[date>=as.Date('2020-06-26'),intervention:='reopening_reversal']
TX[,intervention:=factor(intervention,levels=unique(intervention))]

tx <- ggplot(TX,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color=swy,lwd=2)+
  geom_line(lwd=1.5)+
  # geom_line(data=TX[intervention=='reopening_reversal'],color='red',lwd=2)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-6:0))+
  scale_y_continuous(limits=c(-0.1,0.4))+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1:4,6)])+
  geom_hline(yintercept = 0)+
  ggtitle('Texas')

tx.rt <- ggplot(TX[date>as.Date('2020-03-01')],
                aes(date,growth_rate,color=intervention))+
  geom_line(lwd=1.5)+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1:4,6)])+
  theme(legend.position='none')+
  geom_vline(xintercept = TX[grepl('reversal',intervention),min(date)])+
  scale_y_continuous('r(t)')+  
  scale_x_continuous(limits = as.Date(c('2020-03-01','2020-09-01')),
                     breaks=as.Date(c('2020-03-01','2020-05-01','2020-07-01','2020-09-01')),
                     labels = c('Mar','May','July','Sep'))

tx.cs <- ggplot(TX[date>as.Date('2020-03-01')],
                aes(date,new_confirmed,fill=intervention))+
  geom_bar(stat='identity')+
  theme_bw()+
  scale_fill_manual(values=intervention_cols$color[c(1:4,6)])+
  theme(legend.position='none')+
  scale_y_continuous('N(t)')+
  scale_x_continuous(name=NULL,breaks=NULL)

tx2=tx+annotation_custom(ggplotGrob(tx.rt),
                         xmin=log(3e-4),xmax=log(3e-3),
                         ymin=0.07,ymax=0.25)+
  annotation_custom(ggplotGrob(tx.cs),
                    xmin=log(2.65e-4),xmax=log(3e-3),
                    ymin=0.25,ymax=0.4)


# New York ----------------------------------------------------------------
# NY[date<as.Date('2020-03-23'),intervention:='pre_stay-at-home']
# NY[date>=as.Date('2020-03-23') & date<as.Date('2020-05-19'),intervention:='stay-at-home']
# NY[date>=as.Date('2020-05-19') & date<as.Date('2020-06-22'),intervention:='reopening']

# ggplot(NY,aes(deaths_pc,growth_rate,color=intervention))+
#   geom_line(lwd=1.5)+
#   scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-6:0))+
#   scale_y_continuous(limits=c(-0.2,0.6))

# Louisiana ---------------------------------------------------------------
LA[date<as.Date('2020-03-23'),intervention:='pre-stay-at-home']
LA[date>=as.Date('2020-03-23') & date<as.Date('2020-05-15'),intervention:='stay-at-home']
LA[date>=as.Date('2020-05-15') & date<as.Date('2020-06-05'),intervention:='Phase 1']
LA[date>=as.Date('2020-06-05'),intervention:='Phase 2']

LA[,intervention:=factor(intervention,levels=unique(intervention))]

la <- ggplot(LA,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color=swy,lwd=2)+
  geom_line(lwd=1.5)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-6:0))+
  scale_y_continuous(limits=c(-0.1,0.4))+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[1:4])+
  geom_hline(yintercept = 0)+
  ggtitle('Louisiana')

la.rt <- ggplot(LA[date>as.Date('2020-03-01')],
                aes(date,growth_rate,color=intervention))+
  geom_line(lwd=1.5)+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[1:4])+
  theme(legend.position='none')+
  scale_y_continuous('r(t)')+  
  scale_x_continuous(limits = as.Date(c('2020-03-01','2020-09-01')),
                     breaks=as.Date(c('2020-03-01','2020-05-01','2020-07-01','2020-09-01')),
                     labels = c('Mar','May','July','Sep'))

la.cs <- ggplot(LA[date>as.Date('2020-03-01')],
                aes(date,new_confirmed,fill=intervention))+
  geom_bar(stat='identity')+
  theme_bw()+
  scale_fill_manual(values=intervention_cols$color[1:4])+
  theme(legend.position='none')+
  scale_y_continuous('N(t)')+
  scale_x_continuous(name=NULL,breaks=NULL)

la2 <- la+annotation_custom(ggplotGrob(la.rt),
                            xmin=log(3e-4),xmax=log(3e-3),
                            ymin=0.07,ymax=0.25)+
  annotation_custom(ggplotGrob(la.cs),
                    xmin=log(2.8e-4),xmax=log(3e-3),
                    ymin=0.25,ymax=0.4)


# Florida -----------------------------------------------------------------
FL[date<as.Date('2020-03-30'),intervention:='pre-stay-at-home']
FL[date>=as.Date('2020-03-30') & date<as.Date('2020-06-01'),intervention:='stay-at-home']
FL[date>=as.Date('2020-06-01'),intervention:='reopening']
FL[,intervention:=factor(intervention,levels=unique(intervention))]
fl <- ggplot(FL,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color=swy,lwd=2)+
  geom_line(lwd=1.5)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-6:0))+
  scale_y_continuous(limits=c(-0.1,0.4))+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1,2,4)])+
  geom_hline(yintercept = 0)+
  ggtitle("Florida")

fl.rt <- ggplot(FL[date>as.Date('2020-03-01')],
                aes(date,growth_rate,color=intervention))+
  geom_line(lwd=1.5)+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1,2,4)])+
  theme(legend.position='none')+
  scale_y_continuous('r(t)')+  
  scale_x_continuous(limits = as.Date(c('2020-03-01','2020-09-01')),
                     breaks=as.Date(c('2020-03-01','2020-05-01','2020-07-01','2020-09-01')),
                     labels = c('Mar','May','July','Sep'))

fl.cs <- ggplot(FL[date>as.Date('2020-03-01')],
                aes(date,new_confirmed,fill=intervention))+
  geom_bar(stat='identity')+
  theme_bw()+
  scale_fill_manual(values=intervention_cols$color[c(1,2,4)])+
  theme(legend.position='none')+
  scale_y_continuous('N(t)')+
  scale_x_continuous(name=NULL,breaks=NULL)

fl2=fl+annotation_custom(ggplotGrob(fl.rt),
                         xmin=log(3e-4),xmax=log(3e-3),
                         ymin=0.07,ymax=0.25)+
  annotation_custom(ggplotGrob(fl.cs),
                    xmin=log(2.6e-4),xmax=log(3e-3),
                    ymin=0.25,ymax=0.4)


# Minnesota ---------------------------------------------------------------
MN[date<as.Date('2020-03-27'),intervention:='pre-stay-at-home']
MN[date>=as.Date('2020-03-27') & date<as.Date('2020-05-18'),intervention:='stay-at-home']
MN[date>=as.Date('2020-05-18'),intervention:='reopening']
MN[,intervention:=factor(intervention,levels=unique(intervention))]

mn <-  ggplot(MN,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color=swy,lwd=2)+
  geom_line(lwd=1.5)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-6:0))+
  scale_y_continuous(limits=c(-0.1,0.4))+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1,2,4)])+
  geom_hline(yintercept = 0)+
  ggtitle('Minnesota')

mn.rt <- ggplot(MN[date>as.Date('2020-03-01')],
                aes(date,growth_rate,color=intervention))+
  geom_line(lwd=1.5)+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1,2,4)])+
  theme(legend.position='none')+
  scale_y_continuous('r(t)')+  
  scale_x_continuous(limits = as.Date(c('2020-03-01','2020-09-01')),
                     breaks=as.Date(c('2020-03-01','2020-05-01','2020-07-01','2020-09-01')),
                     labels = c('Mar','May','July','Sep'))

mn.cs <- ggplot(MN[date>as.Date('2020-03-01')],
                aes(date,new_confirmed,fill=intervention))+
  geom_bar(stat='identity')+
  theme_bw()+
  scale_fill_manual(values=intervention_cols$color[c(1,2,4)])+
  theme(legend.position='none')+
  scale_y_continuous('N(t)')+
  scale_x_continuous(name=NULL,breaks=NULL)

mn2=mn+annotation_custom(ggplotGrob(mn.rt),
                         xmin=log(3e-4),xmax=log(3e-3),
                         ymin=0.07,ymax=0.25)+
  annotation_custom(ggplotGrob(mn.cs),
                    xmin=log(2.8e-4),xmax=log(3e-3),
                    ymin=0.25,ymax=0.4)


# California --------------------------------------------------------------
CA[date<as.Date('2020-03-19'),intervention:='pre-stay-at-home']
CA[date>=as.Date('2020-03-19') & date<as.Date('2020-05-07'),intervention:='stay-at-home']
CA[date>=as.Date('2020-05-07') & date<as.Date('2020-06-07'),intervention:='Stage 2']
CA[date>=as.Date('2020-06-07') & date<as.Date('2020-06-28'),intervention:='Stage 3']
CA[date>=as.Date('2020-06-28'),intervention:='reopening_reversal']
CA[,intervention:=factor(intervention,levels=unique(intervention))]

ca <-  ggplot(CA,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color=swy,lwd=2)+
  geom_line(lwd=1.5)+
  # geom_line(data=CA[intervention=='reopening_reversal'],color='red',lwd=2)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-6:0))+
  scale_y_continuous(limits=c(-0.1,0.4))+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1:4,6)])+
  geom_hline(yintercept = 0)+
  ggtitle('California')

ca.rt <- ggplot(CA[date>as.Date('2020-03-01')],
                aes(date,growth_rate,color=intervention))+
  geom_line(lwd=1.5)+
  theme_bw()+
  scale_color_manual(values=intervention_cols$color[c(1:4,6)])+
  theme(legend.position='none')+
  geom_vline(xintercept = CA[grepl('reversal',intervention),min(date)])+
  scale_y_continuous('r(t)')+  
  scale_x_continuous(limits = as.Date(c('2020-03-01','2020-09-01')),
                     breaks=as.Date(c('2020-03-01','2020-05-01','2020-07-01','2020-09-01')),
                     labels = c('Mar','May','July','Sep'))

ca.cs <- ggplot(CA[date>as.Date('2020-03-01')],
                aes(date,new_confirmed,fill=intervention))+
  geom_bar(stat='identity')+
  theme_bw()+
  scale_fill_manual(values=intervention_cols$color[c(1:4,6)])+
  theme(legend.position='none')+
  scale_y_continuous('N(t)')+
  scale_x_continuous(name=NULL,breaks=NULL)

ca2=ca+annotation_custom(ggplotGrob(ca.rt),
                         xmin=log(3e-4),xmax=log(3e-3),
                         ymin=0.07,ymax=0.25)+
  annotation_custom(ggplotGrob(ca.cs),
                    xmin=log(2.65e-4),xmax=log(3e-3),
                    ymin=0.25,ymax=0.4)

# all ---------------------------------------------------------------------



ggarrange(az2,ca2,fl2,tx2,mn2,la2,nrow=3,ncol=2,align='v')
ggsave('figures/state_intervention_trajectories_filtering.png',height=15,width=20)

# ggsave('figures/state_intervention_trajectories.png',height=12,width=15)


plot_state <- function(st,SWE.=SWE,NY.=NY){
  lbl <- rbind(lbl,data.frame('deaths_pc'=1e-5,'growth_rate'=0.23,'label'=st))
  USA[state==st] %>%
    ggplot(aes(deaths_pc,growth_rate))+
    geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
    geom_line(data=SWE,aes(color='black'),color=swy,lwd=2)+
    geom_line(lwd=2,col='steelblue')+
    scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-6:0))+
    scale_y_continuous(limits=c(-0.1,0.4))+
    theme_bw()+
    geom_text(data=lbl,aes(label=label,color=NULL))+
    geom_label(data=lbl,aes(label=label,color=NULL),col=c('black',swy,'steelblue'),size=10)+
    geom_hline(yintercept = 0)+
    ggtitle(st) %>% 
    return
}
plot_state('Georgia')


states <- setdiff(unique(USA$state),c('Guam','District of Columbia'))
ny <- NULL
sw <- NULL
for (st in states){
  dum <- NY[,c('deaths_pc','growth_rate')]
  dum[,state:=st]
  ny <- rbind(ny,dum)
  
  dum <- SWE[,c('deaths_pc','growth_rate')]
  dum[,state:=st]
  sw <- rbind(sw,dum)
}

USA[state %in% states] %>%
  ggplot(aes(deaths_pc,growth_rate))+
  geom_line(data=ny,aes(color='black'),color='black',lwd=1.2)+
  geom_line(data=sw,aes(color='black'),color=swy,lwd=1.2)+
  geom_line(col='steelblue',lwd=1.2)+
  scale_x_continuous(trans='log',limits=c(1e-5,3e-3),breaks=10^(-5:0))+
  scale_y_continuous(limits=c(-0.1,0.4))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~state)

ggsave('figures/all_states_filtering.png',height=10,width=15,units='in')