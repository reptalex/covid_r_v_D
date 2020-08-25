library(data.table)
library(magrittr)
library(ggplot2)
library(ggpubr)
load('data/COVID_growth_rate_deaths_analysis_US_countries.Rd')
SWE <- X[country=='Sweden']

AZ <- USA[state=='Arizona']
NY <- USA[state=='New York']
FL <- USA[state=='Florida']
TX <- USA[state=='Texas']
MN <- USA[state=='Minnesota']
CA <- USA[state=='California']
LA <- USA[state=='Louisiana']

# Arizona -----------------------------------------------------------------

AZ[date<as.Date('2020-03-30'),intervention:='pre-stay-at-home']
AZ[date>=as.Date('2020-03-30') & date<as.Date('2020-05-15'),intervention:='stay-at-home']
AZ[date>=as.Date('2020-05-15') & date<as.Date('2020-06-17'),intervention:='relaxation']
AZ[date>as.Date('2020-06-17'),intervention:='local mask-wearing mandate']
AZ[date>as.Date('2020-06-29'),intervention:='reopening_reversal']
AZ[,intervention:=factor(intervention,levels=unique(intervention))]

az <- ggplot(AZ,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_point(cex=2)+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color='black',lwd=2,lty=3)+
  geom_line(lwd=2)+
  geom_line(data=AZ[intervention=='reopening_reversal'],color='red',lwd=2)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3))+
  scale_y_continuous(limits=c(-0.2,0.5))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  ggtitle('Arizona')

# Texas -------------------------------------------------------------------

TX[date<as.Date('2020-03-31'),intervention:='pre-stay-at-home']
TX[date>=as.Date('2020-03-31') & date<as.Date('2020-05-01'),intervention:='stay-at-home']
TX[date>=as.Date('2020-05-01') & date<as.Date('2020-05-18'),intervention:='Phase 1']
TX[date>=as.Date('2020-05-18') & date<as.Date('2020-06-03'),intervention:='Phase 2']
TX[date>=as.Date('2020-06-03') & date<as.Date('2020-06-26'),intervention:='Phase 3']
TX[date>=as.Date('2020-06-26'),intervention:='reopening_reversal']
TX[,growth_rate:=shift(growth_rate,n=3,type='lead')]
TX[,intervention:=factor(intervention,levels=unique(intervention))]

tx <- ggplot(TX,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_point(cex=2)+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color='black',lwd=2,lty=3)+
  geom_line(lwd=2)+
  geom_line(data=TX[intervention=='reopening_reversal'],color='red',lwd=2)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3))+
  scale_y_continuous(limits=c(-0.2,0.5))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  ggtitle('Texas')

# New York ----------------------------------------------------------------
NY[date<as.Date('2020-03-23'),intervention:='pre_stay-at-home']
NY[date>=as.Date('2020-03-23') & date<as.Date('2020-05-19'),intervention:='stay-at-home']
NY[date>=as.Date('2020-05-19') & date<as.Date('2020-06-22'),intervention:='reopening']

# ggplot(NY,aes(deaths_pc,growth_rate,color=intervention))+
#   geom_line(lwd=2)+
#   scale_x_continuous(trans='log',limits=c(1e-6,3e-3))+
#   scale_y_continuous(limits=c(-0.2,0.6))

# Louisiana ---------------------------------------------------------------
LA[date<as.Date('2020-03-23'),intervention:='pre-stay-at-home']
LA[date>=as.Date('2020-03-23') & date<as.Date('2020-05-15'),intervention:='stay-at-home']
LA[date>=as.Date('2020-05-15') & date<as.Date('2020-06-05'),intervention:='Phase 1']
LA[date>=as.Date('2020-06-05'),intervention:='Phase 2']

LA[,intervention:=factor(intervention,levels=unique(intervention))]

la <- ggplot(LA,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_point(cex=2)+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color='black',lwd=2,lty=3)+
  geom_line(lwd=2)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3))+
  scale_y_continuous(limits=c(-0.2,0.5))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  ggtitle('Louisiana')



# Florida -----------------------------------------------------------------
FL[date<as.Date('2020-03-30'),intervention:='pre-stay-at-home']
FL[date>=as.Date('2020-03-30') & date<as.Date('2020-06-01'),intervention:='stay-at-home']
FL[date>=as.Date('2020-06-01'),intervention:='reopening']
FL[,intervention:=factor(intervention,levels=unique(intervention))]
fl <- ggplot(FL,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_point(cex=2)+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color='black',lwd=2,lty=3)+
  geom_line(lwd=2)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3))+
  scale_y_continuous(limits=c(-0.2,0.5))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  ggtitle("Florida")

# Minnesota ---------------------------------------------------------------
MN[date<as.Date('2020-03-27'),intervention:='pre-stay-at-home']
MN[date>=as.Date('2020-03-27') & date<as.Date('2020-05-18'),intervention:='stay-at-home']
MN[date>=as.Date('2020-05-18'),intervention:='relaxation']
MN[,intervention:=factor(intervention,levels=unique(intervention))]

mn <-  ggplot(MN,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_point(cex=2)+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color='black',lwd=2,lty=3)+
  geom_line(lwd=2)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3))+
  scale_y_continuous(limits=c(-0.2,0.5))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  ggtitle('Minnesota')

# California --------------------------------------------------------------
CA[date<as.Date('2020-03-19'),intervention:='pre-stay-at-home']
CA[date>=as.Date('2020-03-19') & date<as.Date('2020-05-07'),intervention:='stay-at-home']
CA[date>=as.Date('2020-05-07') & date<as.Date('2020-06-07'),intervention:='Stage 2']
CA[date>=as.Date('2020-06-07') & date<as.Date('2020-06-28'),intervention:='Stage 3']
CA[date>=as.Date('2020-06-28'),intervention:='reopening_reversal']
CA[,intervention:=factor(intervention,levels=unique(intervention))]

ca <-  ggplot(CA,aes(deaths_pc,growth_rate,color=intervention))+
  geom_line(col='black')+
  geom_point(cex=1)+
  geom_line(data=NY,aes(color='black'),color='black',lwd=2)+
  geom_line(data=SWE,aes(color='black'),color='black',lwd=2,lty=3)+
  geom_line(lwd=2)+
  geom_line(data=CA[intervention=='reopening_reversal'],color='red',lwd=2)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3))+
  scale_y_continuous(limits=c(-0.2,0.5))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  ggtitle('California')

ggarrange(az,ca,fl,la,mn,tx,nrow=3,ncol=2)
ggsave('figures/state_intervention_trajectories.png',height=12,width=15)
