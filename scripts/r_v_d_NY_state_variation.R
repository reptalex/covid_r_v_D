library(data.table)
library(magrittr)
library(ggplot2)
library(ggpubr)

US <- read.csv('data/nbss_us_states.csv') %>% as.data.table
US[,deaths_pc:=shift(deaths_pc,10,type='lead'),by=state]
USA <- read.csv('data/nbss_us_counties.csv') %>% as.data.table
setkey(USA,state,county,date)
USA[,deaths_pc:=shift(deaths_pc,10,type='lead'),by=c('state','county')]

NY <- USA[state=='New York']
AZ <- USA[state=='Arizona']
FL <- USA[state=='Florida']
MA <- USA[state=='Massachusetts']
TX <- USA[state=='Texas']
focal_counties <- c('New York City','Nassau','Dutchess','Saratoga','Montgomery')
cnty_col <- rgb(.1,.5,1)
grs <- NY[county %in% focal_counties & deaths_pc<1e-6,list('gr'=max(growth_rate,na.rm=T)),by=county]
grs[county=='Saratoga',gr:=0.12]
lbls <- data.table('county'=focal_counties,
                   'deaths_pc'=4e-6,
                   'growth_rate'=grs[match(focal_counties,county),gr])
lbl <- data.frame('deaths_pc'=c(1e-5),
                  'growth_rate'=c(0.39),
                  'county'='New York',
                  'label'=c('New York State'))
ggplot(NY,aes(shift(deaths_pc,0),growth_rate,by=county))+
  # geom_line(data=NY[county %in% c('New York City','Albany','Dutchess')],lwd=2,col='grey')+
  theme_bw()+
  geom_line(data=US[state=='New York'],lwd=2)+
  geom_ribbon(data=US[state=='New York'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),alpha=0.2)+
  geom_line(lwd=3,col=rgb(.5,.5,.5,0.2))+
  geom_point(data=NY[date==as.Date('2020-08-19')],col=rgb(0.3,0.3,0.3),cex=3)+
  geom_line(data=NY[county %in% focal_counties],col=cnty_col,lwd=2)+
  geom_text(data=lbls,aes(label=county,color=NULL))+
  geom_label(data=lbls,aes(label=county,color=NULL),col=cnty_col,size=10)+
  geom_point(data=NY[date==as.Date('2020-08-19') & county %in% focal_counties],col=cnty_col,cex=3)+
  geom_text(data=lbl,aes(label=label,color=NULL))+
  geom_label(data=lbl,aes(label=label,color=NULL),col=c('black'),size=10)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-7:0))

ggsave('figures/county_level_variation_NY.png',height=8,width=11)



lbl2 <- data.frame('deaths_pc'=c(1e-5,2e-5,8e-6),
           'growth_rate'=c(0.39,.15,.05),
           'county'=c('New York','Florida','Arizona'),
           'label'=c('NJ + NY State','Florida','Arizona'))
ggplot(NY,aes(shift(deaths_pc,0),growth_rate,by=county))+
  # geom_line(data=NY[county %in% c('New York City','Albany','Dutchess')],lwd=2,col='grey')+
  theme_bw()+
  geom_line(data=US[state %in% c('New York','New Jersey')],aes(by=state),lwd=2)+
  geom_ribbon(data=US[state=='New York'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),alpha=0.2)+
  geom_line(data=US[state=='Arizona'],lwd=2,col='darkred')+
  geom_line(data=US[state=='Florida'],lwd=2,col='orange')+
  geom_line(lwd=3,col=rgb(.5,.5,.5,0.2))+
  geom_point(data=NY[date==as.Date('2020-08-19')],col=rgb(0.3,0.3,0.3),cex=3)+
  geom_line(data=NY[county %in% focal_counties],col=cnty_col,lwd=2)+
  geom_text(data=lbls,aes(label=county,color=NULL))+
  geom_label(data=lbls,aes(label=county,color=NULL),col=cnty_col,size=7)+
  geom_point(data=NY[date==as.Date('2020-08-19') & county %in% focal_counties],col=cnty_col,cex=3)+
  geom_text(data=lbl2,aes(label=label,color=NULL))+
  geom_label(data=lbl2,aes(label=label,color=NULL),col=c('black','orange','darkred'),size=10)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-7:0))
ggsave('figures/county_level_variation_NY_w_AZ_FL.png',height=8,width=11)



# Arizona -----------------------------------------------------------------
az.lbls <- data.frame('county'=c('Maricopa','Apache','Pima','New York State','New York City'),
                      'deaths_pc'=1e-5,
                      'growth_rate'=c(0.01,0.1,0.21,0.37,0.47))
focal_counties <- c('Maricopa','Pima','Apache')
cnty_col <- rgb(0.9,0.5,0.5)
ggplot(AZ,aes(shift(deaths_pc,0),growth_rate,by=county))+
  # geom_line(data=NY[county %in% c('New York City','Albany','Dutchess')],lwd=2,col='grey')+
  theme_bw()+
  geom_line(data=NY[county=='New York City'],col=rgb(.1,.5,1),lwd=2)+
  geom_line(data=US[state=='New York'],lwd=2,col='black')+
  geom_ribbon(data=US[state=='New York'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),alpha=0.2)+
  geom_line(data=US[state %in% c('Arizona')],aes(by=state),lwd=2,col='darkred')+
  geom_ribbon(data=US[state=='Arizona'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),fill='darkred',alpha=0.2)+
  geom_line(lwd=3,col=rgb(.5,.5,.5,0.2))+
  geom_point(data=AZ[date==as.Date('2020-08-19')],col=rgb(0.3,0.3,0.3),cex=3)+
  geom_line(data=AZ[county %in% focal_counties],col=cnty_col,lwd=3)+
  geom_line(data=AZ[county %in% focal_counties])+
  theme(legend.position='none')+
  geom_text(data=az.lbls,aes(label=county,color=NULL))+
  geom_label(data=az.lbls,aes(label=county,color=NULL),col=c(rep(cnty_col,3),'black',rgb(.1,.5,1)),size=7)+
  # geom_point(data=AZ[date==as.Date('2020-08-19') & county %in% focal_counties],col=cnty_col,cex=3)+
  # geom_text(data=lbl2,aes(label=label,color=NULL))+
  # geom_label(data=lbl2,aes(label=label,color=NULL),col=c('black','orange','darkred'),size=10)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-7:0))
ggsave('figures/county_level_variation_AZ.png',height=8,width=11)


# Florida -----------------------------------------------------------------

fl.lbls <- data.frame('county'=c('Maricopa','Pima'),
                      'deaths_pc'=1e-5,
                      'growth_rate'=c(0.01,0.21))
focal_counties <- c('Maricopa','Pima')
cnty_col <- rgb(0.8,0.6,0.6)
ggplot(FL,aes(shift(deaths_pc,0),growth_rate,by=county))+
  # geom_line(data=NY[county %in% c('New York City','Albany','Dutchess')],lwd=2,col='grey')+
  theme_bw()+
  geom_line(data=NY[county=='New York City'],col='steelblue',lwd=2)+
  geom_line(data=US[state=='New York'],lwd=2,col='black')+
  geom_ribbon(data=US[state=='New York'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),alpha=0.2)+
  geom_line(lwd=3,col=rgb(.5,.5,.5,0.2))+
  geom_point(data=FL[date==as.Date('2020-08-19')],col=rgb(0.3,0.3,0.3),aes(cex=population))+
  geom_line(data=US[state %in% c('Florida')],aes(by=state),lwd=2,col='orange')+
  geom_ribbon(data=US[state=='Florida'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),fill='orange',alpha=0.2)+
  # geom_line(data=FL[county %in% focal_counties],col=cnty_col,lwd=3)+
  # geom_line(data=FL[county %in% focal_counties])+
  # theme(legend.position='none')+
  # geom_text(data=fl.lbls,aes(label=county,color=NULL))+
  # geom_label(data=fl.lbls,aes(label=county,color=NULL),col=cnty_col,size=7)+
  # geom_point(data=AZ[date==as.Date('2020-08-19') & county %in% focal_counties],col=cnty_col,cex=3)+
  # geom_text(data=lbl2,aes(label=label,color=NULL))+
  # geom_label(data=lbl2,aes(label=label,color=NULL),col=c('black','orange','darkred'),size=10)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-7:0))

ggsave('figures/county_level_variation_FL.png',height=8,width=11)

# Texas -------------------------------------------------------------------


tx.lbls <- data.frame('county'=c('Maricopa','Pima'),
                      'deaths_pc'=1e-5,
                      'growth_rate'=c(0.01,0.21))
focal_counties <- c('Maricopa','Pima')
cnty_col <- rgb(0.8,0.6,0.6)
ggplot(TX,aes(shift(deaths_pc,0),growth_rate,by=county))+
  # geom_line(data=NY[county %in% c('New York City','Albany','Dutchess')],lwd=2,col='grey')+
  theme_bw()+
  geom_line(data=NY[county=='New York City'],col='steelblue',lwd=2)+
  geom_line(data=US[state=='New York'],lwd=2,col='black')+
  geom_ribbon(data=US[state=='New York'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),alpha=0.2)+
  geom_line(lwd=3,col=rgb(.5,.5,.5,0.2))+
  geom_point(data=TX[date==as.Date('2020-08-19')],col=rgb(0.3,0.3,0.3),aes(cex=population))+
  geom_line(data=US[state %in% c('Florida')],aes(by=state),lwd=2,col='orange')+
  geom_ribbon(data=US[state=='Florida'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),fill='orange',alpha=0.2)+
  # geom_line(data=TX[county %in% focal_counties],col=cnty_col,lwd=3)+
  # geom_line(data=TX[county %in% focal_counties])+
  # theme(legend.position='none')+
  # geom_text(data=tx.lbls,aes(label=county,color=NULL))+
  # geom_label(data=tx.lbls,aes(label=county,color=NULL),col=cnty_col,size=7)+
  # geom_point(data=TX[date==as.Date('2020-08-19') & county %in% focal_counties],col=cnty_col,cex=3)+
  # geom_text(data=lbl2,aes(label=label,color=NULL))+
  # geom_label(data=lbl2,aes(label=label,color=NULL),col=c('black','orange','darkred'),size=10)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-7:0))

ggsave('figures/county_level_variation_TX.png',height=8,width=11)


# Massachusetts -------------------------------------------------------------

ma.lbls <- data.frame('county'=c('MA State','Hampden','Suffolk','Berkshire','New York State','New York City'),
                      'deaths_pc'=1e-5,
                      'growth_rate'=c(0.3,0.345,0.23,0.15,0.38,0.47))
focal_counties <- c('Hampden','Suffolk','Berkshire')
cnty_col <- rgb(0.1,0.8,0.1)
ggplot(MA,aes(shift(deaths_pc,0),growth_rate,by=county))+
  # geom_line(data=NY[county %in% c('New York City','Albany','Dutchess')],lwd=2,col='grey')+
  theme_bw()+
  geom_line(data=NY[county=='New York City'],col=rgb(.1,.5,1),lwd=2)+
  geom_line(data=US[state=='New York'],lwd=2,col='black')+
  geom_ribbon(data=US[state=='New York'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),alpha=0.2)+
  geom_line(data=US[state %in% c('Massachusetts')],aes(by=state),lwd=2,col='darkgreen')+
  geom_ribbon(data=US[state=='Massachusetts'],aes(ymin=p2.5_growth_rate,ymax=p97.5_growth_rate),fill='darkgreen',alpha=0.2)+
  geom_line(lwd=3,col=rgb(.5,.5,.5,0.2))+
  geom_point(data=MA[date==as.Date('2020-08-19')],col=rgb(0.3,0.3,0.3),cex=3)+
  geom_line(data=MA[county %in% focal_counties],col=cnty_col,lwd=3)+
  geom_line(data=MA[county %in% focal_counties])+
  theme(legend.position='none')+
  geom_text(data=ma.lbls,aes(label=county,color=NULL))+
  geom_label(data=ma.lbls,aes(label=county,color=NULL),col=c('darkgreen',rep(cnty_col,3),'black',rgb(.1,.5,1)),size=7)+
  # geom_point(data=AZ[date==as.Date('2020-08-19') & county %in% focal_counties],col=cnty_col,cex=3)+
  # geom_text(data=lbl2,aes(label=label,color=NULL))+
  # geom_label(data=lbl2,aes(label=label,color=NULL),col=c('black','orange','darkred'),size=10)+
  scale_x_continuous(trans='log',limits=c(1e-6,3e-3),breaks=10^(-7:0))
ggsave('figures/county_level_variation_MA.png')
