library(data.table)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(COVID19)
library(ggridges)
library(heatmaply)


# functions ---------------------------------------------------------------

dpc_bins <- function(deaths_pc){
  bins <- c('1, 0.1','0.1, 0.01','1e-3, 1e-4',
            '1e-4, 1e-5','1e-5, 1e-6','<1e-6')
  x <- floor(abs((log10(deaths_pc))))+1
  x[is.infinite(x)] <- length(bins)
  y <- bins[x] 
  return(y)
}

# countries ---------------------------------------------------------------

NY <- read.csv('data/nbss_us_states.csv') %>% as.data.table
NY <- NY[state=='New York']
NY[,growth_rate:=shift(growth_rate,10)]
C <- read.csv('data/nbss_countries.csv') %>% as.data.table
setkey(C,country,date)
C[,growth_rate:=shift(growth_rate,10),by=country]
SWE <- C[country=='Sweden']



## fit
fit_countries <- mgcv::gam(growth_rate~s(log10(deaths_pc)),data=C[deaths_pc>0])

dd <- data.table('deaths_pc'=10^seq(-8,-3,length.out=1000))
dd$growth_rate <- predict(fit_countries,newdata=dd)
dd$se <- predict(fit,newdata=dd,se.fit = TRUE)$se.fit
dd[,country:='Average']

hit_global <- dd[growth_rate<0,min(deaths_pc)]

######## PLOT
lbl <- data.frame('deaths_pc'=c(1e-5,3e-6,3e-7),
                  'growth_rate'=c(0.37,0.1,0.03),
                  'country'=c('New York','Sweden','Global Fit'))
swy=rgb(1,205/255,0) # color of yellow in Swedish flag
all_countries <- ggplot(C,aes(deaths_pc,growth_rate,by=country))+
  geom_line(col='grey',lwd=2,alpha=0.1)+
  geom_ribbon(data=dd,aes(ymin=growth_rate-2*se,ymax=growth_rate+2*se),alpha=0.3)+
  geom_line(data=dd,col='darkblue',lwd=2)+
  geom_line(data=NY,col='black',lwd=2)+
  geom_line(data=SWE,col=swy,lwd=2)+
  theme_bw()+
  geom_text(data=lbl,aes(label=country))+
  geom_label(data=lbl,aes(label=country),col=c('black',swy,'darkblue'),size=10)+
  scale_x_continuous(trans='log',limits=c(1e-7,3e-3),breaks=10^(-7:0))+
  scale_y_continuous(limits=c(-0.2,0.4))+
  geom_vline(xintercept = hit_global,lty=2,lwd=2,col='darkblue')

all_countries

ggsave('figures/all_countries_trajectories.png',height=8,width=10,units='in')
# Counties ----------------------------------------------------------------


X <- covid19('United States',level=3) %>% as.data.table
X[,state:=administrative_area_level_2]
X[,county:=administrative_area_level_3]
setkey(X,state,county,date)
X[,deaths_pc:=deaths/population]
us <- read.csv('data/nbss_us_counties.csv') %>% as.data.table
us[,state:=administrative_area_level_2]
us[,county:=administrative_area_level_3]
us[,date:=as.Date(date)]

setkey(us,state,county,date)

us <- X[,c('state','county','date','deaths_pc')][us]
us[,growth_rate:=shift(mean_velocity,10),by=c('state','county')]


floor(abs(log10(0.05)))

us[,dpc_bin:=factor(dpc_bins(deaths_pc),levels=c('1, 0.1','0.1, 0.01','1e-3, 1e-4',
                                                 '1e-4, 1e-5','1e-5, 1e-6','<1e-6'))]
rdg=ggplot(us[!is.na(dpc_bin)],aes(x=growth_rate,y=dpc_bin,fill=dpc_bin))+
  geom_density_ridges(alpha=0.5,scale=2)+ggridges::theme_ridges()+
  scale_x_continuous(limits=c(-0.15,0.15))+
  scale_fill_manual(values=rev(heatmaply::RdYlBu(4)))+
  ggtitle('Growth Rates of US County Epidemics by Deaths Per-Capita Bin')
rdg
ggsave('figures/county_growth_rate_by_dpc_bins.png',height=10,width=12)


fit <- mgcv::gam(growth_rate~s(log10(deaths_pc)),data=us[deaths_pc>0],sp = c(0.1,0.1))
dds <- data.table('deaths_pc'=10^seq(-8,-3,length.out=100))
dds$growth_rate <- predict(fit,newdata=dds)
dds$se <- predict(fit,newdata=dds,se.fit = TRUE)$se.fit

hit <- dds[growth_rate<0,min(deaths_pc)]
# 0.0004977024

ft=ggplot(dds,aes(deaths_pc,growth_rate))+
  theme_bw()+
  geom_line(data=NY)+
  geom_ribbon(aes(ymin=growth_rate-2*se,ymax=growth_rate+2*se),alpha=0.2,fill='darkred')+
  geom_line(col='darkred')+
  geom_line(data=dd,col='darkblue')+
  geom_ribbon(data=dd,aes(ymin=growth_rate-2*se,ymax=growth_rate+2*se),alpha=0.2,fill='darkblue')+
  scale_x_continuous('deaths per-capita',trans='log',limits=c(1e-7,3e-3),breaks=10^(-7:0))+
  scale_y_continuous('r(D)',limits=c(-0.02,0.4))+
  geom_vline(xintercept = hit,lty=2)+
  geom_vline(xintercept = hit_global,lty=2,col='darkblue')+
  ggtitle('Predicted r(D)')


rdg+annotation_custom(ggplotGrob(ft),xmin=-0.17,xmax=-0.07,ymin=4.3,ymax=6)
ggsave('figures/county-growth_rate_by_dpc_bins_plus_fit.png',height=8,width=12)




NY[,county:='NYS']
dd[,county:='global_fit']
dds[,county:='county_fit']
lbl <- data.frame('deaths_pc'=c(1e-5,3e-6,3e-7),
                  'growth_rate'=c(0.37,0.1,0.03),
                  'county'=c('New York','County Fit','Global Fit'))
all_counties_plot <- ggplot(us,aes(deaths_pc,growth_rate,by=county))+
  theme_bw()+
  geom_line(lwd=2,col='grey',alpha=0.05)+
  geom_line(data=NY,lwd=2)+
  geom_ribbon(data=dds,aes(ymin=growth_rate-2*se,ymax=growth_rate+2*se),alpha=0.2,fill='darkred')+
  geom_line(data=dds,col='darkred',lwd=2)+
  geom_line(data=dd,col='darkblue',lwd=2)+
  geom_ribbon(data=dd,aes(ymin=growth_rate-2*se,ymax=growth_rate+2*se),alpha=0.2,fill='darkblue')+
  scale_x_continuous('deaths per-capita',trans='log',limits=c(1e-7,3e-3),breaks=10^(-7:0))+
  scale_y_continuous('r(D)',limits=c(-0.02,0.4))+
  geom_vline(xintercept = hit,lty=2,lwd=2,col='darkred')+
  geom_text(data=lbl,aes(label=county))+
  geom_label(data=lbl,aes(label=county),col=c('black','darkred','darkblue'),size=10)+
  geom_vline(xintercept = hit_global,lty=2,col='darkblue',lwd=2)+
  ggtitle('Predicted r(D)')

all_counties_plot
ggsave('figures/all_county_trajectories.png',height=8,width=10)
