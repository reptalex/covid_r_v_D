rm(list=ls())
gc()
library(data.table)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(ggarrange)
library(COVID19)
source('scripts/utils.R')


regions <- data.frame('country'=c('France','United Kingdom','United States','United States'),
                      'level'=c(2,2,2,3),
                      'name'=c('French Provinces','UK provinces','US states','US counties'))
focal_us_states <- c('New Jersey','Louisiana','Georgia')
dispersions <- read.csv('data/precomputed_dispersion.csv')

dat <- NULL
for (i in 1:nrow(regions)){
  country <- regions$country[i]
  level <- regions$level[i]
  
  tbl <- COVID19::covid19(country,level) %>% as.data.table
  if (country=='United States' & level==3){
    tbl <- tbl[administrative_area_level_2 %in% focal_us_states]
  }

  if (lvl==2){
    tbl[,new_confirmed:=c(confirmed[1],diff(confirmed)),by=administrative_area_level_2]
    epidemic_regions <- tbl[,list('cases'=max(confirmed)),by=administrative_area_level_2][cases>20]$administrative_area_level_2
    tbl <- tbl[administrative_area_level_2 %in% epidemic_regions]
    tbl[new_confirmed<0,new_confirmed:=NA]
    
    for (ll in unique(tbl$administrative_area_level_2)){
      fit <- nbss(tbl[administrative_area_level_2==ll]$new_confirmed)
      tbl[]
    }
    
  } else {
    tbl[,new_confirmed:=c(confirmed[1],diff(confirmed)),by=administrative_area_level_3]
    epidemic_regions <- tbl[,list('cases'=max(confirmed)),by=administrative_area_level_3][cases>20]$administrative_area_level_3
    tbl <- tbl[administrative_area_level_3 %in% epidemic_regions]
    tbl[new_confirmed<0,new_confirmed:=NA]
    
  }
  
  
  dat <- rbind(dat,tbl)
}

# regional nbss -----------------------------------------------------------


dat <- as.data.table(dat)
dat[,deaths_pc:=deaths/population]

wght_avg <- function(mean_position,growth_rate,deaths_pc)  weighted.mean(deaths_pc,exp(mean_position+growth_rate))

eDPC <- NULL
for (i in 1:nrow(regions)){
  cntry <- regions$country[i]
  lvl <- regions$level[i]
  nm <- regions$name[i]
  
  rr <- dat[administrative_area_level_1==cntry & administrative_area_level==lvl]
  rr <- rr[,list(experienced_dpc=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]
  rr$region <- nm
  eDPC <- rbind(eDPC,rr)
}

ggplot(eDPC,aes(date,experienced_dpc,by=region,color=region))+
  geom_line(lwd=2)+
  geom_hline(yintercept=5e-4,lwd=2,lty=2)+
  geom_hline(yintercept=1e-3,lwd=2)+
  theme_bw()+
  scale_y_continuous(trans='log',limits=c(1e-7,4e-3))
