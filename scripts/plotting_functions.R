library(ggplot2)
library(cdcfluview)
library(data.table)
library(COVID19)
library(magrittr)

chd_plot <- function(x,nm){
  ggplot(x,aes(date,new_confirmed))+
    geom_bar(stat='identity',col='grey',fill='grey')+
    geom_bar(aes(date,new_deaths),stat='identity',col='black',fill='black')+
    geom_line(aes(date,hosp),col='steelblue')+
    scale_y_continuous(trans='log',breaks=10^(0:4))+
    theme_bw()+
    ggtitle(nm)
}

ili_plot <- function(states,ili=NULL){
  if (is.null(ili)){
    ili <- cdcfluview::ilinet('state') %>% as.data.table
  }
  x <- ili[region %in% states]
  
  mn <- x[year!=2020,list(unweighted_ili=mean(unweighted_ili,na.rm=T),year='baseline'),by=c('region','week')]
  ggplot(x,aes(week,unweighted_ili,group=year))+
    geom_line(col='grey')+
    geom_line(data=mn)+
    geom_line(data=x[year==2020],col='red')+
    theme_bw()+
    facet_wrap(.~region,scale='free')
}


compare_state_cases_ili <- function(states){
  dfs <- function(x){
    x <- c(x[1],diff(x))
    x[x<0] <- NA
    return(x)
  }
  US_states <- COVID19::covid19('United States',level=2) %>% as.data.table()
  US_states[,state:=administrative_area_level_2]
  US_states[,new_confirmed:=dfs(confirmed),by=state]
  US_states[,new_deaths:=dfs(deaths),by=state]
  
  X <- US_states[state %in% states & date>=as.Date('2020-03-01')]
  
  ili <- cdcfluview::ilinet('state') %>% as.data.table
  
  mx_date <- max(X$date)
  
  mx_wk <- ili[year==2019 & week_start+7>mx_date-365,min(week)]
  mn_wk <- ili[year==2020 & week_start<=as.Date('2020-03-01'),max(week)]
  
  ili <- ili[week<=mx_wk & week>=mn_wk]
  
  plot_list <- vector(mode='list',length=length(states))
  names(plot_list) <- states
  for (st in states){
    plot_list[[st]] <- ggpubr::ggarrange(chd_plot(X[state==st],st),
                                         ili_plot(st,ili),nrow=2,align='v')
  }
  
  g_tot <- ggpubr::ggarrange(plotlist = plot_list,ncol=length(plot_list))
  return(list('all_plots'=g_tot,'individual_plots'=plot_list))
}