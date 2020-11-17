plot_state <- function(x,US_states=NULL,US_counties=NULL,rDlag=11){
  if(is.null(US_states)){
    US_states <- read.csv('data/nbss_us_states.csv') %>% as.data.table()
    US_states[,date:=as.Date(date)]
  }
  if(is.null(US_counties)){
    US_counties <- read.csv('data/nbss_us_counties.csv') %>% as.data.table
    US_counties[,date:=as.Date(date)]
    
  }
  
  
  
  X <- US_states[state==x]
  Y <- US_counties[state==x]
  ILI <- cdcfluview::ilinet(region = 'state') %>% as.data.table
  ILI <- ILI[region==x]
  
  
  
  plot_list <- vector(mode='list',length=5)
  names(plot_list) <- c('cases_hosp_deaths_ili','county_cases','map','erl','dpc')
  
  ####################### Cases 
  g1=ggplot(X,aes(date,new_confirmed))+
                        geom_bar(stat='identity',fill='grey',col='grey')+
                        geom_bar(aes(date,new_deaths),stat='identity',col='black',fill='black')+
                        geom_line(aes(date,hosp),lwd=2,col='steelblue')+
                        facet_wrap(.~state,scale='free')+
                        theme_bw()+
                        theme(legend.position='none')+
                        scale_y_continuous(trans='log',breaks=10^(0:4))+
                        ggtitle(paste(x,'cases, deaths, hospitalizations'))
  g_hosp <- ggplot(X,aes(date,hosp))+
    geom_bar(stat='identity',fill='steelblue',col='steelblue')+
    geom_hline(yintercept = unique(X$population)/1e3,col='red',lwd=2)+
    ggtitle('Hospitalizations')+theme_bw()
  
  mn <- ILI[year<2020,list(unweighted_ili=mean(unweighted_ili),
                         year='baseline'),by=c('region','week')]
  g_ili <- ggplot(ILI,aes(week,unweighted_ili,group=year))+
            geom_line()+
            geom_line(data=mn,lwd=2)+
            geom_line(data=ILI[year==2020],col=rgb(0,0.9,0.6),lwd=1.5)+
            theme_bw()+
            ggtitle('Influenza-like Illness')
  
  plot_list[[1]] <- ggpubr::ggarrange(g1,g_hosp,g_ili,nrow=3,align='v')
  ################# Counties ##############
  
  Y[,tot:=max(confirmed,na.rm=T),by=county]
  Y[,rank:=match(tot,sort(unique(tot),decreasing = T))]

  Y[,past_14_d:=sum(new_confirmed[.N-0:13]),by=county]
  Y[,rank_14_d:=match(past_14_d,sort(unique(past_14_d),decreasing = T))]


  county_order <- Y[,list(rank=unique(rank)),by=county]
  setkey(county_order,rank)

  Y[,county:=factor(county,levels=county_order$county)]

  Y[,threshold_date:=min(date[deaths/population>5e-4],na.rm=T),by=county]
  Y[,onek_threshold_date:=min(date[deaths/population>1e-3],na.rm=T),by=county]

  plot_list[[2]] <- ggplot(Y[date>as.Date('2020-03-10') & county %in% county_order$county[1:25]],aes(date,new_confirmed))+
                      geom_bar(stat='identity',col='grey',fill='grey',lwd=0.5)+
                      geom_bar(aes(date,new_deaths),stat='identity',col='black',fill='black')+
                      facet_wrap(~county)+
                      geom_vline(aes(xintercept=threshold_date),lwd=2,col='orange',alpha=0.8)+
                      geom_vline(aes(xintercept=onek_threshold_date),lwd=2,col='red',alpha=0.8)+
                      theme_bw(base_size = 20)+
                      scale_y_continuous(trans='log',breaks=10^(0:3))
  
  #### map
  
  Y[,polyname:=tolower(paste(state,county,sep=','))]
  county.fips <- as.data.table(maps::county.fips)
  setkey(Y,polyname)
  setkey(county.fips,polyname)
  
  Y <- county.fips[Y]
  setkey(Y,county,date)
  
  mx <- Y[,list(growth_rate=growth_rate[.N],
                fips=unique(fips),
                pop=unique(population)),by=county]
  z <- max(abs(mx$growth_rate))
  # plot_list[[3]] <- usmap::plot_usmap(regions='state',data=mx,values='growth_rate',include = "Illinois")+
  #   scale_fill_gradient2(low='blue',mid='grey',high='red',limits=c(-z,z))
  
  #### erl
  
  if (is.null(X$growth_rate)){
    US <- read.csv('data/nbss_us_states.csv') %>% as.data.table()
    US[,date:=as.Date(date)]
    NY <- US[state=='New York']
    X <- US[state==x]
  } else {
    NY <- US_states[state=='New York']
  }
  S <- read.csv('data/nbss_countries.csv') %>% as.data.table()
  S <- S[country=='Sweden']
  S[,date:=as.Date(date)]
  NYC <- US_counties[county=='New York City']
  
  max_date <- X[new_confirmed>0 & !is.na(new_confirmed),max(date)]
  plot_list[[4]] <- ggplot(Y,aes(deaths_pc,growth_rate))+
    geom_line(data=US_states[state=='New York'],lwd=2,col='black')+
    geom_line(data=NYC,lwd=2,col='grey')+
    geom_line(data=Y[date>=max_date-14],aes(group=county))+
    geom_point(data=Y[date==max_date],aes(cex=log(population)),alpha=0.5)+
    geom_point(data=X[state==x][.N],cex=8,col=rgb(0.8,0.2,0,0.8))+
    geom_line(data=X[state==x],lwd=2,col=rgb(0.8,0.2,0))+
    geom_line(data=Y[county==mx[which.max(pop),county]],lwd=2,col='orange')+
    geom_point(data=Y[date==max_date & county==mx[which.max(pop),county]],cex=5,col='orange')+
    scale_x_continuous(trans='log',limits=c(1e-5,5e-3),breaks=10^(-8:1))+
    scale_y_continuous(limits=c(-0.2,0.4))+
    geom_vline(xintercept = 3.6e-3,lwd=2,lty=2)+
    geom_vline(xintercept = 1.2e-3,lwd=2,lty=3)+
    geom_hline(yintercept = 0)+
    theme_bw()+
    theme(legend.position='none')+
    ggtitle(paste(x,'r(D)'))
  
  #### dpc
  wght_avg <- function(mean_position,growth_rate,deaths_pc,population){
    weighted.mean(deaths_pc,exp(mean_position+growth_rate))
    # weighted.mean(deaths_pc,exp(growth_rate))
  }
  avg_dpc <- Y[growth_rate>0 & !is.na(deaths_pc),
                     list(theta=wght_avg(mean_position,growth_rate,deaths_pc)),by=date]
  setkey(avg_dpc,date)
  
  # plot_list[[4]] <- plot_list[[4]] + geom_vline(xintercept = avg_dpc[.N,theta])
  
  Y[,increasing:=factor(sign(growth_rate),levels = c(-1,1))]
  plot_list[[5]] <- ggplot(Y,aes(date,deaths_pc))+
    geom_point(aes(color=increasing),pch=16,alpha=0.3)+
    geom_line(data=X,lwd=4,col=rgb(0.8,0.2,0))+
    geom_line(data=avg_dpc,aes(date,theta),col='black',lwd=2)+
    geom_hline(yintercept=5e-4,lty=4,lwd=2)+
    geom_hline(yintercept = 1.2e-3,lty=3,lwd=2)+
    geom_hline(yintercept = 3.6e-3,lty=2,lwd=2)+
    scale_y_continuous(trans='log',limits=c(1e-6,4e-3),breaks = c(1e-6,1e-5,1e-4,1e-3))+
    theme_bw()+
    theme(legend.position='none')
  
  return(plot_list)
}

