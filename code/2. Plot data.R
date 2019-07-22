#==================================================================
#
#  Louise Slater, July 2019
#  Note: this code plots channel morphology time series 
#  computed from USGS transect data.
#
#==================================================================

rm(list=ls(all=TRUE));cat('\014')

library(ggplot2);library(gridExtra);require(grid);require(lubridate)
require(dplyr);library(tidyverse);library(cowplot); library(MASS)
dir.create("./graphs", showWarnings = F)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import the data              #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Background raw channel data 
rawdata <- read.csv("./data/2_data_clean_raw.csv", colClasses="character",stringsAsFactors = F)
cols = names(rawdata)
cols = cols[!cols %in% c("Site", "Date")]
rawdata[,cols] <- sapply(rawdata[,cols],as.numeric)
sapply(rawdata,class)

# Selected site data
sitedata <- read_csv(paste0("./data/3_alldata_cleaned.csv"))
sites = unique(sitedata$Site)

names(sites) <-NULL
nsites <- length(c(sites)); nsites

# Loop through sites ====   

for (thissite in sites) {  tryCatch({ #thissite = sites[1]    

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= CHANNEL RATINGS ==================== #==== 
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Foreground data
    thisdata = sitedata[sitedata$Site == thissite,]
    extra_y = (max(thisdata$Q)- min(thisdata$Q))*0.2
    extra_x = (max(thisdata$GH)- min(thisdata$GH))*0.3
    
    # Background data
    rawdata2 = rawdata[rawdata$Site == thissite,]
    
    QRating_SIMPLE <- 
      ggplot() + theme_bw()+  
      geom_vline(data=thisdata, aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(data=thisdata, aes(yintercept=exp(lnQatGH50)),colour="grey",linetype = "longdash")+
      coord_cartesian(ylim=c(min(thisdata$Q)-extra_y, max(thisdata$Q)+extra_y),
                      xlim=c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x))+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=Q), size=0.5, col="grey70")+
      geom_point(data=thisdata,aes( x=as.numeric(GH), y=Q), size=0.5, col="red")+
      geom_point(data=thisdata,aes(x=as.numeric(GH50), y=exp(lnQatGH50)), size=2, col="blue")+
      xlab("") + ylab("Q (m3/s)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey",  size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent',  colour='transparent'),
            title=element_text(size=8,face="plain"),
            axis.text.x = element_blank(),
            plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))
    
    
    extra_y2 = (max(thisdata$lnQ)- min(thisdata$lnQ))*0.3
    
    QRating <- 
      ggplot(data=thisdata) + theme_bw()+ 
      geom_vline(aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(aes(yintercept=lnQatGH50),colour="grey",linetype = "longdash")+
      coord_cartesian(ylim=c(min(thisdata$lnQ)-extra_y2, max(thisdata$lnQ)+extra_y2),
                      xlim=c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x) )+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=log(Q)), size=0.5, col="grey70")+
      geom_point(aes(x=as.numeric(GH), y=lnQ), size=0.5, col="red")+
      stat_smooth(aes(x=as.numeric(GH), y=lnQ), col="blue", size = 0.1, se=FALSE )+ 
      geom_point(aes(x=GH50, y=lnQatGH50), size=2, pch=21, col="blue",fill="blue")+
      xlab("") +ylab("ln(Q)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            title=element_text(size=8,face="plain"),
            axis.text.x = element_blank(),
            plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))
    
    
    extra_A = (max(thisdata$area)- min(thisdata$area))*0.3
    
    #done
    ARating_SIMPLE <- 
      ggplot(data=thisdata) + theme_bw()+  
      geom_vline(aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(aes(yintercept=exp(lnAatGH50)),colour="grey",linetype = "longdash")+
      coord_cartesian(ylim=c(min(thisdata$area)-extra_A, max(thisdata$area)+extra_A),
                      xlim=c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x))+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=area), size=0.5, col="grey70")+
      geom_point(data=thisdata,aes( x=as.numeric(GH), y=area), size=0.5, col="red")+
      geom_point(aes(x=as.numeric(GH50), y=exp(lnAatGH50)), size=2, col="blue")+
      xlab("") + 
      ylab("A (m2)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            axis.text.x = element_blank(),
            title=element_text(size=8,face="plain"),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    extra_lA = (max(thisdata$lnA)- min(thisdata$lnA))*0.3
    
    ARating <- ggplot(data=thisdata) + theme_bw()+  
      geom_vline(aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(aes(yintercept=lnAatGH50),colour="grey",linetype = "longdash")+
      coord_cartesian(    ylim=c(min(thisdata$lnA)-extra_lA, max(thisdata$lnA)+extra_lA),
                          xlim=c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x))+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=log(area)), size=0.5, col="grey70")+
      geom_point(aes(x=as.numeric(GH), y=lnA), size=0.5, col="red")+
      stat_smooth(aes(x=as.numeric(GH), y=lnA), method="loess", col="blue", size = 0.1, se=FALSE )+ 
      geom_point(aes(x=GH50, y=lnAatGH50), size=2, pch=21, col="blue",fill="blue")+
      xlab("") +   ylab("ln(A)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            axis.text.x = element_blank(),
            title=element_text(size=8,face="plain"),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    
    extra_V = (max(thisdata$velocity)- min(thisdata$velocity))*0.3
    
    # done
    VRating_SIMPLE <-
      ggplot(data=thisdata) + theme_bw()+  
      geom_vline(aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(aes(yintercept=exp(lnVatGH50)),colour="grey",linetype = "longdash")+
      coord_cartesian(ylim=c(min(thisdata$velocity)-extra_V, max(thisdata$velocity)+extra_V),
                      xlim=c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x))+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=velocity), size=0.5, col="grey70")+
      geom_point(data=thisdata,aes( x=as.numeric(GH), y=velocity), size=0.5, col="red")+
      geom_point(aes(x=as.numeric(GH50), y=exp(lnVatGH50)), size=2, col="blue")+
      xlab("") +    ylab("V (m/s)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            axis.text.x = element_blank(),
            title=element_text(size=8,face="plain"),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    extra_lV = (max(thisdata$lnV)- min(thisdata$lnV))*0.3
    
    VRating <-
      ggplot(data=thisdata) + theme_bw()+ 
      geom_vline(aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(aes(yintercept=lnVatGH50),colour="grey",linetype = "longdash")+
      coord_cartesian(  ylim=c(min(thisdata$lnV)-extra_lV, max(thisdata$lnV)+extra_lV),
                        xlim=c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x))+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=log(velocity)), size=0.5, col="grey70")+
      geom_point(aes(x=as.numeric(GH), y=lnV), size=0.5, col="red")+
      stat_smooth(aes(x=as.numeric(GH), y=lnV), method="loess", col="blue", size = 0.1, se=FALSE )+ 
      geom_point(aes(x=GH50, y=lnVatGH50), size=2, pch=21, col="blue",fill="blue")+
      xlab("") + ylab("ln(V)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            axis.text.x = element_blank(),
            title=element_text(size=8,face="plain"),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    
    extra_W = (max(thisdata$width)- min(thisdata$width))*0.3
    
    WRating_SIMPLE <-
      ggplot(data=thisdata) + theme_bw()+ 
      geom_vline(aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(aes(yintercept=exp(lnWatGH50)),colour="grey",linetype = "longdash")+
      coord_cartesian(xlim =c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x),
                      ylim =c(min(thisdata$width)-extra_W, max(thisdata$width)+extra_W) , expand=TRUE)+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=width), size=0.5, col="grey70")+
      geom_point(data=thisdata, aes( x=as.numeric(GH), y=width), size=0.5, col="red")+
      geom_point(aes(x=as.numeric(GH50), y=exp(lnWatGH50)), size=2, col="blue")+
      xlab("") +    ylab("W (m)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            axis.text.x = element_blank(),
            title=element_text(size=8,face="plain"),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    extra_lW = (max(thisdata$lnW)- min(thisdata$lnW))*0.3
    
    WRating <- 
      ggplot(data=thisdata) + theme_bw()+  
      geom_vline(aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(aes(yintercept=lnWatGH50),colour="grey",linetype = "longdash")+
      coord_cartesian(ylim=c(min(thisdata$lnW)-extra_lW, max(thisdata$lnW)+extra_lW),
                      xlim=c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x) )+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=log(width)), size=0.5, col="grey70")+
      geom_point(aes(x=as.numeric(GH), y=lnW), size=0.5, col="red")+
      stat_smooth(aes(x=as.numeric(GH), y=lnW), method="loess", col="blue", size = 0.1, se=FALSE )+ 
      geom_point(aes(x=GH50, y=lnWatGH50), size=2, pch=21, col="blue",fill="blue")+
      xlab("") +   ylab("ln(W)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            axis.text.x = element_blank(),
            title=element_text(size=8,face="plain"),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    
    extra_B = (max(thisdata$BE)- min(thisdata$BE))*0.3
    rawdata2$BE <- rawdata2$GH - (rawdata2$area/rawdata2$width) 
    
    BRating_SIMPLE <- 
      ggplot(data=thisdata) + theme_bw()+  #ylab("") +
      geom_vline(aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(aes(yintercept=BEatGH50),colour="grey",linetype = "longdash")+
      coord_cartesian(      ylim=c(min(thisdata$BE)-extra_B, max(thisdata$BE)+extra_B),
                            xlim=c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x))+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=BE), size=0.5, col="grey70")+
      geom_point(data=thisdata,aes( x=as.numeric(GH), y=BE), size=0.5, col="red")+
      geom_point(aes(x=as.numeric(GH50), y=BEatGH50), size=2, col="blue")+
      xlab("G (m)") +ylab("BE (m)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            title=element_text(size=8,face="plain"),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    BRating <- ggplot(data=thisdata) + theme_bw()+  #xlab("") +
      geom_vline(aes(xintercept=GH50),colour="grey",linetype = "longdash")+
      geom_hline(aes(yintercept=BEatGH50),colour="grey",linetype = "longdash")+
      coord_cartesian(  ylim=c(min(thisdata$BE)-extra_B, max(thisdata$BE)+extra_B),
                        xlim=c(min(thisdata$GH)-extra_x, max(thisdata$GH)+extra_x))+
      geom_point(data=rawdata2, aes(x=as.numeric(GH), y=BE), size=0.5, col="grey70")+
      geom_point(aes(x=as.numeric(GH), y=BE), size=0.5, col="red")+
      stat_smooth(aes(x=as.numeric(GH), y=BE), method="loess", col="blue", size = 0.1, se=FALSE )+ 
      geom_point(aes(x=GH50, y=BEatGH50), size=2, pch=21, col="blue",fill="blue")+
      xlab("G (m)") +  ylab("BE (m)")+
      theme(panel.background=element_rect(fill='white',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.05, linetype="dotted"),
            panel.grid.minor = element_line(colour=NA),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            title=element_text(size=8,face="plain"),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    

    # ============= Channel geom time series =========== ==========================
    
    if(any(grepl("package:dplyr", search()))) detach("package:dplyr") 
    
    
    QTimeSeries <- 
      ggplot() + theme_bw()+
      geom_point(data=thisdata, aes(x=ymd(Date), y=QRES), size=0.5, col="grey60")+
      geom_smooth(data=thisdata, aes(x=ymd(Date), y=QRES),method="lm",size=2, col="grey40",se=T)+ 
      xlab("") +   ylab("Channel\ncapacity")+
      scale_x_date(date_breaks = "10 years", date_labels = "%Y", expand=c(0,0))+
      coord_cartesian(xlim = as.Date(c('1950-01-01','2020-01-01'))) + 
      theme(legend.position = "none",
            panel.background=element_rect(fill='transparent',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.1, linetype="dotted"),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(angle=0, hjust=0.5, vjust=0.5, size=15, face="bold"),
            axis.text.x = element_blank(),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))
    
    ATimeSeries<- 
      ggplot() + theme_bw()+ # ggtitle(paste0 ("Site ",thissite))+
      geom_point(data=thisdata, aes(x=ymd(Date), y=ARES), size=0.5, col="grey60")+
      geom_smooth(data=thisdata, aes(x=ymd(Date), y=ARES),method="lm",size=2, col="grey40",se=T)+ 
      xlab("") +   ylab("Area")+
      scale_x_date(date_breaks = "10 years", date_labels = "%Y", expand=c(0,0))+
      coord_cartesian(xlim = as.Date(c('1950-01-01','2020-01-01'))) +
      theme(legend.position = "none", 
            axis.text.x = element_blank(),
            panel.background=element_rect(fill='transparent',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.1, linetype="dotted"),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(angle=0, hjust=0.5, vjust=0.5, size=15, face="bold"),
            title=element_text(size=8,face="plain"),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    
    VTimeSeries<- 
      ggplot() + theme_bw()+ 
      geom_point(data=thisdata, aes(x=ymd(Date), y=VRES), size=0.5, col="grey60")+
      geom_smooth(data=thisdata, aes(x=ymd(Date), y=VRES),method="lm",size=2, col="grey40",se=T)+ 
      xlab("")+  ylab("Flow\nvelocity")+ 
      scale_x_date(date_breaks = "10 years", date_labels = "%Y", expand=c(0,0))+
      coord_cartesian( xlim = as.Date(c('1950-01-01','2020-01-01'))) +
      theme(legend.position = "none", #axis.text.x = element_blank(),
            panel.background=element_rect(fill='transparent',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.1, linetype="dotted"),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(angle=0, hjust=0.5, vjust=0.5, size=15, face="bold"),
            axis.text.x = element_blank(),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    
    WTimeSeries<- 
      ggplot() + theme_bw()+
      geom_point(data=thisdata, aes(x=ymd(Date), y=WRES), size=0.5, col="grey60")+
      geom_smooth(data=thisdata, aes(x=ymd(Date), y=WRES),method="lm",size=2, col="grey40",se=T)+ 
      xlab("") +   ylab("Width")+
      scale_x_date(date_breaks = "10 years", date_labels = "%Y", expand=c(0,0))+
      coord_cartesian( xlim = as.Date(c('1950-01-01','2020-01-01'))) +
      theme(legend.position = "none",
            panel.background=element_rect(fill='transparent',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.1, linetype="dotted"),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(angle=0, hjust=0.5, vjust=0.5, size=15, face="bold"),
            axis.text.x = element_blank(),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    
    BTimeSeries <- 
      ggplot() + theme_bw()+
      geom_point(data=thisdata, aes(x=ymd(Date), y=BRES), size=0.5, col="grey60")+
      geom_smooth(data=thisdata, aes(x=ymd(Date), y=BRES),method="lm",size=2, col="grey40",se=T)+ 
      xlab("") +   ylab("Bed\nelevation")+
      scale_x_date(date_breaks = "10 years", date_labels = "%Y", expand=c(0,0))+
      coord_cartesian( xlim = as.Date(c('1950-01-01','2020-01-01'))) +
      theme(legend.position = "none",
            panel.background=element_rect(fill='transparent',colour="grey"),
            panel.grid.major = element_line(colour="grey", size=0.1, linetype="dotted"),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(angle=0, hjust=0.5, vjust=0.5, size=15, face="bold"),
            plot.background=element_rect(fill='transparent', colour='transparent'),
            plot.margin=unit(c(-0.2,0.2,0.2,0.2), "cm"))
    
    
    
    # ============= ASSEMBLE PLOTS ===================== #######
    blank <- grid.rect(gp=gpar(col="white"))
    
    Col1 <- plot_grid( QRating_SIMPLE,ARating_SIMPLE,VRating_SIMPLE,WRating_SIMPLE,BRating_SIMPLE, align = "v", ncol=1)
    Col2 <- plot_grid( QRating,ARating,VRating,WRating,BRating,  align = "v", ncol=1)
    Col3 <- plot_grid(QTimeSeries,ATimeSeries,VTimeSeries,WTimeSeries,BTimeSeries, align = "v", ncol=1)
    
    
    # ============= OUTPUT THE GRAPHS =========================
    
    
    pdf(paste0("./graphs/",thissite,".pdf"),width=12, height=12, paper="special", useDingbats=F)
    Together = grid.arrange(arrangeGrob(Col1, Col2, Col3, ncol=3, widths=c(0.25,0.25,0.5)))
    dev.off()
    
    
    rm(list=setdiff(ls(), c("df","cors", "nsites", "rawdata", "sites", "blank", "sitedata", "ptm")))

}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

