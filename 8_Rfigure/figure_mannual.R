rm(list = ls())

# Packages
library("map")
library(readr)
library(ggplot2)
library(dplyr)
library(viridis)
library("reshape2")
library("data.table")
library("cowplot") # multi-panel figure with common legend
library("gridGraphics") # for drawing multi-panel figure
library("ggthemr") # theme package
library("Cairo")  #for displaying the greek symbols, such as triangle symbols.
library("usmap")
# library("scales")  # for intalling the ggthemr package
# library("devtools") # for intalling the ggthemr package
library("writexl")
library("ggExtra")
library("scales")



#1.simulation ------------------------------------------------------

# 1.1 initialize EDM parameter vector -------------------------------------

de <- -0.53 # demand elasticity
k1 <- 0.017 # gly share
k2 <- 0.019 # comp share
eos <- 0.785  # elasticity of substitution
se1 <- c(0.5,1,1.5)   # gly supply elasticity
se2 <- c(0.5,1.0,1.5)   # comp supply elasticty 
t <- seq(.1,.5,.01)  # tax rate
#t <- sub('^(-)?0[.]', '\\1.', t) this leads to character not number

parlist <- list(de=de,k1=k1,k2=k2,eos=eos,se1=se1,se2=se2,t=t)
par <- expand.grid(parlist) # all combinations of the simdataameter values.

### clean the simdatafull to keep only 3 combinations of (se1,se2)

# transform to data.table
par <- data.table(par) 


### initialize damage prices
d11 <- 4.68; d12 <- 5.66; d13 <- 4.83; d14 <- 5.83;
d2 <- 5.35

### initialize baseline values
q0 <- 13184.63; m0 <- 4.85; 
x10 <- 24.89;x20 <- 35.96; p10 <- 22.93; p20 <- 28.49



# 1.2 equilibrium solutions -----------------------------------------------

sim <- par
sim$d <- sim$eos*(-sim$de+sim$k1*sim$se1+sim$k2*sim$se2)-sim$de*(sim$k2*sim$se1+sim$k1*sim$se2)+sim$se1*sim$se2*(sim$k1+sim$k2)^2
sim$eq <- ((sim$eos+sim$se2*(sim$k1+sim$k2))*sim$k1*sim$se1*sim$de*sim$t)/sim$d
sim$em <- ((sim$eos+sim$se2*(sim$k1+sim$k2))*sim$k1*sim$se1*sim$t)/sim$d
sim$ex1 <- -((-sim$de*sim$eos+(sim$k2*sim$eos-sim$k1*sim$de)*sim$se2)*sim$se1*sim$t)/sim$d
sim$ex2 <- (sim$k1*(sim$eos+sim$de)*sim$se1*sim$se2*sim$t)/sim$d
sim$ep1 <- ((sim$k1*sim$eos-sim$k2*sim$de+sim$se2*(sim$k1+sim$k2)^2)*sim$se1*sim$t)/sim$d
sim$ep2 <- (sim$k1*(sim$eos+sim$de)*sim$se1*sim$t)/sim$d


# 1.3 welfare changes -----------------------------------------------------

# economic welfare and deadweight loss
sim$cs <- -m0*q0*sim$em*(1+0.5*sim$eq)
sim$ps1 <- p10*x10*(sim$ep1-sim$t)*(1+0.5*sim$ex1)
sim$ps2 <- p20*x20*sim$ep2*(1+0.5*sim$ex2)
sim$tax <- sim$t*p10*x10*(1+sim$ex1) #correction to: sim$tax <- sim$t*p10*x10*(1+sim$ep1)*(1+sim$ex1)
sim$ps <- sim$ps1+sim$ps2
sim$dwl <- -(sim$cs+sim$ps1+sim$ps2+sim$tax) # positive if net welfare loss

# environmental benefit, under four scenarios; positive if net environmental gain
for (i in 1:4)
{
  sim[[paste("env",i,sep="")]] <- -get(paste("d1",i,sep = ""))*x10*sim$ex1+d2*x20*sim$ex2
}


# net social welfare, positive if net loss
for (i in 1:4)
{
  sim[[paste("s",i,sep="")]] <- sim$dwl-sim[[paste("env",i,sep="")]]
}

# se combination
sim$secomb <- paste("(",sim$se1,",",sim$se2,")")

# export simulation results
write_xlsx(sim,"sim.xlsx")

# 1.4 plots: welfare effects ----------------------------------------------

# 1.4.1 prepare data ------------------------------------------------------

# extract dwl group
var <- subset(sim,select=c(dwl,cs,ps,tax))
str(var)

for (v in names(var)){
  temp <- subset(sim, select=c(t,get(paste(v)),se1,se2,secomb))
  temp$group <- toupper(v) 
  temp <- subset(temp, se1==0.5&se2==1.5|se1==1&se2==1|se1==1.5&se2==0.5) #filter se combination 
  names(temp)[2] <- paste("welfare")
  assign(paste("sim",v,sep=""),temp)
  }


# extract env group
for (i in 1:4)
  {
  temp <- subset(sim,select=c(t,get(paste("env",i,sep="")),se1,se2,secomb))
  temp$group <- "ENV"
  temp <- subset(temp, se1==0.5&se2==1.5|se1==1&se2==1|se1==1.5&se2==0.5) #filter se combination 
  names(temp)[2] <- paste("welfare")
  assign(paste("simenv",i,sep=""),temp)
  }

# create data for each panel (scenario)
for (i in 1:4)
  {
  temp <- rbind(simcs,simps,simtax,get(paste("simenv",i,sep="")))
  levels(temp$group)["ENV"] <- 1
  assign(paste("pldata",i,sep=""),temp)
  }


# 1.4.2 plot: individual panel  ---------------------------------------------------------------
ggthemr('solarized',layout="clear") 
colour_plot(swatch())
ttsty1 <- element_text(face="plain", size=6, color="black") #the strip font is set separately
listy1 <- element_line(size=0.05,color="black")

for (i in 1:4)
  {
  p <- ggplot(get(paste("pldata",i,sep="")), 
              aes(x=t, y=welfare, fill=factor(group,levels=c("ENV","TAX","CS","PS"))) #use factor() to customize stacking order
              )+ 
        geom_area(colour = "black", size = 0.05,alpha=1)+
              # use colour = "black", size = 0.2, to add seperating lines; use alpha=. to set fill transparency.
        theme_classic()+
        theme(plot.margin = unit(c(10, 5, -15, 5), "pt"))+
        theme(aspect.ratio=1.5)+
            # ratio of length and height
        theme(panel.grid.major=listy1)+
        theme(panel.grid.major = element_line(color="grey",linetype="1F"))+
        theme(panel.background = element_rect(fill = "#FFFFFF"))+
        theme(axis.title=ttsty1)+
        theme(axis.line = listy1,axis.ticks = listy1)+
        theme(axis.text =ttsty1)+
        labs(x="Tax rate",y="Welfare ($,million)")+
        scale_x_continuous(breaks=seq(0.1, 0.5, 0.2))+    
        scale_y_continuous(labels=comma)+
        #scale_x_reverse(breaks=seq(.1, .5, .1), labels=sub("^(-?)0.", "\\1.", sprintf("%.1f", seq(.1, .5, .1))))+
        scale_fill_manual(values=c( "#2aa198","#dc322f","#083642", "#268bd2"),labels=c("HH-E welfare change","Tax transfer","Consumer surplus change","Producer surplus change"))+
        theme(legend.title = element_blank())+
        theme(strip.text = ttsty1)+
        theme(strip.text = element_text(margin=margin(1,0,1,0,"pt"),colour = 'white'), 
              strip.background = element_rect(fill = 'black', linetype = "blank"))
              # the title for each secomb
        #ylim=c(-1650,500)
        #, labels=sub("^(-?)0.", "\\1.", sprintf("%.1f", seq(0.1, 0.5, 0.2)))
  
  if (i==1|i==2){
    p <- p+theme(axis.title.x=element_blank()) # no x-axis title
    p <- p+theme(legend.position = "None")  # no legend
  }
  
  else {
    p <- p+theme(legend.position = "None")  # no legend
  }
  
        
  assign(paste("pl",i,sep=""),p+facet_wrap(~secomb)) # sub-plot by secomb
}




# values=c("#a3c9c7", "#353848","#cb7575", "#ef9e9f"),



# 1.4.3 plot: combined ------------------------------------------------------

# combine panels into one figure with shared legend
plcomb <- plot_grid(pl1,pl2,pl3,pl4,
                    align="hv",
                    labels=c("A","B","C","D"),
                    label_size=12,label_colour="black",
                    hjust=-0.75,vjust=1.5,
                    nrow=2
                    )

# extract the legend from one of the plots
legend <- get_legend(pl1+theme(legend.position="bottom",
                               legend.text = ttsty1,
                               legend.title = element_blank(),
                               legend.key.size=unit(0.2,"cm"),
                               legend.margin=margin(-3,-10,-15,-10,"pt")))


# add the legend underneath the comb. figure made earlier
pl <- plot_grid(plcomb,legend,ncol=1,rel_heights = c(1,0.2))
print(pl)

# save
ggsave(filename="welfare.pdf",plot=pl,width=4.5,height=2.8,units = "in",dpi=300)
ggsave(filename="welfare.png",plot=pl,width=4.5,height=2.8,units = "in",dpi=300)



# 1.5 plot: equilibrium solutions -----------------------------------------

# # extract relevant data
# sol1 <- subset(sim,t==0.10,select=c(eq,em,ex1,ex2,ep1,ep2,t,se1,se2,secomb))
# sol5 <- subset(sim,t==0.50,select=c(eq,em,ex1,ex2,ep1,ep2,t,se1,se2,secomb))
# 
# # wide to long
# temp1 <- melt(sol1, id=c("t","secomb","se1","se2"))
# temp5 <- melt(sol5, id=c("t","secomb","se1","se2"))
# 
# # merge
# temp <- merge(temp1,temp5,by=c("variable","secomb","se1","se2"))
# names(temp)[names(temp) == 'value.x'] <- 'min'
# names(temp)[names(temp) == 'value.y'] <- 'max'
# psodata <- subset(temp,se1==0.5&se2==1.5|se1==1&se2==1|se1==1.5&se2==0.5, select=c(variable, min,max,secomb))
# 
# # set up theme: use ttsty1 and listy1
# ggthemr('light',layout="clean") 
# 
# # creat plot
# pso <- ggplot(data=psodata)+
#         geom_crossbar(aes(x=variable,y=min,ymin=min, ymax=max),fatten=0)+
#         facet_wrap(~secomb)
# pso
# 
# +
#         labs(x="Year",y="Chemical share (%)",fill="Chemical")+
#         scale_fill_discrete(labels=c("Others","Metolachlor","Acetochlor","Atrazine","Glyphosate"))+
#         scale_y_continuous(labels = scales::percent_format(accuracy = 0.01))+
#         theme(panel.background = element_rect(fill = "#FFFFFF"),
#               panel.border = resty2 ,
#               panel.grid.major=listy2,
#               axis.title=ttsty2,
#               axis.line = listy2,axis.ticks = listy2,
#               axis.text =ttsty2,
#               legend.text = ttsty2,
#               legend.title = ttsty2,
#               legend.key.size=unit(0.2,"cm"))+
#         theme(panel.grid.major = element_line(color="grey",linetype="1F"))
# 


# 1.5.1 prepare data ------------------------------------------------------
sol1 <- subset(sim,t==0.10,select=c(eq,em,ex1,ex2,ep1,ep2,t,se1,se2,secomb))

# wide to long
temp1 <- melt(sol1, id=c("t","secomb","se1","se2"))

# subset
psodata <- subset(temp1,se1==0.5&se2==1.5|se1==1&se2==1|se1==1.5&se2==0.5, select=c(variable, value ,secomb))
psodata$value <- 100*psodata$value

ttsty1 <- element_text(face="plain", size=6, color="black") #the strip font is set separately
listy1 <- element_line(size=0.05,color="black")

ggthemr('solarized',layout="clear") 
colour_plot(swatch())

pso <-ggplot(psodata, aes(fill=variable,y=value,x=variable))+
      # barplot(height=value)+
        geom_bar(position="dodge",stat="identity")+
      facet_wrap(~secomb)+
      theme_classic()+
      theme(panel.grid.major=listy1)+
      theme(panel.grid.major = element_line(color="grey",linetype="1F"))+
      theme(panel.background = element_rect(fill = "#FFFFFF"))+
      theme(axis.title=ttsty1)+
      theme(axis.line = listy1,axis.ticks = listy1)+
      theme(axis.text =ttsty1)+
      theme(legend.position = "none")+
      theme(strip.text = ttsty1)+
      theme(strip.text = element_text(margin=margin(1,0,1,0,"pt"),colour = 'white'), 
            strip.background = element_rect(fill = 'black', linetype = "blank"))+
      geom_hline(yintercept=0,color="black",size=0.02)+
      ylab("Percentage change (%)")+
      scale_x_discrete("Market variables",breaks=c("eq","em","ex1","ex2","ep1","ep2"),labels = c('EQ','EM',expression(EX[1]),expression(EX[2]),expression(EP[1]),expression(EP[2])))+
      scale_y_continuous(breaks = seq(-14, 2, by=2), limits=c(-14,2))
pso

ggsave(filename="perchange.pdf",plot=pso,width=8.7,height=5,units = "cm",dpi=300)
ggsave(filename="perchange.png",plot=pso,width=8.7,height=5,units = "cm",dpi=300)








# 2. background -----------------------------------------------------------

# set up theme
ggthemr('solarized',layout="clear") 
colour_plot(swatch())

ttsty2 <- element_text(face="plain", size=6, color="black") #the strip font is set separately
listy2 <- element_line(size=0.05,color="black")
resty2 <- element_rect(size=0.05,colour = "black", fill=NA, )


# 2.1 chemical share -------------------------------------------------------
psdata <- read_csv("fig1a.csv")
save(psdata, file = "psdata.RData")

# To load the data again
load("psdata.RData")


# creat plot
ps <- ggplot(data=psdata, aes(x=year, y=value, fill=factor(variable,levels=c("others","meto","acet","atra","glyp")))) + 
        geom_bar(position="fill", stat="identity",width = 0.3,alpha=1)+
        labs(x="Year",y="Chemical share (%)",fill="Chemical")+
        scale_fill_manual(labels=c("Others","Metolachlor","Acetochlor","Atrazine","Glyphosate"),values=c("#B57638","#073642","#2aa198","#6c71c4","#dc322f"))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              panel.border = resty2 ,
              panel.grid.major=element_blank(),
              axis.title=ttsty2,
              axis.line = listy2,axis.ticks = listy2,
              axis.text =ttsty2,
              legend.text = ttsty2,
              legend.title = ttsty2,
              legend.key.size=unit(0.1,"cm"),
              plot.margin = unit(c(15, 5, 0, 5), "pt"))+
        theme(axis.title.x=element_blank())#+
#        theme(legend.position="left")



print(ps)

ggsave(filename="chemicalshare.png",plot=ps,width=4.5,height=2.8,units = "in",dpi=300)


# 2.2 choice variables ----------------------------------------------------
choice <- read_csv("fig1b.csv")
save(choice, file = "choice.RData")
load("choice.RData")

pcdata <- melt(choice, id=c("year"))

# plot
pc <- ggplot()+
        geom_bar(data=filter(pcdata,variable %in% c("rate_glyp","rate_comp")),aes(x=year,y=value,fill=variable),stat="identity",position=position_dodge(width = 0.5),width=0.3)+
        scale_fill_manual(labels=c("Glyphosate","The Composite"),values = c("#dc322f","#268bd2"))+
        geom_line(data=filter(pcdata,variable %in% c("shr_gt","shr_convt")),aes(x=year,y=value,color=variable,group=variable),linetype="dotted",size=0.01)+
        geom_point(data=filter(pcdata,variable %in% c("shr_gt","shr_convt")),aes(x=year,y=value,color=variable,shape=variable),size=0.5)+
        scale_shape_manual(name="Seed and tillage",
                           labels=c("GT seed","Conv. tillage"),
                           values=c(17,15))+
        scale_color_manual(name="Seed and tillage",
                           labels=c("GT seed","Conv. tillage"),
                           values=c("#b58900","black"))+
        scale_y_continuous(sec.axis = sec_axis(~./1, name = "Adoption rates (%)",labels = scales::percent_format(accuracy = 1)))+
        labs(x="Year",y="Usage (kg/ha)",color="Seed and tillage",fill="Herbicide")+
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              panel.border = resty2 ,
              panel.grid.major=element_blank(),
              axis.title=ttsty2,
              axis.line = listy2,axis.ticks = listy2,
              axis.text =ttsty2,
              legend.text = ttsty2,
              legend.title = ttsty2,
              legend.key.size=unit(0.1,"cm"),
              plot.margin = unit(c(5, 5, 0, 5), "pt"))+
  theme(axis.title.x=element_blank())        

       
      
  

print(pc)
  






# 2.3 resistance ----------------------------------------------------------
resis <- read_csv("trend_wcount.csv")
save(resis, file = "resis.RData")

# To load the data again
load("resis.RData")


# reshape data
prdata <- melt(resis,id=c("year"))

# plot
pr <- ggplot()+
        geom_line(data=prdata,aes(x=year,y=value,color=variable,group=variable),linetype="dotted",size=0.01)+
        geom_point(data=prdata,aes(x=year,y=value,color=variable,shape=variable),size=0.5)+
        scale_shape_manual(name="Herbicide",
                           labels=c("Glyphosate","The Composite","Resist"),
                           values=c(17,15,19))+
        scale_color_manual(name="Herbicide",
                           labels=c("Glyphosate","The Composite","Resist"),
                           values=c("#dc322f","#268bd2","#b58900"))+
        labs(x="Year",y="Weed count",color="Herbicide")+
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              panel.border = resty2 ,
              panel.grid.major=element_blank(),
              axis.title=ttsty2,
              axis.line = listy2,axis.ticks = listy2,
              axis.text =ttsty2,
              legend.text = ttsty2,
              legend.title = ttsty2,
              legend.key.size=unit(0.1,"cm"),
              plot.margin = unit(c(5, 5, 0, 5), "pt"))+
  theme(axis.title.x=element_blank())        
  


print(pr)


# 2.4 herbicide price ----------------------------------------------------------
price <- read_csv("trend_price.csv")
save(price, file = "price.RData")

# To load the data again
load("price.RData")


# reshape data
ppdata <- melt(price,id=c("year"))

# plot

pp <- ggplot()+
  geom_line(data=ppdata,aes(x=year,y=value,color=variable,group=variable),linetype="dotted",size=0.01)+
  geom_point(data=ppdata,aes(x=year,y=value,color=variable,shape=variable),size=0.5)+
  scale_shape_manual(name="Herbicide",
                     labels=c("Glyphosate","The Composite","lnP"),
                     values=c(17,15,19))+
  scale_color_manual(name="Herbicide",
                     labels=c("Glyphosate","The Composite","lnP"),
                     values=c("#dc322f","#268bd2","#b58900"))+
  labs(x="Year",y="Log price",color="Herbicide")+
  theme(panel.background = element_rect(fill = "#FFFFFF"),
        panel.border = resty2 ,
        panel.grid.major=element_blank(),
        axis.title=ttsty2,
        axis.line = listy2,axis.ticks = listy2,
        axis.text =ttsty2,
        legend.text = ttsty2,
        legend.title = ttsty2,
        legend.key.size=unit(0.1,"cm"),
        plot.margin = unit(c(5, 5, 0, 5), "pt"))+
  theme(axis.title.x=element_blank())        

print(pp)

  
# 2.5 combine -------------------------------------------------------------
# combine panels into one figure with shared legend
pb <- plot_grid(ps,pc,pr,pp,
                    align="v",
                    labels=c("A","B","C","D"),
                    label_size=12,label_colour="black",
                    hjust=-0.5,vjust=1.2,
                    nrow=4
)

print(pb)

# save
ggsave2("backgr.pdf",plot=pb,width=8.7,height=11,units = "cm")
ggsave2("backgr.png",plot=pb,width=8.7,height=11,units = "cm")


# 3. resistance map -----------------------------------------------------------

# 3.1 prepare data
res_map <- read_csv("res_map.csv")
save(res_map, file = "res_map.RData")

# To load the data again
load("res_map.RData")

res_map$region <- tolower(res_map$State)
res_map <- subset(res_map,select=c("Year","region","count","herbicide"))
res_map$herbicide[res_map$herbicide=="composite"] <- "The Composite"
res_map$herbicide[res_map$herbicide=="glyphosate"] <- "Glyphosate"

us_states <- map_data("state")
res_cord <- left_join(us_states,res_map,by="region")

#plot
ttsty1 <- element_text(face="plain", size=6, color="black") #the strip font is set separately
listy1 <- element_line(size=0.05,color="black")

mapr <- ggplot(data=res_cord, 
            aes(x=long,y=lat,group=group,fill=count))
mapr <- mapr+geom_polygon(color="gray50",size=0.1)+
  coord_map(projection="albers",lat0=39,lat1=45)+
  theme_map()+  facet_grid(Year~herbicide,drop=T)+
  scale_fill_gradient(low="white",high="#CB454A",breaks=c(0,3,6,9,12,15))+
  theme(strip.text = element_text(margin=margin(3,3,3,3,"pt"),colour = 'black',size=6), 
        strip.background = element_rect(fill = 'gray80', linetype = "blank"))+
  theme(legend.text = ttsty1,
        legend.title = ttsty1)
  # the title for each secomb
  
mapr

#save
ggsave(filename="resmap.png",plot=mapr,width=4.5,height=2.8,units = "in",dpi=300)

# 4. map for all -----------------------------------------------------------
# 4.1 individual plot

for (i in 1:6)
{ mapdata <- read_csv(paste("map_var",i,".csv",sep=""))
  save(mapdata, file = "mapdata.RData")
  
  # To load the data again
  load("mapdata.RData")
  
  mapdata$region <- tolower(mapdata$state)

  us_states <- map_data("state")
  res_cord <- left_join(us_states,mapdata,by="region")
  
  #plot
  ttsty1 <- element_text(face="plain", size=6, color="black") #the strip font is set separately
  listy1 <- element_line(size=0.05,color="black")
  
  mapr <- ggplot(data=res_cord, 
                 aes(x=long,y=lat,group=group,fill=value))
  mapr <- mapr+geom_polygon(color="black",size=0.1)+
    coord_map(projection="albers",lat0=39,lat1=45)+
    theme_map()+
    scale_fill_gradient(low="white",high="skyblue2")+
    theme(legend.text = ttsty1,
          legend.title = ttsty1,
          legend.key.height = unit(0.25, "cm"),
          legend.key.width = unit(0.2, "cm") )
  # the title for each secomb
  
  plot(mapr)
  assign(paste("pl",i,sep=""),mapr) # sub-plot by secomb
}


# combine panels into one figure with separate legend
plcomb <- plot_grid(pl1,pl2,pl3,pl4,pl5,pl6,
                    align="hv",
                    nrow=3,
                    labels=c("A. Number of observations","B. glyphosate cost-share","C. lnP","D. Resistance","E. GT","F. Till"),
                    label_size=6,label_colour="black",
                    label_x=0.1,hjust=0)
    



pl <- plot_grid(plcomb)
print(pl)

#save
ggsave(filename="mapall.pdf",plot=pl,width=4.5,height=3.5,units = "in",dpi=300)
ggsave(filename="mapall.png",plot=pl,width=4.5,height=3.5,units = "in",dpi=300)

# 5. data plot -----------------------------------------------------------
checkout:
https://cran.r-project.org/web/packages/ggExtra/readme/README.html

# example 1
library(ggplot2)
# install.packages("ggpointdensity")
library(ggpointdensity)

df <- data.frame(x = rnorm(5000), y = rnorm(5000))
ggplot(df, aes(x=x, y=y)) + geom_pointdensity() + scale_color_viridis_c()

# attempt
# prepare data
actual <- read_csv("scatter_actual.csv")
save(actual, file = "actual.RData")

# To load the data again
load("actual.RData")

df <- data.frame(actual)

# plot
ggthemr('solarized',layout="clear") 
ggplot(df,aes(x=lnp,y=Actual))+geom_pointdensity() + scale_color_viridis_c()+facet_wrap(~year)+
    geom_smooth(method=lm,size=1)


# example 2
## Data in a data.frame
x1 <- rnorm(n=1E3, sd=2)
x2 <- x1*1.2 + rnorm(n=1E3, sd=2)
df <- data.frame(x1,x2)

## Use densCols() output to get density at each point
x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
df$dens <- col2rgb(x)[1,] + 1L

## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                            "#FCFF00", "#FF9400", "#FF3100"))(256)
df$col <- cols[df$dens]

## Plot it, reordering rows so that densest points are plotted on top
plot(x2~x1, data=df[order(df$dens),], pch=20, col=col, cex=2)

# attemp
# prepare data
actual <- read_csv("scatter_actual.csv")
save(actual, file = "actual.RData")

# To load the data again
load("actual.RData")

df <- data.frame(actual)

x1 <- df$lnp
x2 <- df$Actual

## Use densCols() output to get density at each point
x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
df$dens <- col2rgb(x)[1,] + 1L

## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                            "#FCFF00", "#FF9400", "#FF3100"))(256)
df$col <- cols[df$dens]

## Plot it, reordering rows so that densest points are plotted on top
plot(x2~x1, data=df[order(df$dens),], pch=20, col=col, cex=2)

# 5.1 prepare data-----------------------------------------------------
actual <- read.csv("fig2.csv")
save(actual, file = "actual.RData")
load("actual.RData")

df <- data.frame(actual)

invs <- qnorm(df$s_gly)
lnp <- df$lnp
gt <- df$gt
till <- df$till
wcount <- df$wcount

# df$gtdum <- factor(df$gtdum)
# df$tilldum <- factor(df$tilldum)
# df$wcountmean <- factor(df$wcountmean)


# 5.2 plot individual panel--------------------------------------------------------

plnp <- ggplot(df, aes(lnp, invs))+
        labs(x="lnP",
             y=expression(paste({Phi}^{-1},"(s)", sep="")),size=6)+
        geom_point(size=0.05)+
        geom_smooth(method=lm,size=0.1)+
        theme_bw()+
        theme(panel.grid=element_blank())
pplnp <- ggMarginal(plnp,theme_bw(),xparams=list(binwidth=0.1, fill="#dc322f",alpha=0.3, size=0.1))
plot(pplnp)

pgt <- ggplot(df, aes(gt, invs))+
        labs(x="GT",
             y=expression(paste({Phi}^{-1},"(s)", sep="")),
             size=6)+
        geom_point(size=0.05)+
        geom_smooth(method=lm,size=0.1)+
        theme_bw()+
        theme(panel.grid=element_blank())
ppgt <- ggMarginal(pgt,theme_bw(),xparams=list(binwidth=0.1, fill="#2aa198",alpha=0.3, size=0.1))
plot(ppgt)

ptill <- ggplot(df, aes(till, invs))+
          labs(x="Till",
               y=expression(paste({Phi}^{-1},"(s)", sep="")),
               size=6)+
          geom_point(size=0.05)+
          geom_smooth(method=lm,size=0.1)+
          theme_bw()+
          theme(panel.grid=element_blank())
pptill <- ggMarginal(ptill,theme_bw(),xparams=list(binwidth=0.1, fill="#b58900",alpha=0.3, size=0.1))
plot(pptill)

pwcount <- ggplot(df, aes(wcount, invs))+
          labs(x="Resis",
               y=expression(paste({Phi}^{-1},"(s)", sep="")),
               size=6)+
          geom_point(size=0.05)+
          geom_smooth(method=lm,size=0.1)+
          theme_bw()+
          theme(panel.grid=element_blank())
ppwcount <- ggMarginal(pwcount,theme_bw(),xparams=list(binwidth=0.1, fill="#6c71c4",alpha=0.3, size=0.1))
plot(ppwcount)

# 5.3 combine plots--------------------------------------------------------------------
pb <- plot_grid(pplnp, ppwcount,ppgt, pptill, 
                align="v",
                labels=c("A","B","C","D"),
                label_size=12,label_colour="black",
                hjust=-0.5,vjust=1.2,
                nrow=2
)

print(pb)

# save
ggsave2("estim.pdf",plot=pb,width=7,height=4.5,units = "in")
ggsave2("estim.png",plot=pb,width=7,height=4.5,units = "in")

# 6. data plot for price, by year------------------------------------------------------
# create data for each panel (year)
df$year <- factor(df$year)
for (i in 2010:2016)
{
  print(i)
   temp <- subset(df,year==i,select=c(lnp,s_glyp,year))
   temp$invs <- qnorm(temp$s_glyp)
   assign(paste("p",i,sep=""),temp)
}





for (i in 2010:2016)
{p <- ggplot(get(paste("p",i,sep="")), aes(lnp, invs))+
  labs(x="lnP",
       y=expression(paste({Phi}^{-1},"(s)", sep="")),size=6)+
  geom_point(size=0.05)+
  geom_smooth(method=lm,size=0.1)+
  theme_bw()+
  theme(panel.grid=element_blank())
pp <- ggMarginal(p,theme_bw(),margins='x',type="boxplot", xparams=list(binwidth=0.1, fill="#dc322f",alpha=0.3, size=0.1))
plot(pp)
assign(paste("pl",i,sep=""),pp)
}

pall <- plot_grid(pl2010,pl2011,pl2012,pl2013,pl2014,pl2015,pl2016,
                align="hv",
                labels=c("2010","2011","2012","2013","2014","2015","2016"),
                label_size=12,label_colour="black",
                hjust=-0.5,vjust=1.2,
                nrow=4
)

print(pall)
ggsave2("lnP.pdf",plot=pall,width=7,height=7,units = "in")
ggsave2("lnP.png",plot=pall,width=7,height=7,units = "in")

# 7.Fig. S2 ---------------------------------------------------------------------
seed <- read.csv("figs2.csv")
save(seed, file = "seed.RData")
load("seed.RData")


ttsty3 <- element_text(face="plain", size=8, color="black") #the strip font is set separately
listy3 <- element_line(size=0.05,color="black")

pseed <- ggplot(seed, aes(x=year, y=value, fill=group)) + 
  geom_area()+
  labs(x="Year",y="Planted acres share (%)",fill="group")+
  scale_fill_manual(labels=c("GT-Bt-other HT","GT-other HT","GT-Bt","GT"),values=c("#B57638","#073642","#2aa198","#6c71c4","#dc322f"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid.major=element_blank(),
        axis.title=ttsty3,
        axis.line = listy3,axis.ticks = listy3,
        axis.text =ttsty3,
        legend.text = ttsty3,
        legend.title = element_blank(),
        legend.key.size=unit(0.1,"cm"),
        plot.margin = unit(c(15, 5, 0, 5), "pt"))+
  theme(legend.position="bottom")

# save
ggsave2("seed.pdf",plot=pseed,width=3.4,height=3,units = "in")
ggsave2("seed.png",plot=pseed,width=3.4,height=3,units = "in")

