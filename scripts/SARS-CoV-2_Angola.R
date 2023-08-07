library(ggplot2)
library("readxl")
library(ggpubr)
library(readr)
library(BiocManager)
library(ggtree)
library(tidyverse)
library(tidytree)
library(ape)
library(treeio)
library("dplyr")


######### Epi curve

data1<-read_excel('data/owid.xlsx')

data2<-read_excel('data/GISAID.xlsx') 

##also read in Re data
Re <- read_csv("https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/AGO-estimates.csv")

data1$days<-as.Date(cut(data1$date,
                                  breaks = "day",
                                  start.on.monday = FALSE))

data1$date<-as.Date(cut(data1$date,
                                  breaks = "week",
                                  start.on.monday = FALSE))


data2$days<-as.Date(cut(data2$date,
                        breaks = "day",
                        start.on.monday = FALSE))

data2$date<-as.Date(cut(data2$date,
                        breaks = "2 week",
                        start.on.monday = FALSE))

data2$date1<-as.Date(cut(data2$date,
                        breaks = "1 month",
                        start.on.monday = FALSE))

Re$date<-as.Date(cut(Re$date,
                     breaks = "day",
                     start.on.monday = FALSE))

Re=subset(subset(Re,data_type=="Deaths"),estimate_type=="Cori_slidingWindow")


dateVec <- seq(from = as.Date("2020-03-15"), to = as.Date("2022-03-15"), by = "days")

pEpi_Ang<-ggplot()+
  theme_classic()+
  geom_bar(data=data1, aes(x=days, y=new_cases_smoothed,fill='Cases'),width=1,stat='identity',color='#6699CC')+
  geom_rug(data= data2, aes(x=days, color='Genomes'),alpha=0.2,outside =FALSE,length = unit(0.03, "npc"),show.legend=TRUE) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=13))+
  theme(axis.title.y = element_text(color="black", size=13, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=13))+
  scale_x_date(limits = c(min(dateVec), max=max(dateVec)), date_labels = "%b\n%Y",date_breaks = "2 month")+
  geom_line(data=data1, aes(x=days, y=total_deaths, color='Deaths'), linewidth=1.4)+
  geom_line(data = Re, aes(x = date, y = median_R_mean*500, color = "Re"), size=1.4) +
  scale_color_manual(values=c('lightsalmon','grey40','#990066'), name='')+
  scale_fill_manual(values=c('#6699CC'), name='')+
  geom_hline(yintercept=500, color='black', linetype=2) +
  #geom_ribbon(data = Re,aes(x=date, ymin=median_R_lowHPD*400, ymax=median_R_highHPD*400), fill='purple4', alpha=0.2) +
  xlab(' ')+
  ylab(' ')+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Daily Cases / Cumulative Deaths",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./500, name="Re")
    
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="top",legend.text= element_text(size=13))


pEpi_Ang

####Lineage plot

data2$pango2<-factor(data2$pango2,levels = c("C.16","Beta","Eta","Other","Alpha", "B.1",  "A","B.1.1.275","Gamma", "Theta", "Delta","Omicron (BA.1)"))

Pango<-ggplot()+
  geom_bar(data=data2, aes(x = date1,fill=pango2), width=20,color='black', linewidth=0.2)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 months")+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=11))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=11))+
  scale_fill_manual(values = c('#009999','#990000', '#5c6529', 'grey', '#003366','#EDD29D', '#663300', '#CC9966','#ef7206' , '#99CC99', '#663399', '#FFCC00'), name='Lineages', labels=c("C.16","Beta (B.1.351)","Eta (B.1.525)","Other","Alpha (B.1.1.7)", "B.1 and sub-lineages",  "A lineages","B.1.1.275","Gamma (P.1)", "Theta (P.3)", "Delta (B.1.617.2/AY.x)","Omicron (BA.1)"))+
  #geom_line(data=df, aes(x=date1, y=Travel_sum*5), color='black', size = 1.2)+
scale_y_continuous(
  
  # Features of the first axis
  name = "Genome Count",
  
  # Add a second axis and specify its features
  sec.axis = sec_axis(~./5, name="Travelers")
  
)+
  theme(legend.text = element_text(size=11))+
  theme(legend.title = element_text(size=11,  face="bold"))+
  theme(legend.position = "top")+
  theme(legend.key.size = unit(0.4, "cm"))+
  ylab('Genome Count')


Pango


Pango_prev <-ggplot(data=data2, aes(x = date1,fill=pango2))+
  geom_bar(position='fill',width=20,color='black', size=0.2)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 months")+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=11))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=11))+
  scale_fill_manual(values = c('#009999','#990000', '#5c6529', 'grey', '#003366','#EDD29D', '#663300', '#CC9966','#ef7206' , '#99CC99', '#663399', '#FFCC00'), name='Lineages', labels=c("C.16","Beta (B.1.351)","Eta (B.1.525)","Other","Alpha (B.1.1.7)", "B.1 and sub-lineages",  "A lineages","B.1.1.275","Gamma (P.1)", "Theta (P.3)", "Delta (B.1.617.2/AY.x)","Omicron (BA.1)"))+
  theme(legend.text = element_text(size=11))+
  theme(legend.title = element_text(size=11,  face="bold"))+
  theme(legend.position = "top")+
  #theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.4, "cm"))+
  ylab('Genomic prevalence')


Pango_prev 


####Tree plot

tree1<-read.newick('data/new_tree_outliers_dropped.nwk')

metadata <- read_excel('data/metadata.xlsx')

ggtree(tree1) + geom_text(aes(label=node), size = 1, hjust=-.3) 
ggsave("nodes.pdf", width = 90, height = 150, units= "cm", limitsize = FALSE )

tree2 <- groupClade(tree1,.node=c(19683, 20157, 21497, 23496, 25002, 25338,25793, 27952, 30875, 32039, 33698))

p<-ggtree(tree2, aes(color=group),size=0.35) + 
  scale_colour_manual(values=c("#EDD29D",'#663300','#CC9966', '#CC9966','#003366','#EDD29D','#5c6529',"#009999","#FFCC00", "grey",'#990000','#663399'))

p

p1 <- p %<+% metadata + 
  geom_tippoint(aes(subset=(grepl('Angola',label,fixed=TRUE)==TRUE), fill=group),size=4, align=F, color='black',shape=21)+
  scale_fill_manual(values=c("#EDD29D",'#663300','#CC9966', '#003366','#EDD29D','#5c6529','#009999',"#FFCC00",'#990000','#663399'))+
  theme(legend.position = 'none')+
  ggtree::geom_cladelab(node=19683, label="A", align=FALSE,barsize = 1, barcolour='#663300',textcolour = '#663300', angle =90, fontface=2, hjust = 0.5, vjust = 1.2, offset = 0.0001)+
  geom_cladelab(node=20157, label="B.1 lineages", align=FALSE,barsize = 1, barcolour='#EDD29D',textcolour = '#EDD29D', angle =90, fontface=2,offset = 0.002, hjust = -0.5)+
  geom_cladelab(node=21497, label="B.1.1.275", align=FALSE,barsize = 1, barcolour='white',textcolour = '#CC9966', angle =90, fontface=2, hjust = 0.2, vjust = 3, offset = 0.0005)+
  geom_cladelab(node=23496, label="Alpha", align=FALSE,barsize = 1, barcolour='#003366',textcolour = '#003366', angle =90, fontface=2, hjust = 0.5,vjust = 1, offset = 0.0003, offset.text = 0.00002)+
  geom_cladelab(node=25338, label="Eta", align=FALSE,barsize = 0, barcolour='white',textcolour = '#5c6529', angle =90, offset.text= 0, fontface=2, hjust = 0.65, vjust = 3)+
  geom_cladelab(node=25793, label="C.16", align=FALSE,barsize = 1, barcolour='#009999',textcolour = '#009999', angle =90, fontface=2, hjust = 0.5, vjust = 1, offset = 0.0002,  offset.text = 0.00002)+
  geom_cladelab(node=28283, label="BA.1", align=FALSE,barsize = 1, barcolour='#FFCC00',textcolour = '#FFCC00', angle =90, offset.text= 0, fontface=2, hjust = 1.5, vjust = 1)+
  geom_cladelab(node=30875, label="Other Omicron", align=FALSE,barsize = 1, barcolour='grey',textcolour = 'grey', angle =90, offset = 0, fontface=2, vjust = 1, hjust = 0.5)+
  geom_cladelab(node=32286, label="Beta", align=FALSE,barsize = 1, barcolour='#990000',textcolour = '#990000', angle =90, offset.text= 0, fontface=2, hjust = 0.5, vjust = 1)+
  geom_cladelab(node=33698, label="Delta", align=FALSE,barsize = 1, barcolour='#663399',textcolour = '#663399', angle =90, fontface=2, vjust = 1, hjust = 0.5)


p1



#### Viral Import / export plots

library(ggplot2)
library("readxl")
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library('gridExtra')
library('data.table')
library('scales')
library('lubridate')
library(ggalluvial)
library(sp)
library(rworldmap)
library(countrycode)
library("circlize")

##Read in replicates and merge

replicate1<-read.table(file='replicates/angola_none_omicron_replicates/tree1_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate1$replicate <- "1"
replicate2<-read.table(file='replicates/angola_none_omicron_replicates/tree2_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate2$replicate <- "2"
replicate3<-read.table(file='replicates/angola_none_omicron_replicates/tree3_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate3$replicate <- "3"
replicate4<-read.table(file='replicates/angola_none_omicron_replicates/tree4_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate4$replicate <- "4"
replicate5<-read.table(file='replicates/angola_none_omicron_replicates/tree5_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate5$replicate <- "5"
replicate6<-read.table(file='replicates/angola_none_omicron_replicates/tree6_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate6$replicate <- "6"
replicate7<-read.table(file='replicates/angola_none_omicron_replicates/tree7_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate7$replicate <- "7"
replicate8<-read.table(file='replicates/angola_none_omicron_replicates/tree8_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate8$replicate <- "8"
replicate9<-read.table(file='replicates/angola_none_omicron_replicates/tree9_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate9$replicate <- "9"
replicate10<-read.table(file='replicates/angola_none_omicron_replicates/tree10_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate10$replicate <- "10"

replicate11<-read.table(file='replicates/angola_omicron_replicates/tree1_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate11$replicate <- "11"
replicate12<-read.table(file='replicates/angola_omicron_replicates/tree2_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate12$replicate <- "12"
replicate13<-read.table(file='replicates/angola_omicron_replicates/tree3_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate13$replicate <- "13"
replicate14<-read.table(file='replicates/angola_omicron_replicates/tree4_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate14$replicate <- "14"
replicate15<-read.table(file='replicates/angola_omicron_replicates/tree5_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate15$replicate <- "15"
replicate16<-read.table(file='replicates/angola_omicron_replicates/tree6_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate16$replicate <- "16"
replicate17<-read.table(file='replicates/angola_omicron_replicates/tree7_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate17$replicate <- "17"
replicate18<-read.table(file='replicates/angola_omicron_replicates/tree8_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate18$replicate <- "18"
replicate19<-read.table(file='replicates/angola_omicron_replicates/tree9_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate19$replicate <- "19"
replicate20<-read.table(file='replicates/angola_omicron_replicates/tree10_annottated_tree_events.csv', sep = '\t', header = TRUE)
replicate20$replicate <- "20"



all_replicates<-rbind(replicate1,replicate2,replicate3,replicate4,
                      replicate5,replicate6,replicate7,replicate8,
                      replicate9,replicate10,replicate11,replicate12,replicate13,replicate14,
                      replicate15,replicate16,replicate17,replicate18,
                      replicate19,replicate20)

##Change date format from decimal and cut into monthly intervals

all_replicates$date<-date_decimal(all_replicates$EventTime)
all_replicates$month<-as.Date(cut(all_replicates$date,breaks = "1 month",start.on.monday = FALSE))
all_replicates$biweek<-as.Date(cut(all_replicates$date,breaks = "2 weeks",start.on.monday = FALSE))
all_replicates$date<-as.Date(cut(all_replicates$date,breaks = "day",start.on.monday = FALSE))

all_replicates[all_replicates == "Cote dIvoire"] <- "Ivory Coast"

##Get world data to create columns of continent and region data from country names

world_data<-getMap(resolution='low')@data

country2continent = function(x)
{  
  country_data = subset(world_data, NAME==x)
  
  return (as.character(country_data[['REGION']]))   # returns the continent (7 continent model)
}

country2continent_region = function(x)
{  
  country_data = subset(world_data, NAME==x)
  
  return (as.character(country_data[['IMAGE24']]))  
}

all_replicates$Continent_origin<-lapply(all_replicates$Origin,country2continent)
all_replicates$Continent_dest<-lapply(all_replicates$Destination,country2continent)

all_replicates$Region_origin<-lapply(all_replicates$Origin,country2continent_region)
all_replicates$Region_dest<-lapply(all_replicates$Destination,country2continent_region)


###Getting monthly mean and sd of imports/exports for Angola per continent

Angola_in<-subset(subset(all_replicates,Destination=='Angola'))
Angola_out<-subset(subset(all_replicates,Origin=='Angola'))

Angola_in_table <- Angola_in %>% count(month,biweek,Continent_origin, replicate)
Angola_in_table_summarize <- Angola_in_table %>% group_by(month,biweek,Continent_origin)  %>% 
  summarise(mean = mean(n), sd = sd(n))

Angola_out_table <- Angola_out %>% count(month,biweek,Continent_dest, replicate)
Angola_out_table_summarize <- Angola_out_table %>% group_by(month,biweek,Continent_dest)  %>% 
  summarise(mean = mean(n), sd = sd(n))

Angola_in_table_summarize$Continent_origin<-factor(Angola_in_table_summarize$Continent_origin,levels = c("Africa","Asia","Europe","North America","South America"))



Imports<-ggplot(data=Angola_in_table_summarize, aes(x = month,y = mean, fill=Continent_origin))+
  geom_bar(width=20,color='black', size=0.2, position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  #xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 months")+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=11))+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=11))+
  #scale_fill_manual(values = c('#009999','#990000', '#5c6529', 'grey', '#003366','#EDD29D', '#663300', '#CC9966','#ef7206' , '#99CC99', '#663399', '#FFCC00'), name='Lineages', labels=c("C.16","Beta (B.1.351)","Eta (B.1.525)","Other","Alpha (B.1.1.7)", "B.1 and sub-lineages",  "A lineages","B.1.1.275","Gamma (P.1)", "Theta (P.3)", "Delta (B.1.617.2/AY.x)","Omicron (BA.1)"))+
  theme(legend.text = element_text(size=11))+
  theme(legend.title = element_text(size=11,  face="bold"))+
  theme(legend.position = "top")+
  
  theme(legend.key.size = unit(0.4, "cm"))+
  ylab('Mean number of introductions')


Imports

###Region

Angola_in<-subset(subset(all_replicates,Destination=='Angola'))
library(dplyr)
fwrite(Angola_in, file ='Angola_in.csv')
fwrite(Angola_out, file ='Angola_out.csv')

Angola_in_table2 <- Angola_in_fixed %>% count(month,Region_origin, replicate)
Angola_in_table_summarize2 <- Angola_in_table2 %>% group_by(month,Region_origin)  %>% 
  summarise(mean = mean(n), sd = sd(n))

library(tidyr)
df <- Angola_in_table_summarize2 %>% drop_na

Angola_out<-subset(subset(all_replicates,Origin=='Angola'))
Angola_out_table2 <- Angola_out_fixed %>% count(month,Region_dest, replicate)
Angola_out_table_summarize2 <- Angola_out_table2 %>% group_by(month,Region_dest)  %>% 
  summarise(mean = mean(n), sd = sd(n))

Angola_in_table_summarize2$month<-as.Date(cut(Angola_in_table_summarize2$month,breaks = "1 month",start.on.monday = FALSE))
Angola_in_table_summarize2$Region_origin<-factor(Angola_in_table_summarize2$Region_origin,levels = c("Southern Africa","Western Africa","Central Africa", "India+","Oceania","Western Europe","Central Europe", 'Brazil', 'Middle East', "USA"))
regional_cols<-c("Southern Africa" = 'white',"Western Africa" = 'yellow',"India+" = 'green',"Oceania" = 'blue',"Western Europe" = 'purple', "Brazil" = 'orange', "Middle East" = 'grey')

Imports<-ggplot(Angola_in_table_summarize2, aes(x = month,y = mean, fill=Region_origin))+
  geom_bar(width=20,color='black', linewidth=0.2, position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  scale_x_date(date_labels = "%b\n%y",date_breaks = "2 months")+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=9))+
  theme(axis.title.y = element_text(color="black", size=10, face="bold"))+
  #theme(axis.text.y = element_text(color="black", size=11))+
  scale_fill_manual(values=regional_cols, name='Regions')+
  theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=10,  face="bold"))+
  theme(legend.position = "top")+
  theme(axis.title.x = element_blank())+
  ylab('Mean monthly introductions')+
  ylim(0,25)


Imports

Angola_out_table_summarize2$Region_dest<-factor(Angola_out_table_summarize2$Region_dest,levels = c("Southern Africa","Western Africa","Eastern Africa",'Central Africa','Northern Africa','Middle East',"Western Europe","Central Europe","India+","Southeastern Asia",'Japan',"Indonesia+","Oceania",'Brazil','Rest South America',"Rest Central America", 'Canada', 'USA'))

regional_cols<-c("Southern Africa" = "#31493c","Western Africa"= "#7a9e7e","Eastern Africa" = "#b3efb2",'Central Africa'= "#f1fffa",'Northern Africa'= "#73683b",'Middle East'="#3e2314","Western Europe"="#a88671","Central Europe"="#ece2d0","India+"= "#ee6c4d","Southeastern Asia"="#ffb4a2",'Japan' = "#e5989b","Indonesia+"= "#b5838d","Oceania"="#6d6875",'Brazil'="#054a91",'Rest South America'="#3e7cb1","Rest Central America"="#81a4cd", 'Canada'="#90e0ef", 'USA' = "#1b4965")
Angola_out_table_summarize2$month<-as.Date(cut(Angola_out_table_summarize2$month,breaks = "1 month",start.on.monday = FALSE))


Export<-ggplot(Angola_out_table_summarize2, aes(x = month,y = mean, fill=Region_dest))+
  geom_bar(width=20,color='black', linewidth=0.2, position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  scale_x_date(date_labels = "%b\n%y",date_breaks = "2 months")+
  theme(axis.title.x = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=9))+
  theme(axis.title.y = element_text(color="black", size=10, face="bold"))+
  #theme(axis.text.y = element_text(color="black", size=11))+
  scale_fill_manual(values=regional_cols, name='Regions')+
  theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=10,  face="bold"))+
  theme(legend.position = "top")+
  theme(axis.title.x = element_blank())+
  ylab('Mean monthly exportations')+
  ylim(0,25)

Export

p1 <- ggarrange(Export,Imports, ncol = 2, nrow = 1, common.legend = TRUE, align = "h")
p1


###Total 
Line_in <-subset(subset(all_replicates,Destination=='Angola'))
Line_in2 <- Line_in %>% count(month, replicate)
Line_in3 <- Line_in2 %>% group_by(month)  %>% 
  summarise(mean = mean(n), sd = sd(n))

Line_out <-subset(subset(all_replicates,Origin=='Angola'))
Line_out2 <- Line_out %>% count(month,replicate)
Line_out3 <- Line_out2 %>% group_by(month)  %>% 
  summarise(mean = mean(n), sd = sd(n))


###SA
SA_out <-subset(subset(all_replicates,Destination=='Angola' & Origin=="South Africa"))
SA_out2 <- SA_out %>% count(month, replicate)
SA_out3 <- SA_out2 %>% group_by(month)  %>% 
  summarise(mean = mean(n), sd = sd(n))

SA_in <-subset(subset(all_replicates,Origin=='Angola' & Destination=="South Africa"))
SA_in2 <- SA_in %>% count(month,replicate)
SA_in3 <- SA_in2 %>% group_by(month)  %>% 
  summarise(mean = mean(n), sd = sd(n))

###Portugal
Port_out <-subset(subset(all_replicates,Destination=='Angola' & Origin=="Portugal"))
Port_out2 <- Port_out %>% count(month, replicate)
Port_out3 <- Port_out2 %>% group_by(month)  %>% 
  summarise(mean = mean(n), sd = sd(n))

Port_in <-subset(subset(all_replicates,Origin=='Angola' & Destination=="Portugal"))
Port_in2 <- Port_in %>% count(month,replicate)
Port_in3 <- Port_in2 %>% group_by(month)  %>% 
  summarise(mean = mean(n), sd = sd(n))

###Namibia
Nam_out <-subset(subset(all_replicates,Destination=='Angola' & Origin=="Namibia"))
Nam_out2 <- Nam_out %>% count(month, replicate)
Nam_out3 <- Nam_out2 %>% group_by(month)  %>% 
  summarise(mean = mean(n), sd = sd(n))

Nam_in <-subset(subset(all_replicates,Origin=='Angola' & Destination=="Namibia"))
Nam_in2 <- Nam_in %>% count(month,replicate)
Nam_in3 <- Nam_in2 %>% group_by(month)  %>% 
  summarise(mean = mean(n), sd = sd(n))


Line_exports<-ggplot()  + theme_classic()+
  geom_ribbon(data=Line_out3,aes(x=month, y=mean, ymin=mean-sd,ymax=mean+sd, fill='Total'), alpha=0.6)+
  geom_line(data=Line_out3,aes(x=month, y=mean, color = 'Total'), size=1)+
  geom_ribbon(data=SA_in3,aes(x=month, y=mean,ymin=mean-sd,ymax=mean+sd, fill='South Africa'), alpha=0.6)+
  geom_line(data=SA_in3,aes(x=month, y=mean, color = 'South Africa'), size=1)+
  geom_ribbon(data=Port_in3,aes(x=month, y=mean,ymin=mean-sd,ymax=mean+sd, fill='Portugal'), alpha=0.6)+
  geom_line(data=Port_in3,aes(x=month, y=mean, color = 'Portugal'), size=1)+
  geom_ribbon(data=Nam_in3,aes(x=month, y=mean,ymin=mean-sd,ymax=mean+sd, fill='Namibia'), alpha=0.6)+
  geom_line(data=Nam_in3,aes(x=month, y=mean, color = 'Namibia'), size=1)+
  scale_colour_manual(values=c('#85182a', "#63542e", '#597d7c', 'grey'),name='')+
  scale_fill_manual(values=c('#85182a', "#63542e", '#597d7c', 'grey'),name='')+
  theme(axis.text.x = element_text(color="black", size=16))+
  scale_x_date(date_labels = "%b\n%y",date_breaks = "2 months")+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=9))+
  theme(axis.title.y = element_text(color="black", size=10, face="bold"))+
  theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=10,  face="bold"))+
  theme(legend.position = "bottom")+
  theme(axis.title.x = element_blank())+
  ylab('Mean monthly viral exchanges')+
  ylim(0,25)+
  ggtitle("Exports")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


Line_exports

Line_imports<-ggplot()  + theme_classic()+
  geom_ribbon(data=Line_in3,aes(x=month, y=mean,ymin=mean-sd,ymax=mean+sd, fill='Total'), alpha=0.6)+
  geom_line(data=Line_in3,aes(x=month, y=mean, color = 'Total'), size=1)+
  geom_ribbon(data=SA_out3,aes(x=month, y=mean,ymin=mean-sd,ymax=mean+sd, fill='South Africa'), alpha=0.6)+
  geom_line(data=SA_out3,aes(x=month, y=mean, color = 'South Africa'), size=1)+
  geom_ribbon(data=Port_out3,aes(x=month, y=mean,ymin=mean-sd,ymax=mean+sd, fill='Portugal'), alpha=0.6)+
  geom_line(data=Port_out3,aes(x=month, y=mean, color = 'Portugal'), size=1)+
  geom_ribbon(data=Nam_out3,aes(x=month, y=mean,ymin=mean-sd,ymax=mean+sd, fill='Namibia'), alpha=0.6)+
  geom_line(data=Nam_out3,aes(x=month, y=mean, color = 'Namibia'), size=1)+
  scale_colour_manual(values=c("#85182a", "#63542e", '#597d7c', 'grey'),name='')+
  scale_fill_manual(values=c('#85182a', "#63542e", '#597d7c', 'grey'),name='')+
  theme(axis.text.x = element_text(color="black", size=16))+
  scale_x_date(date_labels = "%b\n%y",date_breaks = "2 months")+
  theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=9))+
  theme(axis.title.y = element_text(color="black", size=10, face="bold"))+
  theme(legend.text = element_text(size=9))+
  theme(legend.title = element_text(size=10,  face="bold"))+
  theme(legend.position = "bottom")+
  theme(axis.title.x = element_blank())+
  ylab('Mean monthly viral exchanges')+
  ylim(0,25)+
  ggtitle("Imports")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

Line_imports


p2 <- ggarrange(Line_imports, Line_exports, ncol = 2, nrow = 1, common.legend = TRUE, align = "h")
p2


###Chord plot

Angola <-subset(subset(all_replicates,Destination=='Angola' | Origin=='Angola'))
Angola_table <- Angola %>% count(Origin,Destination, replicate)
Angola_table_summarize <- Angola_table %>% group_by(Origin,Destination,)  %>% 
  summarise(mean = mean(n))
Ang_in <- subset(subset(Angola_table_summarize,Destination=='Angola'))
Ang_out <- subset(subset(Angola_table_summarize,Origin=='Angola'))

Smaller_angola <-subset(subset(Angola_table_summarize, mean >1))

chordDiagram(Ang_in)
chordDiagram(Ang_out)

chordDiagram(Smaller_angola)

grid.col <- c("Barbados" = '#006466',"Benin"= '#0b525b', "Bulgaria"= '#1b3a4b', "Canada"='#272640',
"Democratic Republic of the Congo"='#3e1f47', "France"='#4d194d', "Gabon" ='#774936',
"Guinea"='#653a2a',"Japan" ='#532c1e', "Mali" = '#2f0e07', "Myanmar" ='#023618',
"Namibia" ='#1e3f1f',"Nigeria"='#474c28', "Portugal"='#63542e',"Republic of the Congo"='#404556',
"Sao Tome and Principe"='#60515c',"Senegal"  ='#777076',"South Africa"='#597d7c',
"South Sudan"='#e01e37',"United Kingdom" ='#c71f37', "USA"='#b21e35',"Zambia"='#85182a',
"Zimbabwe" = '#641220', "Angola"='grey', "Australia" ='#fec89a',"Botswana"='#f9e5d8',
"Brazil" ='#e8a598', "Burkina Faso" ='#fff2b2')

Smaller_angola[Smaller_angola == "Democratic Republic of the Congo"] <- "DRC"
Smaller_angola[Smaller_angola == "Republic of the Congo"] <- "Congo"


chordDiagram(Smaller_angola, annotationTrack = "grid", 
             preAllocateTracks = 1, 
             grid.col = grid.col,
             directional = 1, 
             direction.type = c("diffHeight", "arrows"), 
             link.arr.type = "big.arrow")
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(CELL_META$xcenter, 
              ylim[1] + cm_h(2), 
              sector.name, 
              facing = "clockwise",
              niceFacing = TRUE, 
              adj = c(0, 0.5),
              cex = 1.1,
              #col=grid.col[sector.name],
              font = 1)
  circos.axis(h = "bottom",
              labels.cex = .6,
              sector.index = sector.name
  )
}, bg.border = NA)

