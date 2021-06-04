library(ggplot2)
library(dplyr)
library(corrplot)
library(leaflet)
library(ggpubr)
library(plotly)
library(tidyr)
library(tidyverse)
library(maps)
library(mapproj)
library(stringr)

#Lettura datasets

happiness = read.csv("C:/Users/giaco/Desktop/progetto_esame/happines/2019.csv")

View(happiness)

alcoholConsumption = read.csv("C:/Users/giaco/Desktop/progetto_esame/happines/alccons/HappinessAlcoholConsumption.csv")

View(alcoholConsumption)

unemployment = read.csv("C:/Users/giaco/Desktop/progetto_esame/happines/unemployment-rate.csv")

View(unemployment)

#Modifica e creazione datasets

colnames(unemployment)[4] = "UnemploymentRate"

new_unemployment = unemployment %>%
  filter(Year == "2017") %>%
  select(Entity,UnemploymentRate)

View(new_unemployment)

happinessUnemp = left_join(happiness, new_unemployment, by = c("Country.or.region" = "Entity"))

View(happinessUnemp)

new_alcohol = alcoholConsumption %>%
  select(Country,Region,Beer_PerCapita,Wine_PerCapita,Spirit_PerCapita)

new_dataset = inner_join(happinessUnemp,new_alcohol,by=c("Country.or.region" = "Country"))
View(new_dataset)

new_dataset = new_dataset[c(1,2,11,3,4,5,6,7,8,9,10,12,13,14)]
View(new_dataset)

new_dataset = na.omit(new_dataset)
View(new_dataset)

##### Top 10 Paesi più felici #####

top10 = head(new_dataset,10) %>%
  select(Country.or.region,Region,Score)

top10

##### Mappa Top 10 Paesi più felici #####

worldtb = map_data("world") %>%
  as_tibble()

top10States = left_join(worldtb1,top10,by=c("region" = "Country.or.region"))
View(top10States)

top10States %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = top10States,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=Score), color="black") +
  scale_fill_gradient2(high="yellow") +
  theme_minimal()+
  labs(
    title = "Top 10 Stati più felici", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#E6AF2E"),
    legend.position = "right"
  ) 

#######################

n_regions = new_dataset %>% 
  group_by(Region) %>%
  summarise(count = n())

View(n_regions)

regions = as.vector(n_regions$Region)
regions

media_happ = vector(mode="character", length=9)
media_gdp = vector(mode="character", length=9)
media_supp = vector(mode="character", length=9)
media_corrup = vector(mode="character", length=9)
media_unemp = vector(mode="character", length=9)
media_wine = vector(mode="character", length=9)
media_beer = vector(mode="character", length=9)
media_spirit = vector(mode="character", length=9)

for(i in 1:9){
  
  gg = new_dataset %>%
    filter(Region == regions[i])
  
  media_happ[i] = mean(gg$Score)
  
  media_gdp[i] = mean(gg$GDP.per.capita)
  
  media_supp[i] = mean(gg$Social.support)
  
  media_corrup[i] = mean(gg$Perceptions.of.corruption)
  
  media_unemp[i] = mean(gg$UnemploymentRate)
  
  media_wine[i] = mean(gg$Wine_PerCapita)
  
  media_beer[i] = mean(gg$Beer_PerCapita)
  
  media_spirit[i] = mean(gg$Spirit_PerCapita)
}

media_happ
media_gdp
media_supp
media_corrup
media_unemp
media_wine
media_beer
media_spirit

media_happ = as.double(media_happ)
media_happ

media_happ = signif(media_happ,3)
media_happ

media_gdp = as.double(media_gdp)
media_gdp

media_gdp = signif(media_gdp,3)
media_gdp

media_supp = as.double(media_supp)
media_supp

media_supp = signif(media_supp,3)
media_supp

media_corrup = as.double(media_corrup)
media_corrup

media_corrup = signif(media_corrup,3)
media_corrup

media_unemp = as.double(media_unemp)
media_unemp

media_unemp = signif(media_unemp,3)
media_unemp

media_wine = as.double(media_wine)
media_wine

media_wine = signif(media_wine,3)
media_wine

media_beer = as.double(media_beer)
media_beer

media_beer = signif(media_beer,3)
media_beer

media_spirit = as.double(media_spirit)
media_spirit

media_spirit = signif(media_spirit)
media_spirit

new_data = data.frame(regions,media_happ,media_gdp,media_supp,media_corrup,media_unemp,media_wine,
                      media_beer,media_spirit)
View(new_data)

g1 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_happ), stat="identity", fill="#FFFB1F") +
  ggtitle("Average happiness by region") +
  xlab("Regions") +
  ylab("Average Happiness") +
  coord_flip()          

g2 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_gdp), stat="identity", fill="#4CA8BD") +
  ggtitle("Average GDP by region") +
  xlab("Regions") +
  ylab("Average GDP") +
  coord_flip() 

g3 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_supp), stat="identity", fill="#F97924") +
  ggtitle("Average Social Support by region") +
  xlab("Regions") +
  ylab("Average Social Support") +
  coord_flip() 

g4 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_corrup), stat="identity", fill="#5E277C") +
  ggtitle("Average Corruption by region") +
  xlab("Regions") +
  ylab("Average Corruption") +
  coord_flip() 

g5 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_unemp), stat="identity", fill="dimgray") +
  ggtitle("Average Unemployment by region") +
  xlab("Regions") +
  ylab("Average Unemployment") +
  coord_flip() 

g6 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_wine), stat="identity", fill="darkred") +
  ggtitle("Average Wine consumption by region") +
  xlab("Regions") +
  ylab("Average Wine") +
  coord_flip() 

g7 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_beer), stat="identity", fill="gold") +
  ggtitle("Average Beer consumption by region") +
  xlab("Regions") +
  ylab("Average Beer") +
  coord_flip() 

g8 = new_data %>%
  ggplot() + 
  geom_bar(aes(x=regions, y=media_spirit), stat="identity", fill="deepskyblue") +
  ggtitle("Average Spirit consumption by region") +
  xlab("Regions") +
  ylab("Average Spirit") +
  coord_flip() 


ggarrange(g1, g2, g3, g4,g5,g6,g7,g8,
          ncol = 2, nrow = 4)

########## correlazione tra dati medi di tutte le regioni #####################

x = matrix(c(new_data[,2],new_data[,3],new_data[,4],new_data[,5],new_data[,6]),nrow = 9,ncol=5)

dim(x) = c(9,5)
colnames(x) = c("Average Happiness","Average GDP","Average Support","Average Corruption","Average Unemployment")
x

M = cor(x)
M
pairs(x)

corrplot(M, is.corr = FALSE, method = "ellipse",bg="#A5AC20")

corrplot(M, is.corr = FALSE, method = "number",bg="#A5AC20")

########## correlazione tra dati medi per singola regione #####################

#Oceania and Asia

ANZ_tab = new_dataset %>%
  filter(Region == "Australia and New Zealand" | Region == "Eastern Asia" | Region == "Southeastern Asia")

View(ANZ_tab)

mean(ANZ_tab$GDP.per.capita)

mean(ANZ_tab$Social.support)

mat = matrix(c(ANZ_tab[,4],ANZ_tab[,5],ANZ_tab[,6],ANZ_tab[,10],ANZ_tab[,11],ANZ_tab[,12],ANZ_tab[,13],ANZ_tab[,14]),nrow=11,ncol=8)

dim(mat) = c(11,8)
colnames(mat) = c("Average Happiness","Average GDP","Average Support",
                  "Average Corruption","Average Unemployment","Average Beer",
                  "Average Wine","Average Spirit")
mat

M1 = cor(mat)
pairs(mat)

corrplot(M1, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M1, is.corr = FALSE, method = "number",bg="#72c475")

#Western Europe (correlazione maggiore felicità -> Average Corruption diretta)
 
EU_tab = new_dataset %>%
  filter(Region == "Western Europe")

mean(EU_tab$GDP.per.capita)

mean(EU_tab$Social.support)

View(EU_tab)

mat = matrix(c(EU_tab[,4],EU_tab[,5],EU_tab[,6],EU_tab[,10],EU_tab[,11],EU_tab[,12],EU_tab[,13],EU_tab[,14]),nrow=20,ncol=8)

dim(mat) = c(20,8)
colnames(mat) = c("Average Happiness","Average GDP","Average Support",
                  "Average Corruption","Average Unemployment","Average Beer",
                  "Average Wine","Average Spirit")
mat

M2 = cor(mat)
pairs(mat)

corrplot(M2, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M2, is.corr = FALSE, method = "number",bg="#72c475")

#Central and Eastern Europe (correlazione maggiore felicità -> Average Support)

CEE_tab = new_dataset %>%
  filter(Region == "Central and Eastern Europe")

mean(CEE_tab$GDP.per.capita)

mean(CEE_tab$Social.support)

View(CEE_tab)

mat = matrix(c(CEE_tab[,4],CEE_tab[,5],CEE_tab[,6],CEE_tab[,10],CEE_tab[,11],CEE_tab[,12],CEE_tab[,13],CEE_tab[,14]),nrow=24,ncol=8)

dim(mat) = c(24,8)
colnames(mat) = c("Average Happiness","Average GDP","Average Support",
                  "Average Corruption","Average Unemployment","Average Beer",
                  "Average Wine","Average Spirit")
mat

M3 = cor(mat)
pairs(mat)

corrplot(M3, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M3, is.corr = FALSE, method = "number",bg="#72c475")

#Sub-Saharan Africa (correlazione maggiore felicità -> Average Corruption inversa)

SSA_tab = new_dataset %>%
  filter(Region == "Sub-Saharan Africa")

mean(SSA_tab$GDP.per.capita)

mean(SSA_tab$Social.support)

View(SSA_tab)

mat = matrix(c(SSA_tab[,4],SSA_tab[,5],SSA_tab[,6],SSA_tab[,10],SSA_tab[,11],SSA_tab[,12],SSA_tab[,13],SSA_tab[,14]),nrow=24,ncol=8)

dim(mat) = c(24,8)
colnames(mat) = c("Average Happiness","Average GDP","Average Support",
                  "Average Corruption","Average Unemployment","Average Beer",
                  "Average Wine","Average Spirit")
mat

M4 = cor(mat)
pairs(mat)

corrplot(M4, is.corr = FALSE, method = "ellipse",bg="#72c475")

corrplot(M4, is.corr = FALSE, method = "number",bg="white")

#Middle East and Northern Africa (correlazione maggiore felicità -> Average Unemployment)

MAF_tab = new_dataset %>%
  filter(Region == "Middle East and Northern Africa")

View(MAF_tab)

mean(MAF_tab$GDP.per.capita)

mean(MAF_tab$Social.support)

matMAF = matrix(c(MAF_tab[,4],MAF_tab[,5],MAF_tab[,6],MAF_tab[,10],MAF_tab[,11],MAF_tab[,12],MAF_tab[,13],MAF_tab[,14]),nrow=10,ncol=8)

dim(matMAF) = c(10,8)
colnames(matMAF) = c("Average Happiness","Average GDP","Average Support","Average Corruption","Average Unemployment","Average Beer","Average Wine","Average Spirit")
matMAF

View(matMAF)
M5 = cor(matMAF)
pairs(matMAF)

corrplot(M5, is.corr = FALSE, method = "ellipse",bg="#b6f0b8")

corrplot(M5, is.corr = FALSE, method = "number",bg="#0ac712")

#Americas (correlazione maggiore felicità -> Average GDP)

LAM_tab = new_dataset %>%
  filter(Region == "Latin America and Caribbean" | Region == "North America")

View(LAM_tab)

mean(LAM_tab$GDP.per.capita)
mean(LAM_tab$Social.support)

matLAM = matrix(c(LAM_tab[,4],LAM_tab[,5],LAM_tab[,6],LAM_tab[,10],LAM_tab[,11],LAM_tab[,12],LAM_tab[,13],LAM_tab[,14]),nrow=22,ncol=8)

dim(matLAM) = c(22,8)
colnames(matLAM) = c("Average Happiness","Average GDP","Average Support","Average Corruption","Average Unemployment","Average Beer","Average Wine","Average Spirit")
matLAM

M6 = cor(matLAM)
pairs(matLAM)

corrplot(M6, is.corr = FALSE, method = "ellipse",bg="#A5AC20")

corrplot(M6, is.corr = FALSE, method = "number",bg="#A5AC20")

############################

region = c("Oceania and Asia","Western Europe","Central and Eastern Europe","Sub-Saharan Africa","Middle East and Northern Africa","Americas")

Correlation.gdp = c(0.76,0.8,0.53,0.43,0.8,0.63)

gdp.percapita = c(1.093,1.3621,1.0334,0.5103,1.1467,0.9535)

Correlation.socialSupport = c(0.72,0.68,0.62,0.34,0.68,0.65)

socialSupport = c(1.366545,1.4839,1.3443,0.96513,1.2076,1.34632)


ggdata = data.frame(region,gdp.percapita,Correlation.gdp,socialSupport,Correlation.socialSupport)
View(ggdata)

p = ggplot(data=ggdata,mapping = aes(x = gdp.percapita, y = socialSupport,color=region,size=gdp.percapita)) +
  geom_point()

ggplotly(p)

##### Mappa Stati per felicità #####

worldtb = map_data("world") %>%
  as_tibble()

midpoint = mean(happines$Score)

class(happiness$Country.or.region)

happiness$Country.or.region[happiness$Country.or.region == "United States"] <- "USA"

happiness$Country.or.region[happiness$Country.or.region == "United Kingdom"] <- "UK"

happiness$Country.or.region[happiness$Country.or.region == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"

happiness$Country.or.region[happiness$Country.or.region == "Congo (Brazzaville)"] <- "Republic of Congo"

happiness$Country.or.region[happiness$Country.or.region == "North Macedonia"] <- "Macedonia"

View(happiness)

happinessMap = left_join(worldtb,happiness,by=c("region" = "Country.or.region"))
View(happinessMap)

happinessMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = happinessMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=Score), color="black") +
  scale_fill_gradient2(low="#9C444C",high="#FFFB1F",midpoint = 5.4) +
  theme_minimal()+
  labs(
    title = "Top 10 Stati più felici", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#E6AF2E"),
    legend.position = "right"
  ) 

##### Mappa Stati per GDP pro Capite #####

midpoint = mean(happines$GDP.per.capita)

happinessMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = happinessMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=GDP.per.capita), color="black") +
  scale_fill_gradient2(low="white",mid="#3F74CA",high="black",midpoint = 0.8) +
  theme_minimal()+
  labs(
    title = "Stati per GDP Pro Capita", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#3F74CA"),
    legend.position = "right"
  ) 

##### Mappa Stati per Social Support #####

midpoint2 = mean(happines$Social.support)

happinessMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = happinessMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=Social.support), color="black") +
  scale_fill_gradient2(low="black",mid="#F96D10",high="khaki1",midpoint = 0.8) +
  theme_minimal()+
  labs(
    title = "Stati per Supporto Sociale", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#F96D10"),
    legend.position = "right"
  ) 

##### Mappa Stati per Average Corruptipon #####

midpoint2 = mean(happines$Perceptions.of.corruption)

happinessMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = happinessMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=Perceptions.of.corruption), color="black") +
  scale_fill_gradient2(low="black",mid="#F96D10",high="white",midpoint = 0.23) +
  theme_minimal()+
  labs(
    title = "Stati per Percezionde della Corruzione", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="#F96D10"),
    legend.position = "right"
  ) 

