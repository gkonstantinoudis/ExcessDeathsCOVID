# Code for producing plots/tables 
# given the poisson_samples_all data

library(pbapply)
library(tidyverse)
library(readxl)
library(ggtext)
library(viridis)
library(rgdal)
library(patchwork)

cluster = F


#### Set the WD and load all the necessary data/functions ####
if(cluster){
  setwd("/home/michela/2021_COVID-DM_TheReturn/") 
  source("newfunctions.R")
} else {
  setwd("~/Dropbox/Lavori condivisi/2021_OneYearOfCovid") 
  source("./RCode/newfunctions.R")
}


path2save ="./Output/"

link_table = readRDS("./Output/link_table") #province level
link_table_reg = link_table %>% distinct(COD_REG,.keep_all=T) %>% select(COD_REG,DEN_REG)
link_table_reg$COD_REG = as.numeric(link_table_reg$COD_REG)

pois.samples.list = readRDS("./Output/poisson_samples_all") #samples from the inla model (see Step 5. of model.run.R)
finaldb = readRDS("./Italy_Data/finaldb") #original data


# read the EUROSTAT week file
EUROSTAT <- read_excel("./EUROSTAT_ISO_HMEROLOGIO.xlsx")
EUROSTAT$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
EUROSTAT$year <- as.numeric(format(EUROSTAT$EURO_TIME, "%Y"))
EUROSTAT$month <- format(EUROSTAT$EURO_TIME, "%m")
EUROSTAT$month.an <- month.abb[as.numeric(EUROSTAT$month)]
EUROSTAT$weekID = as.numeric(substring(EUROSTAT$CD_EURO,2,3))
# identify the lockdown weeks for 2020 and the corresponding indexes
lockdowns_week <- EUROSTAT$EURO_LABEL[EUROSTAT$EURO_TIME %in% as.Date(c("2020-03-09", "2020-05-03", "2020-11-06"))]
lockdowns_weekID <- EUROSTAT$weekID[EUROSTAT$EURO_TIME %in% as.Date(c("2020-03-09", "2020-05-03", "2020-11-06", "2020-12-31"))]

# Italian shapefile at the province level
prov.shp = readOGR("./ItaliaLimiti01012020_g/ProvCM01012020_g/ProvCM01012020_g_WGS84.shp")
prov.shp@data$COD_PROV = as.numeric(prov.shp@data$COD_PROV)
prov.shp.table = prov.shp@data
prov.shp.original = prov.shp

# Italian shapefile at the regional level
reg.shp = readOGR("~/Dropbox/Shapefile_collection/ItaliaLimiti01012020_g/Reg01012020_g/Reg01012020_g_WGS84.shp")
reg.shp.table = reg.shp@data %>% select(COD_REG,DEN_REG)


####### PLOT 1: temporal trends ####### 
# Extract country weekly data by age and sex
out = get2020weeklydata(post.samples = pois.samples.list,
                  geo.res="country",
                  link_table=link_table,
                  stratify.by="agesex",
                  country="Italy")

names(out)
head(out[[1]])

# Compute quantiles for the plots
data4plot = lapply(out,
                   function(x) {
                     quantiles = 
                     x %>%
                       dplyr::select(starts_with("V")) %>% 
                       apply(.,1,quantile, probs = c(0.025, seq(from = .1, to = 0.9, by = 0.1), 0.975)) %>% 
                       t()  
                     
                     quantiles_df = data.frame(quantiles)
                     colnames(quantiles_df) = c("2.5%","10%","20%","30%","40%","50%","60%","70%","80%","90%","97.5%")
                     
                     cbind(EURO_LABEL=x$EURO_LABEL,
                           observed=x$observed, 
                           x=1:nrow(quantiles_df),
                           quantiles_df)
                   })

# Specify colors for the plot
fcol <- viridis(15)[7]
lcol <- "black"
mcol <- viridis(30)[24]
alpha.level <- 0.002

# Specify subtitles names for the plot
apply(expand.grid(age = c("40<", "40-59", "60-69", "70-79", "80+"), 
                  sex = c("Females", "Males")), 1, 
      function(X) paste(X[2], X[1], sep = " ")) -> nam.title
#check the labels with groups4cv and names(out)

# Specify the yaxis limits:
# THIS IS DONE MANUALLY! WE SHOULD FIND A WAY FOR AUTOMATIZING THIS
# here you set the limits for the plots
#unlist(lapply(data4plot, function(x) min(x$observed)))*0.8
#unlist(lapply(data4plot, function(x) max(x$observed)))*1.2
ylimsmin <- rep(c(0, 200, 300, 750, 2500), times = 2)
ylimsmax <- rep(c(100, 1000, 1500, 3500, 9000), times = 2)

# get the months as the xaxis
euro_weeks %>% filter(!duplicated(x)) %>% filter(sub) %>% select(x, month.an) -> xaxis
xaxis = EUROSTAT %>% 
  filter(ETOS_EURO==2020) %>% 
  select(weekID,month.an) %>% 
  filter(!(weekID==1 & month.an=="Dec"))
xaxis <- xaxis[!duplicated(xaxis$month.an),]

# start the plotting
p_list <- list()

for(i in 1:length(nam.title)){
  p_list[[i]] = PlotTimeSeries(dat = data4plot[[i]], 
                               lcol = lcol,
                               fcol = fcol, 
                               dat_true = data4plot[[i]], 
                               main.title = nam.title[i], 
                               lockdowns = lockdowns_weekID) +
    ylim(c(ylimsmin[i], ylimsmax[i])) + 
    scale_x_continuous(breaks = xaxis$weekID, labels = xaxis$month.an,expand = c(0, 0)) + 
    theme(
      axis.text.y = element_text(size=7), 
      axis.text.x = element_text(size=7), 
      plot.title = element_text(size=8)
    )
}

png(paste0(path2save, "Fig1Italy.png"), 
    width = 16, height = 21, units = "cm", res = 300)
# (p_list[[1]]|p_list[[6]])/
print((p_list[[2]]|p_list[[7]])/
        (p_list[[3]]|p_list[[8]])/
        (p_list[[4]]|p_list[[9]])/
        (p_list[[5]]|p_list[[10]]) + 
        plot_annotation(title = 'Temporal mortality trends in Italy during 2020') & 
        theme(plot.title = element_text(size = 10)))
dev.off()


####### PLOT 2: caterpillar plot ####### 
# Extract regional data by sex (total for 2020)
# and compute excess of mortality

# Number of regions
N <- length(unique(link_table$COD_REG))
country.name = "Italy"

out_reg = get2020data(post.samples = pois.samples.list,
                        geo.res="region",
                        link_table=link_table,
                        stratify.by="sex",
                        country=country.name)

names(out_reg)

excess4plot_reg = lapply(out_reg,
                             compute.excess,
                             divide.by="obs",
                             geo.name="DEN_REG")
names(excess4plot_reg)

# Extract national data by sex (total for 2020)
# and compute excess of mortality
out_country = get2020data(post.samples = pois.samples.list,
                  geo.res="country",
                  link_table=link_table,
                  stratify.by="sex",
                  country=country.name)
excess4plot_country = lapply(out_country,
                             compute.excess,
                             divide.by="obs",
                             geo.name="COUNTRY")
names(excess4plot_country)


# arrange the data by using the male median excess 
# and then include the country data at the bottom
ord <- order(excess4plot_reg[[which(names(excess4plot_reg)=="M")]]$median.excess)
excess4plotF = data.frame(Map(c,excess4plot_country[[which(names(excess4plot_reg)=="F")]],
                              excess4plot_reg[[which(names(excess4plot_reg)=="F")]][ord,]))
excess4plotF = rename(excess4plotF, "NAMES"="COUNTRY")
excess4plotM = data.frame(Map(c,excess4plot_country[[which(names(excess4plot_reg)=="M")]],
                              excess4plot_reg[[which(names(excess4plot_reg)=="M")]][ord,]))
excess4plotM = rename(excess4plotM, "NAMES"="COUNTRY")

# calculate the densities
datplot_males <- calc_density(Z = excess4plotM)
datplot_females <- calc_density(Z = excess4plotF)

# and plot
p.males <- plotstrips(Z = datplot_males) +
            ggtitle("Males") 
p.females <- plotstrips(Z = datplot_females) +
            ggtitle("Females") + 
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())

png(paste0(path2save, "Fig2Italy.png"), width = 17, height = 7, units = "cm", res = 300)
print((p.males|p.females) +
        plot_annotation(title = paste0("Excess mortality by regions in ", country ," in 2020")) & 
        theme(plot.title = element_text(size = 10)))
dev.off()

####### PLOT 3: MAPS (province-total) of excess deaths and posterior probability ####### 
# Extract province data (total) 
# and compute excess of mortality

out_prov = get2020data(post.samples = pois.samples.list,
                       geo.res="province",
                       link_table=link_table,
                       stratify.by="none",
                       country=country.name)
dim(out_prov)

excess4plot_prov = compute.excess(out_prov,
                                  divide.by="obs",
                                  geo.name="ID_PE")

# include probabilities and categories for excess and probability
excess4plot_prov$ExProb = apply(select(excess4plot_prov,starts_with("xs")) > 0, 1, mean)
range(excess4plot_prov$median.excess)
excess4plot_prov$Median.cat = cut(excess4plot_prov$median.excess, 
                   breaks = c(-1, -0.05, -0.01, 0, 0.01, 0.05, 1), 
                   labels = c("-0.05<", "[-0.05, -0.01)", "[-0.01, 0)", 
                              "[0, 0.01)", "[0.01, 0.05)", "0.05>"), 
                   include.lowest = TRUE, right = FALSE)
excess4plot_prov$ex.cat = cut(excess4plot_prov$ExProb, breaks = c(0, 0.20, 0.80, 1.01), 
               labels = c("[0, 0.2]", "(0.2, 0.8]", "(0.8, 1]"), 
               include.lowest = TRUE, right = FALSE)

prov.shp@data = left_join(prov.shp@data,
                          excess4plot_prov,
                          by=c("COD_PROV"="ID_PE")) 

ggplot() + 
  geom_sf(data = sf::st_as_sf(prov.shp,coords=c(SHAPE_AREA,SHAPE_LEN)),
                   aes(fill = Median.cat), col = NA) + 
  scale_fill_viridis_d(name = "", drop=FALSE) + 
  theme_bw() +
  ggtitle("Excess deaths") + 
  theme(text = element_text(size=8), 
        legend.key =
          element_rect(fill = 'white', color = "white", size = 0.1),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"), 
        legend.position=c(0.856,0.784), 
        legend.box.background = element_rect(colour = "black"), 
        legend.background = element_blank(), 
        legend.spacing.y = unit(.10, "mm"), 
        legend.spacing.x = unit(0, "mm")) -> p1

ggplot() + 
  geom_sf(data = sf::st_as_sf(prov.shp,coords=c(SHAPE_AREA,SHAPE_LEN)),
                   aes(fill = ex.cat), col = NA) + 
  scale_fill_viridis_d(name = "", drop=FALSE, begin = 0, end = 1) + theme_bw() +
  ggtitle("Posterior probability of positive excess") + 
  theme(text = element_text(size=8), 
        legend.key =
          element_rect(fill = 'white', color = "white", size = 0.1),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"), 
        legend.position=c(0.886,0.862), 
        legend.box.background = element_rect(colour = "black"), 
        legend.background = element_blank(), 
        legend.spacing.y = unit(.10, "mm"), 
        legend.spacing.x = unit(0, "mm")) -> p2


png(paste0(path2save, "Fig3Italy.png"),
    width = 16, height = 9, units = "cm", res = 300)
print(p1|p2)
dev.off()


####### PLOT 4: MAPS (province-by age and sex) of excess deaths and posterior probability ####### 
# Extract province data (by age and sex) 
# and compute excess of mortality

out_prov = get2020data(post.samples = pois.samples.list,
                      geo.res="province",
                      link_table=link_table,
                      stratify.by="agesex",
                      country=country.name)

names(out_prov)
dim(out_prov[[1]])

excess4plot_prov = lapply(out_prov,
                          compute.excess,
                          divide.by="obs",
                          geo.name="ID_PE")

# add the column with probability
excess4plot_prov = lapply(excess4plot_prov,
               function(X) {
                 X$ExProb = apply(select(X,starts_with("xs")) > 0, 1, mean)
                 X$Median.cat = cut(X$median.excess, 
                                    breaks = c(-1, -0.05, -0.01, 0, 0.01, 0.05, 1), 
                                    labels = c("-0.05<", "[-0.05, -0.01)", "[-0.01, 0)", 
                                               "[0, 0.01)", "[0.01, 0.05)", "0.05>"), 
                                    include.lowest = TRUE, right = FALSE)
                 X$ex.cat = cut(X$ExProb, breaks = c(0, 0.20, 0.80, 1.01), 
                                labels = c("[0, 0.2]", "(0.2, 0.8]", "(0.8, 1]"), 
                                include.lowest = TRUE, right = FALSE)
                 
                 data.frame(X)
               })

# ready for mapping
p_list_excess = list()
p_list_prob = list()

for(i in 1:length(excess4plot_prov)){
  prov.shp = prov.shp.original
  prov.shp@data = left_join(prov.shp@data, excess4plot_prov[[i]],
                            by=c("COD_PROV"="ID_PE")) 
  
  
  p_list_excess[[i]] <- ggplot() +
    geom_sf(data = sf::st_as_sf(prov.shp,coords=c(SHAPE_AREA,SHAPE_LEN)),
            aes(fill = Median.cat), col = NA) + 
    scale_fill_viridis_d(name = "", drop=FALSE) + 
    theme_bw() +
    ggtitle(names(excess4plot_prov)[i]) + 
    theme(text = element_text(size=8), 
          legend.key =
            element_rect(fill = 'white', color = "white", size = 0.1),
          legend.text = element_text(size = 6),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.3, "cm"), 
          #legend.position=c(0.856,0.784), 
          legend.box.background = element_rect(colour = "black"), 
          legend.background = element_blank(), 
          legend.spacing.y = unit(.10, "mm"), 
          legend.spacing.x = unit(0, "mm")) 
  
  p_list_prob[[i]] = ggplot() +
    geom_sf(data = sf::st_as_sf(prov.shp,coords=c(SHAPE_AREA,SHAPE_LEN)),
            aes(fill = ex.cat), col = NA) + 
    scale_fill_viridis_d(name = "", drop=FALSE, begin = 0, end = 1) + theme_bw() +
    ggtitle(names(excess4plot_prov)[i]) + 
    theme(text = element_text(size=8), 
          legend.key =
            element_rect(fill = 'white', color = "white", size = 0.1),
          legend.text = element_text(size = 6),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.3, "cm"), 
          #legend.position=c(0.886,0.862), 
          legend.box.background = element_rect(colour = "black"), 
          legend.background = element_blank(), 
          legend.spacing.y = unit(.10, "mm"), 
          legend.spacing.x = unit(0, "mm")) 
}      

png(paste0(path2save, "Fig3Italy_postprobs_byagesex.png"), 
    width = 16, height = 21, units = "cm", res = 300)
print((p_list_prob[[2]]|p_list_prob[[7]])/
        (p_list_prob[[3]]|p_list_prob[[8]])/
        (p_list_prob[[4]]|p_list_prob[[9]])/
        (p_list_prob[[5]]|p_list_prob[[10]]) + 
        plot_annotation(
          title = 'Posterior probability of positive excess mortality'
        ) & 
        theme(plot.title = element_text(size = 10)))
dev.off()

png(paste0(path2save, "Fig3Italy_excesss_byagesex.png"), 
    width = 16, height = 21, units = "cm", res = 300)
print((p_list_excess[[2]]|p_list_excess[[7]])/
        (p_list_excess[[3]]|p_list_excess[[8]])/
        (p_list_excess[[4]]|p_list_excess[[9]])/
        (p_list_excess[[5]]|p_list_excess[[10]]) + 
        plot_annotation(
          title = 'Excess mortality'
        ) & 
        theme(plot.title = element_text(size = 10)))
dev.off()

#### STOP here ####
stop()


###############################################################
# Total and net relative excess rate (regional level and total for 2020)
# computed dividing by n.of observed deaths 
###############################################################
out_reg = get2020data(post.samples=pois.samples.list,
                      geo.res="region",
                      link_table=link_table, 
                      stratify.by="none",
                      country="Italy")
out_country = get2020data(post.samples=pois.samples.list,
                          geo.res="country",
                          link_table=link_table, 
                          stratify.by="none",
                          country="Italy")
out_country = out_country %>% rename(DEN_REG = COUNTRY)
out = rbind(out_reg,out_country)

# include info about covid deaths
covid_deaths = read_excel("./Output/COVID_deaths.xlsx")

out_final = left_join(out,covid_deaths,by=c("DEN_REG" = "Region"))
glimpse(out_final)
tail(colnames(out_final))


out_final %>% select(DEN_REG,observed,Total_decessi) 
#  ggplot()+
#  geom_point(aes(observed,Total_decessi))


# Compute relative total excess deaths [= (pred-obs)/obs]
xs = compute.excess(data=out_final, divide.by = "obs",
                    geo.name = "DEN_REG")

# Compute relative net excess deaths [= (est-obs)/obs]
xsnet = compute.excess(data=out_final, divide.by = "obs",
                       remove.covid = "decessi_COVID",
                       geo.name = "DEN_REG")

# Put everything all together
xs_all = rbind(xs %>% select(DEN_REG, mean.excess:upp.excess), 
               xsnet %>% select(DEN_REG, mean.excess:upp.excess))
xs_all$type = rep(c("Total","Net"),each=n_distinct(xs_all$DEN_REG))

xs_all$DEN_REG_ord <- factor(xs_all$DEN_REG, levels = xs_all$DEN_REG[order(xs_all$mean.excess[which(xs_all$type=="Total")])])

# Prepare for plotting
bold.labels <- ifelse(levels(xs_all$DEN_REG_ord)=="Italy",
                      yes = "bold", no = "plain")

p1 = xs_all %>% mutate(Italy= ifelse(DEN_REG_ord=="Italy",1,0)) %>% 
  #mutate(DEN_REG_ord = fct_reorder(DEN_REG, mean.excess)) %>%
  ggplot(aes(x=DEN_REG_ord, y=mean.excess)) + 
  geom_pointrange(aes(ymin=low.excess, ymax=upp.excess,
                      col=type,linetype=factor(Italy)),
                  position = position_dodge(width=0.2))+
  guides(linetype=FALSE) +
  ylab("Excess of mortality") +
  xlab("Region")+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 1,
                                   hjust = 1,
                                   face = bold.labels))
  
png(paste0(path2save, "/tot_net_ex_Italy_reg.png"), 
    width = 16, height = 21, units = "cm", res = 300)
print(p1)
dev.off()




###############################################################
# Total and net relative excess rate (regional level and total for 2020)
# computed dividing by the population
### ----- Same plot as before but computed using population
###############################################################

pop2020 = finaldb %>% 
  filter(Week == 1, Anno==2020) %>% 
  select(popfin,REG)  %>% 
  group_by(REG) %>% 
  summarise(pop=sum(popfin)) 
pop2020 = pop2020 %>% left_join(link_table_reg,by = c("REG"="COD_REG"))  


# include info about population
out_final = out_final %>% left_join(pop2020)
# set total population for Italy
out_final$pop[out_final$DEN_REG=="Italy"] = sum(out_final$pop,na.rm=T)

# Compute relative total excess deaths [= (pred-obs)/obs]
xs = compute.excess(data=out_final,
                    divide.by = "pop",
                    geo.name = "DEN_REG")

# Compute relative net excess deaths [= (est-obs)/obs]
xsnet = compute.excess(data=out_final, divide.by = "pop",
                       remove.covid = "decessi_COVID",
                       geo.name = "DEN_REG")

# Put everything all together
xs_all = rbind(xs %>% select(DEN_REG, mean.excess:upp.excess), 
               xsnet %>% select(DEN_REG, mean.excess:upp.excess))
xs_all$type = rep(c("Total","Net"),each=n_distinct(xs_all$DEN_REG))


# Prepare for plotting
xs_all$DEN_REG_ord <- factor(xs_all$DEN_REG, levels = xs_all$DEN_REG[order(xs_all$mean.excess[which(xs_all$type=="Total")])])

bold.labels <- ifelse(levels(xs_all$DEN_REG_ord)=="Italy",
                      yes = "bold", no = "plain")

p2 = xs_all %>% mutate(Italy= ifelse(DEN_REG_ord=="Italy",1,0)) %>% 
  #mutate(DEN_REG_ord = fct_reorder(DEN_REG, mean.excess)) %>%
  ggplot(aes(x=DEN_REG_ord, y=mean.excess*100000)) + 
  geom_pointrange(aes(ymin=low.excess*100000, ymax=upp.excess*100000,
                      col=type,linetype=factor(Italy)),
                  position = position_dodge(width=0.2))+
  guides(linetype=FALSE) +
  ylab("Excess mortality rate x 100000") +
  xlab("Region")+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 1,
                                   hjust = 1,
                                   face = bold.labels))






###############################################################
# Extract 2020 weekly data
###############################################################

out = get2020weeklydata(post.samples=pois.samples.list,
                      geo.res="country",
                      link_table=link_table, 
                      stratify.by="agesex",
                      country="Italy")
class(out)
dim(out)
tail(colnames(out))
length(out)
dim(out[[1]])
tail(colnames(out[[1]]))

out_country = get2020weeklydata(post.samples=pois.samples.list,
                          geo.res="country",
                          link_table=link_table, 
                          stratify.by="none",
                          country="Italy")

png(paste0(path2save, "/tot_net_ex_pop_Italy_reg.png"), 
    width = 16, height = 21, units = "cm", res = 300)
print(p2)
dev.off()


