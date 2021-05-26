# Code for producing plots/tables 
# given the poisson_samples_all data

library(pbapply)
library(tidyverse)
library(readxl)
library(ggtext)
library(viridis)
library(rgdal)
library(patchwork)
library(sf)
library(RColorBrewer)

cluster = F


#### Set the WD and load all the necessary data/functions ####
setwd("") 
source("newfunctions.R")

path2save ="./Output/"

# this table is needed for the caterpilar plot
link_table = readRDS("./data/link_caterpilar") 
# the link table should look like:
# NAMNUTS2          ID_space  RegionID
# Central Macedonia      PE1  Region-2
#           Attica       PE2  Region-13
#           Epirus       PE3  Region-4
# Central Macedonia      PE4  Region-2
#   Central Greece       PE5  Region-8
#   Central Greece       PE6  Region-8

# NAMNUTS2             the names of the NUTS2 region
# ID_space             the ID of the NUTS3 region as given in the finaldb
# RegionID             the ID of the NUTS2 region
# the link between NUTS2 AND NUTS3


pois.samples.list = readRDS("./Output/poisson_samples_all") #samples from the inla model (see Step 5. of model.run.R)
finaldb = readRDS("./data/finaldb") #original data


# read the EUROSTAT week file
EUROSTAT <- read_excel("./data/EUROSTAT_ISO_HMEROLOGIO.xls")
EUROSTAT$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
EUROSTAT$year <- as.numeric(format(EUROSTAT$EURO_TIME, "%Y"))
EUROSTAT$month <- format(EUROSTAT$EURO_TIME, "%m")
EUROSTAT$month.an <- month.abb[as.numeric(EUROSTAT$month)]
EUROSTAT$weekID = as.numeric(substring(EUROSTAT$CD_EURO,2,3))

country.name <- "Greece"
country.name <- "Switzerland"

# identify the lockdown weeks for 2020 and the corresponding indexes

if(country.name == "Greece"){
  GR <- c("2020-03-13", "2020-05-04", "2020-11-07")
  lockdowns_weekID <- EUROSTAT$weekID[EUROSTAT$EURO_TIME %in% as.Date(c(GR, "2020-12-31"))]
}

if(country.name == "Switzerland"){
  CH <- c("2020-03-17", "2020-04-26", "2020-11-02")
  lockdowns_weekID <- EUROSTAT$weekID[EUROSTAT$EURO_TIME %in% as.Date(c(CH, "2020-12-31"))]
}

if(country.name == "Italy"){
  IT <- c("2020-03-09", "2020-05-03", "2020-11-06")
  lockdowns_weekID <- EUROSTAT$weekID[EUROSTAT$EURO_TIME %in% as.Date(c(IT, "2020-12-31"))]
}


# shapefile at the regional level. This shapefile should have a column ID_PE that is identical with the ID_space on the data file
prov.shp = read_sf("./data/shp.shp")




####### PLOT 1: temporal trends ####### 
# Extract country weekly data by age and sex
out = get2020weeklydata(post.samples = pois.samples.list,
                  geo.res="country",
                  link_table=link_table,
                  stratify.by="agesex",
                  country=country.name)

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
ylimsmin <- rep(c(min(c(data4plot$`F40<`$`2.5%`, data4plot$`M40<`$`2.5%`))*0.5, 
                  min(c(data4plot$`F40-59`$`2.5%`, data4plot$`M40-59`$`2.5%`))*0.5, 
                  min(c(data4plot$`F60-69`$`2.5%`, data4plot$`M60-69`$`2.5%`))*0.5, 
                  min(c(data4plot$`F70-79`$`2.5%`, data4plot$`M70-79`$`2.5%`))*0.5, 
                  min(c(data4plot$`F80+`$`2.5%`, data4plot$`M80+`$`2.5%`))*0.5), 
                times = 2)


ylimsmax <- rep(c(max(c(data4plot$`F40<`$`97.5%`,   data4plot$`M40<`$`97.5%`))*1.3, 
                  max(c(data4plot$`F40-59`$`97.5%`, data4plot$`M40-59`$`97.5%`))*1.3, 
                  max(c(data4plot$`F60-69`$`97.5%`, data4plot$`M60-69`$`97.5%`))*1.3, 
                  max(c(data4plot$`F70-79`$`97.5%`, data4plot$`M70-79`$`97.5%`))*1.3, 
                  max(c(data4plot$`F80+`$`97.5%`,   data4plot$`M80+`$`97.5%`))*1.3), 
                times = 2)

# get the months as the xaxis
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

png(paste0(path2save, "Fig1.png"), 
    width = 16, height = 21, units = "cm", res = 300)
# (p_list[[1]]|p_list[[6]])/
print((p_list[[7]]|p_list[[2]])/
        (p_list[[8]]|p_list[[3]])/
        (p_list[[9]]|p_list[[4]])/
        (p_list[[10]]|p_list[[5]]) + 
        plot_annotation(title = paste0("Temporal mortality trends in ", country.name, " during 2020")) & 
        theme(plot.title = element_text(size = 10)))
dev.off()












####### PLOT 2: caterpillar plot ####### 
# Extract regional data by sex (total for 2020)
# and compute excess of mortality

# Number of regions
N <- length(unique(link_table$RegionID))


# need to remove the younger group
pois.samples.list <- pois.samples.list[-c(1,6)]

out_reg = get2020data(post.samples = pois.samples.list,
                        geo.res="region",
                        link_table=link_table,
                        stratify.by="sex",
                        country=country.name)


excess4plot_reg = lapply(out_reg,
                             compute.excess,
                             divide.by="obs",
                             geo.name="RegionID")


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

datplot_males <- left_join(datplot_males, link_table, by = c("NAMES" = "RegionID"))
datplot_males$NAMNUTS2[is.na(datplot_males$NAMNUTS2)] <- datplot_males$NAMES[is.na(datplot_males$NAMNUTS2)]

datplot_females <- left_join(datplot_females, link_table, by = c("NAMES" = "RegionID"))
datplot_females$NAMNUTS2[is.na(datplot_females$NAMNUTS2)] <- datplot_females$NAMES[is.na(datplot_females$NAMNUTS2)]

# and plot
p.males <- plotstrips(Z = datplot_males) +
            ggtitle("Males") 
p.females <- plotstrips(Z = datplot_females) +
            ggtitle("Females") + 
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())


## NOTE: CHANGE width and height
# width = 17, height = 7 for Greece
# width = 17, height = 5 for Switzerland

png(paste0(path2save, "Fig2.png"), width = 17, height = 5, units = "cm", res = 300)
print((p.males|p.females) +
        plot_annotation(title = paste0("Excess mortality by regions in ", country.name," in 2020")) & 
        theme(plot.title = element_text(size = 10)))
dev.off()




# Table of the caterpillar plot


link_table_un <- link_table[!duplicated(link_table$RegionID),]

excess4plotM <- left_join(excess4plotM, link_table_un, by = c("NAMES" = "RegionID"))
excess4plotF <- left_join(excess4plotF, link_table_un, by = c("NAMES" = "RegionID"))

excess4plotM$NAMNUTS2[is.na(excess4plotM$NAMNUTS2)] <- 
  excess4plotF$NAMNUTS2[is.na(excess4plotF$NAMNUTS2)] <- country.name

tab.resM <- cbind(excess4plotM$median.excess, excess4plotM$low.excess, excess4plotM$upp.excess)
tab.resF <- cbind(excess4plotF$median.excess, excess4plotF$low.excess, excess4plotM$upp.excess)


data.frame(
  Region = excess4plotM$NAMNUTS2, 
  Males = apply(tab.resM, 1, function(X){
    X <- sprintf('%.2f',X)
    return(CrI(X))
  }
  ), 
  Females = apply(tab.resF, 1, function(X){
    X <- sprintf('%.2f',X)
    return(CrI(X))
  }
  )
) -> res.caterpilar



res.caterpilar[nrow(res.caterpilar):1,]

write.csv(res.caterpilar[nrow(res.caterpilar):1,], file = paste0(path2save, "TabCaterpillar.csv"))







####### PLOT 3: MAPS (province-total) of excess deaths and posterior probability ####### 
# Extract province data (total) 
# and compute excess of mortality

out_prov = get2020data(post.samples = pois.samples.list,
                       geo.res="province",
                       link_table=link_table,
                       stratify.by="none",
                       country=country.name)


excess4plot_prov = compute.excess(out_prov,
                                  divide.by="obs",
                                  geo.name="ID_space")


# include probabilities and categories for excess and probability
excess4plot_prov$ExProb = apply(select(excess4plot_prov,starts_with("xs")) > 0, 1, mean)
excess4plot_prov$Median.cat = cut(excess4plot_prov$median.excess, 
                   breaks = c(-100, -0.15, -0.05, 0, 0.05, 0.15, 100), 
                   labels = c("-15<", "[-15, -5)", "[-5, 0)", 
                              "[0, 5)", "[5, 15)", "15>"), 
                   include.lowest = TRUE, right = FALSE)
excess4plot_prov$ex.cat = cut(excess4plot_prov$ExProb, breaks = c(0, 0.20, 0.80, 1.01), 
               labels = c("[0, 0.2]", "(0.2, 0.8]", "(0.8, 1]"), 
               include.lowest = TRUE, right = FALSE)



prov.shp.tmp = left_join(prov.shp,
                     excess4plot_prov,
                     by=c("ID_PE"="ID_space")) 

cols_exd <- brewer.pal(n = 6, name = "RdBu")

ggplot() + 
  geom_sf(data = prov.shp.tmp, aes(fill = Median.cat), col = "grey", size = 0.3) + 
  scale_fill_manual(values=cols_exd[length(cols_exd):1], name = "", drop=FALSE) + 
  theme_bw() +
  ggtitle("Excess deaths") + 
  theme(text = element_text(size=8), 
        legend.key =
          element_rect(fill = 'white', color = "white", size = 0.1),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"), 
        legend.box.background = element_rect(colour = "black"), 
        legend.background = element_blank(), 
        legend.spacing.y = unit(.10, "mm"), 
        legend.spacing.x = unit(0, "mm")) + 
        theme(legend.position="bottom") -> p1





cols_exp <- brewer.pal(n = 3, name = "RdBu")

ggplot() + 
  geom_sf(data = prov.shp.tmp, aes(fill = ex.cat), col = "grey", size = 0.3) + 
  scale_fill_manual(values=cols_exp[length(cols_exp):1], name = "", drop=FALSE) +
  theme_bw() +
  ggtitle("Posterior probability of positive excess") + 
  theme(text = element_text(size=8), 
        legend.key =
          element_rect(fill = 'white', color = "white", size = 0.1),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"), 
        legend.box.background = element_rect(colour = "black"), 
        legend.background = element_blank(), 
        legend.spacing.y = unit(.10, "mm"), 
        legend.spacing.x = unit(0, "mm")) + 
        theme(legend.position="bottom")-> p2


# Greece: width = 16, height = 9
# Switzerland: width = 16, height = 6
png(paste0(path2save, "Fig3.png"),
    width = 16, height = 6, units = "cm", res = 300)
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

# I will replace 0s of the observed with 0.1 so I dont have inf on the (obs-pred)/obs
lapply(out_prov, function(X){
  X$observed[X$observed == 0] <- 0.1 
  return(X)
  }
) -> out_prov

# of course this is subject to extremes which are more aparent in the younger age groups. In 
# any case though these younger groups are not shown in the main manuscript due to the low 
# predictive ability


excess4plot_prov = lapply(out_prov,
                          compute.excess,
                          divide.by="obs",
                          geo.name="ID_space")


# add the column with probability
excess4plot_prov = lapply(excess4plot_prov,
               function(X) {
                 X$ExProb = apply(select(X,starts_with("xs")) > 0, 1, mean)
                 X$Median.cat = cut(X$median.excess, 
                                    breaks = c(-100, -0.15, -0.05, 0, 0.05, 0.15, 100), 
                                    labels = c("-15<", "[-15, -5)", "[-5, 0)", 
                                               "[0, 5)", "[5, 15)", "15>"),
                                     include.lowest = TRUE, right = FALSE)
                 X$ex.cat = cut(X$ExProb, breaks = c(0, 0.20, 0.80, 1.01), 
                                labels = c("[0, 0.2]", "(0.2, 0.8]", "(0.8, 1]"), 
                                include.lowest = TRUE, right = FALSE)
                 
                 data.frame(X)
               })


names.title <- c(paste0("Females ", c("40-59", "60-69", "70-79", "80+")), 
                 paste0("Males ", c("40-59", "60-69", "70-79", "80+")))


# ready for mapping
p_list_excess = list()
p_list_prob = list()

for(i in 1:length(excess4plot_prov)){
  
  prov.shp.tmp = left_join(prov.shp,excess4plot_prov[[i]],
                            by=c("ID_PE"="ID_space")) 

  
  p_list_excess[[i]] <- ggplot() +
    geom_sf(data = prov.shp.tmp,
            aes(fill = Median.cat), col = "grey", size = 0.3) + 
    scale_fill_manual(values=cols_exd[length(cols_exd):1], name = "", drop=FALSE) + 
    theme_bw() +
    ggtitle(names.title[i]) + 
    theme(text = element_text(size=8), 
          legend.key =
            element_rect(fill = 'white', color = "white", size = 0.1),
          legend.text = element_text(size = 6),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.3, "cm"), 
          legend.box.background = element_rect(colour = "black"), 
          legend.background = element_blank(), 
          legend.spacing.y = unit(.10, "mm"), 
          legend.spacing.x = unit(0, "mm")) + 
           theme(legend.position="bottom") 
  
  p_list_prob[[i]] = ggplot() +
    geom_sf(data = prov.shp.tmp,
            aes(fill = ex.cat), col = "grey", size = 0.3) + 
    scale_fill_manual(values=cols_exp[length(cols_exp):1], name = "", drop=FALSE)+ 
    theme_bw() +
    ggtitle(names.title[i]) + 
    theme(text = element_text(size=8), 
          legend.key =
            element_rect(fill = 'white', color = "white", size = 0.1),
          legend.text = element_text(size = 6),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.3, "cm"), 
          legend.box.background = element_rect(colour = "black"), 
          legend.background = element_blank(), 
          legend.spacing.y = unit(.10, "mm"), 
          legend.spacing.x = unit(0, "mm")) + 
    theme(legend.position="bottom") 
}      

##
##
## NOTE: The height and width here should be modified based on the country.
## Greece: width = 15, height = 30
## Switzerland: width = 15, height = 25

png(paste0(path2save, "Fig3_postprobs_byagesex.png"), 
    width = 15, height = 25, units = "cm", res = 300)
print((p_list_prob[[5]]|p_list_prob[[1]])/
        (p_list_prob[[6]]|p_list_prob[[2]])/
        (p_list_prob[[7]]|p_list_prob[[3]])/
        (p_list_prob[[8]]|p_list_prob[[4]]) + 
        plot_annotation(
          title = 'Posterior probability of positive excess mortality'
        ) & 
        theme(plot.title = element_text(size = 10)))
dev.off()

png(paste0(path2save, "Fig3_excesss_byagesex.png"), 
    width = 15, height = 25, units = "cm", res = 300)
print((p_list_excess[[5]]|p_list_excess[[1]])/
        (p_list_excess[[6]]|p_list_excess[[2]])/
        (p_list_excess[[7]]|p_list_excess[[3]])/
        (p_list_excess[[8]]|p_list_excess[[4]]) + 
        plot_annotation(
          title = 'Excess mortality'
        ) & 
        theme(plot.title = element_text(size = 10)))
dev.off()








#######
####### Table 2: Observed, Excess by sex and NUTS2 region


out_reg = get2020data(post.samples = pois.samples.list,
                      geo.res="region",
                      link_table=link_table,
                      stratify.by="sex",
                      country=country.name)

out_reg_country = get2020data(post.samples = pois.samples.list,
                      geo.res="country",
                      link_table=link_table,
                      stratify.by="sex",
                      country=country.name)



lapply(out_reg, function(Z){
  
  Z$observed - Z[startsWith(colnames(Z), "V")] -> Y
  apply(Y, 1, function(X) quantile(X, probs = c(0.5, 0.025, 0.975))) -> X
  return(X)
  
}) -> list.res


cbind(out_reg$`F`$observed, out_reg$`F`[startsWith(colnames(out_reg$`F`), "V")]) + 
  cbind(out_reg$`M`$observed, out_reg$`M`[startsWith(colnames(out_reg$`F`), "V")]) -> out_reg_sum
out_reg_sum <- as.data.frame(out_reg_sum)
colnames(out_reg_sum)[1] <- "observed"


out_reg_sum$observed - out_reg_sum[startsWith(colnames(out_reg_sum), "V")] -> Y
apply(Y, 1, function(X) quantile(X, probs = c(0.5, 0.025, 0.975))) -> out.reg.total



lapply(out_reg_country, function(Z){
  
  Z$observed - Z[startsWith(colnames(Z), "V")] -> Y
  apply(Y, 1, function(X) quantile(X, probs = c(0.5, 0.025, 0.975))) -> X
  return(X)
  
}) -> list.res.country.bysex



c(out_reg_country$`F`$observed, as.numeric(out_reg_country$`F`[startsWith(colnames(out_reg_country$`F`), "V")])) + 
  c(out_reg_country$`M`$observed, as.numeric(out_reg_country$`M`[startsWith(colnames(out_reg_country$`F`), "V")])) -> out_reg_coun.sum

quantile(c(out_reg_coun.sum[1] - out_reg_coun.sum[-1]), probs = c(0.5, 0.025, 0.975)) -> count.res 



# and now bring all together

c(out_reg_country$`M`$observed,
  CrI(round(list.res.country.bysex$`M`)), 
  out_reg_country$`F`$observed,
  CrI(round(list.res.country.bysex$`F`)), 
  c(out_reg_country$`F`$observed + out_reg_country$`M`$observed),
  CrI(round(count.res))) -> first.row


cbind(out_reg$`M`$observed, 
      apply(t(round(list.res$`M`)), 1, CrI),
      out_reg$`F`$observed,
      apply(t(round(list.res$`F`)), 1, CrI),
      out_reg_sum$observed, 
      apply(t(round(out.reg.total)), 1, CrI)
) -> rest.tab


tab.res <- rbind(first.row, rest.tab)
tab.res <- as.data.frame(tab.res)

tab.l <- left_join(out_reg$`M`, link_table[!duplicated(link_table$RegionID),])
tab.res <- cbind(c(country.name, tab.l$NAMNUTS2), tab.res)
colnames(tab.res) <- c("Region", "Observed.males", "Excess.males", "Observed.females", "Excess.females",
                       "Observed.total", "Excess.total")

rownames(tab.res) <- NULL

write.csv(tab.res[order(-as.numeric(tab.res$Observed.males)),], file = paste0(path2save, "Tab2.csv"))



######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
