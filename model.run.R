remove(list=ls())
library(tidyverse)
library(INLA)
library(sf)
library(spdep)
INLA:::inla.dynload.workaround()

#inla.setOption(pardiso.license = "~/.")


# set as working directory the folder where all the R files are.
setwd("") 
source("newfunctions.R")
run.CV = F #2. Do you want to run cross-validation (CV)?
run.postCV = F #3. Do you want to compute CV indexes? (output from Step 2. is required)
run.mod.pred = F #4. Do you want to estimate models using 2015-2019 data and compute prediction for 2020?
Poisson.sampling2020 = F #5. Do you want to sample from the posterior pred. distr.? (output from Step 4. is required )


# define the groups and create a folder to store the output
dir.create("Output")
path2save = paste0("./Output/")
groups4cv = as.data.frame(expand.grid(age.group = c("less40", "40-59", "60-69", "70-79", "80plus"),
                                      sex = c("female", "male")))

#######  1. LOAD & PREPARE DATA and compute the param. starting points ----

# Create a data folder and copy paste inside the folder the relevant files
dir.create("data")

# the 2 relevant files:
# 1. finaldb
finaldb=readRDS("./data/finaldb")

# The finaldb should have the following structure
# ID_space age.group  sex   EURO_LABEL  year  deaths  population   hol mean.temperature
# PE1      40-59      male  2015-W01    2015  3       22410        1   0.930
# PE1      40-59      male  2015-W02    2015  5       22410.       1   1.84 
# PE1      40-59      male  2015-W03    2015  10      22410.       0   4.77 
# PE1      40-59      male  2015-W04    2015  4       22410.       0   8.01 
# PE1      40-59      male  2015-W05    2015  0       22410.       0   6.06 
# PE1      40-59      male  2015-W06    2015  5       22410.       0   6.27 

# ID_space            a unique ID for the spatial units
# age.group           the age group (less40, 40-59, 60-69, 70-79, 80plus)
# sex                 the sex (male, female)
# EURO_LABEL          the ISO code of the week
# year                the year 
# deaths              the number of deaths per age, sex, week and spatial unit
# population          the number of population at risk per age, sex, week and spatial unit
# hol                 1 if this week includes a bank holiday, 0 otherwise
# mean.temperature    the mean weekly temperature in the i-th spatial unit. Data can be downloaded from https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview


# 2. The files of the shapefile of interest. The id should correspond to the ID_space in finaldb
shp = read_sf("./data/shp.shp")
W.nb <- poly2nb(shp)
nb2INLA("W.adj", W.nb) 


if(run.CV | run.predictions){
  # Select data for the period 2015-2019 
  data = finaldb %>% filter(year<max(year))
  
  # Create indexes
  data$id.space = as.numeric(as.factor(data$ID_space))
  data$id.time <- as.numeric(substr(data$EURO_LABEL, start = 7, stop = 8))
  data$id.tmp <- inla.group(data$mean.temperature, n = 100, method = "cut", idx.only = TRUE)
  data$id.year <- data$year - 2014
  
  
  # Run INLA to obtain smart starting points to be used later
  formula = 
    deaths ~ 1 + offset(log(population)) + hol + 
    f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) +
    f(id.year, model='iid', hyper=hyper.iid, constr = TRUE) + 
    f(id.time, model='rw1', hyper=hyper.iid, constr = TRUE, scale.model = TRUE, cyclic = TRUE) +
    f(id.space, model='bym2', graph="W.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym)
  
  # INLA SET UP
  # priors
  hyper.bym <- list(theta1 = list('PCprior', c(1, 0.01)), theta2 = list('PCprior', c(0.5, 0.5)))
  hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))
  
  # Under Poisson uses default set up
  control.family=inla.set.control.family.default()
  
  # Run the model once for getting a smart starting point for theta
  # choose only one gender and class age
  data_start = data %>% filter(age.group=="70-79",sex=="female")
  
  m = inla(formula,
           data=data_start,
           family="Poisson",
           control.family=control.family,
           verbose = TRUE,
           num.threads = round(parallel::detectCores()*.8),
           control.compute=list(config = TRUE), 
           control.results=list(return.marginals.random=FALSE,
                                return.marginals.predictor=FALSE),
           control.predictor=list(link = 1))
  
  summary(m)
}

#######  2. Run Cross validation ----
if(run.CV) {
  
  # Perform space-time cross validation
  list.results <- list()
  years <- 2015:2019
  
  t_0 <- Sys.time()
  
  for(j in 1:nrow(groups4cv)){
    
    print(j)
    
    ageg <- as.character(groups4cv$age.group[j])
    sex <- as.character(groups4cv$sex[j])
    data %>% filter(sex==sex,
                    age.group==ageg) -> datCV_firslop
  
    list.CV.results.spacetime <- list()
    
    
    for(i in 1:5){
      
      datCV <- datCV_firslop
      testIndexes <- which(datCV$year %in% years[i], arr.ind=TRUE)
      datCV$deaths[testIndexes] <- NA
      
      list.CV.results.spacetime[[i]] <- inla.cv(Y = formula)
      gc()
      
    }

    saveRDS(list.CV.results.spacetime,
            file = paste0(path2save, paste0("yearCV_", ageg, "_", sex)))
    
  }
  
  t_1 <- Sys.time()
  print(t_1 - t_0) # 7 hours
}



####### 3. Cross validation indexes (load CV files from Step 2. first) ----
if(run.postCV){
  
  list.results <- apply(groups4cv, 1,
                        function(X) readRDS(paste0(path2save,"yearCV_", X[1], "_", X[2])))
  names(list.results) <- paste0(groups4cv[,1], groups4cv[,2])
  
  
  t_0 <- Sys.time()
  res_checks <- lapply(list.results, postpredchecks)
  t_1 <- Sys.time()
  t_1 - t_0 # 1.67 min
  
  # tables of results
  
  # Correlation table
  round(
    do.call(rbind, 
            lapply(lapply(res_checks, function(X) X$correl), 
                   function(X) quantile(X, probs = c(0.50, 0.025, 0.975)))
    ), 
    digits = 3
  ) -> correl.table
  correl.table
  
  saveRDS(correl.table,
          file = paste0(path2save, "CV.corr.table"))
  
  # Coverage probability table
  
  cov.prob <- lapply(res_checks, function(X)X$cov.prob)  
  as.data.frame(unlist(cov.prob)) %>% 
    rename(`95%CovProb` = `unlist(cov.prob)`) -> cov.prob.table
  
  cov.prob.table <- round(cov.prob.table, digits = 3)
  cov.prob.table
  
  saveRDS(cov.prob.table,
          file = paste0(path2save, "CV.covprob.table"))
  
}

#######  4. Run model for 2015-2019 and compute predictions for 2020 ----
# separate models for each (age class & gender) group will be estimated

if(run.mod.pred){
  
  # Select data for the period 2015-2020
  data = finaldb 
  
  # Create indexes
  data$id.space = (data %>% mutate(ID_prov=group_indices(.,PROV)))$ID_prov
  data$id.time <- as.numeric(substr(data$EURO_LABEL, start = 7, stop = 8))
  data$id.tmp <- inla.group(data$mean.temp, n = 100, method = "cut", idx.only = TRUE)
  data$id.year <- data$Anno - 2014
  
  
  t_0 <- Sys.time()
  for(j in 1:nrow(groups4cv)){
    
    print(j)
    
    ageg <- as.character(groups4cv$CL_ETA[j])
    sex <- as.character(groups4cv$Genere[j])
    data %>% filter((CL_ETA %in% ageg) & (Genere %in% sex)) -> datCV_firslop
    
    truth <- datCV_firslop$Morti[datCV_firslop$Anno >2019] 
    datCV_firslop$Morti[datCV_firslop$Anno >2019] <- NA
    
    
    in.mod = inla(formula,
         data=datCV_firslop,
         family="Poisson",  
         verbose = TRUE, 
         control.family=control.family,
         control.compute=list(config = TRUE), 
         control.mode=list(theta=m$mode$theta, restart=T),
         num.threads = round(parallel::detectCores()*.8), 
         control.predictor = list(link = 1))
    
    
    res.list <- list(mod = in.mod, true_values = truth)
    saveRDS(res.list, file = paste0(path2save,
                                    paste0("yearCV_", ageg, "_", sex, "respred2020")))
    
  }
  
  t_1 <- Sys.time()
  print(t_1 - t_0) #1.7 hours
}  

####### 5. Poisson Samples for 2015-2020 ----
# The output from Step 4. is required

if(Poisson.sampling2020){
  res.list <- apply(groups4cv, 1,
                    function(X) readRDS(paste0(path2save,
                                               paste0("yearCV_", X[1], "_", X[2], "respred2020"))))
  
  data <- finaldb
  # Create indexes
  data$id.space = (data %>% mutate(ID_prov=group_indices(.,PROV)))$ID_prov
  data$id.time <- as.numeric(substr(data$EURO_LABEL, start = 7, stop = 8))
  data$id.tmp <- inla.group(data$mean.temp, n = 100, method = "cut", idx.only = TRUE)
  data$id.year <- data$Anno - 2014
  
  # get the poisson samples
  pois.samples.list <- list()
  
  
  for(j in 1:nrow(groups4cv)){
    
    print(j)
    
    ageg <- as.character(groups4cv$CL_ETA[j])
    sex <- as.character(groups4cv$Genere[j])
    data %>% filter(Anno == max(Anno) & (CL_ETA %in% ageg) & (Genere %in% sex)) -> dat_tmp
    
    set.seed(11)
    post.samples <- inla.posterior.sample(n = 1000, result = res.list[[j]]$mod)
    predlist <- do.call(cbind,
                        lapply(post.samples,
                               function(X) exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
    
    pois.samples <- apply(predlist, 2,
                          function(Z) rpois(n = length(Z), lambda = Z))
    pois.samples <- as.data.frame(pois.samples)
    
    pois.samples$EURO_LABEL <- dat_tmp$EURO_LABEL
    pois.samples$ID_PE <- dat_tmp$PROV
    pois.samples$deaths <- dat_tmp$Morti 
    pois.samples$popfin <- dat_tmp$popfin 
    
    pois.samples.list[[j]] <- pois.samples
  
}

  saveRDS(pois.samples.list,
        file = paste0(path2save, "poisson_samples_all"))

}

################################################################################
#7. Plots
################################################################################
if(plotting){
  pois.samples.list <- readRDS(paste0(path2save, "poisson_samples_all"))
  
  library(pbapply)
  library(viridis)
  library(readxl)
  library(patchwork)
  library(rgdal)
  
  #######
  ####### PLOT 1: temporal trends ####### 
  #######

  
  # Aggregate by week (sum) and compute quantiles for predictions
  pblapply(pois.samples.list, function(X){
    
    X <- X %>% select(starts_with("V"),EURO_LABEL) #only columns referring to the 1000 samples + EURO_LABEL
    X %>% group_by(EURO_LABEL) %>% summarise_all(sum) -> X
    Y <- t(apply(X[,-1], 1,
                 quantile, probs = c(0.025, seq(from = .1, to = 0.9, by = 0.1), 0.975)))
    Y <- as.data.frame(Y)
    Y$x <- 1:nrow(Y)
    Y$EURO_LABEL <- X$EURO_LABEL
    return(Y)
    
  }) -> list.sum.deaths 
  
  # Aggregate by week (sum) and extract observed values 
  pblapply(pois.samples.list, function(X){
    X <- data.frame(EURO_LABEL = X$EURO_LABEL, observed = X$deaths)
    X %>% group_by(EURO_LABEL) %>% summarise(observed = sum(observed)) -> Y
    Y$x <- 1:nrow(Y)
    return(Y)
  }) -> list.observed
  
  
  sub <- startsWith(list.observed[[1]]$EURO_LABEL, "2020")
  
  fcol <- viridis(15)[7]
  lcol <- "black"
  mcol <- viridis(30)[24]
  alpha.level <- 0.002
  
  # colfunc <- colorRampPalette(c("red", "white"))
  # mcol <- colfunc(20)[11]
  # alpha.level <- 0.01
  
  
  apply(expand.grid(age = c("40<", "40-59", "60-69", "70-79", "80+"), 
                    sex = c("Females", "Males")), 1, 
        function(X) paste(X[2], X[1], sep = " ")) ->
    nam.title
  #check with groups4cv
  
  
  # here you set the limits for the plots
  ylimsmin <- rep(c(0, 200, 300, 750, 2500), times = 2)
  ylimsmax <- rep(c(100, 1000, 1500, 3500, 9000), times = 2)
  
  
  # read the EUROSTAT week file
  EUROSTAT <- read_excel("./EUROSTAT_ISO_HMEROLOGIO.xlsx")
  EUROSTAT$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
  EUROSTAT$year <- as.numeric(format(EUROSTAT$EURO_TIME, "%Y"))
  EUROSTAT$month <- format(EUROSTAT$EURO_TIME, "%m")
  EUROSTAT$month.an <- month.abb[as.numeric(EUROSTAT$month)]
  
  euro_weeks <- left_join(data.frame(EURO_LABEL = list.observed[[1]]$EURO_LABEL, 
                                     x = list.observed[[1]]$x), EUROSTAT)
  
  
  # 1st lockdown: 	9 March 2020 – 18 May 2020
  #https://it.wikipedia.org/wiki/Gestione_della_pandemia_di_COVID-19_in_Italia
  # 2nd lockdown: 6th November - 
  
  lockdowns <- euro_weeks$EURO_LABEL[euro_weeks$EURO_TIME %in% as.Date(c("2020-03-09", "2020-05-03", "2020-11-06"))]
  lockdowns <- list.observed[[1]]$x[list.observed[[1]]$EURO_LABEL %in% lockdowns]
  lockdowns[4] <- max(list.observed[[1]]$x)
  
  # get the months as the xaxis
  euro_weeks %>% filter(!duplicated(x)) %>% filter(sub) %>% select(x, month.an) -> xaxis
  xaxis <- xaxis[-1,]
  xaxis <- xaxis[!duplicated(xaxis$month.an),]
  
  p_list <- list()
  
  for(i in 1:length(nam.title)){
    p_list[[i]] <- PlotTimeSeries(dat = as.data.frame(list.sum.deaths[[i]])[sub,], 
                                  lcol = lcol, fcol = fcol, 
                                  dat_true = list.observed[[i]][sub,], 
                                  main.title = nam.title[i], 
                                  lockdowns = lockdowns) + 
      
      ylim(c(ylimsmin[i], ylimsmax[i])) + 
      # geom_vline(xintercept = lockdowns, linetype = 2) + 
      scale_x_continuous(breaks = xaxis$x, labels = xaxis$month.an,expand = c(0, 0)) + 
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
    (p_list[[5]]|p_list[[10]]) + plot_annotation(
      title = 'Temporal mortality trends in Italy during 2020'
    ) & 
    theme(plot.title = element_text(size = 10)))
  dev.off()
  
  
  
  #######
  ####### PLOT 2: caterpillar plot ####### 
  #######
  prov.shp = readOGR("./ItaliaLimiti01012020_g/ProvCM01012020_g/ProvCM01012020_g_WGS84.shp")
  prov.shp@data$COD_PROV = as.numeric(prov.shp@data$COD_PROV)
  prov.shp.table = prov.shp@data
  prov.shp.original = prov.shp
  
  reg.shp = readOGR("~/Dropbox/Shapefile_collection/ItaliaLimiti01012020_g/Reg01012020_g/Reg01012020_g_WGS84.shp")
  reg.shp.table = reg.shp@data %>% select(COD_REG,DEN_REG)
  
  link_table = left_join(prov.shp.table,reg.shp.table,by="COD_REG")
  link_table = link_table %>% select(-COD_CM, -COD_UTS, -SHAPE_AREA,-SHAPE_LEN)
  link_table$ID_PE = as.numeric(link_table$COD_PROV)
  saveRDS(link_table,paste0(path2save,"link_table"))
  
  N <- length(unique(link_table$COD_REG))
  country.name <- "Italy"
  
  
  # Aggregate by region and compute predictions (sum) 
  pblapply(pois.samples.list, function(X){
    
    X <- X[,c(1:1000, which(colnames(X) %in% "ID_PE"))]
    X <- left_join(X,
                   link_table, by = c("ID_PE" = "COD_PROV"))
    X$ID_PE <- X$RegionID <- NULL
    
    X %>% select(starts_with("V")) %>% summarise_all(sum) -> X.all
    X %>% 
      select(starts_with("V"), "DEN_REG") %>% 
      group_by(DEN_REG) %>%
      summarise_all(sum) -> X
    rbind(X, c(country.name, as.character(X.all))) -> X
    X <- as.data.frame(X)
    X[,-1] <- apply(X[,-1], 2, as.numeric)
    
    return(X)
    
  }) -> list.sum.deaths
  
  
  # Aggregate by region and compute n. of observed cases (sum) 
  
  pblapply(pois.samples.list, function(X){
    X <- data.frame(ID_PE = X$ID_PE, observed = X$deaths)
    X <- left_join(X, link_table, by = c("ID_PE" = "COD_PROV"))
    X$ID_PE <- X$RegionID <- NULL
    
    X.all <- sum(X$observed)
    X %>% group_by(DEN_REG) %>% summarise(observed = sum(observed)) -> Y
    rbind(Y, c(country.name, as.character(X.all))) -> Y
    Y$observed <- as.numeric(Y$observed)
    return(Y)
  }) -> list.observed
  
  
  # compute excess mortality ###excluding the young age group
  perc.ex.deaths.females <- perc.ex.sex.deaths(Z = which(groups4cv$Genere=="F"))
  perc.ex.deaths.males <- perc.ex.sex.deaths(Z = which(groups4cv$Genere=="M"))
  
  # order them wrt males, and have the country at the bottom
  ord <- order(perc.ex.deaths.males$x.median)
  perc.ex.deaths.males <- perc.ex.deaths.males[ord,]
  tmp.last <- perc.ex.deaths.males[perc.ex.deaths.males$NAMES %in% country.name,]
  perc.ex.deaths.males[!(perc.ex.deaths.males$NAMES %in% country.name),] -> perc.ex.deaths.males
  perc.ex.deaths.males <- rbind(tmp.last, perc.ex.deaths.males)
  
  
  # similar for females
  perc.ex.deaths.females <- perc.ex.deaths.females[ord,]
  tmp.last <- perc.ex.deaths.females[perc.ex.deaths.females$NAMES %in% country.name,]
  perc.ex.deaths.females[!(perc.ex.deaths.females$NAMES %in% country.name),] -> perc.ex.deaths.females
  perc.ex.deaths.females <- rbind(tmp.last, perc.ex.deaths.females)
  
  
  # calculate the densities
  datplot_males <- calc_density(Z = perc.ex.deaths.males)
  datplot_females <- calc_density(Z = perc.ex.deaths.females)
  
  # and plot
  p.males <- plotstrips(datplot_males) + ggtitle("Males") 
  p.females <- plotstrips(datplot_females) + ggtitle("Females") + 
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  
  png(paste0(path2save, "Fig2Italy.png"), width = 17, height = 7, units = "cm", res = 300)
  print((p.males|p.females) + plot_annotation(title = paste0("Excess mortality by regions in ", country.name ," in 2020")) & 
    theme(plot.title = element_text(size = 10)))
  dev.off()
  
  #######
  ####### PLOT 3: MAPS of excess deaths and posterior probability ####### 
  #######
  
  # Aggregate by province predictions
  pblapply(pois.samples.list, function(X){
    X <- X[,c(1:1000, which(colnames(X) %in% "ID_PE"))] # here ID_PE is the spatial unit
    X %>% group_by(ID_PE) %>% summarise_all(sum) -> X  
    #X$ID_PE <- X$ID_PE
    return(X)
    
  }) -> list.sum.deaths
  
  
  #Aggregate by province observed n. of cases
  pblapply(pois.samples.list, function(X){
    X <- data.frame(ID_PE = X$ID_PE, observed = X$deaths)
    X %>% group_by(ID_PE) %>% summarise(observed = sum(observed)) -> Y
    return(Y)
  }) -> list.observed
  
  # ------
  # General Map (all ages and sexes together, exclude <40)
  # ------
  sum.deaths <- Reduce("+", lapply(list.sum.deaths[-which(groups4cv$CL_ETA=="40<")],
                                   function(X) X[,-1]))
  sum.deaths$ID_PE <- list.sum.deaths[[1]]$ID_PE
  
  sum.observed <- Reduce("+", lapply(list.observed[-which(groups4cv$CL_ETA=="40<")],
                                     function(X) X[,-1]))
  sum.observed$ID_PE <- list.observed[[1]]$ID_PE
  
  perc.ex.deaths <- left_join(sum.deaths, sum.observed)
  sel = which(startsWith(colnames(perc.ex.deaths),"V"))
  perc.ex.deaths[,sel] <- (perc.ex.deaths$observed - perc.ex.deaths[,sel])/perc.ex.deaths[,sel]
  perc.ex.deaths$ExProb <- apply(perc.ex.deaths[,sel] > 0, 1, sum)/length(sel)
  perc.ex.deaths$Median <- apply(perc.ex.deaths[,sel], 1, median)
  
  perc.ex.deaths %>% select(ID_PE, ExProb, Median) %>% 
    
    mutate(Median.cat = cut(Median, breaks = c(-1, -0.05, -0.01, 0, 0.01, 0.05, 1), 
                            labels = c("-0.05<", "[-0.05, -0.01)", "[-0.01, 0)", 
                                       "[0, 0.01)", "[0.01, 0.05)", "0.05>"), 
                            include.lowest = TRUE, right = FALSE)) %>% 
    
    mutate(ex.cat = cut(ExProb, breaks = c(0, 0.20, 0.80, 1.01), 
                        labels = c("[0, 0.2]", "(0.2, 0.8]", "(0.8, 1]"), 
                        include.lowest = TRUE, right = FALSE)) -> perc.ex.deaths.map
    
  prov.shp@data = left_join(prov.shp@data, perc.ex.deaths.map,
                            by=c("COD_PROV"="ID_PE")) 
  
  
  ggplot() + geom_sf(data = sf::st_as_sf(prov.shp,coords=c(SHAPE_AREA,SHAPE_LEN)),
                     aes(fill = Median.cat), col = NA) + 
    scale_fill_viridis_d(name = "", drop=FALSE) + 
    theme_bw() +
    ggtitle("Proportion of excess deaths") + 
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
  
  ggplot() + geom_sf(data = sf::st_as_sf(prov.shp,coords=c(SHAPE_AREA,SHAPE_LEN)),
                     aes(fill = ex.cat), col = NA) + 
    scale_fill_viridis_d(name = "", drop=FALSE, begin = 0, end = 1) + theme_bw() +
    ggtitle("Posterior probability") + 
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
  
  
  png(paste0(path2save, "Fig3Italy.png"), width = 16, height = 9, units = "cm", res = 300)
  print(p1|p2)
  dev.off()
  
  
  # ------
  # Stratified Map (by sex and gender, exclude <40)
  # ------
  p_list_excess = list()
  p_list_prob = list()
  
  for(i in 1:length(nam.title)){
    
    sum.deaths <- list.sum.deaths[[i]]
    sum.observed <- list.observed[[i]]
  
    perc.ex.deaths <- left_join(sum.deaths, sum.observed)
    sel = which(startsWith(colnames(perc.ex.deaths),"V"))
    perc.ex.deaths[,sel] <- (perc.ex.deaths$observed - perc.ex.deaths[,sel])/perc.ex.deaths[,sel]
    perc.ex.deaths$ExProb <- apply(perc.ex.deaths[,sel] > 0, 1, sum)/length(sel)
    perc.ex.deaths$Median <- apply(perc.ex.deaths[,sel], 1, median)
  
    perc.ex.deaths %>% select(ID_PE, ExProb, Median) %>% 
    
      mutate(Median.cat = cut(Median, breaks = c(-1, -0.05, -0.01, 0, 0.01, 0.05, 1), 
                            labels = c("-0.05<", "[-0.05, -0.01)", "[-0.01, 0)", 
                                       "[0, 0.01)", "[0.01, 0.05)", "0.05>"), 
                            include.lowest = TRUE, right = FALSE)) %>% 
    
      mutate(ex.cat = cut(ExProb, breaks = c(0, 0.20, 0.80, 1.01), 
                        labels = c("[0, 0.2]", "(0.2, 0.8]", "(0.8, 1]"), 
                        include.lowest = TRUE, right = FALSE)) -> perc.ex.deaths.map
  
    prov.shp = prov.shp.original
    prov.shp@data = left_join(prov.shp@data, perc.ex.deaths.map,
                            by=c("COD_PROV"="ID_PE")) 
  
  
    p_list_excess[[i]] <- ggplot() +
                  geom_sf(data = sf::st_as_sf(prov.shp,coords=c(SHAPE_AREA,SHAPE_LEN)),
                          aes(fill = Median.cat), col = NA) + 
                  scale_fill_viridis_d(name = "", drop=FALSE) + 
                  theme_bw() +
                  ggtitle(nam.title[i]) + 
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
                      ggtitle(nam.title[i]) + 
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
  
  png(paste0(path2save, "Fig3Italy_postprobstratified.png"), 
      width = 16, height = 21, units = "cm", res = 300)
  print((p_list_prob[[2]]|p_list_prob[[7]])/
          (p_list_prob[[3]]|p_list_prob[[8]])/
          (p_list_prob[[4]]|p_list_prob[[9]])/
          (p_list_prob[[5]]|p_list_prob[[10]]) + 
          plot_annotation(
            title = 'Posterior probability'
          ) & 
          theme(plot.title = element_text(size = 10)))
  dev.off()
  
  png(paste0(path2save, "Fig3Italy_excessstratified.png"), 
      width = 16, height = 21, units = "cm", res = 300)
  print((p_list_excess[[2]]|p_list_excess[[7]])/
          (p_list_excess[[3]]|p_list_excess[[8]])/
          (p_list_excess[[4]]|p_list_excess[[9]])/
          (p_list_excess[[5]]|p_list_excess[[10]]) + 
          plot_annotation(
            title = 'Proportion of excess death'
          ) & 
          theme(plot.title = element_text(size = 10)))
  dev.off()
  
  #######
  ####### PLOT 4: weekly temporal trend for specific provinces stratified by gender
  #######
  selected_prov = c("BG","BS","VR","FI","FG","CT")
  selected_prov_COD = prov.shp.table %>% filter(SIGLA%in%selected_prov) %>% select(COD_PROV)
  selected_prov_longname = prov.shp.table %>% filter(SIGLA%in%selected_prov) %>% select(DEN_UTS)
  
  # Sum values (predicted and observed) for females and males excluding 40<
  sum.deaths.F0 <- Reduce("+", lapply(pois.samples.list[which(groups4cv$Genere=="F" & groups4cv$CL_ETA!="40<")],
                                     function(X) select(X,starts_with("V"))))
  sum.deaths.F0$ID_PE = pois.samples.list[[1]]$ID_PE
  sum.deaths.F0$EURO_LABEL = pois.samples.list[[1]]$EURO_LABEL
  sum.deaths.M0 <- Reduce("+", lapply(pois.samples.list[which(groups4cv$Genere=="M" & groups4cv$CL_ETA!="40<")],
                                     function(X) select(X,starts_with("V"))))
  sum.deaths.M0$ID_PE = pois.samples.list[[1]]$ID_PE
  sum.deaths.M0$EURO_LABEL = pois.samples.list[[1]]$EURO_LABEL
  
  sum.observed.F0 <- Reduce("+", lapply(pois.samples.list[which(groups4cv$Genere=="F" & groups4cv$CL_ETA!="40<")],
                                       function(X) select(X,"deaths")))
  sum.observed.F0$ID_PE = pois.samples.list[[1]]$ID_PE
  sum.observed.F0$EURO_LABEL = pois.samples.list[[1]]$EURO_LABEL
  sum.observed.M0 <- Reduce("+", lapply(pois.samples.list[which(groups4cv$Genere=="M" & groups4cv$CL_ETA!="40<")],
                                       function(X) select(X,"deaths")))
  sum.observed.M0$ID_PE = pois.samples.list[[1]]$ID_PE
  sum.observed.M0$EURO_LABEL = pois.samples.list[[1]]$EURO_LABEL
  
  for(i in 1:length(selected_prov)){
    
    # Aggregate by week (sum) and compute quantiles for predictions
    sum.deaths.F <- sum.deaths.F0 %>% filter(ID_PE == as.numeric(selected_prov_COD[i,]))
    sum.deaths.F %>% group_by(EURO_LABEL) %>% summarise_all(sum) -> sum.deaths.F
    Y.F <- t(apply(sum.deaths.F[,-1], 1,
                   quantile, probs = c(0.025, seq(from = .1, to = 0.9, by = 0.1), 0.975)))
    Y.F <- as.data.frame(Y.F)
    Y.F$x <- 1:nrow(Y.F)
    Y.F$EURO_LABEL <- sum.deaths.F$EURO_LABEL
    
    sum.deaths.M <- sum.deaths.M0 %>% filter(ID_PE == as.numeric(selected_prov_COD[i,]))
    sum.deaths.M %>% group_by(EURO_LABEL) %>% summarise_all(sum) -> sum.deaths.M
    Y.M <- t(apply(sum.deaths.M[,-1], 1,
                            quantile, probs = c(0.025, seq(from = .1, to = 0.9, by = 0.1), 0.975)))
    Y.M <- as.data.frame(Y.M)
    Y.M$x <- 1:nrow(Y.M)
    Y.M$EURO_LABEL <- sum.deaths.M$EURO_LABEL
      
    # Aggregate by week (sum) and extract observed values 
    sum.observed.F <- sum.observed.F0 %>% filter(ID_PE == as.numeric(selected_prov_COD[i,]))
    sum.observed.F <- data.frame(EURO_LABEL = sum.observed.F$EURO_LABEL, observed = sum.observed.F$deaths)
    sum.observed.F %>% group_by(EURO_LABEL) %>% summarise(observed = sum(observed)) -> sum.observed.F
    sum.observed.F$x <- 1:nrow(sum.observed.F)
    sum.observed.M <- sum.observed.M0 %>% filter(ID_PE == as.numeric(selected_prov_COD[i,]))
    sum.observed.M <- data.frame(EURO_LABEL = sum.observed.M$EURO_LABEL, observed = sum.observed.M$deaths)
    sum.observed.M %>% group_by(EURO_LABEL) %>% summarise(observed = sum(observed)) -> sum.observed.M
    sum.observed.M$x <- 1:nrow(sum.observed.M)
      
    sub <- startsWith(sum.observed.M$EURO_LABEL, "2020")
    
    fcol <- viridis(15)[7]
    lcol <- "black"
    mcol <- viridis(30)[24]
    alpha.level <- 0.002
    
    # here you set the limits for the plots
    ylimsmin <- rep(0, times = 2)
    ylimsmax <- rep(max(sum.observed.M$observed,sum.observed.F$observed), times = 2)
    
    
    # read the EUROSTAT week file
    EUROSTAT <- read_excel("./EUROSTAT_ISO_HMEROLOGIO.xlsx")
    EUROSTAT$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
    EUROSTAT$year <- as.numeric(format(EUROSTAT$EURO_TIME, "%Y"))
    EUROSTAT$month <- format(EUROSTAT$EURO_TIME, "%m")
    EUROSTAT$month.an <- month.abb[as.numeric(EUROSTAT$month)]
    
    euro_weeks <- left_join(data.frame(EURO_LABEL = Y.M$EURO_LABEL, 
                                       x = Y.M$x), EUROSTAT)
    
    
    # 1st lockdown: 	9 March 2020 – 18 May 2020
    #https://it.wikipedia.org/wiki/Gestione_della_pandemia_di_COVID-19_in_Italia
    # 2nd lockdown: 6th November - 
    
    lockdowns <- euro_weeks$EURO_LABEL[euro_weeks$EURO_TIME %in% as.Date(c("2020-03-09", "2020-05-03", "2020-11-06"))]
    lockdowns <- list.observed[[1]]$x[list.observed[[1]]$EURO_LABEL %in% lockdowns]
    lockdowns[4] <- max(list.observed[[1]]$x)
    
    # get the months as the xaxis
    euro_weeks %>% filter(!duplicated(x)) %>% filter(sub) %>% select(x, month.an) -> xaxis
    xaxis <- xaxis[-1,]
    xaxis <- xaxis[!duplicated(xaxis$month.an),]
    
    
    p_F <- PlotTimeSeries(dat = Y.F[sub,], 
                                    lcol = lcol, fcol = fcol, 
                                    dat_true = sum.observed.F[sub,], 
                                    main.title = paste0("Females","40+"), 
                                    lockdowns = lockdowns) + 
        ylim(c(ylimsmin[i], ylimsmax[i])) + 
        # geom_vline(xintercept = lockdowns, linetype = 2) + 
        scale_x_continuous(breaks = xaxis$x, labels = xaxis$month.an,expand = c(0, 0)) + 
        theme(
          axis.text.y = element_text(size=7), 
          axis.text.x = element_text(size=7), 
          plot.title = element_text(size=8)
        )
    p_M <- PlotTimeSeries(dat = Y.M[sub,], 
                          lcol = lcol, fcol = fcol, 
                          dat_true = sum.observed.M[sub,], 
                          main.title = paste0("Males","40+"), 
                          lockdowns = lockdowns) + 
      ylim(c(ylimsmin[i], ylimsmax[i])) + 
      # geom_vline(xintercept = lockdowns, linetype = 2) + 
      scale_x_continuous(breaks = xaxis$x, labels = xaxis$month.an,expand = c(0, 0)) + 
      theme(
        axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        plot.title = element_text(size=8)
      )
    
    
    
    png(paste0(path2save, "Trend",selected_prov_longname[i,],".png"), 
        width = 16, height = 21, units = "cm", res = 300)
    print((p_F|p_M) +
            plot_annotation(
              title = paste0("Province of ", selected_prov_longname[i,])
            ) & 
            theme(plot.title = element_text(size = 10)))
    dev.off()
    
  } #end loop across prov
  
} #end plotting  
  

  
  
  
  

  
  
  
  
  
  
  
#   ################################################################################
#   #6. Plot predictions
#   ################################################################################
#   
#   list.results <- apply(groups4cv, 1,
#                         function(X) readRDS(paste0("./Output/yearCV_", X[1], "_", X[2],"respred2020")))
#   names(list.results) <- paste0(groups4cv[,1], groups4cv[,2])
#   
#   for(j in 1:nrow(groups4cv)){
#     print(j)
#   
#     datainla = list.results[[j]]$mod$.args$data
#     datainla = datainla %>% mutate(row_name = row_number()) 
#     # Add predictions
#     datainla$pred0.025 = list.results[[j]]$mod$summary.fitted.values$`0.025quant`
#     datainla$pred0.5 = list.results[[j]]$mod$summary.fitted.values$`0.5quant`
#     datainla$pred0.75 = list.results[[j]]$mod$summary.fitted.values$`0.975quant`
#     # Extract 2020
#     datainla2020 = datainla %>% filter(Anno==2020) 
#     datainla2020$Obs = list.results[[j]]$true_values
#   
#     prov2plot = "FI"
#     ggplot(datainla2020 %>% filter(province == prov2plot),aes(x=Week)) + 
#       geom_line(aes(y=pred0.5),color="blue") + 
#       geom_ribbon(aes(ymin=pred0.025,ymax=pred0.75),alpha=.2) +
#       geom_point(aes(y=Obs),color="red") +
#       geom_line(aes(y=Obs),color="red") +
#       labs(title = paste0("N. of Deaths for province ",prov2plot),
#            subtitle = paste0(groups4cv[j,1], groups4cv[j,2]))
#     
#     # Compute the mortality rate
#     index = datainla %>% filter(Anno==2020, province == prov2plot) %>% select(row_name) %>% pull()
#     # mortrate_list = lapply(list.results[[j]]$mod$marginals.fitted.values[index],
#     #              function(X){
#     #                marg = inla.tmarginal(function(x) {x/mean(datainla$popfin[index])*100000}, X)
#     #                inla.qmarginal(c(0.025,0.5,0.975), marg)
#     #              })
#     
#     mortrate_list = list()
#     for(i in 1:length(list.results[[j]]$mod$marginals.fitted.values[index])){
#       marg = inla.tmarginal(mar=list.results[[j]]$mod$marginals.fitted.values[[index[i]]],
#                             fun=function(x) {x/datainla$popfin[index[i]]*100000})
#       mortrate_list[[i]] = inla.qmarginal(c(0.025,0.5,0.975), marg)
#     }
#     
#     mortrate_df = data.frame(matrix(unlist(mortrate_list),
#                             nrow=length(mortrate_list), byrow=TRUE),
#                     stringsAsFactors=FALSE)
#     colnames(mortrate_df) = c("pred0.025","pred0.5","pred0.975")
#     mortrate_df$Week = seq(1:nrow(mortrate_df))
#     mortrate_df$Obs = datainla2020 %>% 
#       filter(province == prov2plot) %>% 
#       mutate(obs_mortrate = Obs/popfin*100000) %>% 
#       select(obs_mortrate) %>% pull()
#                  
#     ggplot(mortrate_df,aes(x=Week)) + 
#       geom_line(aes(y=pred0.5),color="blue") + 
#       geom_ribbon(aes(ymin=pred0.025,ymax=pred0.975),alpha=.2) +
#       geom_point(aes(y=Obs),color="red") +
#       geom_line(aes(y=Obs),color="red") +
#       labs(title = paste0("Mortality rate x 100000",prov2plot),
#            subtitle = paste0(groups4cv[j,1], groups4cv[j,2]))
#     
#                  
#     
#       
#       
#   
#   }
# 
# ################################################################################
# 
