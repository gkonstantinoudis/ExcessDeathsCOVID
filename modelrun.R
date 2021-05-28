remove(list=ls())
library(tidyverse)
library(INLA)
library(sf)
library(dplyr)
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


# 2. The files of the shapefile of interest. The shp should have a column called ID_PE that corresponds to the 
# ID_space in the finaldb.
shp = read_sf("./data/shp.shp")

W.nb <- poly2nb(shp)
nb2INLA("W.adj", W.nb) 


if(run.CV | run.mod.pred){
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
  data_start = data %>% filter(age.group=="less40",sex=="female")
  
  
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
    sexg <- as.character(groups4cv$sex[j])
    data %>% filter(sex == sexg, 
                    age.group == ageg) -> datCV_firslop
  

    # recreate indexes
    data$id.space = as.numeric(as.factor(data$ID_space))
    data$id.time <- as.numeric(substr(data$EURO_LABEL, start = 7, stop = 8))
    data$id.tmp <- inla.group(data$mean.temperature, n = 100, method = "cut", idx.only = TRUE)
    data$id.year <- data$year - 2014
    data$id.year <- as.numeric(as.factor(data$id.year))
    
    list.CV.results.spacetime <- list()
    
    
    for(i in 1:5){
      
      datCV <- datCV_firslop
      testIndexes <- which(datCV$year %in% years[i], arr.ind=TRUE)
      datCV$deaths[testIndexes] <- NA
      
      list.CV.results.spacetime[[i]] <- inla.cv(Y = formula)
      gc()
      
    }

    saveRDS(list.CV.results.spacetime,
            file = paste0(path2save, paste0("yearCV_", ageg, "_", sexg)))
    
  }
  
  t_1 <- Sys.time()
  print(t_1 - t_0) # 4 hours for Greece, 3h for Switzerland
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
  data$id.space <- as.numeric(as.factor(data$ID_space))
  data$id.time <- as.numeric(substr(data$EURO_LABEL, start = 7, stop = 8))
  data$id.tmp <- inla.group(data$mean.temperature, n = 100, method = "cut", idx.only = TRUE)
  data$id.year <- data$year - 2014
  
  
  t_0 <- Sys.time()
  for(j in 1:nrow(groups4cv)){
    
    print(j)
    
    ageg <- as.character(groups4cv$age.group[j])
    sexg <- as.character(groups4cv$sex[j])
    data %>% filter((age.group %in% ageg) & (sex %in% sexg)) -> datCV_firslop
    
    truth <- datCV_firslop$deaths[datCV_firslop$year > 2019] 
    datCV_firslop$deaths[datCV_firslop$year > 2019] <- NA
    
    
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
                                    paste0("yearCV_", ageg, "_", sexg, "respred2020")))
    
  }
  
  t_1 <- Sys.time()
  print(t_1 - t_0) # 40 minutes for Greece, 20 for Switzerland
}  

####### 5. Poisson Samples for 2015-2020 ----
# The output from Step 4. is required

if(Poisson.sampling2020){
  res.list <- apply(groups4cv, 1,
                    function(X) readRDS(paste0(path2save,
                                               paste0("yearCV_", X[1], "_", X[2], "respred2020"))))
  
  data <- finaldb
  # Create indexes
  data$id.space <- as.numeric(as.factor(data$ID_space))
  data$id.time <- as.numeric(substr(data$EURO_LABEL, start = 7, stop = 8))
  data$id.tmp <- inla.group(data$mean.temperature, n = 100, method = "cut", idx.only = TRUE)
  data$id.year <- data$year - 2014
  
  # get the poisson samples
  pois.samples.list <- list()
  
  
  for(j in 1:nrow(groups4cv)){
    
    print(j)
    
    ageg <- as.character(groups4cv$age.group[j])
    sexg <- as.character(groups4cv$sex[j])
    data %>% filter((age.group %in% ageg) & (sex %in% sexg)) -> dat_tmp
    
    set.seed(11)
    post.samples <- inla.posterior.sample(n = 1000, result = res.list[[j]]$mod)
    predlist <- do.call(cbind,
                        lapply(post.samples,
                               function(X) exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
    
    pois.samples <- apply(predlist, 2,
                          function(Z) rpois(n = length(Z), lambda = Z))
    pois.samples <- as.data.frame(pois.samples)
    
    pois.samples$EURO_LABEL <- dat_tmp$EURO_LABEL
    pois.samples$ID_space <- dat_tmp$ID_space
    pois.samples$deaths <- dat_tmp$deaths 
    pois.samples$population <- dat_tmp$population 
    pois.samples$year <- dat_tmp$year
    pois.samples <- pois.samples[pois.samples$year %in% max(pois.samples$year),]
    
    pois.samples.list[[j]] <- pois.samples
    
}

  saveRDS(pois.samples.list,
        file = paste0(path2save, "poisson_samples_all"))

}

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

