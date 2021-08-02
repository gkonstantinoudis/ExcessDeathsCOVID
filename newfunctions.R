################################################################################
# Cross validation function
################################################################################

# function to fit INLA and store the cv
inla.cv <- function(Y){
  
  in.mod <- 
    inla(formula = as.formula(Y),
         data=datCV,
         family="Poisson",  
         verbose = TRUE, 
         control.family=control.family,
         control.compute=list(config = TRUE), 
         control.mode=list(theta=m$mode$theta, restart=TRUE),
         num.threads = round(parallel::detectCores()*.8), 
         control.predictor = list(link = 1)
    )
  
  
  
  post.samples <- inla.posterior.sample(n = 1000, result = in.mod)
  
  # store the results
  list.CV.results <- list(
    prediction = lapply(post.samples, function(X) exp(X$latent[testIndexes])),
    true_values =  datCV_firslop$deaths[testIndexes]
  )
  
  return(list.CV.results)
  
}



# Function for computing correlation & coverage probability
postpredchecks <- function(Y){
  
  pred.samples <- lapply(Y, function(X) do.call(cbind, X$prediction))
  true_values <- do.call(c, lapply(Y, function(X) X$true_values))
  
  # samples from a Poisson
  set.seed(11)
  pois.samples <- lapply(pred.samples,
                         function(Z) apply(Z, 2, function(X) rpois(n = length(X), lambda = X)))
  
  # calculate quantiles
  pois.quant <- as.data.frame(do.call(rbind, lapply(pois.samples, 
                                                    function(X) t(apply(X, 1, function(Z) quantile(Z, probs = c(0.025, 0.975)))))))
  cov.prob = mean((pois.quant$`2.5%` <= true_values) & (pois.quant$`97.5%` > true_values))
  
  
  res <- list(
    correl = apply(do.call(rbind, pois.samples), 2, function(X) cor(X, true_values)), 
    cov.prob = mean((pois.quant$`2.5%` <= true_values) & (pois.quant$`97.5%` > true_values)) # if you exclude 0s is very low
  )
  
  return(res)
  
}
################################################################################
# Temporal trends plot
################################################################################

PlotTimeSeries <- function(dat, dat_true, fcol, lcol, main.title, lockdowns){
  
  # dat should be a dataframe with the following quantiles: c(0.025, seq(from = .1, to = 0.9, by = 0.1), 0.975)
  # data_true a dataframe with the observed number of deaths and the week index
  # fcol is the colour of the time series
  # lcol the colour of the observed
  # main.title is self explanatory
  
  
  
  ggplot(data = dat) +
    
    geom_rect(aes(xmin = lockdowns[1], xmax = lockdowns[2], ymin = -Inf, ymax = Inf), 
              alpha = alpha.level,  fill = mcol) + 
    
    geom_rect(aes(xmin = lockdowns[3], xmax = lockdowns[4], ymin = -Inf, ymax = Inf), 
              alpha = alpha.level, fill = mcol) +
    
    geom_line(aes(x = x, y = `50%`)) + 
    
    geom_ribbon(aes(x = x,
                    ymin=`2.5%`, 
                    ymax=`97.5%`), 
                linetype=0, alpha=0.2,
                fill = fcol) +
    
    geom_ribbon(aes(x = x,
                    ymin=`10%`, 
                    ymax=`90%`), 
                linetype=0, alpha=0.2,
                fill = fcol) +
    
    geom_ribbon(aes(x = x,
                    ymin=`20%`, 
                    ymax=`80%`), 
                linetype=0, alpha=0.3,
                fill = fcol) +
    
    geom_ribbon(aes(x = x,
                    ymin=`30%`, 
                    ymax=`70%`), 
                linetype=0, alpha=0.3,
                fill = fcol) +
    
    geom_ribbon(aes(x = x,
                    ymin=`40%`, 
                    ymax=`60%`), 
                linetype=0, alpha=0.4,
                fill = fcol) + 
    
    geom_line(data = dat_true, color = lcol, size = 0.4, aes(x= x, y = observed))  + 
    geom_point(data = dat_true, color = lcol, size = 0.55, aes(x= x, y = observed)) + theme_bw() + 
    ggtitle(main.title) + xlab("") + ylab("")
}





################################################################################
# Compute density for the caterpillar plot
################################################################################

calc_density <- function(Z){
  
  # create the densities
  #apply(Z[, -which(colnames(Z) %in% c("NAMES", "observed", "x.median"))], 1, density) -> dens.list
  apply(Z %>% dplyr::select(starts_with("xs")),1,density) -> dens.list
  npoints <- length(dens.list[[1]]$x)
  #nypos <- N + 1 # plus one because you have the total too
  nypos <- nrow(Z)
  
  data.frame(x = do.call(c, lapply(dens.list, function(X) X$x)),
             dens = do.call(c, lapply(dens.list, function(X) X$y)),
             ypos = rep(1:nypos, each = npoints), 
             NAMES = rep(Z$NAMES, each = npoints), 
             #NAMES = rep(Z$DEN_REG, each = npoints), 
             #x.median = rep(Z$x.median, each = npoints)
             x.median = rep(Z$median.excess, each = npoints)
             ) -> datplot
  
  
  datplot$maxdens <- rep(with(datplot, tapply(dens, factor(ypos, levels=unique(ypos)), max)), each=npoints)
  datplot$dens.unscaled <- datplot$dens / datplot$maxdens
  
  datplot$ypos2 <- as.numeric(as.factor(datplot$x.median))
  
  return(datplot)
  
}

################################################################################
# Function for producing the caterpillar plot
################################################################################

plotstrips <- function(Z){
  
  # set the plot
  gPlot <- ggplot() +  
    ylim(c(0, c(N+2))) + 
    #xlim(range(Z$x)) +
    xlim(c(-0.4, 0.4)) + 
    scale_fill_continuous(low="white", high=viridis(15)[7], name = "") + 
    theme_bw()
  
  for(i in 1:c(N+1)){
    gPlot <- gPlot + 
      geom_tile(data = Z[Z$ypos2 == i,],
                aes(x = x, y = ypos, fill =  dens.unscaled), height=1)
  }
  
  
  Z %>% filter(!duplicated(NAMES)) -> x.means
  
  
  gPlot + 
    geom_vline(xintercept = 0, col = "red", linetype = 2, size = 0.5) + 
    geom_point(data = x.means, aes(x = x.median, y = ypos), size = .8) +
    theme(legend.position = "none") + 
    scale_y_continuous(breaks = 1:c(N+1),
                       labels = unique(Z$NAMNUTS2), 
                       expand = expansion(mult = c(0, 0))) + 
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_blank(), 
      panel.grid.minor.x = element_blank(), 
      panel.grid.minor.y = element_line( size=.1, color="black" ),
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE, 
      axis.text.y = element_text(size=7), 
      axis.text.x = element_text(size=7), 
      plot.title = element_text(size=8),
      
    ) + ylab("") + xlab("") -> p2
  
  return(p2)
}

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
      legend.spacing.x = unit(0, "mm"))

################################################################################
# Function for extracting data for 2020 (yearly total)
################################################################################

get2020data = function(post.samples = pois.samples.list, geo.res, link_table=NULL, stratify.by,country="Italy"){
# geo.res = c("province","region","country)
# stratify.by = c("none","age","sex","agesex")   
# link_table is a data frame with NAMNUTS2 (name NUTS2 region) and ID_space (ID of the NUTS3 region as given in the finaldb)
# and RegionID (the NUTS2 region ID)
# groups4cv is the data frame with the 10 combinations of age x sex groups
  
  
  if(length(post.samples) == 8){
    groups4cv <- as.data.frame(expand.grid(age = c("40-59", "60-69", "70-79", "80+"),
                                           sex = c("F", "M")))
  }
  
  if(length(post.samples) == 10){
    groups4cv <- as.data.frame(expand.grid(age = c("40<", "40-59", "60-69", "70-79", "80+"),
                                           sex = c("F", "M")))
  }
  
  # Indexes for 2020 observations
  sub2020 = startsWith(post.samples[[1]]$EURO_LABEL, "2020")
  post.samples = lapply(post.samples, function(X) X[sub2020,])
  
  # For each i in 1:10 extract only the necessary data (1000 simulations + observed data)
  if(geo.res == "province"){
    pblapply(post.samples, function(X){
      
      X <- select(X, starts_with("V"), "ID_space") %>%
            group_by(ID_space) %>%
            summarise_all(sum)
      return(X)
    }) -> list.sum.deaths
    
    pblapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, observed = X$deaths)
      Y <- X %>% 
            group_by(ID_space) %>% 
            summarise(observed = sum(observed))
      return(Y)
    }) -> list.observed
    
    pblapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, population = X$population, year = X$year)
      Y <- X %>% 
        group_by(ID_space) %>% 
        summarise(population = mean(population)) 
      return(Y)
    }) -> list.population
  }
  
  # For each i in 1:10 aggregate predicted/observed values by region 
  if(geo.res == "region"){
    pblapply(post.samples, function(X){
      
      X <- select(X, starts_with("V"), "ID_space")
      X <- left_join(X, link_table, by = c("ID_space"))
      
      X %>% 
        select(starts_with("V"), "RegionID") %>% 
        group_by(RegionID) %>%
        summarise_all(sum) %>% 
        ungroup()-> X
      X <- as.data.frame(X)
      X[,-1] <- apply(X[,-1], 2, as.numeric)
      
      return(X)
      
      }) -> list.sum.deaths
    

    pblapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, observed = X$deaths)
      X <- left_join(X, link_table, by = c("ID_space"))

      X %>% group_by(RegionID) %>% summarise(observed = sum(observed)) -> Y
      Y$observed <- as.numeric(Y$observed)
      return(Y)
    }) -> list.observed
    
    pblapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, population = X$population)
      X <- left_join(X, link_table, by = c("ID_space"))
      Y <- X %>% 
        group_by(RegionID) %>% 
        summarise(population = mean(population))
      return(Y)
    }) -> list.population
  
  }
  
  # For each i in 1:10 aggregate predicted/observed values to get the total for the country
  if(geo.res == "country"){
    pblapply(post.samples, function(X){
      X %>% 
        dplyr::select(starts_with("V")) %>% 
        summarise_all(sum) -> X
      X <- as.data.frame(X)
      X$COUNTRY = country
      return(X)
    }) -> list.sum.deaths
    
    pblapply(post.samples, function(X){
      X %>% summarise(observed = sum(deaths)) -> Y
      Y$COUNTRY = country
      return(Y)
    }) -> list.observed
    
    pblapply(post.samples, function(X){
      X %>% group_by(ID_space) %>% summarise(population = mean(population)) -> Y
      Y %>% summarise(population = sum(population)) -> Y
      Y$COUNTRY = country
      return(Y)
    }) -> list.population
  }
  
  # Aggregate by age and sex  
  if(stratify.by == "none"){
    
    sum.deaths <- Reduce("+", lapply(list.sum.deaths,
                                      function(X) select(X, starts_with("V"))))
    if(geo.res=="province") sum.deaths$ID_space <- list.sum.deaths[[1]]$ID_space
    if(geo.res=="region") sum.deaths$RegionID <- list.sum.deaths[[1]]$RegionID
    if(geo.res=="country") sum.deaths$COUNTRY <- country
   
      
    sum.observed <- Reduce("+", lapply(list.observed,
                                         function(X) select(X, "observed")))
    if(geo.res=="province") sum.observed$ID_space <- list.observed[[1]]$ID_space
    if(geo.res=="region") sum.observed$RegionID <- list.observed[[1]]$RegionID
    if(geo.res=="country") sum.observed$COUNTRY <- country
    
    sum.population <- Reduce("+", lapply(list.population,
                                       function(X) select(X, "population")))
    if(geo.res=="province") sum.population$ID_space <- list.population[[1]]$ID_space
    if(geo.res=="region") sum.population$RegionID <- list.population[[1]]$RegionID
    if(geo.res=="country") sum.population$COUNTRY <- country
      
    #sum.deaths.obs = left_join(sum.deaths, sum.observed) #memory exhausted
    sum.observed <- left_join(sum.observed, sum.population)
    sum.deaths.obs = data.frame(sum.deaths,sum.observed)
    if(sum(endsWith(colnames(sum.deaths.obs),".1")) != 0){
      sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
    }
    out = sum.deaths.obs
      
    return(out)
  }
  

  # Aggregate by age 
  if(stratify.by == "sex"){
    
    out = list()
    for(i in 1:n_distinct(groups4cv$sex)){
      sum.deaths <- Reduce("+", lapply(list.sum.deaths[groups4cv$sex == levels(groups4cv$sex)[i]],
                                     function(X) select(X, starts_with("V"))))
      if(geo.res=="province") sum.deaths$ID_space <- list.sum.deaths[[1]]$ID_space
      if(geo.res=="region") sum.deaths$RegionID <- list.sum.deaths[[1]]$RegionID
      if(geo.res=="country") sum.deaths$COUNTRY <- country
      
      
      sum.observed <- Reduce("+", lapply(list.observed[groups4cv$sex == levels(groups4cv$sex)[i]],
                                           function(X) select(X, "observed")))
      if(geo.res=="province") sum.observed$ID_space <- list.observed[[1]]$ID_space
      if(geo.res=="region") sum.observed$RegionID <- list.observed[[1]]$RegionID
      if(geo.res=="country") sum.observed$COUNTRY <- country
      
      sum.population <- Reduce("+", lapply(list.population[groups4cv$sex == levels(groups4cv$sex)[i]],
                                           function(X) select(X, "population")))
      if(geo.res=="province") sum.population$ID_space <- list.population[[1]]$ID_space
      if(geo.res=="region") sum.population$RegionID <- list.population[[1]]$RegionID
      if(geo.res=="country") sum.population$COUNTRY <- country
      
      sum.observed <- left_join(sum.observed, sum.population)
      #sum.deaths.obs = left_join(sum.deaths, sum.observed) #memory exhausted
      sum.deaths.obs = data.frame(sum.deaths,sum.observed)

      if(sum(endsWith(colnames(sum.deaths.obs),".1")) != 0){
        sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
      }
      out[[i]] = sum.deaths.obs
      
    }
    names(out) = levels(groups4cv$sex)
    
    return(out)
  }

  # Aggregate by sex 
  if(stratify.by == "age"){
    
    out = list()
    for(i in 1:n_distinct(groups4cv$age)){
      sum.deaths <- Reduce("+", lapply(list.sum.deaths[groups4cv$age == levels(groups4cv$age)[i]],
                                       function(X) select(X, starts_with("V"))))
      if(geo.res=="province") sum.deaths$ID_space <- list.sum.deaths[[1]]$ID_space
      if(geo.res=="region") sum.deaths$RegionID <- list.sum.deaths[[1]]$RegionID
      if(geo.res=="country") sum.deaths$COUNTRY <- country
      
      
      sum.observed <- Reduce("+", lapply(list.observed[groups4cv$age == levels(groups4cv$age)[i]],
                                         function(X) select(X, "observed")))
      if(geo.res=="province") sum.observed$ID_space <- list.observed[[1]]$ID_space
      if(geo.res=="region") sum.observed$RegionID <- list.observed[[1]]$RegionID
      if(geo.res=="country") sum.observed$COUNTRY <- country
      
      
      sum.population <- Reduce("+", lapply(list.population[groups4cv$age == levels(groups4cv$age)[i]],
                                         function(X) select(X, "population")))
      if(geo.res=="province") sum.population$ID_space <- list.population[[1]]$ID_space
      if(geo.res=="region") sum.population$RegionID <- list.population[[1]]$RegionID
      if(geo.res=="country") sum.population$COUNTRY <- country
      
      #sum.deaths.obs = left_join(sum.deaths, sum.observed)
      sum.observed <- left_join(sum.observed, sum.population)
      sum.deaths.obs = data.frame(sum.deaths, sum.observed)
      if(sum(endsWith(colnames(sum.deaths.obs),".1")) != 0){
        sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
      }
      out[[i]] = sum.deaths.obs
    }
    names(out) = levels(groups4cv$age)
    
    return(out)
  }
  
  #  don't aggregate
  if(stratify.by == "agesex"){
    
    #out = Map(dplyr::left_join, list.sum.deaths, list.observed)
    list.obs <- list()
    
    for(i in 1:length(post.samples)){
      list.obs[[i]] <- left_join(list.observed[[i]], list.population[[i]])
    }
    
    out = Map(data.frame, list.sum.deaths, list.obs)
    out = lapply(out,function(X) {X=X %>% select(-ends_with(".1"))})
           
    names(out) = paste0(groups4cv$sex,groups4cv$age)
    return(out)
  }

}
  

  
################################################################################
# Function for extracting data for 2020 (weekly total)
################################################################################

get2020weeklydata = function(post.samples = pois.samples.list, geo.res, link_table=NULL, stratify.by,country="Italy"){
  # geo.res = c("province","region","country)
  # stratify.by = c("none","age","sex","agesex")   
  # link_table is a data frame with ID_PE (province code) and DEN_REG (region name). required only if geo.res="region"
  # groups4cv is the data frame with the 10 combinations of age x sex groups
  
  groups4cv <- as.data.frame(expand.grid(age = c("40<", "40-59", "60-69", "70-79", "80+"),
                                         sex = c("F", "M")))
  
  
  # Indexes for 2020 observations
  sub2020 = startsWith(post.samples[[1]]$EURO_LABEL, "2020")
  post.samples = lapply(post.samples, function(X) X[sub2020,])
  
  
  # For each i in 1:10 extract only the necessary data (1000 simulations + observed data)
  if(geo.res == "province"){
    pblapply(post.samples, function(X){
      X <- select(X, starts_with("V"), "ID_PE", "EURO_LABEL")
      return(X)
    }) -> list.sum.deaths
    
    pblapply(post.samples, function(X){
      X <- data.frame(ID_PE = X$ID_PE, observed = X$deaths, EURO_LABEL=X$EURO_LABEL)
      return(X)
    }) -> list.observed
  }
  
  # For each i in 1:10 aggregate predicted/observed values by region 
  if(geo.res == "region"){
    pblapply(post.samples, function(X){
      
      X <- select(X, starts_with("V"), "ID_PE","EURO_LABEL")
      X <- left_join(X, link_table, by = c("ID_PE"))
      
      X %>% 
        select(starts_with("V"), "DEN_REG","EURO_LABEL") %>% 
        group_by(DEN_REG,EURO_LABEL) %>%
        summarise_all(sum) %>% 
        ungroup() -> X
      X <- as.data.frame(X)
      #X[,-1] <- apply(X[,-1], 2, as.numeric)
      
      return(X)
      
    }) -> list.sum.deaths
    
    pblapply(post.samples, function(X){
      X <- data.frame(ID_PE = X$ID_PE, observed = X$deaths,EURO_LABEL=X$EURO_LABEL)
      X <- left_join(X, link_table, by = c("ID_PE"))
      
      X %>% 
        group_by(DEN_REG,EURO_LABEL) %>%
        summarise(observed = sum(observed)) %>% 
        ungroup() -> Y
      Y$observed <- as.numeric(Y$observed)
      return(Y)
    }) -> list.observed
  }
  
  # For each i in 1:10 aggregate predicted/observed values to get the total for the country
  if(geo.res == "country"){
    pblapply(post.samples, function(X){
      
      X %>% 
        select(starts_with("V"),"EURO_LABEL") %>% 
        group_by(EURO_LABEL) %>% 
        summarise_all(sum) %>%  
        ungroup() -> X
      X <- as.data.frame(X)
      X$COUNTRY = country
      return(X)
    }) -> list.sum.deaths
    
    pblapply(post.samples, function(X){
      X %>% 
        group_by(EURO_LABEL) %>% 
        summarise(observed = sum(deaths)) %>% 
        ungroup() -> Y
      Y$COUNTRY = country
      return(Y)
    }) -> list.observed
  }
  
  # Aggregate by age and sex  
  if(stratify.by == "none"){
    
    sum.deaths <- Reduce("+", lapply(list.sum.deaths,
                                     function(X) select(X, starts_with("V"))))
    sum.deaths$EURO_LABEL = list.sum.deaths[[1]]$EURO_LABEL
    if(geo.res=="province") sum.deaths$ID_PE <- list.sum.deaths[[1]]$ID_PE
    if(geo.res=="region") sum.deaths$DEN_REG <- list.sum.deaths[[1]]$DEN_REG
    if(geo.res=="country") sum.deaths$COUNTRY <- country
    
    
    sum.observed <- Reduce("+", lapply(list.observed,
                                       function(X) select(X,observed)))
    sum.observed$EURO_LABEL = list.observed[[1]]$EURO_LABEL
    if(geo.res=="province") sum.observed$ID_PE <- list.observed[[1]]$ID_PE
    if(geo.res=="region") sum.observed$DEN_REG <- list.observed[[1]]$DEN_REG
    if(geo.res=="country") sum.observed$COUNTRY <- country
    
    #sum.deaths.obs = left_join(sum.deaths, sum.observed) #memory exhausted
    sum.deaths.obs = data.frame(sum.deaths,sum.observed)
    sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
    out = sum.deaths.obs
    
    return(out)
  }
  
  
  
  # Aggregate by age 
  if(stratify.by == "sex"){
    
    out = list()
    for(i in 1:n_distinct(groups4cv$sex)){
      sum.deaths <- Reduce("+", lapply(list.sum.deaths[groups4cv$sex == levels(groups4cv$sex)[i]],
                                       function(X) select(X, starts_with("V"))))
      sum.deaths$EURO_LABEL = list.sum.deaths[[1]]$EURO_LABEL
      if(geo.res=="province") sum.deaths$ID_PE <- list.sum.deaths[[1]]$ID_PE
      if(geo.res=="region") sum.deaths$DEN_REG <- list.sum.deaths[[1]]$DEN_REG
      if(geo.res=="country") sum.deaths$COUNTRY <- country
      
      
      sum.observed <- Reduce("+", lapply(list.observed[groups4cv$sex == levels(groups4cv$sex)[i]],
                                         function(X) select(X, observed)))
      sum.observed$EURO_LABEL = list.observed[[1]]$EURO_LABEL
      if(geo.res=="province") sum.observed$ID_PE <- list.observed[[1]]$ID_PE
      if(geo.res=="region") sum.observed$DEN_REG <- list.observed[[1]]$DEN_REG
      if(geo.res=="country") sum.observed$COUNTRY <- country
      
      #sum.deaths.obs = left_join(sum.deaths, sum.observed) #memory exhausted
      sum.deaths.obs = data.frame(sum.deaths,sum.observed)
      sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
      out[[i]] = sum.deaths.obs
      
    }
    names(out) = levels(groups4cv$sex)
    
    return(out)
  }
  
  # Aggregate by sex 
  if(stratify.by == "age"){
    
    out = list()
    for(i in 1:n_distinct(groups4cv$age)){
      sum.deaths <- Reduce("+", lapply(list.sum.deaths[groups4cv$age == levels(groups4cv$age)[i]],
                                       function(X) select(X, starts_with("V"))))
      sum.deaths$EURO_LABEL = list.sum.deaths[[1]]$EURO_LABEL
      if(geo.res=="province") sum.deaths$ID_PE <- list.sum.deaths[[1]]$ID_PE
      if(geo.res=="region") sum.deaths$DEN_REG <- list.sum.deaths[[1]]$DEN_REG
      if(geo.res=="country") sum.deaths$COUNTRY <- country
      
      
      sum.observed <- Reduce("+", lapply(list.observed[groups4cv$age == levels(groups4cv$age)[i]],
                                         function(X) select(X, observed)))
      sum.observed$EURO_LABEL = list.observed[[1]]$EURO_LABEL
      if(geo.res=="province") sum.observed$ID_PE <- list.observed[[1]]$ID_PE
      if(geo.res=="region") sum.observed$DEN_REG <- list.observed[[1]]$DEN_REG
      if(geo.res=="country") sum.observed$COUNTRY <- country
      
      #sum.deaths.obs = left_join(sum.deaths, sum.observed)
      sum.deaths.obs = data.frame(sum.deaths,sum.observed)
      sum.deaths.obs = sum.deaths.obs %>% select(- ends_with(".1"))
      out[[i]] = sum.deaths.obs
    }
    names(out) = levels(groups4cv$age)
    
    return(out)
  }
  
  #  don't aggregate
  if(stratify.by == "agesex"){
    
    #out = Map(dplyr::left_join, list.sum.deaths, list.observed)
    out = Map(data.frame, list.sum.deaths, list.observed)
    out = lapply(out,function(X) {X=X %>% select(-ends_with(".1"))})
    
    names(out) = paste0(groups4cv$sex,groups4cv$age)
    return(out)
  }
  
}

################################################################################
# Compute excess mortality
################################################################################


compute.excess = function(data,divide.by,remove.covid=NULL,geo.name){
  
  #data is a dataframe which contains:
  # - the 1000 predicted values with colnames starting with V 
  # - observed deaths (observed)
  # - population (if divide.by="pop")
  # - the name of the column containing the reference to the spatial unit (e.g. "RegionID")
  
  #divide.by = c("pred","pop")

  # Compute relative total excess deaths:
  # - (obs-pred)/pred if divide.by="pred"
  # - (pop-pred)/pop if divide.by="pop"
  
  # The function returns the 1000 values of the excess mortality rate + 
  # posterior mean, sd and (0.025, 0.975) quantiles
  
  #remove.covid If you have data about covid deaths and you want to remove them from 
  # the observed number of cases then **remove.covid** is the name
  # of the column containing the covid deaths
  
  #geo.name is the name of the column containing the geographical information
  
  
    if(is.null(remove.covid)){
      xs=-1*sweep(as.matrix(data %>% select(starts_with("V"))),
                  1,data$observed,FUN ="-")
    } else {
      covid_cases = data %>% select(remove.covid) %>% pull()
      xs=-1*sweep(as.matrix(data %>% select(starts_with("V"))),
                  1,data$observed-covid_cases,FUN ="-")
    }
    if(divide.by=="pred"){
      xs=as.matrix(xs)/as.matrix(data %>% select(starts_with("V")))
    }else{
      xs=sweep(as.matrix(xs),1,data$population,FUN ="/")
    }
    colnames(xs)=stringr::str_replace(colnames(xs),"V","xs")
    xs = as_tibble(xs)
    
    #xs$DEN_REG = data$DEN_REG
    xs[,geo.name] = data[,geo.name]
    
    # Compute posterior summary statistics
    xs = xs %>% mutate(
      mean.excess=apply(as.matrix(xs %>% select(contains("xs"))),1,mean,na.rm=T),
      median.excess=apply(as.matrix(xs %>% select(contains("xs"))),1,median,na.rm=T),
      sd.excess=apply(as.matrix(xs %>% select(contains("xs"))),1,sd,na.rm=T),
      low.excess=apply(as.matrix(xs %>% select(contains("xs"))),1,quantile,.025,na.rm=T),
      upp.excess=apply(as.matrix(xs %>% select(contains("xs"))),1,quantile,.975,na.rm=T))
    
    return(xs)
}


################################################################################
# Credible intervals function
################################################################################

CrI <- function(X) paste0(X[1], " ", "(", X[2], ", ", X[3], ")")




################################################################################
# Compute excess deaths
################################################################################

# Code taken from 'compute.excess' to obtain summary stats on the
#  estimated excess deaths.
compute.excess.deaths = function(data, remove.covid = NULL, geo.name){

    if(is.null(remove.covid)){
      xs=-1*sweep(as.matrix(data %>% select(starts_with("V"))),
                  1,data$observed,FUN ="-")
    } else {
      covid_cases = data %>% select(remove.covid) %>% pull()
      xs=-1*sweep(as.matrix(data %>% select(starts_with("V"))),
                  1,data$observed-covid_cases,FUN ="-")
    }

    colnames(xs)=stringr::str_replace(colnames(xs),"V","xsd")
    xs = as_tibble(xs)
   
    #xs$DEN_REG = data$DEN_REG
    xs[,geo.name] = data[,geo.name]
   
    # Compute posterior summary statistics
    xs = xs %>% mutate(
      mean.excess.deaths=apply(as.matrix(xs %>% select(contains("xsd"))),1,mean,na.rm=T),
      median.excess.deaths=apply(as.matrix(xs %>% select(contains("xsd"))),1,median,na.rm=T),
      sd.excess.deaths=apply(as.matrix(xs %>% select(contains("xsd"))),1,sd,na.rm=T),
      low.excess.deaths=apply(as.matrix(xs %>% select(contains("xsd"))),1,quantile,.025,na.rm=T),
      upp.excess.deaths=apply(as.matrix(xs %>% select(contains("xsd"))),1,quantile,.975,na.rm=T))

    return(xs)
}





################################################################################
################################################################################
################################################################################
################################################################################
################################################################################



