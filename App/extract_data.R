# Extracts data for the App


library(readxl)
library(sp)
library(sf)
library(xts)
library(spacetime)
library(RColorBrewer)
#library(leafpop)
library(plotly)
library(grid)
library(dplyr)
library(pbapply)
library(viridis)


# Age-sex groups
agesex_grps <- expand.grid(age = c("40<", "40-59", "60-69", "70-79", "80+"),
  sex = c("Females", "Males"))
# Add index (to select data)
agesex_grps$IDX <- 1:nrow(agesex_grps)

# Load data
DB <- list()
# Country names using ISO code
DB$CHE <- readRDS("data/CHE/poisson_samples_all")

# Read maps
MAPS <- list()
MAPS$CHE <- st_read("data/CHE/shp.shp") #readOGR("data/CHE/shp.shp")
MAPS$CHE <- st_transform(MAPS$CHE, '+proj=longlat +datum=WGS84') 

# Number of weeks
n.weeks <- length(unique(DB$CHE[[1]]$EURO_LABEL))

# Nubmer of regions
n.regions <- nrow(MAPS$CHE)

# Link table
link_table = readRDS("./data/CHE/link_caterpilar")


# Load functions (written by Akis)
source("../ExcessDeathsCOVID/newfunctions.R")


# Merge map and link table
MAPS$CHE <- merge(MAPS$CHE, link_table, by.x = "ID_PE", by.y = "ID_space")
MAPS$CHE$COUNTRY <- "Switzerland"

MAPS$CHE <- list(
  country = MAPS$CHE %>% group_by(COUNTRY) %>% summarise() %>% st_simplify(dTolerance = 500),
  region = MAPS$CHE %>% group_by(RegionID) %>% summarise() %>% st_simplify(dTolerance = 500),
  province = MAPS$CHE %>% st_simplify(dTolerance = 500)
)

# Add NAME
MAPS$CHE$country$NAME <- "Switzerland"
MAPS$CHE$region <- merge(MAPS$CHE$region, unique(link_table[, c(1, 3)])) %>%
  rename(NAME = NAMNUTS2)


# Change space ID
MAPS$CHE$province <- rename(MAPS$CHE$province, ID_space = ID_PE)
  

# Define strata for aggregation (argument 'stratify.by')
strata <-  c("none","age","sex","agesex")  

geo.res <- c("country", "region", "province")# Define geographical resolution ('geo.res' argument)


# RE-sale REM variables to be as percentage
# obj: Object with data (tidy object)
rescale_REM <- function(obj) {
    mutate(obj, mean.excess = 100 * mean.excess,          
      median.excess = 100 * median.excess,
      sd.excess = 100 * sd.excess,
      low.excess = 100 * low.excess,
      upp.excess = 100 * upp.excess
    )
}

d <- lapply(geo.res, function(GEO) {
  res <- lapply(strata, function(STRATA) {
    aux <- get2020data(DB$CHE, geo.res = GEO, link_table = link_table,
      stratify.by = STRATA, country = "Switzerland")

    # Get geo.name
    if(GEO == "country")  geo.name <- "COUNTRY"
    if(GEO == "region")  geo.name <- "RegionID"
    if(GEO == "province")  geo.name <- "ID_space"

    if(STRATA == "none") {
      aux2 <- compute.excess(aux, divide.by = "pred", geo.name = geo.name)
      aux3 <- compute.excess.deaths(aux, geo.name = geo.name)
      aux <- cbind(select(aux, !starts_with("V")),
        select(aux2, !starts_with(c("xs", geo.name))),
        select(aux3, !starts_with(c("xs", geo.name))))
     aux <- merge(MAPS$CHE[[GEO]], aux)

     # Re-scale REM
     aux <- rescale_REM(aux)

    } else {
      aux <- lapply(aux, function(X) {
         aux2 <- compute.excess(X, divide.by = "pred", geo.name = geo.name)
         aux3 <- compute.excess.deaths(X, geo.name = geo.name)
         res <- cbind(select(X, !starts_with("V")),
            select(aux2, !starts_with(c("xs", geo.name))),
           select(aux3, !starts_with(c("xs", geo.name))))

         res <- merge(MAPS$CHE[[GEO]], res)

         # Re-scale REM
         res <- rescale_REM(res)

         return(res)
      })
    }

    return(aux)
  })
  # Add strata names
  names(res) <- strata

  return(res)
})
# Add geo names
names(d) <- geo.res



d_week <- lapply(geo.res, function(GEO) {
  res <- lapply(strata, function(STRATA) {
    aux <- get2020weeklydata(DB$CHE, geo.res = GEO, link_table = link_table,
      stratify.by = STRATA, country = "Switzerland")

    # Get geo.name
    if(GEO == "country")  geo.name <- "COUNTRY"
    if(GEO == "region")  geo.name <- "RegionID"
    if(GEO == "province")  geo.name <- "ID_space"

    if(STRATA == "none") {
      aux$COUNTRY <- "Switzerland"
      aux2 <- compute.excess(aux, divide.by = "pred", geo.name = geo.name)
      aux3 <- compute.excess.deaths(aux, geo.name = geo.name)
      aux <- cbind(select(aux, !starts_with("V")),
        select(aux2, !starts_with(c("xs", geo.name))),
        select(aux3, !starts_with(c("xs", geo.name))))

      # Re-scale REM
      aux <- rescale_REM(aux)

     #aux <- merge(MAPS$CHE[[GEO]], aux)
    } else {
      aux <- lapply(aux, function(X) {
         aux2 <- compute.excess(X, divide.by = "pred", geo.name = geo.name)
         aux3 <- compute.excess.deaths(X, geo.name = geo.name)
         res <- cbind(select(X, !starts_with("V")),
            select(aux2, !starts_with(c("xs", geo.name))),
           select(aux3, !starts_with(c("xs", geo.name))))

         #res <- merge(MAPS$CHE[[GEO]], res)

         # Re-scale REM
         res <- rescale_REM(res)

         return(res)
      })
    }

    return(aux)
  })
  # Add strata names
  names(res) <- strata

  return(res)
})
# Add geo names
names(d_week) <- geo.res



save(file = "Switzerland.RData", list = c("d", "d_week"))

stop("")

# The following code is old code as it is not supposed to be run.


d_week <- lapply(geo.res, function(GEO) {
  res <- lapply(strata, function(STRATA) {
    aux <- get2020weeklydata(DB$CHE, geo.res = GEO, link_table = link_table,
      stratify.by = STRATA, country = "Switzerland")
  })
  # Add strata
  names(res) <- strata

  return(res)
})
# Add geo names
names(d_week) <- geo.res





# Example
i <- 1 #Age-sex group
d <- DB$CHE[[i]]

# Aggregate samples by region
post.samples <- lapply(DB$CHE, function(X) {
  X$population <- X$population / n.weeks # To avoid numerical error when adding
  
  X <- select(X, starts_with("V"), "ID_space", "deaths", "population") %>%
            group_by(ID_space) %>%
            summarise_all(sum)

  aux <- select(X, "deaths", starts_with("V"))
  X$excess_deaths <- apply(aux, 1, function(Y) {
    mean(Y[1] - Y[1 + 1:1000])
  })
  X$excess_deaths_sd <- apply(aux, 1, function(Y) {
    sd(Y[1] - Y[1 + 1:1000])
  })

  # Sampled deaths
  X$pprob <- apply(X, 1, function(Y) {mean(Y["deaths"] > Y[1 + 1:1000] )})
  return(X)
})


# Aggregate samples by week
post.samples.week <- lapply(DB$CHE, function(X) {
  X$population <- X$population / n.regions # To avoid numerical error when adding
  
  X <- select(X, starts_with("V"), "EURO_LABEL", "deaths", "population") %>%
            group_by(EURO_LABEL) %>%
            summarise_all(sum)

   aux <- select(X, "deaths", starts_with("V"))
   X$excess_deaths <- apply(aux, 1, function(Y) {
     mean(Y[1] - Y[1 + 1:1000])
   })
   X$excess_deaths_sd <- apply(aux, 1, function(Y) {
     sd(Y[1] - Y[1 + 1:1000])
   })



  # Sampled deaths
  X$pprob <- apply(X, 1, function(Y) {mean(Y["deaths"] > Y[1 +1:1000] )})
  return(X)
})


# Create plots (from 'postanalysis.R')

# Plot 1: Using country-agesex strata
# Compute quantiles for the plots
#out <- d_week[[1]]$agesex
out <- post.samples.week

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
                           observed=x$deaths, #x$observed, 
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


ylimsmax <- rep(c(max(c(data4plot$`F40<`$`97.5%`,   data4plot$`M40<`$`97.5%`,
                          data4plot$`F40<`$observed, data4plot$`M40<`$observed))*1.2,
                    max(c(data4plot$`F40-59`$`97.5%`, data4plot$`M40-59`$`97.5%`,
                          data4plot$`F40-59`$observed, data4plot$`M40-59`$observed))*1.2,
                    max(c(data4plot$`F60-69`$`97.5%`, data4plot$`M60-69`$`97.5%`,
                          data4plot$`F60-69`$observed, data4plot$`M60-69`$observed))*1.2,
                    max(c(data4plot$`F70-79`$`97.5%`, data4plot$`M70-79`$`97.5%`,
                          data4plot$`F70-79`$observed, data4plot$`M70-79`$observed))*1.2,
                    max(c(data4plot$`F80+`$`97.5%`,   data4plot$`M80+`$`97.5%`,
                          data4plot$`F80+`$observed, data4plot$`M80+`$observed))*1.2),
                  times = 2)

# Read EUROSTAT
# read the EUROSTAT week file
EUROSTAT <- read_excel("../ExcessDeathsCOVID/data/EUROSTAT_ISO_HMEROLOGIO.xls")
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





save(file = "extract_data.RData", list = ls())
