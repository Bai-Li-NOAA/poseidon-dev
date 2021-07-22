# devtools::install_github("r4atlantis/atlantisom")
library(atlantisom)
library(tidyr)
require(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(ggforce)
library(ggthemes)
library(stringr)

# Set up d.name and scenario.name 
d.name <- here("NOBA_cod_files", "NOBA_sacc_38")
scenario.name <- "nordic_runresults_01"

# Set species name
species <- c("North_atl_cod")

# Directory with SS files
model_dir <- here::here("NOBA_cod_files", "output", "B")

# Name of SS data file
datfile_name <- "data.ss"

# Write data.ss for Stock Synthesis ------------------------------------------

stocksynthesis.data <- r4ss::SS_readdat_3.30(file.path(
  model_dir,
  datfile_name
))

# Time dimension parameters 
## From the project page: https://ices-eg.github.io/wg_WGSAM/SkillAssessProject.html

# Number of years of data to pull
nyears <- 50

# Atlantis initialization period in years
burnin <- 30

## From project page: https://ices-eg.github.io/wg_WGSAM/SkillAssessProject.html
timestep <- stepperyr <- 5
# fishery output: learned the hard way this can be different from ecosystem outputs

## I don't have access to omlist_ss. It seems that fstepperyr = stepperyr and timestep = stepperyr with a note of 5, so I used 5 here.

# fstepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutfinc
fstepperyr <- 5

## I don't have access to omlist_ss. I used the note 495 as the value of total_sample.

# noutsteps <- omlist_ss$runpar$tstop/omlist_ss$runpar$outputstep
# total_sample <- noutsteps-1 #495
total_sample <- 495


# same time dimensioning parameters as in surveycensus.R
# Vector of indices of catch in numbers to pull (by timestep to sum)
fish_sample_full <- c(0:total_sample) # total_sample defined in sardinesurvey.R
fish_burnin <- burnin * fstepperyr + 1
fish_nyears <- nyears * fstepperyr
fish_times <- fish_sample_full[fish_burnin:(fish_burnin + fish_nyears - 1)]
fish_timesteps <- seq(fish_times[fstepperyr], max(fish_times), by = fstepperyr) # last timestep
fish_years <- unique(floor(fish_times / fstepperyr) + 1) # my original
# fish_years <- unique(floor(fish_times/fstepperyr)) #from Christine's new sardine_config.R

fishtime <- fish_times

stocksynthesis.data$styr <- fish_years[1]
stocksynthesis.data$endyr <- fish_years[length(fish_years)]

# Write time series biomass (tons) and catch (tons) data for SS
truecatchbio <- read_savedfisheries(d.name, "Catch") # total fishery catch in tons; needs to update documentation in atlantisom to use read_savedfisheries(). 
truecatchbio_data <- truecatchbio$census[[1]][truecatchbio$census[[1]]$species %in% species, ]
truecatchbio_data_sort <- truecatchbio_data[order(truecatchbio_data$time), ]
truecatchbio_ss <- truecatchbio_data_sort$atoutput[floor(truecatchbio_data_sort$time / 365) %in% fish_years]

survObsBiom_data <- read_savedsurvs(d.name, "survB")
survnames <- names(survObsBiom_data)
survnames
# Survey and fishery catch sample time
survey_sample_time <- c(3, 1) # fall and spring
names(survey_sample_time) <- survnames

## From project page: https://ices-eg.github.io/wg_WGSAM/SkillAssessProject.html
survey_cv <- c(0.1, 0.1)
names(survey_cv) <- survnames
fish_cv <- c(0.01)
lenage_cv <- c(0.1)

stocksynthesis.data$Nsexes <- 1
stocksynthesis.data$Nareas <- 1
stocksynthesis.data$Nfleets <- 1 + length(survObsBiom_data)
stocksynthesis.data$fleetinfo <- data.frame(
  "type" = c(1, rep(3, length(survObsBiom_data))),
  "surveytiming" = c(-1, rep(1, length(survObsBiom_data))),
  "area" = rep(1, stocksynthesis.data$Nfleets),
  "units" = rep(2, stocksynthesis.data$Nfleets),
  "need_catch_mult" = rep(0, stocksynthesis.data$Nfleets),
  "fleetname" = c(names(truecatchbio), names(survObsBiom_data))
)

stocksynthesis.data$Nfleet <- 1
stocksynthesis.data$Nsurveys <- stocksynthesis.data$Nfleets - 1
stocksynthesis.data$N_areas <- 1
stocksynthesis.data$Ngenders <- stocksynthesis.data$Nsexes


stocksynthesis.data$fleetnames <- stocksynthesis.data$fleetinfo$fleetname
stocksynthesis.data$surveytiming <- c(-1, rep(1, stocksynthesis.data$Nfleets - 1))

survObsBiom_ss <- data_years <- vector("list", length = length(survObsBiom_data))
for (i in seq_along(survObsBiom_data)) {
  species_data <- survObsBiom_data[[i]][[1]][survObsBiom_data[[i]][[1]]$species %in% species, ]
  species_data_sort <- species_data[order(species_data$time), ]
  survObsBiom_ss[[i]] <- species_data_sort$atoutput[species_data_sort$time %in% fish_times]
  data_years[[i]] <- (species_data_sort$time[fish_years] - survey_sample_time[survnames[i]]) / timestep + 1
}

data_years[[length(survObsBiom_data) + 1]] <- fish_years


ts_data <- survObsBiom_ss
ts_data[[length(survObsBiom_data) + 1]] <- truecatchbio_ss

sampling_month <- list(
  rep(10, nyears), # fall survey
  rep(4, nyears), # spring survey
  rep(1, nyears) # fishery
)

stocksynthesis.data <- SS_write_ts(
  ss_data_list = stocksynthesis.data,

  ts_data = ts_data, # Argument from the help page uses cpue_data instead of ts_data, may need to update documentation or update ts_data to cpue_data

  CVs = c(
    survey_cv,
    fish_cv
  ),

  data_years = data_years,

  sampling_month = sampling_month,

  units = c("biomass", "biomass", "biomass"),
  data_type = c("CPUE", "CPUE", "catch"),
  fleets = c(2, 3, 1)
)

stocksynthesis.data$CPUE

stocksynthesis.data$catch

## f.method works better when we converting ICES SAM Cod to SS Cod
f.method <- 2
if (f.method == 2) {
  temp <- stocksynthesis.data$catch[1, ]
  temp$year <- -999
  rownames(temp) <- "-999"
  stocksynthesis.data$catch <- rbind(temp, stocksynthesis.data$catch)
}

stocksynthesis.data$CPUEinfo <- data.frame(
  "Fleet" = 1:stocksynthesis.data$Nfleets,
  "Units" = rep(2, stocksynthesis.data$Nfleets), # biomass
  "Errtype" = rep(0, stocksynthesis.data$Nfleets),
  "SD_Report" = rep(0, stocksynthesis.data$Nfleets)
)
row.names(stocksynthesis.data$CPUEinfo) <- stocksynthesis.data$fleetnames

# Writing age composition data  

# Read age_comp_data from Atlantisom output
fish_age_comp_data <- read_savedfisheries(d.name, "catchAge") # fishery age class composition
fish_age_comp_species <- fish_age_comp_data$census[[1]][fish_age_comp_data$census[[1]]$species %in% species, ]

survey_age_comp_data <- read_savedsurvs(d.name, "survAnnAge") # survey annual age composition
survey_age_comp_ss <- vector("list", length = length(survey_age_comp_data))
for (i in seq_along(survey_age_comp_data)){
  species_data <- survey_age_comp_data[[i]][[1]][survey_age_comp_data[[i]][[1]]$species %in% species,]
  survey_age_comp_ss[[i]] <- species_data[order(species_data$time), ]
}

sapply(1:length(survey_age_comp_ss), function(x) range(unique(survey_age_comp_ss[[x]]$agecl)))
range(fish_age_comp_species$agecl)

stocksynthesis.data$use_lencomp <- 0 # Use length composition or not?

stocksynthesis.data$N_agebins <- length(unique(survey_age_comp_ss[[1]]$agecl))
stocksynthesis.data$agebin_vector <- unique(survey_age_comp_ss[[1]]$agecl)
stocksynthesis.data$N_ageerror_definitions <- 1
stocksynthesis.data$Nages <- length(unique(survey_age_comp_ss[[1]]$agecl))
stocksynthesis.data$ageerror <- matrix(c(rep(-1, stocksynthesis.data$Nages + 1), rep(0, stocksynthesis.data$Nages + 1)), nrow = 2, byrow = TRUE)

stocksynthesis.data$age_info <- data.frame(
  "mintailcomp" = rep(0, stocksynthesis.data$Nfleets),
  "addtocomp" = rep(1e-07, stocksynthesis.data$Nfleets), 
  "combine_M_F" = rep(1, stocksynthesis.data$Nfleets),
  "CompressBins" = rep(0, stocksynthesis.data$Nfleets),
  "CompError" = rep(0, stocksynthesis.data$Nfleets), 
  "ParmSelect" = rep(0, stocksynthesis.data$Nfleets),
  "minsamplesize" = rep(0.001, stocksynthesis.data$Nfleets)
)
row.names(stocksynthesis.data$age_info) <- stocksynthesis.data$fleetnames

stocksynthesis.data$Lbin_method <- 1


# Get the age bins
age_comp_flat <- list(
  reformat_compositions(survey_age_comp_ss[[1]],
                        round.places = 4,
                        comp_type = "agecomp"
  ),
  
  reformat_compositions(survey_age_comp_ss[[2]],
                        round.places = 4,
                        comp_type = "agecomp"
  ),
  
  reformat_compositions(fish_age_comp_species,
                        round.places = 4,
                        comp_type = "agecomp"
  )
)

fish_age_comp_flat <- as.data.frame(matrix(0, nrow=nrow(age_comp_flat[[3]]), ncol=ncol(age_comp_flat[[1]])))
colnames(fish_age_comp_flat) <- colnames(age_comp_flat[[1]])
fish_age_comp_flat[, colnames(age_comp_flat[[3]])] <- age_comp_flat[[3]]


age_comp_flat[[1]]$time <- (age_comp_flat[[1]]$time - survey_sample_time[survnames[1]]) / timestep + 1
age_comp_flat[[2]]$time <- (age_comp_flat[[2]]$time - survey_sample_time[survnames[2]]) / timestep + 1

age_comp_flat[[3]] <- fish_age_comp_flat
fish_age_comp_id <- which(age_comp_flat[[3]]$time %in% fish_timesteps)
age_comp_flat[[3]]$time <- floor(age_comp_flat[[3]]$time / fstepperyr) + 1

age_bins <- as.character(stocksynthesis.data$agebin_vector) 
## Write age composition data for survey
stocksynthesis.data <- SS_write_comps(
  ss_data_list = stocksynthesis.data,
  
  comp_matrix = list(
    age_comp_flat[[1]][age_comp_flat[[1]]$time %in% fish_years, ], 
    age_comp_flat[[2]][age_comp_flat[[2]]$time %in% fish_years, ],
    age_comp_flat[[3]][fish_age_comp_id, ]
                     ),
  
  data_rows = list(
    stocksynthesis.data$styr:(stocksynthesis.data$styr + nyears - 1),
    stocksynthesis.data$styr:(stocksynthesis.data$styr + nyears - 1),
    stocksynthesis.data$styr:(stocksynthesis.data$styr + nyears - 1)
    ),
  
  sampling_month = sampling_month,
  
  data_type = c("agecomp", "agecomp", "agecomp"),
  fleet_number = c(2, 3, 1),
  bins = list(age_bins, age_bins, age_bins),
  caal_bool = c(FALSE, FALSE, FALSE)
)
stocksynthesis.data$agecomp

stocksynthesis.data$use_MeanSize_at_Age_obs <- 0
stocksynthesis.data$MeanSize_at_Age_obs <- NULL

r4ss::SS_writedat(
  datlist = stocksynthesis.data, verbose = FALSE, outfile = file.path(here("NOBA_cod_files", "output", "atlantis2ss"), "data.ss"),
  overwrite = TRUE
)



# Write control.ss for Stock Synthesis ------------------------------------


