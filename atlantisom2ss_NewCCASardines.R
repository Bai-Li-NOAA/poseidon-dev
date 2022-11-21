## Notes:
## ## could be notes in a vignette
## ### could delete the code later

remotes::install_github("r4atlantis/atlantisom")
remotes::install_github("r4ss/r4ss")
### remotes::install_github("Bai-Li-NOAA/saconvert")

library(atlantisom)
library(tidyr)
require(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(ggforce)
library(ggthemes)
library(stringr)
library(r4ss)

## A test run for sardines with the updated California Current model (September 2022). This workflow loads Atlantisom input and output files and converts the files to Stock Synthesis (SS3) input files, which include starter.ss, forecast.ss, data.ss, control.ss, and wtatage.ss.

# Load Atlantisom data --------------------------------------------

### source(here::here("NOBA_cod_files", "Rscript", "reformat_compositions.R"))

# Warning message after running atlantisom::om_init
# In `[<-.data.frame`(`*tmp*`, , 3:12, value = list(`1` = c("0.1",  : provided 13 variables to replace 10 variables
# truth can be found in CCom$truth
CCom <- atlantisom::om_init(here("config", "CCConfigSep22.R"))
species_ss <- c("Pacific_sardine")
omlist_ss <- atlantisom::om_species(species_ss, CCom)
# get(load(here::here(d.name, paste0(scenario.name, "run_truth.RData")))) # To load result (truth) only. It is CCom$truth


# Atlantisom timestep setup ---------------------------------------

# Number of years of data to pull
nyears <- 50

# Atlantis initialization period in years
burnin <- 30

# fishery output: learned the hard way this can be different from ecosystem outputs
fstepperyr <- if (omlist_ss$runpar$outputstepunit == "days") 365 / omlist_ss$runpar$toutfinc # 5

noutsteps <- omlist_ss$runpar$tstop / omlist_ss$runpar$outputstep # 600
timeall <- c(0:noutsteps) # 0:600
stepperyr <- if (omlist_ss$runpar$outputstepunit == "days") 365 / omlist_ss$runpar$toutinc # 5
midptyr <- round(median(seq(0, stepperyr))) # 2

timestep <- stepperyr # 5

# same time dimensioning parameters as in surveycensus.R
# Vector of indices of catch in numbers to pull (by timestep to sum)
fish_sample_full <- c(0:total_sample) # total_sample defined in sardinesurvey.R: 0:599
fish_burnin <- burnin * fstepperyr + 1 # 151
fish_nyears <- nyears * fstepperyr # 250
fish_times <- fish_sample_full[fish_burnin:(fish_burnin + fish_nyears - 1)] # 150:399
fish_timesteps <- seq(fish_times[fstepperyr], max(fish_times), by = fstepperyr) # last timestep #154:399
# fish_years <- unique(floor(fish_times/fstepperyr)+1) # my original
fish_years <- unique(floor(fish_times / fstepperyr)) # from Christine's new sardine_config.R #30:79

fishtime <- fish_times

fitstartyr <- 30
fitendyr <- 79

atlantis_full <- c(0:total_sample) # 0:599
mod_burnin <- fitstartyr * stepperyr + 1 # 151
fit_nyears <- fitendyr - fitstartyr + 1 # 50
fit_ntimes <- fit_nyears * stepperyr # 250
fittimes <- atlantis_full[mod_burnin:(mod_burnin + fit_ntimes - 1)] # 150-399
fit_timesteps <- seq(fittimes[stepperyr], max(fittimes), by = stepperyr) # last timestep:154-399
fit_years <- unique(floor(fittimes / stepperyr)) # from Christine's new sardine_config.R: 30-79
fittimes.days <- fittimes * 73 # 10950, 11023, ...29127

# fishery sampling area
# should return all model areas, this assumes you see everything that it caught
fishboxes <- c(0:(omlist_ss$boxpars$nbox - 1)) # 0:88

# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
fisheffN <- data.frame(species = species_ss, effN = rep(1000, length(species_ss)))

# this adjusts for subannual fishery output so original effN is for whole year
fisheffN$effN <- fisheffN$effN / fstepperyr # 200

# fishery catch cv can be used in sample_survey_biomass
# perfect observation
fish_cv <- data.frame(species = species_ss, cv = rep(0.01, length(species_ss)))

# Survey name
survey_sample_time <- c(7)

# Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr # 5

# Which atlantis timestep does the survey run in?--now from census_spec.R
# with 5 output steps per year, 0 is Jan-Feb-midMar, 1 is midMar-Apr-May,
# 2 is June-July-midAug, 3 is midAug-Sept-Oct, 4 is Nov-Dec (ish)

survey_sample_time <- midptyr # 2; defined in omdimensions.R

# The last timestep to sample
total_sample <- noutsteps - 1 # 495

# Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
  total_sample,
  by = timestep
)

survtime <- survey_sample_full

# survey area
# should return all model areas
survboxes <- allboxes

# survey efficiency (q)
# should return a perfectly efficient survey
surveffic <- data.frame(
  species = species_ss,
  efficiency = rep(0.5, length(species_ss))
)

# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
surveffN <- data.frame(species = species_ss, effN = rep(1000, length(species_ss)))

# survey index cv needed for sample_survey_xxx
# cv = 0.1
surv_cv <- data.frame(species = species_ss, cv = rep(0.1, length(species_ss)))

# length at age cv for input into calc_age2length function
# function designed to take one cv for all species, need to change to pass it a vector
lenage_cv <- 0.1

# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 150
# Inputs of atlantisom2ss function --------------------------------

user_od <- here::here("inst", "extdata", "atlantisom2ss_Sardine_ss_files_Sep2022")
if (!dir.exists(user_od)) dir.create(user_od)

ss3_data_lbin_method <- 1
ss3_data_use_lencomp <- 1

ss3_data_agebin_vector <- unique(CCom$truth$biomass_ages$agecl[CCom$truth$biomass_ages$species == species_ss])
ss3_data_N_agebins <- length(ss3_data_agebin_vector)


### forN <- 1 # An integer value specifying the number of forecast years for the SS projections.
# Write data.ss for Stock Synthesis ------------------------------------------

ss3_data <- ss3_simple_data <- r4ss::SS_readdat(
  verbose = FALSE,
  file = dir(utils::tail(dir(system.file("extdata", package = "r4ss"), pattern = "simple", full.names = TRUE), 1), pattern = "data", full.names = TRUE)
)

# Load time series catch (tons) and biomass (tons) data for SS
truecatchbio <- atlantisom::read_savedfisheries(d.name, "Catch")
truecatchbio_data <- truecatchbio[[1]][[1]][truecatchbio[[1]][[1]]$species %in% species_ss, ]

truecatchbio_data_sort <- truecatchbio_data[order(truecatchbio_data$time), ]
truecatchbio_ss <- truecatchbio_data_sort$atoutput[floor(truecatchbio_data_sort$time / 365) %in% fit_years]
fleetnames <- names(truecatchbio)

survObsBiom_data <- atlantisom::read_savedsurvs(d.name, "survB")
survnames <- names(survObsBiom_data)
# Survey and fishery catch sample time
names(survey_sample_time) <- survnames
survObsBiom_ss <- survey_years <- vector("list", length = length(survObsBiom_data))
for (i in seq_along(survObsBiom_data)) {
  species_data <- survObsBiom_data[[i]][[1]][survObsBiom_data[[i]][[1]]$species %in% species_ss, ]
  species_data_sort <- species_data[order(species_data$time), ]
  survObsBiom_ss[[i]] <- species_data_sort$atoutput[species_data_sort$time %in% fittimes]
  survey_years[[i]] <- (species_data_sort$time[fit_years] - survey_sample_time[survnames[i]]) / timestep + 1
}

# Load fleet and survey composition data
len_comp_data <- readRDS(file.path(d.name, paste0(
  scenario.name,
  "_",
  survey.name, "survObsLenComp.rds"
)))
fish_len_comp_data <- readRDS(file.path(d.name, paste0(
  scenario.name,
  "_",
  fishery.name, "fishObsLenComp.rds"
)))
age_comp_data <- readRDS(file.path(d.name, paste0(
  scenario.name,
  "_",
  survey.name, "survObsAgeComp.rds"
)))
catch_age_comp <- readRDS(file.path(d.name, paste0(
  scenario.name,
  "_",
  fishery.name, "fishObsAgeComp.rds"
)))

len_comp_data <- len_comp_data[[1]]
fish_len_comp_data <- fish_len_comp_data[[1]]

fish_len_comp_data$time <- as.integer(floor(fish_len_comp_data$time / fstepperyr))

len <- len_comp_data

if (fstepperyr > 1) {
  fish_len_comp_anndata <- fish_len_comp_data %>%
    mutate(yr = floor(time / fstepperyr)) %>%
    group_by(species, agecl, lower.bins, upper.bins, time = as.integer(yr)) %>%
    summarise(annnatlength = sum(atoutput)) %>%
    rename(atoutput = annnatlength)
} else {
  fish_len_comp_anndata <- fish_len_comp_data
}

# Add over age classes to get sample size
len_comp_flat <- atlantisom::reformat_compositions(len_comp_data,
  round.places = 0,
  comp_type = "lencomp"
)
# remove burnin
len_comp_final <- filter(
  len_comp_flat,
  time %in% survey_years
)
len_comp_final <- as.data.frame(len_comp_final)

length_bins <- as.integer(names(len_comp_final))
length_bins <- length_bins[!is.na(length_bins)]

# fishery length comps are still 5 timesteps per year
# need to aggregate to annual (done above)
# also,  make effN annual goal/fstepsperyr (done above)
### fish_len_comp_flat <- atlantisom::reformat_compositions(fish_len_comp_anndata,
###                                            round.places = 0,
###                                            comp_type = "lencomp"
### )

fish_len_comp_flat <- atlantisom::reformat_compositions(fish_len_comp_data,
  round.places = 0,
  comp_type = "lencomp"
)
# remove burnin works after adjustment above
fish_len_comp_final <- filter(
  fish_len_comp_flat,
  time %in% fish_years
)
fish_len_comp_final <- as.data.frame(fish_len_comp_final)

notbins <- c("time", "nsamp")

# fish_length_bins <- as.integer(names(fish_len_comp_final))
# fish_length_bins <- fish_length_bins[!is.na(fish_length_bins)]

# need to fill empty length bins with 0s to have same bins as survey for SS_write_comps
missing.lengths <- setdiff(length_bins, names(fish_len_comp_final)[!names(fish_len_comp_final) %in% notbins])
fish_len_comp_final[, as.character(missing.lengths)] <- 0 # Add them, filled with '0's
fish_len_comp_final <- fish_len_comp_final[c("time", length_bins, "nsamp")]


# fishery age comps also 5 timesteps per year
if (fstepperyr > 1) {
  fish_age_comp_anndata <- catch_age_comp[[1]] %>%
    mutate(yr = floor(time / fstepperyr)) %>%
    group_by(species, agecl, time = as.integer(yr)) %>%
    summarise(annnatage = sum(atoutput)) %>%
    rename(atoutput = annnatage)
} else {
  fish_age_comp_anndata <- catch_age_comp[[1]]
}

fish_age_comp_flat <- reformat_compositions(fish_age_comp_anndata,
  comp_type = "agecomp"
)

# remove burnin (not necessary?fish comps made with fish_years only)
fish_age_comp_final <- filter(
  fish_age_comp_flat,
  time %in% fish_years
)
fish_age_comp_final <- as.data.frame(fish_age_comp_final)

# #SS_write_comps breaking because fishery age bins start with 2 not 1; extracting bins from fish file may help?
# fish_age_bins <- names(fish_age_comp_flat)[!names(fish_age_comp_flat) %in% notbins]

# that leaves an empty column in data file, so instead fill with 0s
age_bins <- ss3_data_agebin_vector
missing.ages <- setdiff(age_bins, names(fish_age_comp_final)[!names(fish_age_comp_final) %in% notbins])
fish_age_comp_final[, as.character(missing.ages)] <- 0 # Add them, filled with '0's
fish_age_comp_final <- fish_age_comp_final[c("time", age_bins, "nsamp")]

### len <- dplyr::filter(len_comp_data, time %in% c(55:175))

# Load survey age compositoin data
age_comp_data <- readRDS(file.path(d.name, paste0(
  scenario.name,
  "_",
  survey.name, "survObsAgeComp.rds"
)))
age_comp_data <- age_comp_data[[1]]

Natage <- age_comp_data

### Natage <- filter(age_comp_data, time %in% c(150:270))

fish_age_comp <- readRDS(file.path(d.name, paste0(
  scenario.name,
  "_",
  fishery.name, "fishObsAgeComp.rds"
)))
fish_age_comp <- fish_age_comp[[1]]

# add this to om_indices function so that this has years when read in
fish_age_comp$time <- fish_age_comp$time / fstepperyr

Natage <- fish_age_comp

### Natage <- filter(fish_age_comp, time %in% c(30:53))


# Load weight-at-age data
wtage <- readRDS(file.path(d.name, paste0(
  scenario.name,
  "_",
  survey.name, "survObsWtAtAge.rds"
)))
wtage <- wtage[[1]]

## From project page: https://ices-eg.github.io/wg_WGSAM/SkillAssessProject.html
survey_cv <- surv_cv$cv
names(survey_cv) <- survnames
fish_cv <- fish_cv$cv
lenage_cv <- lenage_cv

ss3_data$styr <- fit_years[1]
ss3_data$endyr <- tail(fit_years, n = 1)
if (settlement_age == 1) ss3_data$spawn_month <- 1.0001 # default is 1
ss3_data$Nsexes <- ss3_data$Ngenders <- (-1) # use -1 for 1 sex setup with SSB multiplied by female_frac parameter
ss3_data$Nages <- ss3_data_N_agebins
ss3_data$Nareas <- 1

ss3_data$Nfleet <- length(truecatchbio)
ss3_data$Nsurveys <- length(survObsBiom_data)
ss3_data$Nfleets <- ss3_data$Nfleet + ss3_data$Nsurveys

### survey_id <- 1:ss3_data$Nsurveys

ss3_data$fleetinfo <- data.frame(
  "type" = c(rep(1, ss3_data$Nfleet), rep(3, ss3_data$Nsurveys)),
  "surveytiming" = c(-1, 1), # -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)
  "area" = rep(1, ss3_data$Nfleets),
  "units" = rep(1, ss3_data$Nfleets),
  "need_catch_mult" = rep(0, ss3_data$Nfleets),
  "fleetname" = c(names(truecatchbio), names(survObsBiom_data))
)

ss3_data$fleetnames <- ss3_data$fleetinfo$fleetname
ss3_data$surveytiming <- c(-1, survey_sample_time)
ss3_data$units_of_catch <- ss3_data$fleetinfo$units

### fleet_year_id <- names(sa_data$fishery$obs_total_catch_biomass$fleet1) %in% model_year
ss3_data$catch <- data.frame(
  "year" = fit_years,
  "seas" = 1,
  "fleet" = ss3_data$Nfleet,
  "catch" = truecatchbio_ss,
  "catch_se" = round(sqrt(log(1 + fish_cv^2)), digits = 2)
)

ss3_data$CPUEinfo <- data.frame(
  Fleet = 1:ss3_data$Nfleets,
  Units = c(1, rep(1, ss3_data$Nsurveys)),
  Errtype = 0,
  SD_Report = 0
)
row.names(ss3_data$CPUEinfo) <- ss3_data$fleetnames

cpue_data <- list()
for (i in 1:length(survObsBiom_ss)) {
  cpue_data[[i]] <- data.frame(
    "year" = survey_years[[i]] + 1, # Need to check survey years: 29-78
    "seas" = survey_sample_time[1],
    "index" = i + ss3_data$Nfleet,
    "obs" = survObsBiom_ss[[i]],
    "se_log" = round(sqrt(log(1 + survey_cv[i]^2)), digits = 2)
  )
}

ss3_data$CPUE <- do.call(rbind, cpue_data)

# set up population length bin structure
ss3_data$lbin_method <- ss3_data_lbin_method
ss3_data$use_lencomp <- ss3_data_use_lencomp
ss3_data$comp_tail_compression <- rep(-0.0001, times = ss3_data$Nfleets)
ss3_data$add_to_comp <- rep(1e-5, times = ss3_data$Nfleets)
ss3_data$len_info <- data.frame(
  mintailcomp = rep(-0.0001, times = ss3_data$Nfleets),
  addtocomp = 1e-5,
  combine_M_F = 0,
  CompressBins = 0,
  CompError = 0,
  ParmSelect = 0,
  minsamplesize = 1
)

ss3_data$N_lbins <- length(length_bins)
ss3_data$lbin_vector <- length_bins

ss3_data$lencomp



# Writing age composition data

# Read age_comp_data from Atlantisom output
fish_age_comp_data <- read_savedfisheries(d.name, "catchAnnAge") # fishery annual age class composition
fish_age_comp_species <- fish_age_comp_data$census[[1]][fish_age_comp_data$census[[1]]$species %in% species, ]

fish_age_comp_species %>%
  group_by(time) %>%
  summarise(sum = sum(atoutput)) # 200 fish per timestep


survey_age_comp_data <- read_savedsurvs(d.name, "survAnnAge") # survey annual age composition
survey_age_comp_ss <- vector("list", length = length(survey_age_comp_data))
for (i in seq_along(survey_age_comp_data)) {
  species_data <- survey_age_comp_data[[i]][[1]][survey_age_comp_data[[i]][[1]]$species %in% species, ]
  survey_age_comp_ss[[i]] <- species_data[order(species_data$time), ]
}

sapply(1:length(survey_age_comp_ss), function(x) range(unique(survey_age_comp_ss[[x]]$agecl)))
range(fish_age_comp_species$agecl)

ss3_data$use_lencomp <- 0 # Use length composition or not?

ss3_data$N_agebins <- length(unique(survey_age_comp_ss[[1]]$agecl))
ss3_data$agebin_vector <- unique(survey_age_comp_ss[[1]]$agecl)
ss3_data$N_ageerror_definitions <- 1
ss3_data$Nages <- max(ss3_data$agebin_vector)
ss3_data$ageerror <- matrix(c(rep(-1, ss3_data$Nages + 1), rep(0, ss3_data$Nages + 1)), nrow = 2, byrow = TRUE)

report.ages <- c(1, ss3_data$Nages - 2) # Two values, a min and max age used for reporting fishing

ss3_data$age_info <- data.frame(
  "mintailcomp" = rep(0, ss3_data$Nfleets),
  "addtocomp" = rep(1e-07, ss3_data$Nfleets),
  "combine_M_F" = rep(1, ss3_data$Nfleets),
  "CompressBins" = rep(0, ss3_data$Nfleets),
  "CompError" = rep(0, ss3_data$Nfleets),
  "ParmSelect" = rep(0, ss3_data$Nfleets),
  "minsamplesize" = rep(0.001, ss3_data$Nfleets)
)
row.names(ss3_data$age_info) <- ss3_data$fleetnames

ss3_data$Lbin_method <- 1

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

# TO-DO: Need to consider the case that fishery age composition is less than fish age composition
fish_age_comp_flat <- as.data.frame(matrix(0, nrow = nrow(age_comp_flat[[3]]), ncol = ncol(age_comp_flat[[3]])))
colnames(fish_age_comp_flat) <- colnames(age_comp_flat[[3]])
fish_age_comp_flat[, colnames(age_comp_flat[[3]])] <- age_comp_flat[[3]]

fish_age_comp_final <- filter(
  fish_age_comp_flat,
  time %in% fit_timesteps
)


plot(as.numeric(fish_age_comp_final[1, 2:(ncol(fish_age_comp_final) - 1)]))
true_fish_age <- omlist_ss$truecatchage_ss[
  omlist_ss$truecatchage_ss$species == "North_atl_cod" &
    omlist_ss$truecatchage_ss$time %in% fittimes,
]

aggregate_true_fish_age <- aggregate(true_fish_age$atoutput, by = list(true_fish_age$species, true_fish_age$agecl, true_fish_age$time), FUN = "sum")
names(aggregate_true_fish_age) <- c("species", "agecl", "time", "atoutput")
true_fish_age_comp <- reformat_compositions(aggregate_true_fish_age,
  round.places = 4,
  comp_type = "agecomp"
)
true_fish_age_comp <- as.data.frame(true_fish_age_comp)

row_id <- 4
plot(as.numeric(true_fish_age_comp[row_id, 2:21]),
  xlab = "Age Class", ylab = "Proportion"
)
lines(as.numeric(fish_age_comp_final[row_id, 2:21]))

age_comp_flat[[1]]$time <- (age_comp_flat[[1]]$time - survey_sample_time[survnames[1]]) / timestep + 1 # Fall: step 3
age_comp_flat[[1]] <- as.data.frame(age_comp_flat[[1]])

age_comp_flat[[2]]$time <- (age_comp_flat[[2]]$time - survey_sample_time[survnames[2]]) / timestep + 1 # Spring: step 1
age_comp_flat[[2]] <- as.data.frame(age_comp_flat[[2]])

age_comp_flat[[3]] <- fish_age_comp_final
fish_age_comp_id <- which(age_comp_flat[[3]]$time %in% fit_timesteps)
age_comp_flat[[3]]$time <- floor(age_comp_flat[[3]]$time / fstepperyr)

age_bins <- ss3_data$agebin_vector

## Write age composition data for survey
ss3_data$agecomp <- ss3_data$agecomp[, 1:9]
ss3_data <- SS_write_comps(
  ss_data_list = ss3_data,
  comp_matrix = list(
    age_comp_flat[[1]][age_comp_flat[[1]]$time %in% fit_years, ],
    age_comp_flat[[2]][age_comp_flat[[2]]$time %in% fit_years, ],
    age_comp_flat[[3]][fish_age_comp_id, ]
  ),
  data_rows = list(
    ss3_data$styr:(ss3_data$styr + fit_nyears - 1),
    ss3_data$styr:(ss3_data$styr + fit_nyears - 1),
    ss3_data$styr:(ss3_data$styr + fit_nyears - 1)
  ),
  sampling_month = sampling_month,
  data_type = c("agecomp", "agecomp", "agecomp"),
  fleet_number = c(2, 3, 1),
  bins = list(age_bins, age_bins, age_bins),
  caal_bool = c(FALSE, FALSE, FALSE)
)

# ss3_data$agecomp <- ss3_data$agecomp[which(ss3_data$agecomp$FltSvy %in% c(2,3)), ]

ss3_data$agecomp <- ss3_data$agecomp[which(ss3_data$agecomp$FltSvy == 1), ]

# proportion_sum <- apply(ss3_data$agecomp[, 10:ncol(ss3_data$agecomp)], 1, sum)
# plot(proportion_sum, xlab="Year Index", ylab="Sum")
#
# catch_agecomp <- ss3_data$agecomp[which(ss3_data$agecomp$FltSvy==1), 10:ncol(ss3_data$agecomp)]
#
# plot(apply(catch_agecomp, 2, mean), col="blue",
#      xlab="Age Class", ylab="Proportion")
# par(mfrow=c(4,5), mar=c(3,4,0,0))
# for (i in 1:nrow(catch_agecomp)){
#   plot(as.numeric(catch_agecomp[i,]),
#        ylab="Agecomp",
#        type="l")
#   lines(as.numeric(true_fish_age_comp[true_fish_age_comp$time %in% fit_timesteps, ][i, 2:21]), col="blue") # true fishery age composition
#   legend("top", legend=paste("Year", 39+i), bty="n")
# }
#
# plot(fish_age_comp_species$atoutput[fish_age_comp_species$time==399] )
# plot(fish_age_comp_species$atoutput[fish_age_comp_species$time==399]/sum(fish_age_comp_species$atoutput[fish_age_comp_species$time==399]), type="l")
# lines(as.numeric(catch_agecomp[40,]), col="blue")


ss3_data$use_MeanSize_at_Age_obs <- 0
ss3_data$MeanSize_at_Age_obs <- NULL

# ss3_data$spawn_month <- 1.0001 # if ctl recruitment age = 1

r4ss::SS_writedat(
  datlist = ss3_data, verbose = FALSE, outfile = file.path(user.od, "data.ss"),
  overwrite = TRUE
)

# Write wtatage.ss for Stock Synthesis ------------------------------------

modify_matrices <- function(matr_to_turn, time_id) {
  matr_to_turn$kg <- matr_to_turn$atoutput / 1000
  matr_to_turn <- filter(matr_to_turn, time %in% time_id)
  return_mat <- dcast(
    data = matr_to_turn,
    formula = time ~ agecl,
    value.var = "kg",
    fun.aggregate = NULL
  )

  return(return_mat)
}

surv_wtage <- read_savedsurvs(d.name, "survAnnWtage") # survey weight at age class
surv_wtage_species <- vector(mode = "list", length = length(survnames))
surv_meanwt <- vector(mode = "list", length = length(survnames))
for (i in 1:length(survnames)) {
  surv_wtage_species[[i]] <- surv_wtage[[survnames[i]]][[1]][surv_wtage[[survnames[i]]][[1]]$species %in% species, ]
  names(surv_wtage_species)[i] <- survnames[i]
  surv_wtage_species[[survnames[i]]]$kg <- surv_wtage_species[[survnames[i]]]$atoutput / 1000

  surv_wtage_species[[survnames[i]]]$time <- (surv_wtage_species[[survnames[i]]]$time - survey_sample_time[survnames[i]]) / timestep + 1

  surv_meanwt[[i]] <- modify_matrices(as.data.table(surv_wtage_species[[survnames[i]]]), time_id = fit_years)
  surv_meanwt[[i]] <- as.data.frame(surv_meanwt[[i]])
}


fish_wtage <- read_savedfisheries(d.name, "catchAnnWtage") # fishery weight at age class
fish_wtage_species <- fish_wtage$census[[1]][fish_wtage$census[[1]]$species %in% species, ]
fish_wtage_species$kg <- fish_wtage_species$atoutput / 1000

catch_meanwt <- modify_matrices(as.data.table(fish_wtage_species), time_id = fit_timesteps)
catch_meanwt[, 2:6] <- surv_meanwt[[2]][, 2:6]
catch_meanwt <- as.data.frame(catch_meanwt)

waa.array <- array(0, dim = c((ss3_data$endyr - ss3_data$styr + 1), ss3_data$Nages, ss3_data$Nfleets))

fish_waa_id <- which(catch_meanwt$time %in% fish_timesteps)
waa.array[, as.numeric(names(catch_meanwt)[2:ncol(catch_meanwt)]), 1] <- as.matrix(catch_meanwt[fish_waa_id, names(catch_meanwt)[2:ncol(catch_meanwt)]])

waa.array[, , 2] <- as.matrix(surv_meanwt[[1]][, 2:ncol(surv_meanwt[[1]])])
waa.array[, , 3] <- as.matrix(surv_meanwt[[2]][, 2:ncol(surv_meanwt[[2]])])

waa.new <- do.call(
  "rbind",
  replicate((length(survnames) + 3), data.frame(
    "Yr" = data_years[[3]],
    # todo: check season of weight at age
    "Seas" = 1,
    "Sex" = 1,
    "Bio_Pattern" = 1,
    "BirthSeas" = 1,
    "Fleet" = 1,
    "0" = waa.array[, 1, 3],
    waa.array[, , 3]
  ), simplify = FALSE)
)

waa.new$Fleet <- rep(-1:(length(survnames) + 1),
  each = ss3_data$endyr - ss3_data$styr + 1
)

# Use waa.array[, , 1] for catch WT
waa.new[waa.new$Fleet == 1, ] <- do.call(
  "rbind",
  replicate(1, data.frame(
    "Yr" = data_years[[1]],
    # todo: check season of weight at age
    "Seas" = 1,
    "Sex" = 1,
    "Bio_Pattern" = 1,
    "BirthSeas" = 1,
    "Fleet" = 1,
    "0" = waa.array[, 1, 1],
    waa.array[, , 1]
  ), simplify = FALSE)
)


waa.new[waa.new$Fleet == 2, ] <- do.call(
  "rbind",
  replicate(1, data.frame(
    "Yr" = data_years[[2]],
    # todo: check season of weight at age
    "Seas" = 1,
    "Sex" = 1,
    "Bio_Pattern" = 1,
    "BirthSeas" = 1,
    "Fleet" = 2,
    "0" = waa.array[, 1, 2],
    waa.array[, , 2]
  ), simplify = FALSE)
)

# Load into function environment as objects
maturity_mean <- c(
  as.numeric(truth$biolprm$maturityogive[truth$biolprm$maturityogive$code == species_code, 3:ncol(truth$biolprm$maturityogive)]),
  rep(1, ss3_data$Nages - truth$biolprm$maturityogive$nagecl[truth$biolprm$maturityogive$code == species_code])
)

names(maturity_mean) <- ss3_data$agebin_vector
maturity_matrix <- matrix(rep(maturity_mean, each = length(data_years[[3]])), nrow = length(data_years[[3]]))

fecmat <- waa.array[, , 3] * maturity_matrix
colnames(fecmat) <- as.character(ss3_data$agebin_vector)
waa.fec <- data.frame(
  "Yr" = data_years[[3]],
  "Seas" = 1,
  "Sex" = 1,
  "Bio_Pattern" = 1,
  "BirthSeas" = 1,
  "Fleet" = -2,
  "0" = fecmat[, 1],
  fecmat
)

waa.new <- rbind(waa.new, waa.fec)

if (ss3_data$Nsexes == 2) {
  waa.mal <- waa.new
  waa.mal$Sex <- 2
  waa.new <- rbind(waa.new, waa.mal)
}

waa.forecast <- waa.new[waa.new$Yr == ss3_data$endyr, ]
waa.forecast$Yr <- 1 + waa.forecast$Yr
waa.new <- rbind(waa.new, waa.forecast)

waa.new$X0 <- 0
r4ss::SS_writewtatage(
  mylist = waa.new, dir = user.od,
  verbose = FALSE, overwrite = TRUE
)





# Write control.ss for Stock Synthesis ------------------------------------
simple_ctl <- r4ss::SS_readctl(
  verbose = FALSE,
  file = dir(utils::tail(dir(system.file("extdata", package = "r4ss"), pattern = "simple", full.names = TRUE), 1), pattern = "control", full.names = TRUE),
  use_datlist = TRUE,
  datlist = dir(utils::tail(dir(system.file("extdata", package = "r4ss"), pattern = "simple", full.names = TRUE), 1), pattern = "data", full.names = TRUE)
)

ctl <- simple_ctl

ctl$EmpiricalWAA <- 1

# ctl$recr_dist_pattern$age <- 1 # recruitment age


ctl$Growth_Age_for_L1 <- 1
ctl$Growth_Age_for_L2 <- utils::tail(ss3_data$agebin_vector, 1)

ctl$MG_parms$PHASE[ctl$MG_parms$PHASE > 0] <- ctl$MG_parms$PHASE[ctl$MG_parms$PHASE > 0] * (-1)


# ctl$MG_parms[grep("Frac", rownames(ctl$MG_parms)), ] <- c(0.000001, 0.99, 0.5, 0.5, 0.5, 0, -1, 0, 0, 0, 0, 0, 0, 0,14)
ctl$MG_parms[grep("Frac", rownames(ctl$MG_parms)), ] <- c(0.000001, 0.99, 0.5, 0.5, 0.5, 0, -1, 0, 0, 0, 0, 0, 0, 0)
# ctl$MG_parms[grep("Frac", rownames(ctl$MG_parms)), ] <- c(0.000001, 0.99, 0.99, 0.99, 0.5, 0, -1, 0, 0, 0, 0, 0, 0, 0,14)

ctl$maturity_option <- 5 # disable maturity and use maturity in wtatage.ss?
ctl$First_Mature_Age <- as.numeric(which(maturity_mean > 0)[1])
ctl$MainRdevYrFirst <- fit_years[1]
ctl$MainRdevYrLast <- utils::tail(fit_years, 1)
ctl$recdev_phase <- 1
ctl$recr_dist_method <- 4

ctl$N_Block_Designs <- 0
ctl$blocks_per_pattern <- NULL
ctl$Block_Design <- NULL


ctl$MG_parms <- ctl$MG_parms[-grep("RecrDist", rownames(ctl$MG_parms)), ]
# todo: change early start year
ctl$recdev_early_start <- ss3_data$Nages * -1 # ctl$recdev_early_start <- catch.nages * -3
ctl$recdev_early_phase <- 3
ctl$Fcast_recr_phase <- 6

ctl$last_early_yr_nobias_adj <- fit_years[1] - 1
ctl$first_yr_fullbias_adj <- fit_years[1]
ctl$last_yr_fullbias_adj <- utils::tail(fit_years, 1)
ctl$first_recent_yr_nobias_adj <- utils::tail(fit_years, 1) + 1

# ctl$last_early_yr_nobias_adj <- -11
# ctl$first_yr_fullbias_adj <- 36.9
# ctl$last_yr_fullbias_adj <- 71.9
# ctl$first_recent_yr_nobias_adj <- 80
# ctl$max_bias_adj <- 0

ctl$F_ballpark_year <- fit_years[1]

# Selectivity
ctl$size_selex_types <- do.call("rbind", replicate(n = ss3_data$Nfleets, expr = c(0, 0, 0, 0), simplify = FALSE))
ctl$size_selex_types <- as.data.frame(ctl$size_selex_types) # Convert matrix to dataframe and deal with PType and printdf in SS_writectl_3.30
ctl$age_selex_types <- do.call("rbind", replicate(n = ss3_data$Nfleets, expr = c(17, 0, 0, ss3_data$Nages), simplify = FALSE))
ctl$age_selex_types <- as.data.frame(ctl$age_selex_types)

ctl$age_selex_parms <- data.frame(
  "LO" = c(-10002, -1, rep(-10, ss3_data$Nages - 1)),
  "HI" = c(1, rep(10, ss3_data$Nages)),
  "INIT" = c(-1000, 0, rep(0.01, ss3_data$Nages - 1)),
  "PRIOR" = 0, "SD" = 0, "PR_TYPE" = 0,
  "PHASE" = c(-4, -4, rep(4, ss3_data$Nages - 1)),
  matrix(0, ncol = 7, nrow = ss3_data$Nages + 1)
)
ctl$age_selex_parms <- do.call(
  "rbind",
  replicate(length(survnames) + 1,
    ctl$age_selex_parms,
    simplify = FALSE
  )
)

dmpars <- do.call("rbind", replicate(length(survnames) + 1,
  data.frame(
    "LO" = -7, "HI" = 7, "INIT" = 1,
    "PRIOR" = 0, "SD" = 0, "PR_TYPE" = 0, "PHASE" = 7,
    0, 0, 0, 0, 0, 0, 0
  ),
  simplify = FALSE
))
colnames(dmpars) <- colnames(simple_ctl$age_selex_parms)
colnames(ctl$age_selex_parms) <- colnames(simple_ctl$age_selex_parms)
ctl$age_selex_parms <- rbind(ctl$age_selex_parms, dmpars)

slx <- 12

if (slx == 26) { # Exponential logistic
  ctl$age_selex_types <- do.call("rbind", replicate(n = ss3_data$Nfleets, expr = c(26, 0, 0, 0), simplify = FALSE))
  ctl$age_selex_types <- as.data.frame(ctl$age_selex_types)

  ctl$age_selex_parms <- data.frame(
    "LO" = rep(c(0.02, 0.0001, 0.0001), ss3_data$Nfleets),
    "HI" = rep(c(max(ss3_data$agebin_vector), 1, 1), ss3_data$Nfleets),
    "INIT" = c(c(2, 0.9, 0.01), c(0.9, 0.001, 0.9), c(0.9, 0.001, 0.9)), # use -999 to decay young and old fish selectivity according to p3 and p4
    "PRIOR" = 0, "SD" = 1, "PR_TYPE" = 0,
    "PHASE" = rep(c(2, 2, 2), ss3_data$Nfleets), # Fix -999 options and parameters 2 and 4
    matrix(0, ncol = 7, nrow = ss3_data$Nfleets)
  )
  colnames(ctl$age_selex_parms) <- colnames(simple_ctl$age_selex_parms)
}

if (slx == 12) { # Simple logistic
  ctl$age_selex_types <- do.call("rbind", replicate(n = ss3_data$Nfleets, expr = c(12, 0, 0, 0), simplify = FALSE))
  ctl$age_selex_types <- as.data.frame(ctl$age_selex_types)

  ctl$age_selex_parms <- data.frame(
    "LO" = rep(c(1, 0.1), ss3_data$Nfleets),
    "HI" = rep(c(max(ss3_data$Nages), 5), ss3_data$Nfleets),
    "INIT" = c(10, 3, 5, 1, 5, 1),
    "PRIOR" = 0, "SD" = 0, "PR_TYPE" = 0,
    "PHASE" = rep(2, ss3_data$Nfleets * 2),
    matrix(0, ncol = 7, nrow = ss3_data$Nfleets * 2)
  )
  colnames(ctl$age_selex_parms) <- colnames(simple_ctl$age_selex_parms)
}

if (slx == 20) { # double normal
  ctl$age_selex_types <- do.call("rbind", replicate(n = ss3_data$Nfleets, expr = c(20, 0, 0, 0), simplify = FALSE))
  ctl$age_selex_types <- as.data.frame(ctl$age_selex_types)

  ctl$age_selex_parms <- data.frame(
    "LO" = rep(c(0, rep(-15, 5)), ss3_data$Nfleets),
    "HI" = rep(c(max(ss3_data$Nages), rep(15, 5)), ss3_data$Nfleets),
    "INIT" = rep(c(max(ss3_data$Nages) / 2, 3, 5, 5, rep(-999, 2)), ss3_data$Nfleets), # use -999 to decay young and old fish selectivity according to p3 and p4
    "PRIOR" = 0, "SD" = 0, "PR_TYPE" = 0,
    "PHASE" = rep(c(2, 1, 2, 1, rep(-1, 2)), ss3_data$Nfleets), # Fix -999 options and parameters 2 and 4
    matrix(0, ncol = 7, nrow = ss3_data$Nfleets)
  )
  colnames(ctl$age_selex_parms) <- colnames(simple_ctl$age_selex_parms)
}

fleet_slx17 <- data.frame(
  "LO" = c(-1002, rep(-5, ss3_data$Nages)),
  "HI" = c(3, rep(9, ss3_data$Nages)),
  "INIT" = c(-1000, rep(0.01, 16), rep(1, 4)),
  "PRIOR" = 0, "SD" = 0, "PR_TYPE" = 0,
  "PHASE" = c(-2, rep(2, ss3_data$Nages)),
  matrix(0, ncol = 7, nrow = ss3_data$Nfleets)
)
colnames(fleet_slx17) <- colnames(simple_ctl$age_selex_parms)
ctl$age_selex_parms <- rbind(
  fleet_slx17,
  ctl$age_selex_parms[3:nrow(ctl$age_selex_parms), ]
)

ctl$age_selex_types[1, ] <- c(17, 0, 0, 0)
ctl$age_selex_types

ctl$size_selex_parms <- NULL

# todo: implement added SD for all surveys
ctl$Q_options <- data.frame(
  "fleet" = 2:(length(survnames) + 1),
  "link" = 1, "link_info" = 0, "extra_se" = 0,
  "biasadj" = 0, "float" = 0
)

ctl$Q_parms <- rbind(data.frame(
  "LO" = rep(-10, length(survnames)),
  "HI" = rep(10, length(survnames)),
  "INIT" = log(jitter(rep(0.05, length(survnames)), 30)),
  "PRIOR" = rep(0, length(survnames)),
  "SD" = rep(0, length(survnames)),
  "PR_TYPE" = rep(0, length(survnames)),
  "PHASE" = rep(1, length(survnames)),
  matrix(0, ncol = 7, nrow = length(survnames))
))


# todo: this assumes a time-invariant fixed natural mortality
ctl$natM_type <- 3
####################################################################################
# matage <- rep(3, ss3_data$Nages+1)
matage <- rep(0.2, ss3_data$Nages + 1)
# matage <- c(0.2, 1.2, 0.8, rep(0.2, 18)) ###

if (ss3_data$Nsexes == 1) {
  ctl$natM <- as.data.frame(matage)
}

if (ss3_data$Nsexes == 2) {
  ctl$natM <- as.data.frame(rbind(matage, matage))
}

ctl$MG_parms <- ctl$MG_parms[-grep("NatM", rownames(ctl$MG_parms)), ]

if (ss3_data$Nsexes == 1) {
  ctl$MG_parms <- ctl$MG_parms[-grep("Mal", rownames(ctl$MG_parms)), ]
}

# SR
ctl$Use_steep_init_equi <- 0

Fmult.y1 <- 0.1
naa.y1 <- (ctl$natM[1, 1] / (ctl$natM[1, 1] + Fmult.y1)) * ss3_data$catch$catch[2:nrow(ss3_data$catch)] / (1 - exp(-ctl$natM[1, 1] - Fmult.y1))

if (naa.y1[1] %in% sort(naa.y1)[1:round(length(naa.y1) / 5)]) naa.y1[1] <- 10 * mean(naa.y1)

ctl$SR_function <- 3 # 3: B-H
ctl$SR_parms[grep("sigma", rownames(ctl$SR_parms)), "INIT"] <- 0.1
ctl$SR_parms[grep("sigma", rownames(ctl$SR_parms)), "PHASE"] <- -2 ###
ctl$SR_parms[grep("steep", rownames(ctl$SR_parms)), "INIT"] <- 0.99
ctl$SR_parms[grep("steep", rownames(ctl$SR_parms)), "PHASE"] <- -2 ###
ctl$SR_parms[grep("steep", rownames(ctl$SR_parms)), "PR_type"] <- 0
ctl$SR_parms[grep("R0", rownames(ctl$SR_parms)), "INIT"] <- log(naa.y1[1])

ctl$N_lambdas <- 1
ctl$lambdas <- ctl$lambdas[-c(1:nrow(ctl$lambdas)), ]
ctl$lambdas[1, ] <- c(9, 1, 1, 0, 1)

ctl$more_stddev_reporting <- 0

ctl$stddev_reporting_selex[1] <- -1
ctl$stddev_reporting_growth[1] <- -1
ctl$stddev_reporting_N_at_A[1] <- -1

if (f.method == 2) {
  ctl$F_Method <- 2
  ctl$F_setup <- c(0.01, 5, 0.00)
  names(ctl$F_setup) <- c("F_setup_1", "F_setup_2", "F_setup_3")
  ctl$init_F <- data.frame(
    "LO" = 0,
    "HI" = 5,
    "INIT" = 0.01,
    "PRIOR" = 0.01,
    "PR_SD" = 0.2,
    "PR_type" = 0,
    "PHASE" = 1,
    "PType" = 18
  )
  ctl$maxF <- 5
}

YOY <- omlist_ss$YOY_ss
YOY_ss <- YOY %>%
  select(Time, "NCO.0")
plot(YOY_ss$NCO.0[YOY_ss$Time %in% fittimes.days])

truenums_ss <- truth$nums[truth$nums$species == species, ]
fgs <- truth$fgs
biolprm <- truth$biolprm

fullresZ <- calc_Z(
  yoy = YOY_ss,
  nums = truenums_ss,
  fgs = fgs,
  biolprm = biolprm,
  toutinc = omlist_ss$runpar$toutinc
)
Z <- mean(fullresZ$atoutput)

# cod_ctl <- SS_write_biol(ctl_obj = ctl,
#                          biolprm_object = biolprm,
#                          species_code = species_code,
#                          M_est = NULL, wtsage=meanwt_spp)

# vector of parameters needed
needed_pars <- c("BHalpha", "BHbeta", "kgw2d", "redfieldcn", "maturityogive", "fsp", "kwsr", "kwrr", "wl")
ind <- rep(0, length(needed_pars))

# Extract needed parameters from a list and assign them to unique vars
for (i in 1:length(needed_pars)) {
  if (is.null(biolprm[needed_pars[i]])) {
    stop(paste("Warning: the biolprm object is missing parameter", needed_pars[i], "- cannot write biology."))
  }
  if (length(biolprm[[needed_pars[i]]]) == 1) {
    assign(needed_pars[i], biolprm[[needed_pars[i]]])
  } else {
    # index of the parameter object matching the species
    ind[i] <- which(biolprm[[needed_pars[i]]][, 1] == species_code)

    if (is.null(ind[i])) {
      stop(paste("Missing value for", needed_pars[i], "for species", species_code))
    }

    # assign to the right variable name
    assign(needed_pars[i], biolprm[[needed_pars[i]]][ind[i], -1])
  }
}

# Calculate recruitment parameters from atlantis values

meanwt_spp <- surv_wtage_species[[2]] %>%
  filter(time > burnin) %>%
  group_by(agecl) %>%
  summarize(meanwt = mean(atoutput))
# Translate weight at age from grams to nitrogen
wtsage_N <- meanwt_spp %>%
  mutate(weight = meanwt * 20 * 5.7)

M_est <- NULL
if (is.null(M_est)) {
  M_est <- 0.2
}


# bh_lnro <- log(BHalpha) - log(kwrr+kwsr)
# # sb0 <- exp(bh_lnro)*sum(exp(-M_est*wtsage_N[,"agecl"])*fsp*wtsage_N[,"weight"]*as.numeric(t(maturity_mean)))
# sb0 <- exp(bh_lnro)*sum(exp(-M_est*wtsage_N[,"agecl"])*fsp*wtsage_N[,"weight"]*as.numeric(t(maturity_mean)))/100000
# b0 <- sum(exp(-Z*wtsage_N[,"agecl"])*exp(bh_lnro)*wtsage_N[,"weight"])
# bh_steepness <- ((kwrr+kwsr)*0.2*sb0)/(BHbeta+0.2*sb0)
# ctl$SR_parms[grep("steep", rownames(ctl$SR_parms)), "INIT"] <- bh_steepness

r4ss::SS_writectl(ctl,
  outfile = file.path(user.od, "control.ss"),
  overwrite = TRUE, verbose = FALSE, version = "3.30"
)


# Write starter.ss for Stock Synthesis ------------------------------------

stocksynthesis.starter <- r4ss::SS_readstarter(
  verbose = FALSE,
  file = dir(utils::tail(dir(system.file("extdata", package = "r4ss"), pattern = "simple", full.names = TRUE), 1), pattern = "starter", full.names = TRUE)
)

stocksynthesis.starter$sourcefile <- file.path(here("NOBA_cod_files", "output", "atlantis2ss"), "starter.ss")
stocksynthesis.starter$datfile <- "data.ss"
stocksynthesis.starter$ctlfile <- "control.ss"

if (is.null(report.ages)) {
  stocksynthesis.starter$F_age_range <- range(ss3_data$agebin_vector)
} else {
  stocksynthesis.starter$F_age_range <- report.ages
}


stocksynthesis.starter$F_report_basis <- 0

r4ss::SS_writestarter(stocksynthesis.starter,
  dir = user.od,
  overwrite = TRUE, verbose = FALSE
)

# Write forecast.ss for Stock Synthesis ------------------------------------

stocksynthesis.forecast <- r4ss::SS_readforecast(
  verbose = FALSE,
  file = dir(utils::tail(dir(system.file("extdata", package = "r4ss"), pattern = "simple", full.names = TRUE), 1), pattern = "forecast", full.names = TRUE)
)

stocksynthesis.forecast$sourcefile <- paste0(user.od, "forecast.ss")
stocksynthesis.forecast$benchmarks <- 1
stocksynthesis.forecast$MSY <- 2
# forecast$SPRtarget
# forecast$Btarget
stocksynthesis.forecast$Bmark_years <- rep(c(-999, 0), 5)
stocksynthesis.forecast$Bmark_relF_Basis <- 2
stocksynthesis.forecast$Forecast <- 4
stocksynthesis.forecast$Nforecastyrs <- forN
# forecast$F_scalar
stocksynthesis.forecast$Fcast_years <- rep(c(-999, 0), 3)
stocksynthesis.forecast$Fcast_selex <- 0
stocksynthesis.forecast$ControlRuleMethod <- 1
# forecast$BforconstantF
# forecast$BfornoF
stocksynthesis.forecast$Flimitfraction <- 1
stocksynthesis.forecast$FirstYear_for_caps_and_allocations <- max(fit_years) + 1
r4ss::SS_writeforecast(stocksynthesis.forecast,
  dir = user.od,
  overwrite = TRUE, verbose = FALSE
)
