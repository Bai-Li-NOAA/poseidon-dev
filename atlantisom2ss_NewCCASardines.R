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

#The last timestep to sample
total_sample <- noutsteps-1 #599
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

# Survey month
survey_sample_month <- c(7)

# Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr # 5

# Which atlantis timestep does the survey run in?--now from census_spec.R
# with 5 output steps per year, 0 is Jan-Feb-midMar, 1 is midMar-Apr-May,
# 2 is June-July-midAug, 3 is midAug-Sept-Oct, 4 is Nov-Dec (ish)

survey_sample_time <- midptyr # 2; defined in omdimensions.R

# Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
  total_sample,
  by = timestep
)

survtime <- survey_sample_full

### survey area
### should return all model areas
### survboxes <- allboxes

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

settlement_age <- 1
ss3_data_lbin_method <- 1
ss3_data_use_lencomp <- 1

ss3_data_agebin_vector <- unique(CCom$truth$biomass_ages$agecl[CCom$truth$biomass_ages$species == species_ss])
ss3_data_N_agebins <- length(ss3_data_agebin_vector)

ss3_ctl_Growth_Age_for_L1 <- 0.5

ss3_ctl_maturity_option <- 3 # read age-maturity matrix by growth_pattern
ss3_ctl_Age_Maturity <- data.frame(t(c(0, 0.1, 0.5, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)))

# LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
ss3_ctl_MG_parms_NatM <- c(0.2, 0.7, 0.4, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_L_at_Amin <- c(15, 30, 15.188, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_L_at_Amax <- c(40, 50, 48.39, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_VonBert_K <- c(0.05, 0.99, 0.4, 0, 99, 0, 3, rep(0, 7))
ss3_ctl_MG_parms_CV_young <- c(0.05, 0.3, 0.25, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_CV_old <- c(0.01, 0.2, 0.09, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_Wtlen_1 <- c(-3, 3, 1.0e-06, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_Wtlen_2 <- c(-3, 5, 3.113, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_Mat50 <- c(9, 19, 15.88, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_Mat_slope <- c(-20, 3, -0.90461, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_Eggs_alpha <- c(0, 10, 1, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_Eggs_beta <- c(-1, 5, 0, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_CohortGrowDev <- c(-4, 4, 1, 0, 99, 0, -3, rep(0, 7))
ss3_ctl_MG_parms_FracFemale <- c(0.000001, 0.999999, 0.5, 0.5, 0.5, 0, -3, rep(0, 7))

ss3_ctl_SR_parms_R0 <- c(3, 25, 20.45, 0, 99, 0, 1, rep(0, 7))
ss3_ctl_SR_parms_steep <- c(0.2, 1, 0.95, 0, 99, 0, -1, rep(0, 7))
ss3_ctl_SR_parms_sigmaR <- c(0, 2, 0.727, 0, 99, 0, -3, rep(0, 7))

ss3_ctl_Q_parms_init <- -0.69

ss3_ctl_age_selex_types_pattern <- c(10, 0)
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
fishery.name <- names(truecatchbio)

survObsBiom_data <- atlantisom::read_savedsurvs(d.name, "survB")
survey.name <- names(survObsBiom_data)
# Survey and fishery catch sample time
names(survey_sample_time) <- names(survey_sample_month) <- survey.name
survObsBiom_ss <- survey_years <- vector("list", length = length(survObsBiom_data))
for (i in seq_along(survObsBiom_data)) {
  species_data <- survObsBiom_data[[i]][[1]][survObsBiom_data[[i]][[1]]$species %in% species_ss, ]
  species_data_sort <- species_data[order(species_data$time), ]
  survObsBiom_ss[[i]] <- species_data_sort$atoutput[species_data_sort$time %in% fittimes]
  survey_years[[i]] <- (species_data_sort$time[fit_years] - survey_sample_time[survey.name[i]]) / timestep + 1
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

len_comp_flat <- len_comp_final <- vector("list", length = length(survObsBiom_data))
for (i in seq_along(survObsBiom_data)){
  len_comp <- len_comp_data[[i]]
  ### len <- len_comp
  # Add over age classes to get sample size
  len_comp_flat[[i]] <- atlantisom::reformat_compositions(len_comp,
                                                     round.places = 0,
                                                     comp_type = "lencomp"
  )
  len_comp_flat[[i]]$time <- (len_comp_flat[[i]]$time-2)/5
  # remove burnin
  len_comp_final[[i]] <- filter(
    len_comp_flat[[i]],
    time %in% survey_years[[i]]
  )
  len_comp_final[[i]] <- as.data.frame(len_comp_final[[i]])
  
  length_bins <- as.integer(names(len_comp_final[[i]]))
  length_bins <- length_bins[!is.na(length_bins)]
}

fish_len_comp_data <- fish_len_comp_data[[1]]
fish_len_comp_data$time <- as.integer(floor(fish_len_comp_data$time / fstepperyr))
if (fstepperyr > 1) {
  fish_len_comp_anndata <- fish_len_comp_data %>%
    mutate(yr = floor(time / fstepperyr)) %>%
    group_by(species, agecl, lower.bins, upper.bins, time = as.integer(yr)) %>%
    summarise(annnatlength = sum(atoutput)) %>%
    rename(atoutput = annnatlength)
} else {
  fish_len_comp_anndata <- fish_len_comp_data
}

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

# need to fill empty length bins with 0s to have same bins as survey for SS_write_comps
missing.lengths <- setdiff(length_bins, names(fish_len_comp_final)[!names(fish_len_comp_final) %in% notbins])
fish_len_comp_final[, as.character(missing.lengths)] <- 0 # Add them, filled with '0's
fish_len_comp_final <- fish_len_comp_final[c("time", length_bins, "nsamp")]

caal_comp_flat <- reformat_compositions(len_comp_data[[1]],
                                        round.places = 4,
                                        comp_type = "caalcomp"
)

caal_comp_flat$time <- (caal_comp_flat$time-2)/5
# remove burnin
caal_comp_final <- filter(
  caal_comp_flat,
  time %in% survey_years[[1]]
)
caal_comp_final <- as.data.frame(caal_comp_final)

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


age_comp_flat <- age_comp_final <- vector("list", length = length(survObsBiom_data))
for (i in seq_along(survObsBiom_data)){
  age_comp <- age_comp_data[[i]]
  # Add over age classes to get sample size
  age_comp_flat[[i]] <- atlantisom::reformat_compositions(age_comp,
                                                          comp_type = "agecomp"
  )
  age_comp_flat[[i]]$time <- (age_comp_flat[[i]]$time-2)/5
  # remove burnin
  age_comp_final[[i]] <- filter(
    age_comp_flat[[i]],
    time %in% survey_years[[i]]
  )
  age_comp_final[[i]] <- as.data.frame(age_comp_final[[i]])
  
  survey_age_bins <- as.integer(names(age_comp_final[[i]]))
  survey_age_bins <- survey_age_bins[!is.na(survey_age_bins)]
}
  


# Load weight-at-age data
wtage <- readRDS(file.path(d.name, paste0(
  scenario.name,
  "_",
  survey.name, "survObsWtAtAge.rds"
)))
wtage <- wtage[[1]]

## From project page: https://ices-eg.github.io/wg_WGSAM/SkillAssessProject.html
survey_cv <- surv_cv$cv
names(survey_cv) <- survey.name
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
ss3_data$surveytiming <- c(-1, survey_sample_month)
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
    "year" = survey_years[[i]], 
    "seas" = survey_sample_month[i],
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
ss3_data$lbin_vector <- ss3_data$lbin_vector_pop <- length_bins

survey_lencomp_data <- list()
for (i in 1:length(survObsBiom_ss)){
  survey_lencomp_data[[i]] <- data.frame(
    Yr = len_comp_final[[i]]$time,
    Seas = survey_sample_month[i],
    FltSvy = i + ss3_data$Nfleet,
    Gender = 0,
    Part = 0,
    Nsamp = len_comp_final[[i]]$nsamp,
    len_comp_final[[i]][, as.character(length_bins)]
  )
}

ss3_data$lencomp <- do.call(rbind, survey_lencomp_data)
ss3_data$lencomp <- rbind(
  ss3_data$lencomp, 
  data.frame(
    Yr = fish_len_comp_final$time,
    Seas = 1,
    FltSvy = 1,
    Gender = 0,
    Part = 0,
    Nsamp = fish_len_comp_final$nsamp,
    fish_len_comp_final[, as.character(length_bins)]
  )
)

# Writing age composition data
ss3_data$N_agebins <- ss3_data_N_agebins
ss3_data$agebin_vector <- ss3_data_agebin_vector
ss3_data$N_ageerror_definitions <- 1
ss3_data$ageerror <- matrix(c(rep(-1, ss3_data$Nages + 1), rep(0.001, ss3_data$Nages + 1)), nrow = 2, byrow = TRUE)
ss3_data$age_info <- data.frame(
  mintailcomp = rep(0, times = ss3_data$Nfleets),
  addtocomp = 1e-7,
  combine_M_F = 1,
  CompressBins = 0,
  CompError = 0,
  ParmSelect = 0,
  minsamplesize = 0.001
)
row.names(ss3_data$age_info) <- ss3_data$fleetnames

ss3_data$Lbin_method <- 3 # lengths

survey_agecomp_data <- list()
for (i in 1:length(survObsBiom_ss)){
  survey_agecomp_data[[i]] <- data.frame(
    Yr = caal_comp_final$time,
    Seas = survey_sample_month[i],
    FltSvy = i + ss3_data$Nfleet,
    Gender = 0,
    Part = 0,
    Ageerr = 1, 
    Lbin_lo = caal_comp_final$lower.bins,
    Lbin_hi = caal_comp_final$upper.bins,
    Nsamp = caal_comp_final$nsamp,
    caal_comp_final[, as.character(survey_age_bins)]
  )
}

ss3_data$agecomp <- do.call(rbind, survey_agecomp_data)
ss3_data$agecomp <- rbind(
  ss3_data$agecomp, 
  data.frame(
    Yr = fish_age_comp_final$time,
    Seas = 1,
    FltSvy = 1,
    Gender = 0,
    Part = 0,
    Ageerr = 1, 
    Lbin_lo = -1,
    Lbin_hi = -1,
    Nsamp = fish_age_comp_final$nsamp,
    fish_age_comp_final[, as.character(age_bins)]
  )
)

ss3_data$use_MeanSize_at_Age_obs <-
  ss3_data$N_environ_variables <-
  ss3_data$N_sizefreq_methods <-
  ss3_data$do_tags <-
  ss3_data$morphcomp_data <-
  ss3_data$use_selectivity_priors <- 0

ss3_data$MeanSize_at_Age_obs <- NULL

r4ss::SS_writedat(
  datlist = ss3_data, verbose = FALSE, outfile = file.path(user_od, "data.ss"),
  overwrite = TRUE
)

# Write control.ss for Stock Synthesis ------------------------------------
ss3_ctl <- ss3_simple_ctl <- r4ss::SS_readctl(
  verbose = FALSE,
  file = dir(utils::tail(dir(system.file("extdata", package = "r4ss"), pattern = "simple", full.names = TRUE), 1), pattern = "control", full.names = TRUE),
  use_datlist = TRUE,
  datlist = dir(utils::tail(dir(system.file("extdata", package = "r4ss"), pattern = "simple", full.names = TRUE), 1), pattern = "data", full.names = TRUE)
)

ss3_ctl$EmpiricalWAA <- 0 # SS3 default

ss3_ctl$recr_dist_method <- 4 # or 2 from previous sardine settings? 4 is SS3 default
if (settlement_age == 1) ss3_ctl$recr_dist_pattern$age <- 1
ss3_ctl$N_Block_Designs <- 0 # or 1 from previous sardine settings? 0 is SS3 default

# Natural mortality
ss3_ctl$natM_type <- 0 # 1 parameter; SS3 default

# Growth model
ss3_ctl$GrowthModel <- 1 # SS3 default
ss3_ctl$Growth_Age_for_L1 <- ss3_ctl_Growth_Age_for_L1
ss3_ctl$Growth_Age_for_L2 <- 999 # to use as Linf; SS3 default
ss3_ctl$Exp_Decay <- -999 # SS3 default

# Maturity option
ss3_ctl$maturity_option <- ss3_ctl_maturity_option 
ss3_ctl$Age_Maturity <- ss3_ctl_Age_Maturity

# Growth parameters
if (ss3_data$Nsexes == 1 | ss3_data$Nsexes == -1) {
  ss3_ctl$MG_parms <- ss3_ctl$MG_parms[-grep("Mal", rownames(ss3_ctl$MG_parms)), ]
}

if (ss3_ctl$natM_type == 0) ss3_ctl$MG_parms[grep("NatM", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_NatM 
ss3_ctl$MG_parms[grep("L_at_Amin", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_L_at_Amin 
ss3_ctl$MG_parms[grep("L_at_Amax", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_L_at_Amax 
ss3_ctl$MG_parms[grep("VonBert_K", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_VonBert_K
ss3_ctl$MG_parms[grep("CV_young", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_CV_young  
ss3_ctl$MG_parms[grep("CV_old", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_CV_old 
ss3_ctl$MG_parms[grep("Wtlen_1", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_Wtlen_1 
ss3_ctl$MG_parms[grep("Wtlen_2", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_Wtlen_2 
ss3_ctl$MG_parms[grep("Mat50", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_Mat50 
ss3_ctl$MG_parms[grep("Mat_slope", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_Mat_slope 
ss3_ctl$MG_parms[grep("Eggs_alpha", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_Eggs_alpha 
ss3_ctl$MG_parms[grep("Eggs_beta", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_Eggs_beta 
ss3_ctl$MG_parms[grep("CohortGrowDev", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_CohortGrowDev 
ss3_ctl$MG_parms[grep("FracFemale", rownames(ss3_ctl$MG_parms)), ] <- ss3_ctl_MG_parms_FracFemale 

# spawner-recruitment
ss3_ctl$SR_parms[grep("R0", rownames(ss3_ctl$SR_parms)), ] <- ss3_ctl_SR_parms_R0 
ss3_ctl$SR_parms[grep("steep", rownames(ss3_ctl$SR_parms)), ] <- ss3_ctl_SR_parms_steep 
ss3_ctl$SR_parms[grep("sigmaR", rownames(ss3_ctl$SR_parms)), ] <- ss3_ctl_SR_parms_sigmaR 

# rec dev
ss3_ctl$MainRdevYrFirst <- ss3_data$styr
ss3_ctl$MainRdevYrLast <- ss3_data$endyr
ss3_ctl$recdev_phase <- 1
### ss3_ctl$N_Read_recdevs <- 1
ss3_ctl$recdev_early_phase <- 2
ss3_ctl$Fcast_recr_phase <- 0
ss3_ctl$lambda4Fcast_recr_like <- 1
ss3_ctl$first_recent_yr_nobias_adj <- ss3_data$endyr + 1
ss3_ctl$last_early_yr_nobias_adj <- ss3_data$styr - 1
ss3_ctl$first_yr_fullbias_adj <- ss3_data$styr
ss3_ctl$last_yr_fullbias_adj <- ss3_data$endyr
ss3_ctl$max_bias_adj <- 0

# Fishing mortality
ss3_ctl$F_ballpark <- 0.1
ss3_ctl$F_ballpark_year <- -ss3_data$endyr
ss3_ctl$F_Method <- 3
ss3_ctl$maxF <- 4
ss3_ctl$F_iter <- 5

# Catchability
ss3_ctl$Q_options <- data.frame(
  fleet = ss3_data$CPUEinfo$Fleet[2:length(ss3_data$CPUEinfo$Fleet)],
  link = 1,
  link_info = 0,
  extra_se = 0,
  biasadj = 0,
  float = 0
)
row.names(ss3_ctl$Q_options) <- ss3_data$fleetnames[-1]

ss3_ctl$Q_parms <- rbind(data.frame(
  "LO" = rep(-10, ss3_data$Nsurveys),
  "HI" = rep(10, ss3_data$Nsurveys),
  "INIT" = ss3_ctl_Q_parms_init,
  "PRIOR" = rep(0, ss3_data$Nsurveys),
  "SD" = rep(0, ss3_data$Nsurveys),
  "PR_TYPE" = rep(0, ss3_data$Nsurveys),
  "PHASE" = rep(1, ss3_data$Nsurveys),
  
  matrix(0, ncol = 7, nrow = ss3_data$Nsurveys)
))

# Selectivity
ss3_ctl$size_selex_types <- data.frame(
  Pattern = rep(0, ss3_data$Nfleets),
  Discard = 0,
  Male = 0,
  Special = 0
)
row.names(ss3_ctl$size_selex_types) <- ss3_data$fleetnames

ss3_ctl$age_selex_types <- data.frame(
  Pattern = ss3_ctl_age_selex_types_pattern,
  Discard = 0,
  Male = 0,
  Special = 0
)
row.names(ss3_ctl$age_selex_types) <- ss3_data$fleetnames

ss3_ctl$N_lambdas <- 1
ss3_ctl$lambdas <- data.frame(
  like_comp = 9,
  fleet = 1,
  phase = 1,
  value = 0,
  sizefreq_method = 1
)
ss3_ctl$more_stddev_reporting <- 0

r4ss::SS_writectl(ss3_ctl,
  outfile = file.path(user_od, "control.ss"),
  overwrite = TRUE, verbose = FALSE, version = "3.30"
)


# Write starter.ss for Stock Synthesis ------------------------------------

ss3_starter <- ss3_simple_starter <- r4ss::SS_readstarter(
  verbose = FALSE,
  file = dir(utils::tail(dir(system.file("extdata", package = "r4ss"), pattern = "simple", full.names = TRUE), 1), pattern = "starter", full.names = TRUE)
)

ss3_starter$sourcefile <- file.path(user_od, "starter.ss")
ss3_starter$datfile <- "data.ss"
ss3_starter$ctlfile <- "control.ss"

ss3_starter$run_display_detail <- 1
ss3_starter$parmtrace <- 4
ss3_starter$cumreport <- 2
ss3_starter$prior_like <- 0
ss3_starter$N_bootstraps <- 2
ss3_starter$MCMCburn <- 10
ss3_starter$MCMCthin <- 2
ss3_starter$maxyr_sdreport <- -1
ss3_starter$SPR_basis <- 1
ss3_starter$F_report_units <- 1

r4ss::SS_writestarter(ss3_starter,
  dir = user_od,
  overwrite = TRUE, verbose = FALSE
)

# Write forecast.ss for Stock Synthesis ------------------------------------

ss3_forecast <- ss3_simple_forecast <- r4ss::SS_readforecast(
  verbose = FALSE,
  file = dir(utils::tail(dir(system.file("extdata", package = "r4ss"), pattern = "simple", full.names = TRUE), 1), pattern = "forecast", full.names = TRUE)
)

ss3_forecast$sourcefile <- paste0(user_od, "forecast.ss")
ss3_forecast$benchmarks <- 1
ss3_forecast$MSY <- 2
ss3_forecast$SPRtarget <- 0.4
ss3_forecast$Btarget <- 0.4
ss3_forecast$Bmark_years <- rep(c(ss3_data$endyr, ss3_data$endyr), 5)
ss3_forecast$Bmark_relF_Basis


ss3_forecast$Bmark_relF_Basis <- 2
ss3_forecast$Forecast <- 4
ss3_forecast$Nforecastyrs <- 1
# forecast$F_scalar
ss3_forecast$Fcast_years <- rep(c(-999, 0), 3)
ss3_forecast$Fcast_selex <- 0
ss3_forecast$ControlRuleMethod <- 2
ss3_forecast$BforconstantF <- 0.4
ss3_forecast$BfornoF <- 0.01
ss3_forecast$Flimitfraction <- 0
ss3_forecast$N_forecast_loops <- 3
ss3_forecast$First_forecast_loop_with_stochastic_recruitment <- 3
ss3_forecast$fcast_rec_option <- 0
ss3_forecast$FirstYear_for_caps_and_allocations <- ss3_data$endyr+1
ss3_forecast$Ydecl <- ss3_data$endyr
ss3_forecast$Yinit <- ss3_data$endyr

r4ss::SS_writeforecast(ss3_forecast,
  dir = user_od,
  overwrite = TRUE, verbose = FALSE
)
