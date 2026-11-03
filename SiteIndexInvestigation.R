##site index investigation
library(cmdstanr)
library(climr)
library(data.table)
library(ggplot2)
library(Rcpp)

##SIBayes models
source("./portfolio_fns.R")
source("./sprint_functions.R")
sourceCpp("./src/growth_sim.cpp")

source("../SIBayes/draw_si_sigmoid.R")
run_dir <- "../SIBayes/si_fits_quadratic_sigmoid/run_5fb023e0188b/"

fit_sigmoid <- list(
  fit      = readRDS(file.path(run_dir, "fit.rds")),
  prepared = readRDS(file.path(run_dir, "prepared.rds"))
)

current_unit <- "ICHdw1/101"
bgc_nm <- "ICHdw1"
location <- data.table(lat =  49.10443,	lon = -117.78877, elev =	1088, id = 1)

current_unit <- "SBSmc2/01"
location <- data.table(lat =  54.7797,	lon = -126.91428, elev =	896, id = 1)
bgc_nm <- "SBSmc2"
# 
current_unit <- "IDFxh1/101"
bgc_nm <- "IDFxh1"
folder <- "IDF_Example/"
location <- data.table(lat =  50.23796,	lon = -119.22145, elev =	484, id = 1)

#### climate data for site index
clim_dat <- downscale(location, gcms = "MPI-ESM1-2-HR", ssps = "ssp245", 
                      run_nm = "r1i1p1f1", gcm_ssp_years = 2015:2100, 
                      vars = c("CMD_an", "DD5_an"), db_option = "local")
clim_si <- clim_dat[,.(id,GCM,SSP,RUN,PERIOD, CMD_an, DD5_an)]
clim_si[,run_id := interaction(id,GCM,SSP,RUN)]
clim_si <- na.omit(clim_si)

spp_list <- c("Fd", "Pl", "Lw", "Py", "At", "Cw", "Hw")

###suitability covariance matrix
suit <- copy(ccissr::S1)
suit <- suit[spp %in% spp_list,]
suit <- unique(suit)

smat <- dcast(suit, ss_nospace ~ spp, value.var = "newfeas", fun.aggregate = mean)
num_cols <- names(smat)[sapply(smat, is.numeric)]
setnafill(smat, fill = 5, cols = num_cols)
smat[,ss_nospace := NULL]

R <- cor(
  smat,
  method = "spearman"
)
# library(psych)
# 
# R <- polychoric(
#   smat
# )$rho

sd_vec <- apply(
  smat,
  2,
  sd
)
sigma2 <- diag(sd_vec) %*%
  R %*%
  diag(sd_vec)


rownames(sigma2) <- names(smat)
colnames(sigma2) <- names(smat)

###testing different covariance options with portfolios

port_suit_corr <- plot_portfolio_draws(
  climate = clim_si,
  spp_list = spp_list,
  bgc = bgc_nm,
  covariance = R,
  n_draws = 100)

ggsave(
  paste0(folder,"ef_suit_corr.png"),
  port_suit_corr$plot,
  width = 12,
  height = 13,
  dpi = 300
)

port_suit_cov <- plot_portfolio_draws(
  climate = clim_temp,
  spp_list = spp_list,
  bgc = bgc_nm,
  covariance = sigma2,
  n_draws = 100)

ggsave(
  paste0(folder,"ef_suit_cov.png"),
  port_suit_cov$plot,
  width = 12,
  height = 13,
  dpi = 300
)

port_SI_cov <- plot_portfolio_draws(
  climate = clim_temp,
  spp_list = spp_list,
  bgc = bgc_nm,
  covariance = NULL,
  use_returns = TRUE,
  n_draws = 100)

ggsave(
  paste0(folder,"ef_SI_cov.png"),
  port_SI_cov$plot,
  width = 12,
  height = 13,
  dpi = 300
)

port_suit_corr_SI_sd <- plot_portfolio_draws(
  climate = clim_temp,
  spp_list = spp_list,
  bgc = bgc_nm,
  covariance = R,
  use_returns_sd = TRUE,
  n_draws = 100)

ggsave(
  paste0(folder,"ef_suit_corr_SI_SD.png"),
  port_suit_corr_SI_sd$plot,
  width = 12,
  height = 13,
  dpi = 300
)

##combine them to make comparison bar plots
port_suit_corr$selected_summary[,setup := "Suit-Corr"]
port_suit_cov$selected_summary[,setup := "Suit-Cov"]
port_SI_cov$selected_summary[,setup := "SI-Cov"]
port_suit_corr_SI_sd$selected_summary[,setup := "Suit-Corr SI-SD"]

dat <- rbindlist(list(port_suit_corr$selected_summary,
                      port_suit_cov$selected_summary,
                      port_SI_cov$selected_summary,
                      port_suit_corr_SI_sd$selected_summary))

dat[,setup := factor(setup, levels = c("Suit-Corr","Suit-Cov", "Suit-Corr SI-SD", "SI-Cov"))]
ggplot(dat, aes(x = setup, y = mean_weight, group = species, fill = species)) +
  geom_bar(stat = "identity") +
  makeColScale(unique(dat$species), fread("./new_species_colours.csv")) +
  facet_wrap(~PortfolioType)

ggsave(
  paste0(folder, "Portfolio_Comparison.png"),
  width = 12,
  height = 6,
  dpi = 300
)


###Ordination
port_suit_corr$selected_weights[,setup := "Suit-Corr"]
port_suit_cov$selected_weights[,setup := "Suit-Cov"]
port_SI_cov$selected_weights[,setup := "SI-Cov"]
port_suit_corr_SI_sd$selected_weights[,setup := "Suit-Corr SI-SD"]

dat_draw <- rbindlist(list(port_suit_corr$selected_weights,
                      port_suit_cov$selected_weights,
                      port_SI_cov$selected_weights,
                      port_suit_corr_SI_sd$selected_weights))

##subsample so the nmds doesn't take years
draw_subset <- subsample_portfolio_draws(
  dat_draw,
  n_per_setup = 50,
  seed = 123
)

draw_ord <- portfolio_nmds(
  data = draw_subset,
  level = "draw",
  mean_data = dat,
  show_hulls = TRUE,
  trymax = 75
)

#temp <- draw_ord$distance_matrix

draw_ord$plot
ggsave(
  paste0(folder, "portfolio_draws_nmds.png"),
  draw_ord$plot,
  width = 9,
  height = 6,
  dpi = 300
)


##########Probabilistic loss ~ CCISS Suitability####################


##choose example
current_unit <- "ICHdw1/101"
bgc_nm <- "ICHdw1"
folder <- "ICH_Examples/"
location <- data.table(lat =  49.10443,	lon = -117.78877, elev =	1088, id = 1)

current_unit <- "SBSmc2/01"
location <- data.table(lat =  54.7797,	lon = -126.91428, elev =	896, id = 1)
bgc_nm <- "SBSmc2"

current_unit <- "IDFxh1/101"
bgc_nm <- "IDFxh1"
folder <- "IDF_Example/"
location <- data.table(lat =  50.23796,	lon = -119.22145, elev =	484, id = 1)


library(ranger)
BGCmodel <- readRDS("../Common_Files/WNA_BGCv13/BGCmodel_WNA_V4.2gini.rds")
vars_needed <- c("CMD_sm", "DDsub0_sp", "DD5_sp", "Eref_sm", "Eref_sp", "EXT", 
                 "MWMT", "NFFD_sm", "NFFD_sp", "PAS_an", "PAS_sp", "SHM", "Tave_sm", "Tave_sp", "Tmax_sm", "Tmax_sp", "Tmin_an", "Tmin_at", "Tmin_sm", 
                 "Tmin_sp", "Tmin_wt","CMI_an", "PPT_05","PPT_06","PPT_07","PPT_08","PPT_09","PPT_at","PPT_wt","CMD_07","CMD_an"
)

##climate data for rand for model (use periods not annual)
clim <- downscale(location, gcm_periods = list_gcm_periods(), ssps = "ssp245", 
                  gcms = "MPI-ESM1-2-HR", run_nm = "r1i1p1f1", vars = vars_needed, db_option = "local")
clim <- na.omit(clim)
ccissr::addVars(clim)

temp <- predict(BGCmodel, clim)
bgc_preds <- data.table(Year = substr(clim$PERIOD,1,4), BGC = temp$predictions)
setkey(bgc_preds, "BGC")
eda <- copy(ccissr::E1)
eda <- eda[is.na(SpecialCode) & Edatopic == "C4",]
setkey(eda, "BGC")
eda <- unique(eda)
ss_preds <- eda[,.(BGC,SS_NoSpace)][bgc_preds]

#### climate data for site index
clim_dat <- downscale(location, gcms = "MPI-ESM1-2-HR", ssps = "ssp245", 
                      run_nm = "r1i1p1f1", gcm_ssp_years = 2015:2100, 
                      vars = c("CMD_an", "DD5_an"), db_option = "local")
clim_si <- clim_dat[,.(id,GCM,SSP,RUN,PERIOD, CMD_an, DD5_an)]
clim_si[,run_id := interaction(id,GCM,SSP,RUN)]
clim_si <- na.omit(clim_si)

spp_list <- c("Fd", "Pl", "Lw", "Py", "At", "Cw", "Hw", "Bl", "Sx")

##run portfolio
port_prob <- plot_portfolio_draws(
  climate = clim_si,
  ss_preds = ss_preds,
  spp_list = spp_list,
  bgc = bgc_nm,
  covariance = NULL,
  use_returns = TRUE,
  portfolio_fn = indiv_portfolio_prob_loss,
  n_draws = 100)


ggsave(
  paste0(folder,"ef_prob_loss_v2.png"),
  port_prob$plot,
  width = 12,
  height = 13,
  dpi = 300
)

###violin plot simulations
port_weights <- port_prob$selected_summary[,.(PortfolioType, species, mean_weight)]
temp <- data.table(PortfolioType = "JustPl", species = spp_list, mean_weight = 0)
temp[species == "Pl", mean_weight := 1]
port_weights <- rbind(port_weights,temp)

sim_res <- simulate_portfolio(clim_si, ss_preds, spp_list, bgc_nm, port_weights)

##parameters for probability distributions
feas_prob <- data.table(suit = c(1,2,3,4), gamma_shp = c(2,2,2,2), gamma_rate = c(2,1.8,0.7,0.2), ProbEvent = c(90,85,70,40))
sim_ls <- list()
for(i in 1:250) {
  if(i %% 50 == 0) message(i)
  sim_ls[[i]] <- simulate_portfolio(clim_si, ss_preds, spp_list, bgc_nm, port_weights, feas_prob)
}

sim_res <- rbindlist(sim_ls, idcol = "iteration")
sim_res[,PortfolioType := factor(PortfolioType, levels = c("JustPl", "MaxReturn", "90%", "MaxSharpe", "MinVariance"))]
temp <- melt(sim_res, id.vars = c("PortfolioType", "iteration"))

##plot
portfolio_order <- c(
  "JustPl", "MaxReturn", "90%", "MaxSharpe", "MinVariance"
)

# Simulation outcomes in long format
plot_data <- melt(
  copy(sim_res),
  id.vars = c("PortfolioType", "iteration"),
  variable.name = "metric",
  value.name = "value"
)

plot_data <- plot_data[is.finite(value)]
plot_data[, x := match(as.character(PortfolioType), portfolio_order)]

# Species compositions
weights <- copy(port_weights)
weights[, PortfolioType := as.character(PortfolioType)]
weights[, species := factor(species, levels = spp_list)]
weights[, x := match(PortfolioType, portfolio_order)]

stopifnot(
  !anyNA(weights$x),
  !anyNA(weights$species),
  all(is.finite(weights$mean_weight)),
  all(weights$mean_weight >= 0),
  all(abs(weights[, sum(mean_weight), by = PortfolioType]$V1 - 1) < 1e-6)
)

setorder(weights, PortfolioType, species)

weights[, `:=`(
  prop_top = cumsum(mean_weight),
  prop_bottom = cumsum(mean_weight) - mean_weight
), by = PortfolioType]

# Position bars using each facet's own outcome range
facet_ranges <- plot_data[, .(
  bottom = min(value),
  span = diff(range(value))
), by = metric]

# Handle a constant outcome
facet_ranges[
  span == 0,
  span := pmax(abs(bottom), 1) * 0.1
]

# Repeat the compositions in every facet
bars <- merge(
  weights[, join_key := 1L],
  facet_ranges[, join_key := 1L],
  by = "join_key",
  allow.cartesian = TRUE
)

bar_height <- 0.35  # Fraction of each facet's outcome range

bars[, `:=`(
  xmin = x + 0.24,
  xmax = x + 0.40,
  ymin = bottom + prop_bottom * span * bar_height,
  ymax = bottom + prop_top * span * bar_height
)]

p <- ggplot() +
  geom_violin(
    data = plot_data,
    aes(
      x = x - 0.12,
      y = value,
      group = PortfolioType
    ),
    draw_quantiles = c(0.25,0.5,0.75),
    width = 0.60,
    fill = "grey92",
    colour = "grey35",
    linewidth = 0.4,
    trim = TRUE
  ) +
  geom_rect(
    data = bars,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = species
    ),
    colour = "white",
    linewidth = 0.15
  ) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_x_continuous(
    breaks = seq_along(portfolio_order),
    labels = portfolio_order,
    limits = c(0.45, length(portfolio_order) + 0.55),
    expand = expansion(mult = 0)
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Species",
    caption = paste(
      "Violin marks: 25th, 50th and 75th percentiles.",
      "Adjacent stacked bars: species allocation proportions."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  ) +
  makeColScale(
    spp_list,
    fread("./new_species_colours.csv")
  )

p

ggsave("SBS_Example/Ruin_probloss_bitless.png", width = 12, height = 8)



####OLDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
# ggplot(ef_long, aes(x = frontier_sd, y = value)) +
#   geom_area(aes(fill = variable, group = variable), size = 0.00001, col = "grey50", stat = "identity")+
#   geom_line(data = ef, aes(x = frontier_sd, y = return/max(return)), colour = "black", linetype = "F1",linewidth = .75)+
#   scale_x_reverse() +
#   scale_fill_manual(values = spp_clrs) +
#   geom_vline(data = ef[sharpe == max(sharpe),], aes(xintercept = frontier_sd,colour = "MaxSharpe"), 
#            linetype = "twodash", size = .75) +
#   geom_vline(xintercept = ret90, linetype = "twodash", size = .75) +
#   xlab("Max Return --> Minimized Risk")+
#   ylab("Portfolio Ratio")+
#   guides(fill=guide_legend("Species"))
# 
# 
# conditions <- clim_temp[
#   , .(species = spp), by = .(PERIOD, DD5_an, CMD_an)
# ]
# setnames(conditions, old = c("DD5_an", "CMD_an"), new = c("DD5", "CMD"))
# conditions[, `:=`(
#   bgc = "ICHdw1",
#   moisture = 4,
#   nutrient = "C"
# )]
# 
# si <- draw_si_sigmoid(
#   fit_sigmoid,
#   newdata = conditions,
#   n_draws = 100,
#   type = "mean"
# )
# 
# si <- colMeans(si)
# trajectory <- copy(conditions)
# trajectory[, SI := as.numeric(si[1, ])]
# trajectory[, SI := as.numeric(si)]
# 
# # setorder(trajectory, species, PERIOD)
# # t2 <- trajectory[,.(SI_stand = SI - mean(SI[1:10]), PERIOD = PERIOD), by = species]
# 
# temp <- dcast(trajectory, PERIOD ~ species, value.var = "SI")
# setorder(temp, PERIOD)
# temp[, PERIOD := NULL]
# covmat <- cor(temp)


# trajectory[,PERIOD := as.numeric(PERIOD)]
# ggplot(trajectory, aes(x = PERIOD, y = SI, group = species, col = species)) +
#   geom_line() +
#   geom_smooth()
# 
# ggplot(t2, aes(x = PERIOD, y = SI_stand, group = species, col = species)) +
#   geom_line() +
#   geom_smooth()

## SI based portfolio
# boundDat <- data.table(Spp = names(temp))
# boundDat[,`:=`(minWt = 0, maxWt = 1)]
# ef <- optimise_portfolio(temp, sigma2, boundDat, minTot = 0.01) 
# ef_long <- melt(ef, id.vars = c("frontier_sd","return","sharpe"))
# 
# RetCurve <- stats::approx(ef$return,ef$frontier_sd,xout = 0.9*max(ef$return))
# ret90 <- RetCurve$y
# ggplot(ef_long, aes(x = frontier_sd, y = value)) +
#   geom_area(aes(fill = variable, group = variable), size = 0.00001, col = "grey50", stat = "identity")+
#   geom_line(data = ef, aes(x = frontier_sd, y = return/max(return)), colour = "black", linetype = "F1",linewidth = .75)+
#   scale_x_reverse() +
#   scale_fill_manual(values = spp_clrs)
#   geom_vline(data = ef[sharpe == max(sharpe),], aes(xintercept = frontier_sd,colour = "MaxSharpe"), 
#              linetype = "twodash", size = .75) +
#   geom_vline(xintercept = ret90, linetype = "twodash", size = .75) +
#   xlab("Max Return --> Minimized Risk")+
#   ylab("Portfolio Ratio")+
#   guides(fill=guide_legend("Species"))


#### Annual BGC and SIBEC (looks suspect) #############################
vars_needed <- c("CMD_sm", "DDsub0_sp", "DD5_sp", "Eref_sm", "Eref_sp", "EXT", 
                 "MWMT", "NFFD_sm", "NFFD_sp", "PAS_an", "PAS_sp", "SHM", "Tave_sm", "Tave_sp", "Tmax_sm", "Tmax_sp", "Tmin_an", "Tmin_at", "Tmin_sm", 
                 "Tmin_sp", "Tmin_wt","CMI_an", "PPT_05","PPT_06","PPT_07","PPT_08","PPT_09","PPT_at","PPT_wt","CMD_07","CMD_an"
)

clim <- downscale(location, gcm_ssp_years = 2015:2099, ssps = "ssp245", 
                  gcms = "MPI-ESM1-2-HR", run_nm = "r1i1p1f1", vars = vars_needed)
clim <- na.omit(clim)
ccissr::addVars(clim)

temp <- predict(BGCmodel, clim)
bgc_preds <- data.table(Year = clim$PERIOD, BGC = temp$predictions)
setkey(bgc_preds, "BGC")
eda <- copy(ccissr::E1)
eda <- eda[is.na(SpecialCode) & Edatopic == "C4",]
setkey(eda, "BGC")
eda <- unique(eda)

ss_preds <- eda[,.(BGC,SS_NoSpace)][bgc_preds]

spp <- c("Fd", "Pl", "Lw", "Py")
sibec <- copy(ccissr::SIBEC)
sibec <- sibec[TreeSpp %in% spp,]
sibec[SS_NoSpace == "IDFww/01", SS_NoSpace := "IDFww/101"]
sibec[SS_NoSpace == "CWHds1/01", SS_NoSpace := "CWHds1/101"]
si_preds <- sibec[ss_preds, on = "SS_NoSpace"]
setorder(si_preds, TreeSpp, Year)
si_preds <- na.omit(si_preds)
si_preds <- si_preds[,.(SI = mean(MeanPlotSiteIndex)), by = .(Year, TreeSpp)]
si_preds[,Year := as.numeric(Year)]

setnames(trajectory, old = c("PERIOD","species"), new = c("Year", "TreeSpp"))
trajectory[,Model := "Bayesian"]
si_preds[,Model := "SIBEC"]

allSI <- rbind(si_preds, trajectory[,.(Year,TreeSpp,SI,Model)])

ggplot(allSI, aes(x = Year, y = SI, group = TreeSpp, col = TreeSpp)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~Model)
