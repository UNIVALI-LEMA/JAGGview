# Install JAGGview dependencies
install.packages(c("remotes", "rjags", "R2jags"), dependencies = TRUE)

# Install JABBA and JAGGview from GitHub
remotes::install_github("jabbamodel/JABBA", dependencies = TRUE)
remotes::install_github("UNIVALI-LEMA/JAGGview", dependencies = TRUE)

# Load JAGGview
library(JAGGview)
library(JABBA)

set.seed(123)

# Get JABBA data
data(iccat)
whm <- iccat$whm

# Scenario S01: simple Schaefer, without se
jb_S01 <- build_jabba(
  catch = whm$catch, 
  cpue = whm$cpue, 
  se = NULL, 
  assessment = "WHM", 
  scenario = "S01"
)

fit.S01 <- fit_jabba(jb_S01, quickmcmc = TRUE)

# Scenario S02: Pella with priors
jb_S02 <- build_jabba(
  catch = whm$catch, 
  cpue = whm$cpue, 
  se = whm$se, 
  assessment = "WHM", 
  scenario = "S02",
  model.type = "Pella",
  r.prior = c(0.181, 0.18),
  BmsyK = 0.39,
  igamma = c(0.001, 0.001)
)

fit.S02 <- fit_jabba(jb_S02, quickmcmc = TRUE)

# Hindcast (retrospective) for each model
hc_S01 <- hindcast_jabba(jb_S01, fit.S01, peels = 1:5)
hc_S02 <- hindcast_jabba(jb_S02, fit.S02, peels = 1:5)

# List JABBA Model results
list_fit_models <- list(fit.S01, fit.S02)
list_hc_models  <- list(hc_S01, hc_S02)

# Convert JABBA fit output into the data required by the plotting function 
fits <- fits_data(list_fit_models)

# Create a ggplot2 object of observed indices, and 80% / 95% credibility 
# interval
fits_ggplot(
  df_lists = fits,
  title_x = "Year",
  title_y = "Abundance index"
)

# Convert JABBA hindcast output into the data required by the plotting function
hindcast <- hindcast_data(list_hc_models)

# Create a ggplot2 object of hindcast trajectories, observed and predicted 
# points, and uncertainty ribbons
hindcast_ggplot(
  df_lists = hindcast,
  position = "top-left",
  text_size = 5,
  title_x = "Year",
  title_y = "Index",
  zoom = TRUE
)

# Convert JABBA fit output into the data required by the plotting function 
kobe <- kobe_data(list_fit_models)

# Create a ggplot2 object of the trajectory of the status of fish stocks in 
# terms of biomass and fishing mortality, including uncertainty contours
kobe_ggplot(
  df_lists = kobe,
  n_col = 3,
  title_x = expression(B/B[MSY]),
  title_y = expression(F/F[MSY])
)

# Convert JABBA fit output into the data required by the plotting function 
priors_posteriors <- priors_posteriors_data(list_fit_models)

# Create a ggplot2 object of comparing prior and posterior distribution for the 
# selected parameter
priors_posteriors_ggplot(
  df_lists = priors_posteriors,
  indicator_name = "K",
  n_col = 3,
  position = "top-left",
  text_size = 5,
  use_si_suffix = TRUE,
  title_y = "Density",
  title_x = "Carrying capacity (K)",
  x_lim = c(0, 2.5e6),
  palette = c("red", "blue")
)

priors_posteriors_ggplot(
  df_lists = priors_posteriors,
  indicator_name = "r",
  n_col = 3,
  position = "top-left",
  text_size = 5,
  use_si_suffix = FALSE,
  title_y = "Density"
)

priors_posteriors_ggplot(
  df_lists = priors_posteriors,
  indicator_name = "psi",
  n_col = 3,
  position = "top-left",
  text_size = 5,
  use_si_suffix = FALSE,
  palette = c("red", "blue"),
  x_decimals = NULL
)

# Convert JABBA hindcast output into the data required by the plotting function
retrospective_analysis <- retrospective_analysis_data(list_hc_models)

# Create a ggplot2 object of retrospective analyses, including time series of 
# key indices and surplus production curves, with rho value
retrospective_analysis_ggplot(
  df_lists = retrospective_analysis,
  indicator_name = "B",
  n_col = 3,
  position = "top-left",
  use_si_suffix = TRUE
)

retrospective_analysis_ggplot(
  df_lists = retrospective_analysis,
  indicator_name = "F",
  n_col = 3,
  position = "top-left",
  use_si_suffix = FALSE
)

retrospective_analysis_ggplot(
  df_lists = retrospective_analysis,
  indicator_name = "BBmsy",
  n_col = 3,
  position = "top-left",
  use_si_suffix = FALSE,
  y_decimals = 0
)

retrospective_analysis_ggplot(
  df_lists = retrospective_analysis,
  indicator_name = "FFmsy",
  y_decimals = 0
)

retrospective_analysis_ggplot(
  df_lists = retrospective_analysis,
  indicator_name = "procB"
)

retrospective_analysis_ggplot(
  df_lists = retrospective_analysis,
  indicator_name = "MSY",
  use_si_suffix = TRUE
)

# Convert JABBA fit output into the data required by the plotting function 
residuals <- runs_tests_data(list_fit_models)

# Create a ggplot2 object of runs tests diagnostics, including residuals, 
# credibility limits, and p-values
runs_tests_ggplot(
  df_lists = residuals,
  position = "top-left",
  text_size = 6
)

# Create a ggplot2 object of CPUE residuals, including reference line, residual 
# segments, smoothed trends and RMSE value
cpue_residuals_ggplot(
  df_lists = residuals,
  n_col = 3,
  position = "top-left",
  text_size = 6
)

# Convert JABBA fit output into the data required by the plotting function 
trajectories <- trajectories_data(list_fit_models)

# Create a ggplot2 object of model trajectory over time, including trends and 
# uncertainty intervals
trajectories_ggplot(
  df = trajectories,
  indicator_name = "BB0",
  n_col = 2,
  use_si_suffix = FALSE
)

trajectories_ggplot(
  df = trajectories,
  indicator_name = "BBmsy",
  n_col = 2,
  use_si_suffix = FALSE
)

trajectories_ggplot(
  df = trajectories,
  indicator_name = "FFmsy",
  n_col = 2,
  use_si_suffix = FALSE
)

trajectories_ggplot(
  df = trajectories,
  indicator_name = "Bdev",
  n_col = 2,
  use_si_suffix = FALSE
)

trajectories_ggplot(
  df = trajectories,
  indicator_name = "B",
  n_col = 2,
  use_si_suffix = TRUE
)

trajectories_ggplot(
  df = trajectories,
  indicator_name = "H",
  n_col = 2,
  use_si_suffix = FALSE
)

trajectories_ggplot(
  df = trajectories,
  indicator_name = "Catch",
  n_col = 2,
  use_si_suffix = TRUE
)

# Extract fitted estimates
estimates <- get_estimates(list_fit_models)
print(estimates)

# Visualize fitted estimates as table
summary_table(estimates)

# Extract model parameters
parameters <- get_pars(list_fit_models)
print(parameters)

# Visualize model parameters as table
summary_table(parameters)

# Extract summary statistics
statistics <- get_stats(list_fit_models)
print(statistics)

# Visualize summary statistics as table
summary_table(statistics)

# Extract hindcast fitted estimates
hc_estimates <- get_hc_estimates(list_hc_models)
print(hc_estimates)

# Visualize hindcast fitted estimates as table
summary_table(hc_estimates)

# Extract hindcast model parameters
hc_parameters <- get_hc_pars(list_hc_models)
print(hc_parameters)

# Visualize hindcast model parameters as table
summary_table(hc_parameters)

# Extract hindcast summary statistics
hc_statistics <- get_hc_stats(list_hc_models)
print(hc_statistics)

# Visualize hindcast summary statistics as table
summary_table(hc_statistics)

# Extract MASE values
MASE <- get_mase(hindcast)
print(MASE)

# Visualize MASE values as table
summary_table(MASE)

# Extract PPMR values
PPMR <- get_ppmr(priors_posteriors)
print(PPMR)

# Visualize PPMR values as table
summary_table(PPMR)

# Extract PPVR values
PPVR <- get_ppvr(priors_posteriors)
print(PPVR)

# Visualize PPVR values as table
summary_table(PPVR)

# Extract reference points
ref_points <- get_refpts(list_fit_models)
print(ref_points)

# Visualize reference points as table
summary_table(ref_points)

# Extract retrospective bias metric (rho) values
rho <- get_rho(retrospective_analysis)
print(rho)

# Visualize retrospective bias metric (rho) values as table
summary_table(rho)

# Launch the report as a dashboard
create_report(
  fits_data = fits, 
  hind_data = hindcast, 
  kobe_data = kobe,
  pp_data = priors_posteriors, 
  ra_data = retrospective_analysis, 
  res_data = residuals, 
  traj_data = trajectories, 
  list_fit_models = list_fit_models,
  list_hc_models = list_hc_models,
  animation = TRUE, 
  verbose = TRUE,
  use_models_for_data = FALSE,
  use_si_suffix = TRUE
)
