# ──────────────────────────────────────────────────────────────────────────────
# ITN Mixed‐Effects Modelling Workflow
#
# PURPOSE:
# 1) Show why we start with a fixed-effects-only logistic regression.
# 2) Incrementally add random intercepts (region, cluster, household).
# 3) Diagnose convergence, overdispersion, zero-inflation, residual fit.
# 4) Interpret odds‐ratios and random‐effect variances.
#
# ──────────────────────────────────────────────────────────────────────────────

# 0. PACKAGES -----------------------------------------------------------------
# install.packages(c("glmmTMB","DHARMa","minqa","dplyr","stringr"))
library(dplyr)       # data‐wrangling
library(stringr)     # string ops
library(glmmTMB)     # mixed models
library(DHARMa)      # diagnostic simulations
library(minqa)       # alternative optimizer (bobyqa)

# 1. DATA PREP: from raw survey rows → modelling frame ------------------------

# 1a) Read your person‐level CSV (with SurveyId, slept_under_itn, age, sex, etc.)
data <- readr::read_csv("data/DHS/DHS_ITN_person_data.csv")

# 1b) Filter out any rows missing core fields, derive new vars:
data_mod <- data %>%
  filter(
    !is.na(sex),
    !is.na(age),
    !is.na(slept_under_itn)
  ) %>%
  mutate(
    # 1b(i) numeric outcome 0/1 for logistic
    itn_use       = as.integer(slept_under_itn == "yes"),
    # 1b(ii) survey year (e.g. "2013" in "ZM2013DHS")
    survey_year   = as.integer(str_sub(SurveyId, 3, 6)),
    # 1b(iii) scale the year (mean=0, sd=1) for stable optimization
    survey_year_s = as.numeric(scale(survey_year)),
    # 1b(iv) country factor from ID prefix
    country       = if_else(str_starts(SurveyId, "ZM"), "Zambia", "Tanzania"),
    country       = factor(country),
    # 1b(v) create age groups
    age_group = case_when(
      age <  5 ~ "Under 5",
      age <= 14 ~ "School-age (5–14)",
      age <= 49 ~ "Adults (15–49)",
      TRUE      ~ "Older adults (50+)"
    ) %>% 
      factor(levels = c("Under 5","School-age (5–14)","Adults (15–49)","Older adults (50+)")),
    # 1b(vi) unique household‐in‐survey ID for later nesting
    hh_svy        = paste0(SurveyId, "_", householdid)
  ) %>%
  ungroup()

# Quick cross-tab to ensure no singleton levels:
lapply(data_mod %>% select(country, sex, age_group), table)

# ── 2. WHY FIXED EFFECTS FIRST? ------------------------------------------------
# In any multi-level modeling workflow we start by estimating the “fixed effects”:
# these are the population-level parameters (βs) that tell us, on average,
# how the log-odds of ITN use vary by country, year, age group and sex.
#
#   logit[Pr(itn_use)] = β0 
#                        + β1·country 
#                        + β2·survey_year_s 
#                        + β3·age_group 
#                        + β4·sex 
#                        + β5·(interactions) 
#
# By fitting this baseline model first we
#  • get interpretable odds-ratios for each predictor,
#  • check for any obvious mis-specifications (e.g. non-linearity in year),
#  • and ensure that our data and coding are correct.
# Only *after* we understand these average effects do we
# add random intercepts to account for clustering.

###################################################################################
# ── Formula operators cheat‐sheet ─────────────────────────────────────────────
#   +   : add separate terms to the model
#         e.g. y ~ A + B       fits main effects of A and B
#
#   *   : add both main effects and their interaction
#         A * B  ≡  A + B + A:B
#         useful for “fully factorial” terms
#
#   :   : add only the interaction between factors (no mains)
#         A:B    ≡  interaction of A and B only
#
#   /   : nesting shorthand for factors
#         A / B ≡ A + A:B
#         i.e. B nested within A (separate B effects in each A)
#
#   (1 | g) : random intercept for grouping factor g
#         allows each level of g its own baseline deviation u_g ~ N(0,σ²)
#         e.g. (1 | cluster) adds a cluster‐level random effect
#
#   Combining random effects:
#     (1 | A/B)   ≡  (1 | A) + (1 | A:B)
#     (1 | A/B/C) ≡  (1 | A) + (1 | A:B) + (1 | A:B:C)
###################################################################################

# 2a) Fit fixed-effects-only logistic regression with glmmTMB
#     (could be done with glm() too but we use glmmTMB even here so that `update()` will work seamlessly
#      when we add random effects later)
mod_simple <- glmmTMB(
  formula = itn_use ~ country * survey_year_s * age_group + sex,
  family  = binomial(link = "logit"),
  data    = data_mod
)

# Summarise the fixed-effects estimates
summary(mod_simple)

# Convert the log-odds coefficients into odds ratios for easier interpretation
# fixef(mod_simple)$cond is a named vector of fixed-effect estimates
odds_ratios_simple <- exp(fixef(mod_simple)$cond)
print(odds_ratios_simple)

# 2b) Quick residual diagnostic using DHARMa
#     We simulate the residuals under the fitted model and plot:
sim_simple <- simulateResiduals(mod_simple)  # may take a few seconds
plot(sim_simple)      # look for uniformity & no patterns
# We can also run automated tests:
testDispersion(sim_simple)      # is there over/under-dispersion?
testZeroInflation(sim_simple)   # too many zeros?
testUniformity(sim_simple)      # overall goodness-of-fit
# All good - let's proceed!

# 3. ADD REGION-LEVEL RANDOM INTERCEPT ---------------------------------------
# Households within the same admin1 region likely share unmeasured factors.
# We allow each region its own intercept u_region ~ Normal(0, σ²_region).
# --> We nest admin1_name within country using country/admin1_name:
#   (1 | country/admin1_name) ≡ (1|country) + (1|country:admin1_name)

mod_region <- update(
  mod_simple, # This just means that our previous is getting updated, everything stays the same
  . ~ . + (1 | country/admin1_name) # We just add this!
)
summary(mod_region)
# Random effects:
VarCorr(mod_region)

# Diagnose
sim_region <- simulateResiduals(mod_region)
plot(sim_region) # I know I know - lets check things
testDispersion(sim_region)      # over/under‐dispersion?
testZeroInflation(sim_region)   # zero‐inflation?
testUniformity(sim_region)

# In very large samples, the KS test is ultrasensitive. 
# Since our dispersion and zero-inflation are fine,
# and you see no clear patterns in the residual-vs-predicted plot, 
# I’d consider this model diagnostics “passed.”
# We can proceed to adding random intercepts

rm(sim_region, sim_simple, sim0) # Let's clean up our environment - sim_region is a 600MB object, 
# remove anything else that you don't need in the future - if you run out of RAM, R will crash.
# If you do run out of RAM, you can re-run all of this by splitting the data into countries:

# # split data
# zt <- split(data_mod, data_mod$country)

# 4. ADD CLUSTER-LEVEL RANDOM INTERCEPT --------------------------------------
# Survey clusters (EAs) within the same region may share additional similarity
# beyond the region effect.  We add a random intercept u_cluster ~ N(0,σ²_cluster):
#   (1 | country/admin1_name/clusterid)
# which expands to (1|country) + (1|country:admin1_name) + (1|country:admin1_name:clusterid)

mod_cluster <- update(
  mod_simple, # going back to the simple one which means: itn_use ~ country * survey_year_s * age_group + sex +
  . ~ . + (1 | country/admin1_name/clusterid) # This will take quite a bit of time
)
summary(mod_cluster)

# Examine the estimated variance components
VarCorr(mod_cluster)
# Random‐effect variances tell you how much unexplained heterogeneity exists at each level. 
# A large variance at the cluster level would mean clusters differ a lot from each other beyond 
# region effects, etc.

# 4a) Residual diagnostics for the cluster model
sim_cluster <- simulateResiduals(mod_cluster) # Cuppa
plot(sim_cluster)
testDispersion(sim_cluster)      # Check for over/under‐dispersion
testZeroInflation(sim_cluster)   # Check for zero‐inflation
testUniformity(sim_cluster)      # Global goodness‐of‐fit

# If all good - clean up 
rm(sim_cluster)

# 5. ADD HOUSEHOLD-WITHIN-SURVEY RANDOM INTERCEPT -----------------------------
# Households (within a survey) will share behaviour that isn’t fully captured
# by age/sex/country/year/cluster.  We use the hh_svy ID to soak up that grouping so we
# encode the entire DHS sampling hierarchy in one nested random‐effects term!!

# So this is complex and you might run out of RAM but this is how it's supposed to be done properly :)
mod_hh <- update(
  mod_simple, # going back to the simple one which means: itn_use ~ country * survey_year_s * age_group + sex +
  . ~ . + (1 | country/admin1_name/clusterid/hh_svy) # hh is nested within all of it  - again, this will take a lot of time (prob ~10 mins)
)

# Whislt it's running why are we doing this..?
# Country: captures any overall baseline difference between Zambia vs Tanzania.
# Admin 1  within country: allows each region to have its own overall intercept shift around its country’s baseline.
# Cluster within region: accounts for the fact that enumeration areas within the same region may share unmeasured characteristics.
# Household within cluster: people in the same household almost surely behave more similarly than people in different households.

# Drumroll:
summary(mod_hh)
VarCorr(mod_hh)

# 5a) Diagnostics again
sim_hh <- simulateResiduals(mod_hh) # time for another cuppa
plot(sim_hh)
testDispersion(sim_hh)
testZeroInflation(sim_hh)
testUniformity(sim_hh)
# What do we think?




# Bit of help:

# (Intercept)
#   ≈ 0.37  → OR = exp(0.37) ≈ 1.45
#   This is the baseline odds of ITN use for our reference group:
#     • Tanzania
#     • survey_year_s = 0 (i.e. around the mean DHS year)
#     • age_group = “Under 5”
#     • sex = female
#   An OR of 1.45 means these under-5 Tanzanian girls in the “average” year
#   have 45% higher odds of ITN use than the hypothetical log-odds = 0 baseline.

# countryZambia
#   ≈ –0.25 → OR = exp(–0.25) ≈ 0.78
#   Holding everything else constant, Zambian respondents have about 22% lower
#   odds of sleeping under an ITN compared to Tanzanian respondents at the same
#   scaled survey year, age, and sex.

# survey_year_s
#   ≈ 0.31 → OR = exp(0.31) ≈ 1.37
#   A one-SD increase in survey year (i.e. moving from older to more recent DHS
#   by roughly 4–5 years) is associated with a 37% increase in the odds of ITN use,
#   *on average* in Tanzania (before accounting for Zambia’s interaction).

# age_group (reference = “Under 5”)
#   School-age (5–14):    –0.51 → OR ≈ exp(–0.51) ≈ 0.60  
#     School-age children in Tanzania have ~40% lower odds of ITN use 
#     compared to under-5s, all else equal.
#   Adults (15–49):       –0.41 → OR ≈ 0.66  
#     Adults have ~34% lower odds vs under-5s.
#   Older adults (50+):   –0.35 → OR ≈ 0.70  
#     Older adults have ~30% lower odds vs under-5s.

# sexmale
#   ≈ –0.42 → OR = exp(–0.42) ≈ 0.66
#   Males have about 34% lower odds of ITN use than females, controlling for
#   country, year, age, and the full random‐effects hierarchy.

# ——— two‐way interactions ———
# countryZambia:survey_year_s
#   ≈ 0.59 → OR ≈ exp(0.59) ≈ 1.80  
#   The year‐to‐year increase in ITN use is *even steeper* in Zambia: each SD jump
#   in survey year multiplies the Tanzanian OR (1.37) by another 1.80, giving overall
#   ~2.47× higher odds per SD year in Zambia.

# countryZambia:age_group
#   School-age:  –0.58 → OR ≈ 0.56  
#     In Zambia vs Tanzania, the relative gap between school-age and under-5 is
#     ~44% smaller (i.e. extra disadvantage for school-age).
#   Adults:       +0.17 → OR ≈ 1.18  
#     Zambian adults fare slightly better (+18% odds) relative to under-5s, 
#     compared to the same contrast in Tanzania.
#   Older adults:+0.93 → OR ≈ 2.54  
#     In Zambia, the older adults vs under-5 gap is much smaller (they use nets
#     at nearly comparable rates to under-5s) than in Tanzania.

# survey_year_s:age_group
#   (mostly non‐significant except Adults & Older adults show small negative slopes,
#    meaning the age gaps narrowed slightly over time in Tanzania)

# ——— three-way interaction ———
# countryZambia:survey_year_s:age_group
#   School-age:   +0.19 → OR ≈ 1.21  
#     The additional time‐trend boost for school-age in Zambia vs Tanzania.
#   Adults & Older adults: small/non‐significant.

####################################################################################################
# — Summary overall —  
#  • Overall ITN use has increased over time in both countries, more steeply in Zambia.  
#  • Under-5s remain the highest-using group; school-age kids and adults lag behind,  
#    but the adult vs under-5 gap is smaller in Zambia than Tanzania.  
#  • Men are consistently less likely to sleep under ITNs than women, across all ages,  (oh, men!)
#    years, and both countries.  
#  • The random‐effects variances confirm strong clustering at the household→cluster→region levels.
####################################################################################################

# 6. MODEL COMPARISON ---------------------------------------------------------
# Compare model fit (e.g. AIC) as we added levels of random effects
AIC(mod_simple, mod_region, mod_cluster, mod_hh)
#  → Each layer of clustering (region → cluster → household) **significantly** 
#     improves model fit, so the full hierarchy is warranted.

# Optionally, likelihood‐ratio tests to see if random terms significantly improve:
anova(mod_region, mod_cluster, test="Chisq")
anova(mod_cluster, mod_hh,     test="Chisq")

# 7. INTERPRETATION OF FINAL MODEL --------------------------------------------
# Fixed‐effects odds ratios:
final_fixed_OR <- exp(fixef(mod_hh)$cond)
print(final_fixed_OR)

#    (Intercept)                             1.45   ← baseline odds for 
#                                              under-5 Tanzanian females  
#    countryZambia                            0.78   ← 22% lower odds in Zambia  
#    survey_year_s                            1.37   ← 37% higher odds per SD‐year  
#    age_groupSchool-age (5–14)               0.60   ← school-age 40% lower vs under-5  
#    age_groupAdults (15–49)                  0.67   ← adults 33% lower vs under-5  
#    age_groupOlder adults (50+)              0.70   ← older adults 30% lower vs under-5  
#    sexmale                                  0.66   ← males 34% lower odds vs females  
#    countryZambia:survey_year_s              1.81   ← Zambia’s time-trend ×1.81  
#    countryZambia:age_groupSchool-age…       0.56   ← extra school-age gap in Zambia  
#    countryZambia:age_groupAdults…           1.18   ← adults fare slightly better in Zambia  
#    countryZambia:age_groupOlder adults…     2.54   ← older adults gap much smaller in Zambia  
#    survey_year_s:age_group…                 ~1.01–0.96  ← small shifts in age gaps over time  
#    countryZambia:survey_year_s:age_group…   ~1.08–1.20  ← three-way tweaks (mostly on school-age)


# Random‐effects standard deviations:
print( VarCorr(mod_hh) )

#    hh_svy:cluster:admin1:country   SD ≈ 3.276   ← **huge** within‐household clustering  
#    cluster:admin1:country          SD ≈ 1.428   ← strong within‐cluster similarity  
#    admin1:country                   SD ≈ 0.957   ← moderate region‐level variation  
#    country                          SD ≈ 0.0006 ← negligible country‐level RE (already in fixed effect)
#
#  Interpretation:  most of the unexplained variability lives at the
#                 household and cluster levels → justifies full nesting.

# The three‐way interaction country:survey_year_s:age_group
# tells us how the change in odds over time differs by age‐group in Zambia vs Tanzania.
# You can write up the odds ratios who's more likely to sleep under ITN etc - this is an awesome model now!

############################################################################################
# So - I think for ITN, travel time, hf accessibility would be super useful - if you'd like, you need to read in the accessibility
# raster from malariaAtlas R package, sum/mean per admin1 in the country shapefiles and then re-attach to the data frame. 
# It would be quite a good for all your data wrangling skills and switching between rasters to polygon to data frames!
############################################################################################


# 8. EXTRACTING RESIDUALS FOR SPATIAL PLOTTING --------------------------------
# We can summarise any remaining spatial pattern by averaging residuals per region:
resid_vec <- residuals(sim_hh)
# bind them back to the original data and summarise by admin1_name
resid_df <- data_mod %>%
  mutate(resid = resid_vec) %>%
  group_by(admin1_name) %>%
  summarise(
    mean_resid = mean(resid, na.rm=TRUE),
    sd_resid   = sd(resid,   na.rm=TRUE),
    n_obs      = n()
  )

# Now resid_df has one row per admin1, with the average scaled DHARMa residual.
print(resid_df)

# Get the shapefile
library(sf)

library(dplyr)
library(sf)
library(ggplot2)
library(viridis)   # for the viridis colour scale


# TZA #############################################################
# Read in admin1 shapefile for Tanzania
tza_adm1 <- st_read("C:/Users/Admin/Documents/GitHub/MAP_ECR_ITN/data/shapefiles/tza_adm1.shp")

# Make sure the region name column lines up
tza_map <- tza_adm1 %>%
  rename(admin1_name = name_1) %>%
  left_join(resid_df, by = "admin1_name")

# Quick check for any unmatched regions
anti_join(
  resid_df %>% select(admin1_name),
  tza_map   %>% st_drop_geometry() %>% select(admin1_name),
  by = "admin1_name"
)  # should return zero rows if all matched

# Plot mean scaled residual for each region
ggplot(tza_map) +
  geom_sf(aes(fill = mean_resid), colour = "grey70", size = 0.2) +
  scale_fill_viridis_c(
    option   = "viridis",
    name     = "Mean scaled\nresidual",
    na.value = "white"
  ) +
  labs(
    title    = "Model fit residuals by region — Tanzania",
    subtitle = "Mean DHARMa scaled residual per admin1"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title  = element_text(size = 11),
    legend.text   = element_text(size = 10)
  )

# ZMB  #############################################################
# Read in admin1 shapefile for Zambia
zmb_adm1 <- st_read("C:/Users/Admin/Documents/GitHub/MAP_ECR_ITN/data/shapefiles/zmb_adm1.shp")

# Make sure the region name column lines up
zmb_adm1 <- zmb_adm1 %>%
  rename(admin1_name = name_1) %>%
  left_join(resid_df, by = "admin1_name")

# Quick check for any unmatched regions
anti_join(
  resid_df %>% select(admin1_name),
  zmb_adm1   %>% st_drop_geometry() %>% select(admin1_name),
  by = "admin1_name"
)  # should return zero rows if all matched

# Plot mean scaled residual for each region
ggplot(zmb_adm1) +
  geom_sf(aes(fill = mean_resid), colour = "grey70", size = 0.2) +
  scale_fill_viridis_c(
    option   = "viridis",
    name     = "Mean scaled\nresidual",
    na.value = "white"
  ) +
  labs(
    title    = "Model fit residuals by region — Zambia",
    subtitle = "Mean DHARMa scaled residual per admin1"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title  = element_text(size = 11),
    legend.text   = element_text(size = 10)
  )

# How to use these:
# Residual > 0.5 (lighter green/yellow): the model is, on average,
# under‐predicting ITN use there (observed use is higher than the model expects).

# Residual < 0.5 (blue/purple): the model is, on average, over‐predicting ITN use in that region 
# (observed use is lower than predicted).

# Regions with mean residuals far from 0.5 are ones where the fixed and random effects in your model aren’t 
# capturing some local pattern—maybe there’s a missing covariate 
# (e.g. local net‐distribution campaigns, unmeasured socioeconomic factors, micro‐climate) or spatial correlation.

