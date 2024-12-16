#### Detecting concave curves from Maxent-like glm and glmnet models ####

#Date: December 12, 2024
#Authors: Weverton C.F. Trindade, Luis F. Arias-Giraldo and Marlon E. Cobos

#Load packages
library(glmnet)

#Load functions
source("R/glm_mx.R") #To fit Maxent-like glm models
source("R/glmnet_mx.R") #To fit Maxent-like glmnet models
source("R/helpers_glmnetmx.R") #Helpers fit Maxent-like glmnet models
source("R/predict_methods.R") #To predict Maxent-like glmnet models
source("R/response.R") #To generate data to get response curves
source("R/detect_concave.R") #To detect concave curves


#Import calibration data
data <- readRDS("data/calib_data.rds")

#Import formulas
f <- readRDS("data/formulas.rds")
f <- as.formula(f[2])


#### Concave curves in Maxent-like glmnet models ####
#Fit model
m_glmnet <- glmnet_mx(p = data$pr_bg,
                      data = data,
                      f = f,
                      regmult = 0.1,
                      regfun = maxnet.default.regularization,
                      addsamplestobackground = TRUE,
                      weights = NULL,
                      calculate_AIC = FALSE)
#Get coefficients
coef_m_glmnet <- coef(m_glmnet)[,200]
coef_m_glmnet
#Get means of variables
var_means <- colMeans(data[,sapply(data, is.numeric)])[-1]

#Calculate vertex
vertex_glm <- calculate_vertex(coefs = coef_m_glmnet, means = var_means)

#Set lower limits of variables
vl <- list("bio_2" = c(0, NA),
           "bio_15" = c(0, 100),
           "sand" = c(0, NA),
           "clay" = c(0, NA))

#Detect concave curves
# And save image
## Extrapolation factor = 0.1
png(filename = "images/glmnet_outside_range_01.png",
    width = 9.5, height = 6, units = "in", res = 600)
d_glmnet_01 <- detect_concave(model = m_glmnet, calib_data = data,
                           averages_from = "pr",
                           var_limits = vl,
                           plot = T, plot_linear = T, legend = TRUE, mfrow = c(2,3),
                           extrapolation_factor = 0.1)
dev.off()
## Extrapolation factor = 2
png(filename = "images/glmnet_outside_range_4.png",
    width = 9.5, height = 6, units = "in", res = 600)
d_glmnet_4 <- detect_concave(model = m_glmnet, calib_data = data,
                              averages_from = "pr",
                              var_limits = vl,
                              plot = T, legend = TRUE, mfrow = c(2,3),
                              extrapolation_factor = 4)
dev.off()

#### Concave curves in Maxent-like glm models ####
#Fit model
m_glm <- glm_mx(formula = as.formula(f), family = binomial(link = "cloglog"),
                data = data,
                weights = NULL)
#Get coefficients
coef_m_glm <- coef(m_glm)

#Get means of variables
var_means <- colMeans(data[,sapply(data, is.numeric)])[-1]

#Calculate vertex
vertex_glm <- calculate_vertex(coefs = coef_m_glm, means = var_means)

#Set lower limits of variables
vl <- list("bio_2" = c(0, NA),
           "bio_15" = c(0, 100),
           "sand" = c(0, NA),
           "clay" = c(0, NA))

#Detect concave curves
# And save image
## Extrapolation factor = 0.1
png(filename = "images/glm_outside_range_01.png",
    width = 9.5, height = 6, units = "in", res = 600)
d_glm_01 <- detect_concave(model = m_glm, calib_data = data,
                              averages_from = "pr",
                              var_limits = vl,
                              plot = T, legend = TRUE, mfrow = c(2,3),
                              extrapolation_factor = 0.1)
dev.off()
## Extrapolation factor = 2
png(filename = "images/glm_outside_range_4.png",
    width = 9.5, height = 6, units = "in", res = 600)
d_glm_4 <- detect_concave(model = m_glm, calib_data = data,
                             averages_from = "pr",
                             var_limits = vl,
                             plot = T, legend = TRUE, mfrow = c(2,3),
                             extrapolation_factor = 4)
dev.off()
