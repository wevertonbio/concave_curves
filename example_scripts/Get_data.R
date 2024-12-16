# #### Generate data ####
# m <- readRDS("../test_kuenm2/Paper_example/Araucaria_angustifolia/concave_models.rds")
# #Get calibration data
# calib_data <- m$calibration_data
# saveRDS(calib_data, "data/calib_data.rds")
# #Get formulas
# f <- m$selected_models$Formulas
# f <- paste0("pr_bg ", f[1:2])
# saveRDS(f, "data/formulas.rds")
#
#
#
# #Get weights
# weights <- ifelse(calib_data$pr_bg == 1, 1, 100)
#
# #Fit glm
# m_glm <- glm(formula = f[2],
#              family =  binomial(link = "cloglog"), data = calib_data,
#              weights = NULL)
# coef(m_glm)
#
# kuenm2::detect_concave(model = m_glm, calib_data = calib_data,
#                extrapolation_factor = 0.5,
#                averages_from = "pr",
#                var_limits = NULL, plot = TRUE,
#                mfrow = c(3, 2), legend = TRUE)
#
#
# model = m_glm; calib_data = calib_data;
# extrapolation_factor = 0.1;
# averages_from = "pr";
# var_limits = NULL; plot = TRUE;
# mfrow = NULL; legend = FALSE
