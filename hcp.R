Model_0 <- lm(CorrDist ~ Age_in_Yrs + GEN + BMI + white + black + asian + PSQI + fd + Bvol, data = hcp_0.2_cutoff)
Model_1 <- lm(CorrDist ~ Age_in_Yrs + GEN + BMI + white + black + asian + PSQI + fd + Bvol + CogFluid + CogCrystal, data = hcp_0.2_cutoff)
Model_2 <- lm(CorrDist ~ Age_in_Yrs + GEN + BMI + white + black + asian + PSQI + fd + Bvol + CogFluid + CogCrystal + fluid2 + crystal2, data = hcp_0.2_cutoff)
Model_3 <- lm(CorrDist ~ Age_in_Yrs + GEN + BMI + white + black + asian + PSQI + fd + Bvol + CogFluid + CogCrystal + fluid2 + crystal2 + posEM + negEM, data = hcp_0.2_cutoff)
Model_4 <- lm(CorrDist ~ Age_in_Yrs + GEN + BMI + white + black + asian + PSQI + fd + Bvol + CogFluid + CogCrystal + fluid2 + crystal2 + posEM + negEM + NEO_A + NEO_O + NEO_C + NEO_N + NEO_E, data = hcp_0.2_cutoff)
Model_5 <- lm(CorrDist ~ Age_in_Yrs + GEN + BMI + white + black + asian + PSQI + fd + Bvol + CogFluid + CogCrystal + fluid2 + crystal2 + posEM + negEM + NEO_A + NEO_O + NEO_C + NEO_N + NEO_E + STR + END, data = hcp_0.2_cutoff)

#subset male and female
female = hcp_0.2_cutoff[hcp_0.2_cutoff$GEN == 0,]
male = hcp_0.2_cutoff[hcp_0.2_cutoff$GEN == 1,]

Model_4_without_Bvol <- lm(CorrDist ~ Age_in_Yrs + GEN + BMI + white + black + asian + PSQI + fd + CogFluid + CogCrystal + fluid2 + crystal2 + posEM + negEM + NEO_A + NEO_O + NEO_C + NEO_N + NEO_E, data = hcp_0.2_cutoff)
Model_4_Males <- lm(CorrDist ~ Age_in_Yrs + BMI + white + black + asian + PSQI + fd + Bvol + CogFluid + CogCrystal + fluid2 + crystal2 + posEM + negEM + NEO_A + NEO_O + NEO_C + NEO_N + NEO_E, data = male)
Model_4_Females <- lm(CorrDist ~ Age_in_Yrs + BMI + white + black + asian + PSQI + fd + Bvol + CogFluid + CogCrystal + fluid2 + crystal2 + posEM + negEM + NEO_A + NEO_O + NEO_C + NEO_N + NEO_E, data = female)


summary(Model_0)
summary(Model_1)
summary(Model_2)
summary(Model_3)
summary(Model_4)
summary(Model_5)

anova(Model_0,Model_1)
anova(Model_1, Model_2)
anova(Model_2, Model_3)
anova(Model_3, Model_4)
anova(Model_4, Model_5)

##qqplot
qqnorm(Model_4$residuals)
qqline(Model_4$residuals)

##plot observed vs predict
plot(predict(Model_4),                              
     hcp_0.2_cutoff$CorrDist,
     xlab = "Predicted Values",
     ylab = "Observed Values")
# Add straight line
abline(a = 0,                                        
       b = 1,
       col = "red",
       lwd = 2)


##plot fitted vs residual
plot(fitted(Model_4), resid(Model_4), ylab = "Residual", xlab = "Fitted")
abline(h = 0, col = "red", lwd = 2)

c = cor(hcp_0.2_cutoff[,c(2:4,9:12,15:23)], method = "spearman", use = "complete.obs")
psych::cor.plot(c, xlas = 2, gr = colorRampPalette(c("blue", "white", "red")), cex = 0.45)


