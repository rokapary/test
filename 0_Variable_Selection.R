################################
## 1.1 PCA correlation circle ##
################################

# The predictive environmental variables were chosen following the methodology of 
# (Engler et al., 2009) and (Patsiou et al., 2014)
# Values for the topo-climatic variables based on the 1971-2000 data
# Calculated at a 25m resolution

env0 <- read.table('Predictors_PLOT.txt', header=TRUE, stringsAsFactors = TRUE, sep = "")
dim(env0) #550 plots, 5 variables

# gdd_0_modi - Sum of daily mean temperature > 0°C (GDD) 
# Units: C x d x yr-1

# mind_68_modi - Moisture Index

# srad_yy_modi - Global Solar Radiation (SOLRAD)
# Units: kj x m-2 x yr-1

# slope_25_modi - Slope
# Units: Angle Degrees

# topos_25_modi - Topographic position
# Unitless


# LOAD PACKAGES
library(ade4)
library(ecospat)
library(usdm)


# Performs a principal component analysis
pca.pred <- dudi.pca(df = env0[,4:8], scannf = FALSE, nf = 5)

# Variance explained by axe 1
pca.pred$eig[1]/sum(pca.pred$eig)
# 0.4905996

# Variance explained by axe 2
pca.pred$eig[2]/sum(pca.pred$eig)
# 0.2341328

# Variance explained by axe 3
pca.pred$eig[3]/sum(pca.pred$eig)
# 0.1507787

# Plot PCA correlation circle
par(mfrow=c(1,1))
s.corcircle(pca.pred$co,clabel=.7, cgrid = 2, full = FALSE, sub = "Correlation circle - predictors", 
            csub = 1, possub = "bottomright", box = TRUE)

# Save graph in your workspace
jpeg("PCA_CorCircle_Predictors.jpg",width = 30, height = 30,units="cm",res=150)
s.corcircle(pca.pred$co,clabel=.7, cgrid = 2, full = FALSE, sub = "Correlation circle - SDM predictors", csub = 1, 
            possub = "bottoright", box = TRUE)
dev.off()

#################################################################################################################################
#################################################################################################################################

########################################
## 1.2 Correlation between predictors ##
########################################

# Correlations
df.cor.pred <-data.frame(round(cor(env0[,4:8]),3))

df.cor.pred
#                     gdd         mind       srad_yy       slope_25      topos
# gdd_0_modi         1.000       -0.902       0.184        -0.366       -0.290
# mind_68_modi      -0.902        1.000      -0.436         0.495        0.289
# srad_yy_modi       0.184       -0.436       1.000        -0.198        0.137
# slope_25_modi     -0.366        0.495      -0.198         1.000        0.046
# topos_25_modi     -0.290        0.289       0.137         0.046        1.000


# Save correlations in your workspace
write.table(df.cor.pred, file="Cor_Pred_Ini.txt",sep="\t",append=F,row.names=T,col.names=T,quote=F)

# You can also plot the correlation in a correlation plot for visualisation
ecospat.cor.plot(env0[,c("gdd_0_modi","mind_68_modi", "srad_yy_modi", "slope_25_modi", "topos_25_modi")])

# Save predictor names for later use
bioclimsub1 <- c("gdd_0_modi","mind_68_modi", "srad_yy_modi", "slope_25_modi", "topos_25_modi") 

# Use VIF values to further assess collinearity 
usdm::vif(env0[,bioclimsub1]) 
#      Variables       VIF
# 1    gdd_0_modi  8.396988
# 2  mind_68_modi 12.086002
# 3  srad_yy_modi  2.028920
# 4 slope_25_modi  1.471576
# 5 topos_25_modi  1.256116

usdm::vifstep(env0[,bioclimsub1]) 
#1 variables from the 5 input variables have collinearity problem: mind_68_modi


# VIF of 1 represents no multicollinearity
# A value between 1 and 5 indicates moderate correlation 
# between a given predictor variable and other predictor variables in the model,
# but this is often not severe enough to require attention
# Above 5 may be problematic



# FINAL SELLECTION BASED ON MULTI-COLLINEARITY
# The temperature over the growing season is likely a significant determinant of
# how much moisture would be available in the air and soil, rather than vice versa, 
# making GDD the less confounded of the two variables. 
# As such, MoistureIndex was excluded from the variable selection. 


bioclimsub1 <- c("gdd_0_modi", "srad_yy_modi", "slope_25_modi", "topos_25_modi") 
ecospat.cor.plot(env0[,c("gdd_0_modi","srad_yy_modi", "slope_25_modi", "topos_25_modi")])
usdm::vif(env0[,bioclimsub1]) # variance inflation factor analysis max: 1.303706
usdm::vifstep(env0[,bioclimsub1]) 
# No variable from the 4 input variables has collinearity problem. 


