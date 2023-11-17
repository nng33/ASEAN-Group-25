library(readxl)
library(xts)
library(ReIns)
library(rugarch)
library(mclust)
library(evmix)
library(car)
library(evir)
library(ExtremeRisks)
library(ggplot2)
library(moments)

### FUNCTIONS ###

# Split residual into negative and positive for left tail and right tail

splitresid <- function(resid)
{
  resid_left <- NULL
  resid_right <- NULL
  
  for(i in 1:length(resid))
  {
    if (resid[i] < 0)
    {
      resid_left <- rbind(resid_left,resid[i])
    }
    else
    {
      resid_right <- rbind(resid_right, resid[i])
    }
  }
  result = list("resid_left" = resid_left, "resid_right" = resid_right)
  return(result)
  
}

hillnorm <- function(resid, k)
{
  # Obtain positive residuals
  resid_right <- splitresid(resid)$resid_right
  
  # Obtain (Smooth) hill estimates
  hill_est <- HTailIndex(resid_right, k)
  
  # Retrieve the desired alpha estimate according to k
  alpha <- 1/hill_est$gammaHat
  
  result <- list("alpha" = alpha, "xi" = hill_est$gammaHat)
  return(result)
}
#################################################################################

# Read Data
data_ts <- read_excel("D:/YEAR 4/FYP/data/CLEANED S&P500 vs NI225.xlsx")
# convert column to date class
data_ts$Date <- as.Date(data_ts$Date)

# Convert to xts time series
x1 <- xts(data_ts$NI, data_ts$Date) # Primary risk
x2 <- xts(data_ts$SP, data_ts$Date) # Reference risk

################################################################################

# Fit AR(1)-GARCH(1,1)

# x1 (YESSSS)

spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                         garchOrder = c(1, 1), 
                                         submodel = NULL, 
                                         external.regressors = NULL, 
                                         variance.targeting = FALSE), 
                   
                   mean.model     = list(armaOrder = c(1, 0), 
                                         external.regressors = NULL))


model_x1 <- ugarchfit(spec = spec, data = x1, solver.control = list(trace=0))


# Retrieve standardized N(0,1) ARMA(1,1) disturbances (WHAT WE WANT to model!):
x1_resid <- model_x1@fit$z

# x2
model_x2 <- ugarchfit(spec = spec, data = x2, solver.control = list(trace=0))


# Retrieve standardized N(0,1) ARMA(1,1) disturbances (WHAT WE WANT to model!):
x2_resid <- model_x2@fit$z

################################################################################

### ALPHA ESTIMATION ###

# Obtain array of alpha = 1/gammaopt with rolling window

# METHOD 3: HTailIndex
# just specify k to be 10% as recommended by DuMouchel (1982)

roll <- 1000
times <- data_ts$Date[roll:nrow(data_ts)]
k <- 0.1 * roll 

# Alpha for x1

alpha_array1 = NULL

startTime <- Sys.time()
for (p in 1:(length(x1_resid)-roll+1)) # we lose the first roll-1 observation
{
  alpha1 <- hillnorm(x1_resid[p:(roll-1+p)], k)$alpha
  alpha_array1 <- rbind(alpha_array1, alpha1)
}
endTime <- Sys.time()
execTime <- endTime - startTime
execTime


# Create data frame containing Alpha 
df <- data.frame(Date = times, Alpha1 = alpha_array1)

# Plot time series alpha
p_alpha1 <- ggplot(df, aes(x=Date, y=Alpha1)) + 
  geom_line() +
  scale_x_date(date_labels = "%Y") +
  theme_minimal()+
  xlab("Date") +
  ylab(expression(paste("Shape Paramter ", alpha)))
p_alpha1

################################################################################
# Alpha for x2

alpha_array2 = NULL

startTime <- Sys.time()
for (p in 1:(length(x2_resid)-roll+1)) # we lose the first roll-1 observation
{
  alpha2 <- hillnorm(x2_resid[p:(roll-1+p)], k)$alpha
  alpha_array2 <- rbind(alpha_array2, alpha2)
}
endTime <- Sys.time()
execTime <- endTime - startTime
execTime


# Create data frame containing Alpha 
df$Alpha2 <- alpha_array2

# Plot time series alpha
p_alpha2 <- ggplot(df, aes(x=Date, y=Alpha2)) + 
  geom_line() +
  scale_x_date(date_labels = "%Y") +
  theme_minimal()+
  xlab("Date") +
  ylab(expression(paste("Shape Paramter ", alpha)))
p_alpha2

#################################################################################

# Combine alpha graph
p_comb_alpha <- ggplot(df, aes(x = Date)) + 
  geom_line(aes(y = Alpha1, color = 'Japan')) +
  geom_line(aes(y = Alpha2, color = 'US')) +
  scale_x_date(date_labels = "%Y") +
  theme_minimal() +
  xlab("Date") +
  ylab(expression(paste("Shape Paramter ", alpha)))
p_comb_alpha 


##################################################################################

#### KAPPA ESTIMATION ####

# Need Frechet transformed data
# Read Data
data2 <- read_excel("D:/YEAR 4/FYP/data/frechet sp500 ni225.xlsx")
# convert column to date class
data2$Date <- as.Date(data2$Date)
fx1 <- data2$NI
fx2 <- data2$SP

# Structure r.v.
ft <- NULL
for (j in 1:length(fx1))
{
  min <- min(c(fx1[j],fx2[j]))
  ft <- rbind(ft, min)
}

feta_array3 = NULL

startTime <- Sys.time()
for (p in 1:(length(ft)-roll+1)) # we lose the first roll-1 observation
{
  feta3 <- hillnorm(ft[p:(roll-1+p)], k)$xi
  feta_array3 <- rbind(feta_array3, feta3)
}
endTime <- Sys.time()
execTime <- endTime - startTime
execTime

# Step 2: Kappa Estimate
kappa_array3 <- 1/feta_array3

# add kappa to data frame
df$Kappa <- kappa_array3

# Plot time series kappa
p_kappa <- ggplot(df, aes(x=Date, y=Kappa)) + 
  geom_line() +
  scale_x_date(date_labels = "%Y") +
  theme_minimal()+
  xlab("Date") +
  ylab(expression(paste("Tail Order ", kappa)))
p_kappa

################################################################################

### POWER GRAPH ###
# Check which method you use

# First power -> x1 given x2
power <- -(1-kappa_array3+1/alpha_array1)

# add power to data frame
df$Power <- power

# Plot time series power 
p_power <- ggplot(df, aes(x=Date, y=Power)) + 
  geom_line() +
  scale_x_date(date_labels = "%Y") +
  theme_minimal() +
  xlab("Date") +
  ylab(expression(kappa-1-1/alpha))
p_power

################################################################################

# Second power -> x2 given x1
power2 <- -(1-kappa_array3+1/alpha_array2)

# add power to data frame
df$Power2 <- power2

# Plot time series power 
p_power2 <- ggplot(df, aes(x=Date, y=Power2)) + 
  geom_line() +
  scale_x_date(date_labels = "%Y") +
  theme_minimal() +
  xlab("Date") +
  ylab(expression(kappa-1-1/alpha))
p_power2

###########################################################

# Combine Graph

p_comb_power<- ggplot(df, aes(x = Date)) + 
  geom_line(aes(y = Power2, color = 'US given Japan')) +
  geom_line(aes(y = Power, color = 'Japan given US')) +
  scale_x_date(date_labels = "%Y") +
  geom_hline(yintercept=0, linetype='dashed') +
  theme_minimal() +
  xlab("Date") +
  ylab(expression(kappa-1-1/alpha))
p_comb_power




















