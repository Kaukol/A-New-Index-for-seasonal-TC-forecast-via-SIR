#We will apply supervised DR methods like SIR or SAVE to seasonal TC problem and
#show the advantage over PCA with single index model

#Loocv for glm
LoocvHindcast <- function(glmfit, K=n){
  data <- glmfit$model
  n <- nrow(data)
  seq_len <- 1:n
  Hindcast <- vector()
  Call <- glmfit$call
  for(i in 1:n) {
    j.out <- seq_len == i
    j.in <- seq_len != i
    ## we want data from here but formula from the parent.
    Call$data <- data[j.in, , drop=FALSE]
    d.glm <- eval.parent(Call)
    Hindcast[i] <- predict(d.glm, data[j.out, , drop=FALSE], type = "response")
  }
  return(Hindcast)
}

#library, we use "dr" package for SIR and SAVE
library(dr)
setwd("H:/UbuntuRv2/SeasonalSIR")

#data
TC <- read.csv("TC_original_data_Nino.csv")

TC_AR <- TC$AR
TC_ARW <- TC$AR.W
TC_ARE <- TC$AR.E

TC_SP <- TC$SP
TC_SPW <- TC$SP.W
TC_SPE <- TC$SP.E

x <- scale(TC[,c(2:7,9,10)])

n <- dim(x)[1]
p <- dim(x)[2]

x5VAR <- TC$X5VAR

#PCA method

x_PCA <- prcomp(x)
x_PCA_d1 <- x_PCA$x[,1]

#SDR method

#AR
y <- TC_AR
z <- as.data.frame(cbind(y,x))
TC_AR_SIR <- dr(y~., data = z, numdir = 1, nslice = 5, method = "sir")
TC_AR_SAVE <- dr(y~., data = z, numdir = 1, nslice = 5, method = "save")

TC_AR_SIR_d1  <- (TC_AR_SIR$x %*% TC_AR_SIR$evectors)[,1]
TC_AR_SAVE_d1 <- (TC_AR_SIR$x %*% TC_AR_SAVE$evectors)[,1]

TC_AR_PCA_poi <- glm(TC_AR~x_PCA_d1, family = "poisson")
TC_AR_SIR_poi <- glm(TC_AR~TC_AR_SIR_d1, family = "poisson")
TC_AR_SAVE_poi <- glm(TC_AR~TC_AR_SAVE_d1, family = "poisson")
TC_AR_x5VAR_poi <- glm(TC_AR~x5VAR, family = "poisson")
TC_AR_NULL_poi <- glm(TC_AR~1, family = "poisson")

#ARW
y <- TC_ARW
z <- as.data.frame(cbind(y,x))
TC_ARW_SIR <- dr(y~., data = z, numdir = 1, nslice = 5, method = "sir")
TC_ARW_SAVE <- dr(y~., data = z, numdir = 1, nslice = 5, method = "save")

TC_ARW_SIR_d1  <- (TC_ARW_SIR$x %*% TC_ARW_SIR$evectors)[,1]
TC_ARW_SAVE_d1 <- (TC_ARW_SIR$x %*% TC_ARW_SAVE$evectors)[,1]

TC_ARW_PCA_poi <- glm(TC_ARW~x_PCA_d1, family = "poisson")
TC_ARW_SIR_poi <- glm(TC_ARW~TC_ARW_SIR_d1, family = "poisson")
TC_ARW_SAVE_poi <- glm(TC_ARW~TC_ARW_SAVE_d1, family = "poisson")
TC_ARW_x5VAR_poi <- glm(TC_ARW~x5VAR, family = "poisson")
TC_ARW_NULL_poi <- glm(TC_ARW~1, family = "poisson")

#ARE
y <- TC_ARE
z <- as.data.frame(cbind(y,x))
TC_ARE_SIR <- dr(y~., data = z, numdir = 1, nslice = 5, method = "sir")
TC_ARE_SAVE <- dr(y~., data = z, numdir = 1, nslice = 5, method = "save")

TC_ARE_SIR_d1  <- (TC_ARE_SIR$x %*% TC_ARE_SIR$evectors)[,1]
TC_ARE_SAVE_d1 <- (TC_ARE_SIR$x %*% TC_ARE_SAVE$evectors)[,1]

TC_ARE_PCA_poi <- glm(TC_ARE~x_PCA_d1, family = "poisson")
TC_ARE_SIR_poi <- glm(TC_ARE~TC_ARE_SIR_d1, family = "poisson")
TC_ARE_SAVE_poi <- glm(TC_ARE~TC_ARE_SAVE_d1, family = "poisson")
TC_ARE_x5VAR_poi <- glm(TC_ARE~x5VAR, family = "poisson")
TC_ARE_NULL_poi <- glm(TC_ARE~1, family = "poisson")

#SP
y <- TC_SP
z <- as.data.frame(cbind(y,x))
TC_SP_SIR <- dr(y~., data = z, numdir = 1, nslice = 5, method = "sir")
TC_SP_SAVE <- dr(y~., data = z, numdir = 1, nslice = 5, method = "save")

TC_SP_SIR_d1  <- (TC_SP_SIR$x %*% TC_SP_SIR$evectors)[,1]
TC_SP_SAVE_d1 <- (TC_SP_SIR$x %*% TC_SP_SAVE$evectors)[,1]

TC_SP_PCA_poi <- glm(TC_SP~x_PCA_d1, family = "poisson")
TC_SP_SIR_poi <- glm(TC_SP~TC_SP_SIR_d1, family = "poisson")
TC_SP_SAVE_poi <- glm(TC_SP~TC_SP_SAVE_d1, family = "poisson")
TC_SP_x5VAR_poi <- glm(TC_SP~x5VAR, family = "poisson")
TC_SP_NULL_poi <- glm(TC_SP~1, family = "poisson")

#SPW
y <- TC_SPW
z <- as.data.frame(cbind(y,x))
TC_SPW_SIR <- dr(y~., data = z, numdir = 1, nslice = 5, method = "sir")
TC_SPW_SAVE <- dr(y~., data = z, numdir = 1, nslice = 5, method = "save")

TC_SPW_SIR_d1  <- (TC_SPW_SIR$x %*% TC_SPW_SIR$evectors)[,1]
TC_SPW_SAVE_d1 <- (TC_SPW_SIR$x %*% TC_SPW_SAVE$evectors)[,1]

TC_SPW_PCA_poi <- glm(TC_SPW~x_PCA_d1, family = "poisson")
TC_SPW_SIR_poi <- glm(TC_SPW~TC_SPW_SIR_d1, family = "poisson")
TC_SPW_SAVE_poi <- glm(TC_SPW~TC_SPW_SAVE_d1, family = "poisson")
TC_SPW_x5VAR_poi <- glm(TC_SPW~x5VAR, family = "poisson")
TC_SPW_NULL_poi <- glm(TC_SPW~1, family = "poisson")

#SPE
y <- TC_SPE
z <- as.data.frame(cbind(y,x))
TC_SPE_SIR <- dr(y~., data = z, numdir = 1, nslice = 5, method = "sir")
TC_SPE_SAVE <- dr(y~., data = z, numdir = 1, nslice = 5, method = "save")

TC_SPE_SIR_d1  <- (TC_SPE_SIR$x %*% TC_SPE_SIR$evectors)[,1]
TC_SPE_SAVE_d1 <- (TC_SPE_SIR$x %*% TC_SPE_SAVE$evectors)[,1]

TC_SPE_PCA_poi <- glm(TC_SPE~x_PCA_d1, family = "poisson")
TC_SPE_SIR_poi <- glm(TC_SPE~TC_SPE_SIR_d1, family = "poisson")
TC_SPE_SAVE_poi <- glm(TC_SPE~TC_SPE_SAVE_d1, family = "poisson")
TC_SPE_x5VAR_poi <- glm(TC_SPE~x5VAR, family = "poisson")
TC_SPE_NULL_poi <- glm(TC_SPE~1, family = "poisson")

#LOOCV analysis
TC_AR_Loocv_PCA <- LoocvHindcast(TC_AR_PCA_poi)
TC_AR_Loocv_SIR <- LoocvHindcast(TC_AR_SIR_poi)
TC_AR_Loocv_SAVE <- LoocvHindcast(TC_AR_SAVE_poi)
TC_AR_Loocv_x5VAR <- LoocvHindcast(TC_AR_x5VAR_poi)
TC_AR_Loocv_NULL <- LoocvHindcast(TC_AR_NULL_poi)

TC_ARW_Loocv_PCA <- LoocvHindcast(TC_ARW_PCA_poi)
TC_ARW_Loocv_SIR <- LoocvHindcast(TC_ARW_SIR_poi)
TC_ARW_Loocv_SAVE <- LoocvHindcast(TC_ARW_SAVE_poi)
TC_ARW_Loocv_x5VAR <- LoocvHindcast(TC_ARW_x5VAR_poi)
TC_ARW_Loocv_NULL <- LoocvHindcast(TC_ARW_NULL_poi)

TC_ARE_Loocv_PCA <- LoocvHindcast(TC_ARE_PCA_poi)
TC_ARE_Loocv_SIR <- LoocvHindcast(TC_ARE_SIR_poi)
TC_ARE_Loocv_SAVE <- LoocvHindcast(TC_ARE_SAVE_poi)
TC_ARE_Loocv_x5VAR <- LoocvHindcast(TC_ARE_x5VAR_poi)
TC_ARE_Loocv_NULL <- LoocvHindcast(TC_ARE_NULL_poi)

TC_SP_Loocv_PCA <- LoocvHindcast(TC_SP_PCA_poi)
TC_SP_Loocv_SIR <- LoocvHindcast(TC_SP_SIR_poi)
TC_SP_Loocv_SAVE <- LoocvHindcast(TC_SP_SAVE_poi)
TC_SP_Loocv_x5VAR <- LoocvHindcast(TC_SP_x5VAR_poi)
TC_SP_Loocv_NULL <- LoocvHindcast(TC_SP_NULL_poi)

TC_SPW_Loocv_PCA <- LoocvHindcast(TC_SPW_PCA_poi)
TC_SPW_Loocv_SIR <- LoocvHindcast(TC_SPW_SIR_poi)
TC_SPW_Loocv_SAVE <- LoocvHindcast(TC_SPW_SAVE_poi)
TC_SPW_Loocv_x5VAR <- LoocvHindcast(TC_SPW_x5VAR_poi)
TC_SPW_Loocv_NULL <- LoocvHindcast(TC_SPW_NULL_poi)

TC_SPE_Loocv_PCA <- LoocvHindcast(TC_SPE_PCA_poi)
TC_SPE_Loocv_SIR <- LoocvHindcast(TC_SPE_SIR_poi)
TC_SPE_Loocv_SAVE <- LoocvHindcast(TC_SPE_SAVE_poi)
TC_SPE_Loocv_x5VAR <- LoocvHindcast(TC_SPE_x5VAR_poi)
TC_SPE_Loocv_NULL <- LoocvHindcast(TC_SPE_NULL_poi)

#MSE and MAE
TC_AR_MSE_PCA <- sum((TC_AR_Loocv_PCA - TC_AR)^2)/n
TC_AR_MAE_PCA <- sum(abs(TC_AR_Loocv_PCA - TC_AR))/n
TC_AR_MSE_SIR <- sum((TC_AR_Loocv_SIR - TC_AR)^2)/n
TC_AR_MAE_SIR <- sum(abs(TC_AR_Loocv_SIR - TC_AR))/n
TC_AR_MSE_SAVE <- sum((TC_AR_Loocv_SAVE - TC_AR)^2)/n
TC_AR_MAE_SAVE <- sum(abs(TC_AR_Loocv_SAVE - TC_AR))/n
TC_AR_MSE_x5VAR <- sum((TC_AR_Loocv_x5VAR - TC_AR)^2)/n
TC_AR_MAE_x5VAR <- sum(abs(TC_AR_Loocv_x5VAR - TC_AR))/n
TC_AR_MSE_NULL <- sum((TC_AR_Loocv_NULL - TC_AR)^2)/n
TC_AR_MAE_NULL <- sum(abs(TC_AR_Loocv_NULL - TC_AR))/n
TC_AR_MSE <- cbind(TC_AR_MSE_PCA, TC_AR_MSE_SIR, TC_AR_MSE_SAVE, 
                   TC_AR_MSE_x5VAR, TC_AR_MSE_NULL)
TC_AR_MAE <- cbind(TC_AR_MAE_PCA, TC_AR_MAE_SIR, TC_AR_MAE_SAVE, 
                   TC_AR_MAE_x5VAR, TC_AR_MAE_NULL)

TC_ARW_MSE_PCA <- sum((TC_ARW_Loocv_PCA - TC_ARW)^2)/n
TC_ARW_MAE_PCA <- sum(abs(TC_ARW_Loocv_PCA - TC_ARW))/n
TC_ARW_MSE_SIR <- sum((TC_ARW_Loocv_SIR - TC_ARW)^2)/n
TC_ARW_MAE_SIR <- sum(abs(TC_ARW_Loocv_SIR - TC_ARW))/n
TC_ARW_MSE_SAVE <- sum((TC_ARW_Loocv_SAVE - TC_ARW)^2)/n
TC_ARW_MAE_SAVE <- sum(abs(TC_ARW_Loocv_SAVE - TC_ARW))/n
TC_ARW_MSE_x5VAR <- sum((TC_ARW_Loocv_x5VAR - TC_ARW)^2)/n
TC_ARW_MAE_x5VAR <- sum(abs(TC_ARW_Loocv_x5VAR - TC_ARW))/n
TC_ARW_MSE_NULL <- sum((TC_ARW_Loocv_NULL - TC_ARW)^2)/n
TC_ARW_MAE_NULL <- sum(abs(TC_ARW_Loocv_NULL - TC_ARW))/n
TC_ARW_MSE <- cbind(TC_ARW_MSE_PCA, TC_ARW_MSE_SIR, TC_ARW_MSE_SAVE, 
                    TC_ARW_MSE_x5VAR, TC_ARW_MSE_NULL)
TC_ARW_MAE <- cbind(TC_ARW_MAE_PCA, TC_ARW_MAE_SIR, TC_ARW_MAE_SAVE, 
                    TC_ARW_MAE_x5VAR, TC_ARW_MAE_NULL)

TC_ARE_MSE_PCA <- sum((TC_ARE_Loocv_PCA - TC_ARE)^2)/n
TC_ARE_MAE_PCA <- sum(abs(TC_ARE_Loocv_PCA - TC_ARE))/n
TC_ARE_MSE_SIR <- sum((TC_ARE_Loocv_SIR - TC_ARE)^2)/n
TC_ARE_MAE_SIR <- sum(abs(TC_ARE_Loocv_SIR - TC_ARE))/n
TC_ARE_MSE_SAVE <- sum((TC_ARE_Loocv_SAVE - TC_ARE)^2)/n
TC_ARE_MAE_SAVE <- sum(abs(TC_ARE_Loocv_SAVE - TC_ARE))/n
TC_ARE_MSE_x5VAR <- sum((TC_ARE_Loocv_x5VAR - TC_ARE)^2)/n
TC_ARE_MAE_x5VAR <- sum(abs(TC_ARE_Loocv_x5VAR - TC_ARE))/n
TC_ARE_MSE_NULL <- sum((TC_ARE_Loocv_NULL - TC_ARE)^2)/n
TC_ARE_MAE_NULL <- sum(abs(TC_ARE_Loocv_NULL - TC_ARE))/n
TC_ARE_MSE <- cbind(TC_ARE_MSE_PCA, TC_ARE_MSE_SIR, TC_ARE_MSE_SAVE, 
                    TC_ARE_MSE_x5VAR, TC_ARE_MSE_NULL)
TC_ARE_MAE <- cbind(TC_ARE_MAE_PCA, TC_ARE_MAE_SIR, TC_ARE_MAE_SAVE, 
                    TC_ARE_MAE_x5VAR, TC_ARE_MAE_NULL)

TC_SP_MSE_PCA <- sum((TC_SP_Loocv_PCA - TC_SP)^2)/n
TC_SP_MAE_PCA <- sum(abs(TC_SP_Loocv_PCA - TC_SP))/n
TC_SP_MSE_SIR <- sum((TC_SP_Loocv_SIR - TC_SP)^2)/n
TC_SP_MAE_SIR <- sum(abs(TC_SP_Loocv_SIR - TC_SP))/n
TC_SP_MSE_SAVE <- sum((TC_SP_Loocv_SAVE - TC_SP)^2)/n
TC_SP_MAE_SAVE <- sum(abs(TC_SP_Loocv_SAVE - TC_SP))/n
TC_SP_MSE_x5VAR <- sum((TC_SP_Loocv_x5VAR - TC_SP)^2)/n
TC_SP_MAE_x5VAR <- sum(abs(TC_SP_Loocv_x5VAR - TC_SP))/n
TC_SP_MSE_NULL <- sum((TC_SP_Loocv_NULL - TC_SP)^2)/n
TC_SP_MAE_NULL <- sum(abs(TC_SP_Loocv_NULL - TC_SP))/n
TC_SP_MSE <- cbind(TC_SP_MSE_PCA, TC_SP_MSE_SIR, TC_SP_MSE_SAVE, 
                   TC_SP_MSE_x5VAR, TC_SP_MSE_NULL)
TC_SP_MAE <- cbind(TC_SP_MAE_PCA, TC_SP_MAE_SIR, TC_SP_MAE_SAVE, 
                   TC_SP_MAE_x5VAR, TC_SP_MAE_NULL)

TC_SPW_MSE_PCA <- sum((TC_SPW_Loocv_PCA - TC_SPW)^2)/n
TC_SPW_MAE_PCA <- sum(abs(TC_SPW_Loocv_PCA - TC_SPW))/n
TC_SPW_MSE_SIR <- sum((TC_SPW_Loocv_SIR - TC_SPW)^2)/n
TC_SPW_MAE_SIR <- sum(abs(TC_SPW_Loocv_SIR - TC_SPW))/n
TC_SPW_MSE_SAVE <- sum((TC_SPW_Loocv_SAVE - TC_SPW)^2)/n
TC_SPW_MAE_SAVE <- sum(abs(TC_SPW_Loocv_SAVE - TC_SPW))/n
TC_SPW_MSE_x5VAR <- sum((TC_SPW_Loocv_x5VAR - TC_SPW)^2)/n
TC_SPW_MAE_x5VAR <- sum(abs(TC_SPW_Loocv_x5VAR - TC_SPW))/n
TC_SPW_MSE_NULL <- sum((TC_SPW_Loocv_NULL - TC_SPW)^2)/n
TC_SPW_MAE_NULL <- sum(abs(TC_SPW_Loocv_NULL - TC_SPW))/n
TC_SPW_MSE <- cbind(TC_SPW_MSE_PCA, TC_SPW_MSE_SIR, TC_SPW_MSE_SAVE, 
                    TC_SPW_MSE_x5VAR, TC_SPW_MSE_NULL)
TC_SPW_MAE <- cbind(TC_SPW_MAE_PCA, TC_SPW_MAE_SIR, TC_SPW_MAE_SAVE, 
                    TC_SPW_MAE_x5VAR, TC_SPW_MAE_NULL)

TC_SPE_MSE_PCA <- sum((TC_SPE_Loocv_PCA - TC_SPE)^2)/n
TC_SPE_MAE_PCA <- sum(abs(TC_SPE_Loocv_PCA - TC_SPE))/n
TC_SPE_MSE_SIR <- sum((TC_SPE_Loocv_SIR - TC_SPE)^2)/n
TC_SPE_MAE_SIR <- sum(abs(TC_SPE_Loocv_SIR - TC_SPE))/n
TC_SPE_MSE_SAVE <- sum((TC_SPE_Loocv_SAVE - TC_SPE)^2)/n
TC_SPE_MAE_SAVE <- sum(abs(TC_SPE_Loocv_SAVE - TC_SPE))/n
TC_SPE_MSE_x5VAR <- sum((TC_SPE_Loocv_x5VAR - TC_SPE)^2)/n
TC_SPE_MAE_x5VAR <- sum(abs(TC_SPE_Loocv_x5VAR - TC_SPE))/n
TC_SPE_MSE_NULL <- sum((TC_SPE_Loocv_NULL - TC_SPE)^2)/n
TC_SPE_MAE_NULL <- sum(abs(TC_SPE_Loocv_NULL - TC_SPE))/n
TC_SPE_MSE <- cbind(TC_SPE_MSE_PCA, TC_SPE_MSE_SIR, TC_SPE_MSE_SAVE, 
                    TC_SPE_MSE_x5VAR, TC_SPE_MSE_NULL)
TC_SPE_MAE <- cbind(TC_SPE_MAE_PCA, TC_SPE_MAE_SIR, TC_SPE_MAE_SAVE, 
                    TC_SPE_MAE_x5VAR, TC_SPE_MAE_NULL)

