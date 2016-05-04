
setwd('~/R/DynamicRegressionModel/')
msleep <- read.table(file = "DataAuto.txt",sep="\t",header=TRUE,dec=".",row.names=1)

colnames(msleep) <- c("Price","Cylinder","Power","Weight","Consumption")

#Omitting NA in the data frame
msleep <- na.omit(msleep)

#Setting the response variable
response <- "Consumption"

#Number of colums of the data frame
getLen <- dim(msleep)[2]

for (i in 1:getLen){
        #Removing the response variable in the temporary data frame
        if (names(msleep)[i] == response){
		        expVariable <- msleep [,-i]
				break;
        }
}
#Number of colums of the temporary data frame of explanatory variables
getLenExpVar <- dim(expVariable)[2]

#Initializing a Matrix which will contain all combinations of predictor model
ExpVarMatrix <- matrix( ncol = getLenExpVar)

#Creating combination
for (i in 1:getLenExpVar){
        #Combination function
		comb    <- t(combn(names(expVariable),i))
		numbRow <- nrow(comb)
		numbCol <- length(names(expVariable))
		numbRowNA  <- numbRow
		numbColNA  <- numbCol-ncol(comb)
		naMatr   <- matrix(rep(NA, numbRowNA*numbColNA), nrow = numbRowNA, ncol = numbColNA)
		result   <- cbind(comb, naMatr)
		ExpVarMatrix <- rbind(ExpVarMatrix, result)
}
#Removing all NA
ExpVarMatrix <- ExpVarMatrix[-1,]

#Final result of combination between response and explanatory variables

#Setting an empty data frame
dynamicRegression <- data.frame()
for (i in 1:nrow(ExpVarMatrix)){

        getVal <- na.omit (ExpVarMatrix[i, ])
		mdRegComb <- paste (response, " ~ ", paste (getVal, collapse = " + "), sep = "")
		#print(mdRegComb)
        mdLM <- lm(as.formula(mdRegComb), data = msleep)
        SMry <- summary(mdLM)
		
		#Diagnostic parameters
		FStats <- SMry$fstatistic[1] #Fstatistic
        f <- SMry$fstatistic #p-value
        modelPValue <- pf( f[1], f[2], f[3],lower = FALSE ) #model p-Value
        RSqrt  <- SMry[8]   #R-Squared
        AdjRSqrt <- SMry[9] #adj R-Squared
        AIC  <- AIC(mdLM)#AIC
        BIC  <- BIC(mdLM)#BIC
		#Assembling diagnostic parameters per model predictor in Matrix of all combinations
		dFrame <- data.frame(modelReg = mdRegComb, RSquared = RSqrt, AdjustedRSquared = AdjRSqrt, AIC = AIC, BIC = BIC, modelPValue = modelPValue, FStats = f[1])
		
		#Loading data framme
		dynamicRegression <- rbind(dynamicRegression, dFrame)
}

#Writing dynamicRegression in a csv file
write.csv(dynamicRegression, "dynamicRegression.csv", row.names = F)
out <- dynamicRegression[,c(1,2,3,4,5)]
colnames(out) <- c("modelReg","RQqrt","adjRSqrt","AIC","BIC")
print(out)
