rollLoadedDieTrials <- function(trialSizes, values, valuesPdf) {
dMean = sum(values*valuesPdf)
ssPdf = sum(values^2*valuesPdf)
dVariance <- ssPdf - dMean^2
means <- vector(mode="double", length=length(trialSizes))
variances <- vector(mode="double", length=length(trialSizes))

for (i in 1:length(trialSizes))
{
	rolls <- vector(length=trialSizes[i], mode="double")
	for (j in 1:trialSizes[i])
	{
		rolls[j] <- sample(values, 1, replace=TRUE, prob=valuesPdf)
	}
	means[i] <- mean(rolls)
	variances[i] <- var(rolls)
}

plot(log10(trialSizes), means)
lines(log10(trialSizes), rep(dMean, length(trialSizes)))
plot(log10(trialSizes), variances)
lines(log10(trialSizes), rep(dVariance, length(trialSizes)))

return(data.frame(dMean, dVariance))
}