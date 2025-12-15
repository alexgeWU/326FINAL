# built into R
data("iris")

speciesID <- as.numeric(as.factor(iris$Species))

# red = setosa, green = versicolor, blue = virginica
color <- c("red2", "limegreen", "mediumblue")
colorVals <- color[speciesID]

# circle, triangle, X
pchVals <- c(1, 2, 4)[speciesID]

# Graphs of pairs of traits with different colors for each species
par(mfrow = c(2, 3), mar = c(4, 4, 1, 1), oma = c(0, 0, 2, 0)) 
combinations <- combn(1:4, 2)
for(i in 1:6) {
  xCol <- combinations[1, i]
  yCol <- combinations[2, i]
  
  xLabel <- paste(names(iris)[xCol], "(cm)")
  yLabel <- paste(names(iris)[yCol], "(cm)")
  
  plot(iris[, xCol], iris[, yCol],
       col = colorVals,
       pch = pchVals,
       xlab = xLabel,
       ylab = yLabel)
}
mtext("Comparison of Iris Traits", outer = TRUE, font = 2)

# Box plots of each feature by species
par(mfrow = c(2, 2),
    mar = c(2, 2, 2, 1),
    oma = c(0, 0, 0, 0),
    mgp = c(2.5, 1, 0))
boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Sepal Length (cm) by Species Box Plot",
        col = color)
boxplot(Sepal.Width ~ Species,
        data = iris,
        main = "Sepal Width (cm) by Species Box Plot",
        col = color)
boxplot(Petal.Length ~ Species,
        data = iris,
        main = "Petal Length (cm) by Species Box Plot",
        col = color)
boxplot(Petal.Width ~ Species,
        data = iris,
        main = "Petal Width (cm) by Species Box Plot",
        col = color)

# seperate iris data set into each species
setosa <- iris[iris$Species == "setosa",]
versicolor <- iris[iris$Species == "versicolor",]
virginica <- iris[iris$Species == "virginica",]

# histogram of each feature by species
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1), mgp = c(2.2, 0.7, 0))
hist(setosa$Petal.Length, 
     main = "Setosa Petal Length (cm)",
     xlab = "Length (cm)", 
     col = color[1])
hist(setosa$Petal.Width, 
     main = "Setosa Petal Width (cm)",
     xlab = "Width (cm)", 
     col = color[1])

hist(versicolor$Petal.Length, 
     main = "Versicolor Petal (cm)",
     xlab = "Length (cm)", 
     col = color[2])
hist(versicolor$Petal.Width, 
     main = "Versicolor Petal Width (cm)",
     xlab = "Width (cm)", 
     col = color[2])

hist(virginica$Petal.Length, 
     main = "Virginica Petal Length (cm)",
     xlab = "Length (cm)", 
     col = color[3])
hist(virginica$Petal.Width, 
     main = "Virginica Petal Width (cm)",
     xlab = "Width (cm)", 
     col = color[3])

# gets confidence interval of the mean for a data set 
# default significance level of .05
getMeanConfidenceInterval <- function(dataSet, sigLevel = .05) {
  
  xBar <- mean(dataSet)
  S <- sd(dataSet) 
  n <- length (dataSet)
  
  dof <- n-1 # degree of freedom
  stndErr <- S / sqrt(n)
  
  tVal <- qt(1 - (sigLevel/2), dof)
  
  lowerBound <- xBar - (tVal * stndErr)
  upperBound <- xBar + (tVal * stndErr)
  
  return (c(lowerBound, upperBound))
}

# use getMeanConfidnceInterval to find CI of petal length for each species
ptlLenCIsetosa <- getMeanConfidenceInterval(setosa$Petal.Length)
ptlLenCIversicolor <- getMeanConfidenceInterval(versicolor$Petal.Length)
ptlLenCIvirginica <- getMeanConfidenceInterval(virginica$Petal.Length)

# round to make display better
roundPLCISet <- round(ptlLenCIsetosa, 4)
roundPLCIVer <- round(ptlLenCIversicolor, 4)
roundPLCIVir <- round(ptlLenCIvirginica, 4)

# display results
print("95% Confidence Intervals for Petal Length Means:")
print(paste0("Setosa: [", roundPLCISet[1], ", ", roundPLCISet[2], "]"))
print(paste0("Versicolor: [", roundPLCIVer[1], ", ", roundPLCIVer[2], "]"))
print(paste0("Virginica: [", roundPLCIVir[1], ", ", roundPLCIVir[2], "]"))

testTwoVar <- function(dataSet1, dataSet2, sigLevel = .05) {
  
  dof1 <- length(dataSet1) - 1
  dof2 <- length(dataSet2) - 1
  
  sampleVar1 <- var(dataSet1)
  sampleVar2 <- var(dataSet2)
  
  testStatisic <- sampleVar1 / sampleVar2
  
  pLeft <- pf(testStatisic, dof1, dof2)
  pRight <- 1 - pf(testStatisic, dof1, dof2)
    
  pVal <- 2 * min(pLeft, pRight)
  
  # TRUE if variance are equal
  return (pVal > sigLevel)
}

# Tests mean1 > mean2 returns TRUE if 1 > 2
testMeanGreater <- function(dataSet1, dataSet2, sigLevel = .05) {
  
  isEqualVar <- testTwoVar(dataSet1, dataSet2)
  
  n1 <- length(dataSet1)
  xBar1 <- mean(dataSet1)
  sampleVar1 <- var(dataSet1)
  
  n2 <- length(dataSet2)
  xBar2 <- mean(dataSet2)
  sampleVar2 <- var(dataSet2)

  # observed value
  obsVal <- 0
  dof <- 0
  
  if (isEqualVar) {
    dof <- n1 + n2 - 2
    
    pooledVar <- ((n1-1)*sampleVar1 + (n2-1)*sampleVar2) / dof
    
    obsVal <- (xBar1 - xBar2) / sqrt(pooledVar*(1/n1 + 1/n2))
  }
  
  else {
    dof <- floor( (sampleVar1/n1 + sampleVar2/n2)^2 / 
                  (
                    ((sampleVar1/n1)^2) / (n1 - 1) +
                    ((sampleVar2/n2)^2) / (n2 - 1)
                  )
                )
    
    obsVal <- (xBar1 - xBar2) / sqrt(sampleVar1/n1 + sampleVar2/n2)
  }
  
  pVal <- pt(obsVal, dof, lower.tail = FALSE)
  
  return (pVal < sigLevel)
}

ptlVirGrtVer <- testMeanGreater(virginica$Petal.Length, versicolor$Petal.Length)
ptlVerGrtSet <- testMeanGreater(versicolor$Petal.Length, setosa$Petal.Length)

if (ptlVirGrtVer) {
  print("Virginica petal length greater than versicolor")
} else {
  print("Virginica petal length less than versicolor")
}

if (ptlVerGrtSet) {
  print("Versicolor petal length greater than setosa")
} else {
  print("Versicolor petal length less than setosa")
}




