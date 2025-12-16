# built into R
data("iris")

# ____CONSTANTS____
speciesID <- as.numeric(as.factor(iris$Species))

# red = setosa, green = versicolor, blue = virginica
color <- c("red2", "limegreen", "mediumblue")
colorVals <- color[speciesID]

# circle, triangle, X
pchVals <- c(1, 2, 4)[speciesID]

# ____COMPARISON OF IRIS TRAITS____

# Graphs of pairs of traits with different colors for each species
par(mfrow = c(3, 2),
    mar = c(4, 4, 1, 1),
    oma = c(0, 0, 4, 0)) 
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
       ylab = yLabel,
       cex.lab = 1.15)
}
# title
mtext("Comparison of Iris Traits", outer = TRUE, font = 2, line = 2)

# legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 2, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", legend = c("Setosa", "Versicolor", "Virginica"),
       col = color, pch = c(1, 2, 4), horiz = TRUE, bty = "n", cex = 1.2)

# ____FEATURE BOX PLOTS____

# Box plots of each feature by species
par(mfrow = c(2, 2),
    mar = c(2, 4, 2, 1),
    oma = c(2, 0, 0, 0),
    mgp = c(2.5, 1, 0))
boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Sepal Length by Species Box Plot",
        ylab = "Sepal Length (cm)",
        cex.lab = 1.15,
        col = color)
boxplot(Sepal.Width ~ Species,
        data = iris,
        main = "Sepal Width by Species Box Plot",
        ylab = "Sepal Width (cm)",
        cex.lab = 1.15,
        col = color)
boxplot(Petal.Length ~ Species,
        data = iris,
        main = "Petal Length by Species Box Plot",
        ylab = "Petal Length (cm)",
        cex.lab = 1.15,
        col = color)
boxplot(Petal.Width ~ Species,
        data = iris,
        main = "Petal Width by Species Box Plot",
        ylab = "Petal Width (cm)",
        cex.lab = 1.15,
        col = color)

# legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", legend = c("Setosa", "Versicolor", "Virginica"), 
       fill = color, horiz = TRUE, bty = "n", cex = 1.15)

# ____HISTOGRAMS____

par(mfrow = c(1, 2),
    mar = c(4, 4, 2, 1),
    oma = c(1.5, 0, 0, 0),
    mgp = c(2.2, .7, 0))

# seperate iris data set into each species
setosa <- iris[iris$Species == "setosa",]
versicolor <- iris[iris$Species == "versicolor",]
virginica <- iris[iris$Species == "virginica",]

# histogram color adjustment
histColor <- adjustcolor(color, alpha.f = .6)

# petal length params
ylimits <- c(0, 20)
xlimits <- c(min(iris$Petal.Length), max(iris$Petal.Length))
breakLen <- pretty(iris$Petal.Length, n = 25)

# histogram of petal lengths
hist(setosa$Petal.Length,
     breaks = breakLen,
     ylim = ylimits,
     xlim = xlimits,
     main = "Petal Length by Species",
     xlab = "Petal Length (cm)", 
     col = histColor[1])
hist(versicolor$Petal.Length,
     breaks = breakLen,
     col = histColor[2],
     add = TRUE)
hist(virginica$Petal.Length,
     breaks = breakLen,
     col = histColor[3],
     add = TRUE)

# petal width params
ylimits <- c(0, 35)
xlimits <- c(min(iris$Petal.Width), max(iris$Petal.Width))
breakLen <- pretty(iris$Petal.Width, n = 20)

# histogram of petal widths
hist(setosa$Petal.Width,
     breaks = breakLen,
     ylim = ylimits,
     xlim = xlimits,
     main = "Petal Width by Species",
     xlab = "Petal Width (cm)", 
     col = histColor[1])
hist(versicolor$Petal.Width,
     breaks = breakLen,
     col = histColor[2],
     add = TRUE)
hist(virginica$Petal.Width,
     breaks = breakLen,
     col = histColor[3],
     add = TRUE)

# legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", legend = c("Setosa", "Versicolor", "Virginica"), 
       fill = histColor, horiz = TRUE, bty = "n")

# ____CONFIDENCE INTERVALS____

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

# ___HYPOTHESIS TEST ON MEAN____

# helper test variance equivalence of two unknown var
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

# tests mean1 > mean2 returns TRUE if mean1 > mean2
testMeanGreater <- function(dataSet1, dataSet2, sigLevel = .05) {
  
  isEqualVar <- testTwoVar(dataSet1, dataSet2)
  
  n1 <- length(dataSet1)
  xBar1 <- mean(dataSet1)
  sampleVar1 <- var(dataSet1)
  
  n2 <- length(dataSet2)
  xBar2 <- mean(dataSet2)
  sampleVar2 <- var(dataSet2)

  obsVal <- 0 # observed value
  dof <- 0 # degree of freedom
  
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
  
  return (c(pVal < sigLevel, sigLevel, pVal))
}


PLVirGrtVer <- testMeanGreater(virginica$Petal.Length, versicolor$Petal.Length)
PLVerGrtSet <- testMeanGreater(versicolor$Petal.Length, setosa$Petal.Length)

if (PLVirGrtVer[1]) {
  # significant case
  print(paste0("Reject Null: Virginica length is greater (p = ", 
               PLVirGrtVer[3] ," > ", PLVirGrtVer[2],")"))
} else {
  # not significant case
  print(paste0("Fail to Reject: Virginica length is NOT significantly greater",
               " (p = ", PLVirGrtVer[3]," > ", PLVirGrtVer[2],")"))
}

if (PLVerGrtSet[1]) {
  print(paste0("Reject Null: Versicolor length is greater (p = ", 
               PLVerGrtSet[3] ," > ", PLVerGrtSet[2],")"))
} else {
  print(paste0("Fail to Reject: Versicolor length is NOT significantly greater",
               " (p = ", PLVerGrtSet[3]," > ", PLVerGrtSet[2],")"))
}

