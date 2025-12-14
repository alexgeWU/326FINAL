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
