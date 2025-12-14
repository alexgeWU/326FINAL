# built into R
data("iris")

speciesID <- as.numeric(as.factor(iris$Species))

par(mfrow = c(1,1), mar = c(4, 4, 2, 1))
plot(iris$Sepal.Length, iris$Sepal.Width,
     pch = c(0, 1, 2)[speciesID],
     col = speciesID,
     xlab = "Sepal.Length",
     ylab = "Sepal.Width",
     main = "Sepal Length vs Width")
legend("topright",
       legend = levels(iris$Species),
       pch = c(0, 0, 0),
       col = speciesID,
       title = "Species",
       bg = "white")

par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
pairs(iris[, 1:4],
      col = speciesID,
      pch = 16,
      cex = .75,
      labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"),
      main = "Iris Visualization")

setosa <- iris[iris$Species == "setosa",]
versicolor <- iris[iris$Species == "versicolor",]
virginica <- iris[iris$Species == "virginica",]

par(mfrow = c(2, 2), mar = c(2, 4, 2, 1))
boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Sepal Length by Species Box Plots",
        ylab = "Sepal Length cm",
        xlab = "Species")
boxplot(Sepal.Width ~ Species,
        data = iris,
        main = "Sepal Width by Species Box Plots",
        ylab = "Sepal Width cm",
        xlab = "Species")
boxplot(Petal.Length ~ Species,
        data = iris,
        main = "Petal Length by Species Box Plots",
        ylab = "Petal Length cm",
        xlab = "Species")
boxplot(Petal.Width ~ Species,
        data = iris,
        main = "Petal Width by Species Box Plots",
        ylab = "Petal Width cm",
        xlab = "Species")
