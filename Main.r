# built into R
data("iris")

speciesID <- as.numeric(as.factor(iris$Species))

plot(iris$Sepal.Length, iris$Sepal.Width,
     pch = speciesID,
     col = speciesID,
     xlab = "Sepal.Length",
     ylab = "Sepal.Width",
     main = "Sepal Length vs Width")

pairs(iris[, 1:4],
      col = speciesID,
      pch = 16,
      main = "Iris Visualization")
      
boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Sepal Length Data Box Plots",
        ylab = "Sepal Length cm",
        xlab = "Species")
