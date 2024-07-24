##### Chapter 9 SVM

## Example 1; svm{e1071}
library("e1071")
data(iris)
svm.e1071 <- svm(Species ~ . , data = iris,
                 type = "C-classification",  # "C-classification", "eps-regression", ets
                 kernel = "radial", # GaussianRBF 
                 cost = 10, gamma = 0.1) # tune parametor
summary(svm.e1071)

plot(svm.e1071, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
