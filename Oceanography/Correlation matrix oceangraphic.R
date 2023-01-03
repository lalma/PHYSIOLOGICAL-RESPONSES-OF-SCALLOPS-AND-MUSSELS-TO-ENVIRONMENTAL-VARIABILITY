install.packages("Hmisc")
library(readxl)
oceangraphic_correlation_matrix <- read_excel("C:/Users/Lindsay/Dropbox/MUSCA/Taylor Oceanographic Data/oceangraphic correlation matrix.xlsx")
View(oceangraphic_correlation_matrix)



install.packages("corrplot")
library(corrplot)
M<-cor(oceangraphic_correlation_matrix)
head(round(M,2))
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(oceangraphic_correlation_matrix)
head(p.mat[, 1:5])
col<- colorRampPalette(c("#990033","#CC3333",  "steelblue4","#000066"))(50)
corrplot(M, type="upper", order="alphabet", col=col,
         p.mat = p.mat, sig.level = 0.05, tl.col="black")



