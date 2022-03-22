## For Grad Students: Computer exercise 1 and 2 on page 156, in chapter 3 of Duda’s textbook.
# For Undergrad Students: Computer exercise 1 and 10 on page 156 and 158 (respectively), in chapter
# 3 of Duda’s textbook.

#data! 
#make overall data class
 classData = t(matrix(c(0.42,-0.087,0.58,-0.4,0.58,0.089,0.83,1.6,-0.014
 ,-0.2,-3.3,-3.4,-0.31,0.27,-0.04,1.1,1.6,0.48
 ,1.3,-0.32,1.7,0.38,0.055,-0.035,-0.44,-0.41,0.32
 ,0.39,0.71,0.23,-0.15,0.53,0.011,0.047,-0.45,1.4
 ,-1.6,-5.3,-0.15,-0.35,0.47,0.034,0.28,0.35,3.1
 ,-0.029,0.89,-4.7,0.17,0.69,0.1,-0.39,-0.48,0.11
 ,-0.23,1.9,2.2,-0.011,0.55,-0.18,0.34,-0.079,0.14
 ,0.27,-0.3,-0.87,-0.27,0.61,0.12,-0.3,-0.22,2.2
 ,-1.9,0.76,-2.1,-0.065,0.49,0.0012,1.1,1.2,-0.46
 ,0.87,-1.0,-2.6,-0.12,0.054,-0.063,0.18,-0.11,-0.49), nrow = 9, ncol = 10))

 #assign each data to its specific class
 w1 = classData[,c(1,2,3)]
 w2 = classData[,c(4,5,6)]
 w3 = classData[,c(7,8,9)]
 
 
 
 ##1.
# Consider Gaussian density models in diﬀerent dimensions.
# (a) Write a program to ﬁnd the maximum likelihood values ˆµ and ˆσ2.
# Apply your program individually to each of the three features
# xi of category ω1 in the table above.
#
# (b) Modify your program to apply to two-dimensional Gaussian data p(x)∼N(µ,Σ).
# Apply your data to each of the three possible pairings of two features for ω1.
#
# (c) Modify your program to apply to three-dimensional Gaussian data. 
# Apply your data to the full three-dimensional data for ω1.
#
# (d) Assume your three-dimensional model is separable, so that
# Σ = diag(σ21,σ22,σ23).
# Write a program to estimate the mean and the diagonal components of Σ.
# Apply your program to the data in ω2
#
#(e) Compare your results for the mean of each feature µi calculated 
#in the aboveways. Explain why they are the same or diﬀerent.

#(f) Compare your results for the variance of each feature σ2i
#calculated in the aboveways. Explain why they are the same or diﬀerent.
 
 
 