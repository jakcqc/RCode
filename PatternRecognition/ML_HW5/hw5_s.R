library(MASS)



w1 = t(matrix(c(1, 1, 1, 2, 1, 4, 2, 1, 3, 1, 3, 3), nrow = 2, ncol = 6))
w2 = t(matrix(c(2, 2, 3, 2, 3, 4, 5, 1, 5, 4, 5, 5), nrow = 2, ncol = 6))
plot(c(w1[,1],w2[,1]),c(w1[,2], w2[,2]))
  
classMat = matrix(c(w1,w2), ncol = 4)    
classMat

w1Mean = c(0,0)
w2Mean = c(0,0)

for(x in 1:2)
{
  for(y in 1:6)
  {
    w2Mean[x] = w2Mean[x] + w2[y,x]
    w1Mean[x] = w1Mean[x] + w1[y,x]
  }
}
w1Mean = w1Mean * (1/6)
w2Mean = w2Mean * (1/6)
w1Mean
w2Mean


w1Scatter = c(0,0)
w2Scatter = c(0,0)
for(x in 1:2)
{
  for(y in 1:6)
  {
    w2Scatter[x] = w2Scatter[x] - w2Mean[x]
    w1Scatter[x] = w1Scatter[x] - w1Mean[x]
  }
}


plot(classMat[,c(1)], classMat[,c(2)],  col = 'red', xlim = range(-2:12), ylim = range(-6:7))
points(classMat[,c(3)], classMat[,c(4)],  col = 'blue', xlim = range(-2:12), ylim = range(-6:7))

lines(c(w1Mean[1],w2Mean[1]),c(w1Mean[2],w2Mean[2]))
SB = t(t(w1Mean - w2Mean))

SB = SB %*% t(SB)
SB
w1Mean
S1 = matrix(c(0,0,0,0), ncol = 2)
S2 = matrix(c(0,0,0,0), ncol = 2)
for(x in 1:6)
{
  S1 = S1 + ((t(t(w1[x,])) - t(t(w1Mean))) %*% (t( t(t(w1[x,])) - t(t(w1Mean)) )))
  S2 = S2 + ((t(t(w2[x,])) - t(t(w2Mean))) %*% (t( t(t(w2[x,])) - t(t(w2Mean)) )))
}
SW = S1 + S2
S1
S2
SW
w = solve(SW) %*% (w1Mean - w2Mean)
w
JW = (t(w) %*% SB %*% w) / (t(w) %*% SW %*% w)
JW
lines(-2:14, (JW*-2:14) -4, col = "green")
w1Proj = c(0,0,0,0,0,0)
w2Proj = c(0,0,0,0,0,0)
scaledW = w * -10
for(x in 1:6)
{
  w1Proj[x] = t(scaledW) %*% w1[x,]
  w2Proj[x] = t(scaledW) %*% w2[x,]
}
points(w1Proj, JW * (w1Proj)-4, pch = 6, col = 'red')
points(w2Proj, JW * (w2Proj)-4, pch = 5, col = 'blue')

w1MeanProj = t(t(c(0,0)))
w1MeanProj
w2MeanProj = t(t(c(0,0)))
w2MeanProj
w1Mean


w1MeanProj = w1MeanProj
points(w1MeanProj[1], w1MeanProj[2], pch = 20, col = 'red')
points(w2MeanProj[1], w2MeanProj[2], pch = 20, col = 'blue')
lines(c(w1MeanProj[1], w2MeanProj[1]), c(w1MeanProj[2], w2MeanProj[2]))
