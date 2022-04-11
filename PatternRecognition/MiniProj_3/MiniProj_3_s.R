##mini project 3
pxabcd = function(x,a,b,c,d)
{
  PP = 0
  PC = 0
  PD = 0
  PCT = 0
  bIndex = 0
  aIndex = 1
  cIndex = 1
  dIndex = 1
  
  #find parent class prob first
  for(ab in X[,x])
  {
    PP = PP + (a[aIndex] * b[bIndex%%3 + 1] * ab)
    bIndex = bIndex + 1
    if(bIndex%%4 == 0)
    {
       aIndex = aIndex + 1
    }
  }
  print("Parent Prob")
  print(PP)
  return(PP)
  #find child liklihood
  for(xc in C[x,])
  {
    PC = PC + (xc * c[cIndex])
    cIndex = cIndex + 1
  }
  for(xd in D[x,])
  {
    PD = PD + (xd * d[dIndex])
    dIndex = dIndex + 1
  }
  
  PCT = PC * PD
  PCPP = PCT * PP
  allProbs = c(PP,PC,PD, PCT, PCPP)
  return (allProbs)
}

pabxcd_cond_abxcd = function(fish, condif, season, locale, lightness, thickness)
{
  #default prior probs 
  a = c(1,1,1,1) * .25
  b = c(1,1,1) * .33
  c = c(1,1,1) * .33
  d = c(1,1,1) * .33
  
  
}



X = t(matrix(c(0.6,0.2,0.2
                   ,0.5,0.3,0.2
                   ,0.8,0.1,0.1
                   ,0.7,0.1,0.2
                  ,0.4,0.3,0.3
                  ,0.3,0.5,0.2
                   ,0.2,0.3,0.5
                   ,0.6,0.1,0.3
                   ,0.2,0.1,0.7
                   ,0.1,0.1,0.8
                   ,0.1,0.3,0.6
                   ,0.2,0.7,0.1),  ncol = 12))


rownames(X) = c('a1b1', 
                      'a1b2',
                      'a1b3',
                      'a2b1',
                      'a2b2', 
                      'a2b3',
                      'a3b1', 
                      'a3b2',
                      'a3b3', 
                      'a4b1',
                      'a4b2',
                      'a4b3')

colnames(X) = c('salmon','tuna','seabass')
A = matrix(c(.25,.25,.25,.25), ncol = 4)
colnames(A) = c('winter','spring','summer','autumn')

B = matrix(c(4,.2,.4), ncol = 3)
colnames(B) = c('northAtlantic','midAtlantic','southAtlantic')


C = t(matrix(c(.3,.3,.4,.5,.3,.2,.4,.2,.4), ncol = 3))
rownames(C) = c('salmon','tuna','seabass')
colnames(C) = c('light','medium','dark')

D = t(matrix(c(.3,.4,.3,.4,.2,.4,.2,.3,.5), ncol = 3))
rownames(D) = c('salmon','tuna','seabass')
colnames(D) = c('wide','average','thin')
