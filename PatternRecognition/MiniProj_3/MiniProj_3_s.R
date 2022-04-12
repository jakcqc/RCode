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
  if(all(a == 0))
  {
    a[1,] = 1
  }
  if(all(b == 0))
  {
    b[1,] = 1
  }
  for(ab in X[,x])
  {
    PP = PP + (a[aIndex] * b[bIndex%%3 + 1] * ab)
    bIndex = bIndex + 1
    if(bIndex%%4 == 0)
    {
       aIndex = aIndex + 1
    }
  }
  
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
  if(PD == 0)
  {
    PD = 1
  }
  if(PC == 0)
  {
    PD = 1
  }
  PCT = PC * PD
  if(PP == 0)
  {
    PP = 1
  }
  if(PCT == 0)
  {
    PCT = 1
  }
  PCPP = PCT * PP
  allProbs = matrix(c(PP,PC,PD, PCT, PCPP))
  return (allProbs)
}

pabxcd_cond_abxcd = function(fish, condif, season, locale, lightness, thickness)
{
  #default prior probs 
  #season
  a = A
  a = a *0
  #local
  b = B
  b = b*0
  #lightness
  c = C[1,]
  c = c*0
  #size
  d = D[1,]
  d = d*0
  ##light, thin, north atlantic, prob of season, prob of each class. 
  for(zz in colnames(a))
  {
    if(zz == season)
    {
      a = a * 0
      a[,season] = 1
    }
  }
  for(zz in colnames(b))
  {
    if(zz == locale)
    {
      b = b * 0
      b[,locale] = 1
    }
  }
  for(zz in colnames(c))
  {
    if(zz == lightness)
    {
      c = c * 0
      c[,lightness] = 1
    }
  }
  for(zz in colnames(d))
  { 
    
    if(zz == thickness)
    {
      d = d * 0
      d[,thickness] = 1
    }
  }
  
  sProb = pxabcd('salmon', a,b,c,d)
  tProb = pxabcd('tuna', a,b,c,d)
  seaProb = pxabcd('seabass', a,b,c,d)
  
 nSP = sProb[5]/(sProb[5]+tProb[5]+seaProb[5])
 nTP = tProb[5]/(sProb[5]+tProb[5]+seaProb[5])
 seaTP = seaProb[5]/(sProb[5]+tProb[5]+seaProb[5])
print('salmon prob')
print(nSP)
print('tuna prob')
print(nTP)
print('seabass prob')
print(seaTP)
print('all probs')
print(seaProb)
print(tProb)
print(sProb)

}
pabxcd_cond_abxcd(NULL, NULL, 'summer', 'northAtlantic','light','thin')
pabxcd_cond_abxcd(NULL, NULL, 'NULL', 'northAtlantic','light','average')
pabxcd_cond_abxcd(NULL, NULL, 'sunmmer', 'NULL','medium','thin')
pabxcd_cond_abxcd(NULL, NULL, 'sunmmer', 'midAtlantic','dark','wide')


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
