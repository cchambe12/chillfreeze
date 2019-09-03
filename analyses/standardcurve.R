### 22 July 2019 - Cat
# Let's make a standard curve for Total Phenolics


df.caffeic <- data.frame(conc=c(100, 150, 200, 250, 300), absorb=c(0.29,0.44,0.57,0.56,0.89))

df.caf.correct <- data.frame(conc=c(20, 40, 70), absorb=c(7.9, 9.58, 10.13))

lm(absorb~conc, data=df.caf.correct)


plot(absorb~conc, data=df.caf.correct)


## Function for absorbance
folinfunc <- function(x) { 
  
  #z = 765 - ((0.00264*x + 0.022) * -1)
  
  x = x*-1
  
  z = (x-0.04253)/7.36053
  
  tpc = (z*0.02)/0.05
  
}
