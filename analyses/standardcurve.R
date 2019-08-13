### 22 July 2019 - Cat
# Let's make a standard curve for Total Phenolics


df.caffeic <- data.frame(conc=c(100, 150, 200, 250, 300), absorb=c(0.29,0.44,0.57,0.56,0.89))

lm(absorb~conc, data=df.caffeic)


plot(absorb~conc, data=df.caffeic)
