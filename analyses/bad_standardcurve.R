### 22 July 2019 - Cat
# Let's make a standard curve for Total Phenolics

df <- data.frame(conc = c(100, 150, 200, 250, 300), absorb = c(261, 362, 484, 529, 823))

lm((absorb/765) ~ conc, data=df)


plot(absorb/765~conc, data=df)


df.oneround <- data.frame(conc=c(100, 150, 200, 250), absorb=c(0.02, 0.0339, 0.0423, 0.07))
lm(absorb/765~conc, data=df.oneround)
