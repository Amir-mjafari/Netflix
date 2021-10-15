---
  title: "Final Project"
author: "Amirhossein Moghaddas Jafari"
date: "8/9/2021"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Final Project

## PHASE I: Factor Screening

```{r}
## Design matrix

## Two-level experiment with a 2^4-1 fractional factorial design.

## A
Tile.Size <- c(-1,1,-1,1,-1,1,-1,1)
## B
Match.Score <- c(-1,-1,1,1,-1,-1,1,1)
## C
Prev.Length <- c(-1,-1,-1,-1,1,1,1,1)
## D = ABC
Prev.Type <- c(-1,1,1,-1,1,-1,-1,1)

dm.c <- data.frame(Tile.Size,Match.Score,Prev.Length,Prev.Type)

View(dm.c)
```

```{r}
# Function for converting from natural units to coded units
convert.N.to.C <- function(U,UH,UL){
  x <- (U - (UH+UL)/2) / ((UH-UL)/2)
  return(x)
}

# Function for converting from coded units to natural units
convert.C.to.N <- function(x,UH,UL){
  U <- x*((UH-UL)/2) + (UH+UL)/2
  return(U)
}
```

```{r}
## dm.n is design matrix in natural units.

Prev.Type[Prev.Type==-1] <- "TT"
Prev.Type[Prev.Type==1] <- "AC"


dm.n <- data.frame(Tile.Size=convert.C.to.N(x=Tile.Size, UH=0.3,UL=0.1),Match.Score=convert.C.to.N(x=Match.Score, UH=100,UL=80),Prev.Length=convert.C.to.N(x=Prev.Length, UH=120,UL=100),Prev.Type)

View(dm.n)

## Converting data frame to CSV file
write.csv(dm.n,"/cloud/project/20804524.CSV", row.names = F)
```


```{r}

netflix1 <- read.csv("/cloud/project/RESULTS_20804524_2021-08-09.csv", header = TRUE)
netflix2 <- read.csv("/cloud/project/RESULTS_20804524_2021-08-12.csv", header = TRUE)
netflix.ph1 <- rbind(netflix1,netflix2)

head(netflix.ph1,1600)
```


```{r}

## Change from natural unit to coded unit
Tile.Size <- convert.N.to.C(U=netflix.ph1$Tile.Size, UH=0.3,UL=0.1)

Match.Score <- convert.N.to.C(U=netflix.ph1$Match.Score, UH=100,UL=80)

Prev.Length <- convert.N.to.C(U=netflix.ph1$Prev.Length, UH=120,UL=100)

netflix.ph1$Prev.Type[netflix.ph1$Prev.Type == "TT"] <- -1
netflix.ph1$Prev.Type[netflix.ph1$Prev.Type == "AC"] <- 1
Prev.Type <-  netflix.ph1$Prev.Type
Prev.Type <- as.numeric(Prev.Type)

Browse.Time <-  netflix.ph1$Browse.Time








## Try fitting the full model with all 2^4 terms that would be of interest

fd.model <- lm(Browse.Time~(Tile.Size+Match.Score+Prev.Length+Prev.Type)^4) 
summary(fd.model)
```

**Conclusion:**All factors are influential except Tile Size. The three interaction here is aliased with the preview type. The interaction between Match score and prev length is significant, but, it is jointly showing the influence of the interaction between tile size and prev type.

```{r}
## Main Effect and interaction plot
## Graphical Summaries of the data
library(gplots)
par(mfrow = c(2,2))

plotmeans(netflix.ph1$Browse.Time ~ netflix.ph1$Match.Score, main = "Main Effect Plot for Match Score", xlab = "Match Score (%)", ylab = "Browse Time (min)", pch = 16, ylim = c(0,30))
legend("topright", legend = c("P-value:","<2e10^-16"), lty = c(1,1), col=c("white", "red", "blue"), cex = 0.8, bty = "n") 

plotmeans(netflix.ph1$Browse.Time ~ netflix.ph1$Prev.Length, main = "Main Effect Plot for Prev Length", xlab = "Prev Length (sec)", ylab = "Browse Time (min)", pch = 16, ylim = c(0,30))


netflix.ph1$Prev.Type[netflix.ph1$Prev.Type == "-1"] <- "TT"
netflix.ph1$Prev.Type[netflix.ph1$Prev.Type == "1"] <- "AC"
plotmeans(netflix.ph1$Browse.Time ~ netflix.ph1$Prev.Type, main = "Main Effect Plot for Prev Type", xlab = "Prev Type", ylab = "Browse Time (min)", pch = 16, ylim = c(0,30))


plotmeans(netflix.ph1$Browse.Time ~ netflix.ph1$Tile.Size, main = "Main Effect Plot for Tile Size", xlab = "Tile Ratio", ylab = "Browse Time (min)", pch = 16, ylim = c(0,30))

interaction.plot(netflix.ph1$Prev.Length, netflix.ph1$Match.Score, netflix.ph1$Browse.Time, ylab = "Browse Time (min)", xlab = "Prev Length (Sec)", main = "Interaction Plot", ylim = c(0, 30), legend = F)

legend("bottomleft", legend = c("Match.Score","80 %", "100 %"), lty = c(1,1,2), col=c("white", "black", "black"), cex = 0.8, bty = "n") 


## interaction.plot(netflix.ph1$Tile.Size, netflix.ph1$Prev.Length, netflix.ph1$Browse.Time, ylab = "Browse Time (min)", xlab = "Prev Length (Sec)", main = "Interaction Plot", ylim = c(0, 30), legend = F)

```

## PHASE II: Method of Steepest Descent

### Searching for omptimum when preview type is TT

```{r}

## Reusing data from phase 1

netflix.ph2.c <- data.frame(Tile.Size,Match.Score,Prev.Length,Prev.Type,Browse.Time)
head(netflix.ph2.c,1900)

```
### Searching for omptimum when preview type is TT

```{r}

## Reusing the data from phase 1 when prev type is TT
netflix.ph2.c.tt <-data.frame(Match.Score=netflix.ph2.c$Match.Score[netflix.ph2.c$Prev.Type == -1], Prev.Length = netflix.ph2.c$Prev.Length[netflix.ph2.c$Prev.Type == -1], Tile.Size = netflix.ph2.c$Tile.Size[netflix.ph2.c$Prev.Type == -1], Browse.Time = netflix.ph2.c$Browse.Time[netflix.ph2.c$Prev.Type == -1])
head(netflix.ph2.c.tt,1000)

## We know this region is not in the vicinity of optimum so we should
## perform a steepest descent phase.


## Fit the first order model to determine the direction of the path of 
## steepest descent


model.ph2.tt.fo <- lm(Browse.Time ~ Prev.Length + Match.Score  , data = netflix.ph2.c.tt)

model.ph2.tt.fo2 <- lm(Browse.Time ~ Match.Score + Prev.Length + Tile.Size, data = netflix.ph2.c.tt)

## Checking whether tile size is significant or not.
anova(model.ph2.tt.fo,model.ph2.tt.fo2)
## Tile size is not significant

summary(model.ph2.tt.fo)
beta0 <- coef(model.ph2.tt.fo)[1]
beta1 <- coef(model.ph2.tt.fo)[2]
beta2 <- coef(model.ph2.tt.fo)[3]

## Calculate the coordinates along this path that we will experiment at

# The gradient vector
g <- matrix(c(beta1, beta2), nrow = 1)

## The factors and their low/center/high levels are as follows:
## Preview Length: 100  vs 110  vs 120
## Match Score:   80 vs 90 vs 100

# We will take steps of size 5 seconds in preview length. In coded units this is
PL.step <- convert.N.to.C(U = 110 + 5, UH = 120, UL = 100)
lamda <- PL.step/abs(beta1)

## Step 0: The center point we've already observed
x.old <- matrix(0, nrow=1, ncol=2)
step0 <- data.frame(Prev.Length = convert.C.to.N(x = 0, UH = 120, UL = 100), 
                    Match.Score = round(convert.C.to.N(x = 0, UH = 100, UL = 80)))


## Step 1: 
x.new <- x.old - lamda*g
step1 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 2: 
x.old <- x.new
x.new <- x.old - lamda*g
step2 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 3: 
x.old <- x.new
x.new <- x.old - lamda*g
step3 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 4: 
x.old <- x.new
x.new <- x.old - lamda*g
step4 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))


## Step 5: 
x.old <- x.new
x.new <- x.old - lamda*g
step5 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))



## The following is a list of the conditions along the path of steepest descent
pstd.cond <- data.frame(rbind(step0, step1, step2, step3, step4, step5),Prev.Type = rep("TT",6))
pstd.cond

## Converting data frame to CSV file
write.csv(pstd.cond,"/cloud/project/20804524.CSV", row.names = F)

## after getting the data we will read them
pstd.data <- read.csv("/cloud/project/RESULTS_20804524_2021-08-12 (1).csv", header = TRUE)

## Calculate the average browsing time in each of these conditions and find the 
## condition that minimizes it

pstd.means <- aggregate(pstd.data$Browse.Time, 
                        by = list(Prev.Length = pstd.data$Prev.Length, 
                                  Match.Score = pstd.data$Match.Score), 
                        FUN = mean)

## we need to continue 
## Step 6: 
x.old <- x.new
x.new <- x.old - lamda*g
step6 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 7: 
x.old <- x.new
x.new <- x.old - lamda*g
step7 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 8: 
x.old <- x.new
x.new <- x.old - lamda*g
step8 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

pstd.cond2 <- data.frame(rbind(step6, step7, step8),Prev.Type = rep("TT",3))
pstd.cond2

## Converting data frame to CSV file
write.csv(pstd.cond2,"/cloud/project/20804524.CSV", row.names = F)


## after getting the data we will read them
pstd.data2 <- read.csv("/cloud/project/RESULTS_20804524_2021-08-12 (2).csv", header = TRUE)

## Calculate the average browsing time in each of these conditions and find the 
## condition that minimizes it
pstd.means2 <- aggregate(pstd.data2$Browse.Time, 
                         by = list(Prev.Length = pstd.data2$Prev.Length, 
                                   Match.Score = pstd.data2$Match.Score), 
                         FUN = mean)
pstd.means2

pstd.means.full <- data.frame(apply(rbind(pstd.means2,pstd.means),2,rev))


## Plotting the path

plot(x = 0:8, y = pstd.means.full$x,
     type = "l", xlab = "Step Number", ylab = "Average Browsing Time")
points(x = 0:8, y = pstd.means.full$x,
       col = "red", pch = 16)

## CLEARLY STEP5 in minimizing the browsing time

## We should follow this up with 2^2 factorial conditions to ensure we're close to optimum
## We will re-center our coded scale in this new region as follows:
## Preview Length: 70  vs 85  vs 100
## Match Score:   49 vs 64 vs 79

## check for curve
curve.check.cond <- data.frame(Prev.Length=c(70,100,70,100,85), Match.Score=c(49,49,79,79,64), Prev.Type=c("TT","TT","TT","TT","TT"))

## getting the data
write.csv(curve.check.cond,"/cloud/project/20804524.CSV", row.names = F)

## Reading the data
curve.check.data <- read.csv("/cloud/project/RESULTS_20804524_2021-08-12 (3).csv", header = TRUE)
View(curve.check.data)

## converting data to coded unit

curve.check.data.c <- data.frame(Match.Score=convert.N.to.C(U=curve.check.data$Match.Score, UH=79,UL=49),Prev.Length=convert.N.to.C(U=curve.check.data$Prev.Length, UH=100,UL=70), Browse.Time=curve.check.data$Browse.Time)
curve.check.data.c$xPQ <- (curve.check.data.c$Match.Score^2 + curve.check.data.c$Prev.Length^2)/2

## Check to see if that's significant
m <- lm(Browse.Time~Prev.Length+Match.Score+Prev.Length*Match.Score +xPQ, data =curve.check.data.c )
summary(m)

## Yes, it is significant and so there is significant quadratic curvature in
## this region of the response surface. We should now commence phase 3 and 
## perform a respond surface design and fit a full second order model.



```





### Searching for omptimum when preview type is AC

```{r}

## Reusing the data from phase 1 when prev type is AC
netflix.ph2.c.ac <-data.frame(Match.Score=netflix.ph2.c$Match.Score[netflix.ph2.c$Prev.Type == 1], Prev.Length = netflix.ph2.c$Prev.Length[netflix.ph2.c$Prev.Type == 1], Browse.Time = netflix.ph2.c$Browse.Time[netflix.ph2.c$Prev.Type == 1])
head(netflix.ph2.c.ac,1000)

## We know this region is not in the vicinity of optimum so we should
## perform a steepest descent phase.


## Fit the first order model to determine the direction of the path of 
## steepest descent


model.ph2.ac.fo <- lm(Browse.Time ~ Prev.Length + Match.Score, data = netflix.ph2.c.ac)
summary(model.ph2.ac.fo)
beta0 <- coef(model.ph2.ac.fo)[1]
beta1 <- coef(model.ph2.ac.fo)[2]
beta2 <- coef(model.ph2.ac.fo)[3]

## Calculate the coordinates along this path that we will experiment at

# The gradient vector
g2 <- matrix(c(beta1, beta2), nrow = 1)

## The factors and their low/center/high levels are as follows:
## Preview Length: 100  vs 110  vs 120
## Match Score:   80 vs 90 vs 100

# We will take steps of size 5 seconds in preview length. In coded units this is
PL.step <- convert.N.to.C(U = 110 + 5, UH = 120, UL = 100)
lamda2 <- PL.step/abs(beta1)

## Step 0: The center point we've already observed
x.old <- matrix(0, nrow=1, ncol=2)
step01 <- data.frame(Prev.Length = convert.C.to.N(x = 0, UH = 120, UL = 100), 
                     Match.Score = round(convert.C.to.N(x = 0, UH = 100, UL = 80)))


## Step 1: 
x.new <- x.old - lamda2*g2
step11 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                     Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 2: 
x.old <- x.new
x.new <- x.old - lamda2*g2
step21 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                     Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 3: 
x.old <- x.new
x.new <- x.old - lamda2*g2
step31 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                     Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 4: 
x.old <- x.new
x.new <- x.old - lamda2*g2
step41 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                     Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))


## Step 5: 
x.old <- x.new
x.new <- x.old - lamda2*g2
step51 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                     Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 6: 
x.old <- x.new
x.new <- x.old - lamda2*g2
step61 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                     Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 7: 
x.old <- x.new
x.new <- x.old - lamda2*g2
step71 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                     Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## Step 8: 
x.old <- x.new
x.new <- x.old - lamda2*g2
step81 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                     Match.Score = round(convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80)))

## The following is a list of the conditions along the path of steepest descent
pstd.cond1 <- data.frame(rbind(step01, step11, step21, step31, step41, step51,step61,step71,step81),Prev.Type = rep("AC",9))
pstd.cond1

## Converting data frame to CSV file
write.csv(pstd.cond1,"/cloud/project/20804524.CSV", row.names = F)

## after getting the data we will read them
pstd.data1 <- read.csv("/cloud/project/RESULTS_20804524_2021-08-12 (4).csv", header = TRUE)

## Calculate the average browsing time in each of these conditions and find the 
## condition that minimizes it
pstd.means1 <- aggregate(pstd.data1$Browse.Time, 
                         by = list(Prev.Length = pstd.data1$Prev.Length, 
                                   Match.Score = pstd.data1$Match.Score), 
                         FUN = mean)

pstd.means1 <- data.frame(apply(rbind(pstd.means1),2,rev))

## Plotting the path

plot(x = 0:8, y = pstd.means1$x,
     type = "l", xlab = "Step Number", ylab = "Average Browsing Time")
points(x = 0:8, y = pstd.means1$x,
       col = "red", pch = 16)

## CLEARLY STEP5 in minimizing the browsing time

## We should follow this up with 2^2 factorial conditions to ensure we're close to optimum
## We will re-center our coded scale in this new region as follows:
## Preview Length: 70  vs 85  vs 100
## Match Score:   53 vs 68 vs 83

## check for curve
curve.check.cond1 <- data.frame(Prev.Length=c(70,100,70,100,85), Match.Score=c(53,53,83,83,68), Prev.Type=c("AC","AC","AC","AC","AC"))

## getting the data
write.csv(curve.check.cond1,"/cloud/project/20804524.CSV", row.names = F)


curve.check.data1 <- read.csv("/cloud/project/RESULTS_20804524_2021-08-12 (5).csv", header = TRUE)
View(curve.check.data1)

## converting data to coded unit

curve.check.data.c1 <- data.frame(Match.Score=convert.N.to.C(U=curve.check.data1$Match.Score, UH=83,UL=53),Prev.Length=convert.N.to.C(U=curve.check.data1$Prev.Length, UH=100,UL=70), Browse.Time=curve.check.data1$Browse.Time)
curve.check.data.c1$xPQ <- (curve.check.data.c1$Match.Score^2 + curve.check.data.c1$Prev.Length^2)/2

## Check to see if that's significant
m1 <- lm(Browse.Time~Prev.Length+Match.Score+Prev.Length*Match.Score +xPQ, data =curve.check.data.c1 )
summary(m1)

## Yes, it is significant and so there is significant quadratic curvature in
## this region of the response surface. We should now commence phase 3 and 
## perform a respond surface design and fit a full second order model.

```
```{r}

## red is when prev type is TT
plot(x = 0:8, y = pstd.means.full$x,
     type = "l", xlab = "Step Number", ylab = "Average Browsing Time (Minutes)",col = "red",ylim=c(10,25))
points(x = 0:8, y = pstd.means.full$x,
       col = "red", pch = 16)

## blue is when prev type is AC
lines(x = 0:8, y = pstd.means1$x,
      type = "l", xlab = "Step Number", ylab = "Average Browsing Time (Minutes)",col="blue")
points(x = 0:8, y = pstd.means1$x,
       col = "blue", pch = 16)
legend("bottomleft", legend = c("Prev.Type","TT", "AC"), lty = c(1,1,1), col=c("white", "red", "blue"), cex = 0.8, bty = "n") 


```


## PHASE III: Response Optimization

## Preview type = TT

```{r}

## PHASE III: Response Optimization
## Preview type = TT

## point near the optimum:
## Preview Length: 70  vs 85  vs 100
## Match Score:   49 vs 64 vs 79

## Choosing a ccd with alpha=2^(1/2)

m = convert.C.to.N(x=1.41  , UH= 100 , UL= 70)
m

n = convert.N.to.C(U=110  , UH= 100 , UL= 70)
n

## We chose alpha to be +- 1.3333
## CCD: 
## Preview length: 65 vs 70 vs 85 vs 100 vs 105
## Match Score:    44 vs 49 vs 64 vs 79  vs 84

curve.check.data

## we have already 5 conditions in "curve.check.data". we just need to get data for the outside points of the ccd design.

ccd.out.tt.cond <- data.frame(Prev.Length=c(65,105,85,85), Match.Score=c(64,64,44,84), Prev.Type=c("TT", "TT", "TT", "TT"))

## ccd.out.tt.cond <- data.frame(Prev.Length=c(60,110,85,85), Match.Score=c(64,64,39,89), Prev.Type=c("TT", "TT", "TT", "TT"))

write.csv(ccd.out.tt.cond, "/cloud/project/20804524.CSV", row.names = F)

ccd.out.tt.data <- read.csv("/cloud/project/RESULTS_20804524_2021-08-13.csv", header = TRUE)

## ccd.out.tt.data <- read.csv("/cloud/project/RESULTS_20804524_2021-08-13 (2).csv", header = TRUE)

head(ccd.out.tt.data,1000)

## All 9 conditions have been provided for fitting the second order model.
ccd.tt <- rbind(ccd.out.tt.data,curve.check.data)

## Converting to coded unit
ccd.tt.c <- data.frame(Prev.Length=convert.N.to.C(U=ccd.tt$Prev.Length, UH= 100 , UL= 70), Match.Score=convert.N.to.C(U=ccd.tt$Match.Score, UH= 79 , UL= 49), Browse.Time=ccd.tt$Browse.Time )


## We then fit the full 2nd-order response surface
model <- lm(Browse.Time ~ Prev.Length + Match.Score + Prev.Length*Match.Score + I(Prev.Length^2) + I(Match.Score^2), data = ccd.tt.c,)
summary(model)

## Let's visualize this surface:
beta0 <- coef(model)[1]
beta1 <- coef(model)[2]
beta2 <- coef(model)[3]
beta12 <- coef(model)[6]
beta11 <- coef(model)[4]
beta22 <- coef(model)[5]

# Function to create x and y grids for contour plots 
mesh <- function(x, y) { 
  Nx <- length(x)
  Ny <- length(y)
  list(
    x = matrix(nrow = Nx, ncol = Ny, data = x),
    y = matrix(nrow = Nx, ncol = Ny, data = y, byrow = TRUE)
  )
}

grd <- mesh(x = seq(convert.N.to.C(U = 50, UH = 100, UL = 70), 
                    convert.N.to.C(U = 120, UH = 100, UL = 70), 
                    length.out = 100), 
            y = seq(convert.N.to.C(U = 30, UH = 79, UL = 49), 
                    convert.N.to.C(U = 100, UH = 79, UL = 49), 
                    length.out = 100))
x1 <- grd$x
x2 <- grd$y
eta.so <- beta0 + beta1*x1 + beta2*x2 + beta12*x1*x2 + beta11*x1^2 + beta22*x2^2

# 2D contour plot (coded units)
# Function to create blues
blue_palette <- colorRampPalette(c(rgb(247,251,255,maxColorValue = 255), rgb(8,48,107,maxColorValue = 255)))

contour(x = seq(convert.N.to.C(U = 50, UH = 100, UL = 70), 
                convert.N.to.C(U = 120, UH = 100, UL = 70), 
                length.out = 100), 
        y = seq(convert.N.to.C(U = 30, UH = 79, UL = 49), 
                convert.N.to.C(U = 100, UH = 79, UL = 49), 
                length.out = 100), 
        z = eta.so, xlab = "Preview Length", ylab = "Match Score",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

## Let's find the maximum of this surface and the corresponding factor levels 
## at which this is achieved
b <- matrix(c(beta1,beta2), ncol = 1)
B <- matrix(c(beta11, 0.5*beta12, 0.5*beta12, beta22), nrow = 2, ncol = 2)
x.s <- -0.5*solve(B) %*% b 
points(x = x.s[1], y = x.s[2], col = "red", pch = 16)

# The predicted book rate at this configuration is:
eta.s <- beta0 + 0.5*t(x.s) %*% b
## eta.s is equal to 10.42, it is the minimum browsing time in this configuration.

# In natural units this optimum is located at
m <- convert.C.to.N(x = x.s[1,1], UH = 100, UL = 70)
n <- convert.C.to.N(x = x.s[2,1], UH = 79, UL = 49)

## the corresponding preview length is 64.40 and the match score is 81.60. 
## for practical purposes we should change them to pl=65 and ms=82.


# Remake the contour plot but in natural units
contour(x = seq(50, 120, length.out = 100), 
        y = seq(30, 100, length.out = 100), 
        z = eta.so, xlab = "Preview Length (Seconds)", ylab = "Match Score (%)",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

points(x = convert.C.to.N(x = x.s[1,1], UH = 100, UL = 70),
       y = convert.C.to.N(x = x.s[2,1], UH = 79, UL = 49), 
       col = "red", pch = 16)

points(x = 65, y = 82, pch = 16, col = "green")

legend("bottomright", legend = c("Preview Type:","TT"), lty = c(1,1), col=c("white", "white", "white"), cex = 0.8, bty = "n")



## 95% prediction interval at this optimum:
n.data <- data.frame(Prev.Length=x.s[1,1], Match.Score=x.s[2,1])
pred <- predict(model, newdata = n.data, type = "response", se.fit = TRUE)
pred 
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))




## 95% prediction interval at convenient near-optimum:
x1 <- convert.N.to.C(U=65  , UH= 100 , UL= 70)
x2 <- convert.N.to.C(U=82  , UH= 79 , UL= 49)
n.data <- data.frame(Prev.Length=x1, Match.Score=x2)

pred <- predict(model, newdata = n.data, type = "response", se.fit = TRUE, interval = "confidence")
pred
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))



pred.tt.pred <- predict(model, newdata = n.data, type = "response", se.fit = TRUE, interval="predict")

pred.tt.pred


pred.tt.conf <- predict(model, newdata = n.data, type = "response", se.fit = TRUE, interval="confidence")

pred.tt.conf






############# Prediction interval :  10.42964 8.382823 12.47645 ##################
############# Confidence interval :  10.42964 10.14942 10.70985 ##################

```







## Preview type = AC

```{r}

## PHASE III: Response Optimization
## Preview type = AC

## point near the optimum:
## Preview Length: 70  vs 85  vs 100
## Match Score:   53 vs 68 vs 83

## Choosing a ccd with alpha=2^(1/2)

m = convert.C.to.N(x=1.41  , UH= 100 , UL= 70)
m

n = convert.N.to.C(U=106.15  , UH= 100 , UL= 70)
n

## We chose alph to be +- 1.3333
## CCD: 
## Preview length: 65 vs 70 vs 85 vs 100 vs 105
## Match Score:    48 vs 53 vs 68 vs 83  vs 88

curve.check.data1

## we have already 5 conditions in "curve.check.data1". we just need to get data for the outside points of the ccd design.

ccd.out.ac.cond <- data.frame(Prev.Length=c(65,105,85,85), Match.Score=c(68,68,48,88), Prev.Type=c("AC", "AC", "AC", "AC"))

## ccd.out.tt.cond <- data.frame(Prev.Length=c(60,110,85,85), Match.Score=c(64,64,39,89), Prev.Type=c("TT", "TT", "TT", "TT"))

write.csv(ccd.out.ac.cond, "/cloud/project/20804524.CSV", row.names = F)

ccd.out.ac.data <- read.csv("/cloud/project/RESULTS_20804524_2021-08-13 (3).csv", header = TRUE)



head(ccd.out.ac.data,1000)

## All 9 conditions have been provided for fitting the second order model.
ccd.ac <- rbind(ccd.out.ac.data,curve.check.data1)

## Converting to coded unit
ccd.ac.c <- data.frame(Prev.Length=convert.N.to.C(U=ccd.ac$Prev.Length, UH= 100 , UL= 70), Match.Score=convert.N.to.C(U=ccd.ac$Match.Score, UH= 83 , UL= 53), Browse.Time=ccd.ac$Browse.Time )


## We then fit the full 2nd-order response surface
model1 <- lm(Browse.Time ~ Prev.Length + Match.Score + Prev.Length*Match.Score + I(Prev.Length^2) + I(Match.Score^2), data = ccd.ac.c,)
summary(model1)

## Let's visualize this surface:
beta0 <- coef(model1)[1]
beta1 <- coef(model1)[2]
beta2 <- coef(model1)[3]
beta12 <- coef(model1)[6]
beta11 <- coef(model1)[4]
beta22 <- coef(model1)[5]

# Function to create x and y grids for contour plots 
mesh <- function(x, y) { 
  Nx <- length(x)
  Ny <- length(y)
  list(
    x = matrix(nrow = Nx, ncol = Ny, data = x),
    y = matrix(nrow = Nx, ncol = Ny, data = y, byrow = TRUE)
  )
}

grd <- mesh(x = seq(convert.N.to.C(U = 50, UH = 100, UL = 70), 
                    convert.N.to.C(U = 120, UH = 100, UL = 70), 
                    length.out = 100), 
            y = seq(convert.N.to.C(U = 30, UH = 83, UL = 53), 
                    convert.N.to.C(U = 100, UH = 83, UL = 53), 
                    length.out = 100))
x1 <- grd$x
x2 <- grd$y
eta.so1 <- beta0 + beta1*x1 + beta2*x2 + beta12*x1*x2 + beta11*x1^2 + beta22*x2^2
## eta.s is equal to 15.35, it is the minimum browsing time in this configuration.

# 2D contour plot (coded units)
# Function to create blues
blue_palette <- colorRampPalette(c(rgb(247,251,255,maxColorValue = 255), rgb(8,48,107,maxColorValue = 255)))

contour(x = seq(convert.N.to.C(U = 50, UH = 100, UL = 70), 
                convert.N.to.C(U = 120, UH = 100, UL = 70), 
                length.out = 100), 
        y = seq(convert.N.to.C(U = 30, UH = 83, UL = 53), 
                convert.N.to.C(U = 100, UH = 83, UL = 53), 
                length.out = 100), 
        z = eta.so1, xlab = "Preview Length", ylab = "Match Score",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

## Let's find the maximum of this surface and the corresponding factor levels 
## at which this is achieved
b <- matrix(c(beta1,beta2), ncol = 1)
B <- matrix(c(beta11, 0.5*beta12, 0.5*beta12, beta22), nrow = 2, ncol = 2)
x.s1 <- -0.5*solve(B) %*% b 
points(x = x.s1[1], y = x.s1[2], col = "red", pch = 16)

# The predicted book rate at this configuration is:
eta.s1 <- beta0 + 0.5*t(x.s1) %*% b
## eta.s is equal to 15.35, it is the minimum browsing time in this configuration.

# In natural units this optimum is located at
m <- convert.C.to.N(x = x.s1[1,1], UH = 100, UL = 70)
n <- convert.C.to.N(x = x.s1[2,1], UH = 83, UL = 53)

## the corresponding preview length is 70.17 and the match score is 77.26. 
## for practical purposes we should change them to pl=70 and ms=77.


# Remake the contour plot but in natural units
contour(x = seq(50, 120, length.out = 100), 
        y = seq(30, 100, length.out = 100), 
        z = eta.so1, xlab = "Preview Length (Seconds)", ylab = "Match Score (%)",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

points(x = convert.C.to.N(x = x.s1[1,1], UH = 100, UL = 70),
       y = convert.C.to.N(x = x.s1[2,1], UH = 83, UL = 53), 
       col = "red", pch = 16)

points(x = 70, y = 77, pch = 16, col = "green")

legend("bottomright", legend = c("Preview Type:","AC"), lty = c(1,1), col=c("white", "white", "white"), cex = 0.8, bty = "n")



## 95% prediction interval at this optimum:
n.data <- data.frame(Prev.Length=x.s1[1,1], Match.Score=x.s1[2,1])
pred <- predict(model1, newdata = n.data, type = "response", se.fit = TRUE)
pred 
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))




## 95% prediction interval at convenient near-optimum:

## for practical purposes we should change them to pl=70 and ms=77.
x1 <- convert.N.to.C(U=70  , UH= 100 , UL= 70)
x2 <- convert.N.to.C(U=77  , UH= 83 , UL= 53)
n.data <- data.frame(Prev.Length=x1, Match.Score=x2)

pred <- predict(model1, newdata = n.data, type = "response", se.fit = TRUE)
pred
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))

pred.ac.pred <- predict(model1, newdata = n.data, type = "response", se.fit = TRUE, interval="predict")

pred.ac.pred


pred.ac.conf <- predict(model1, newdata = n.data, type = "response", se.fit = TRUE, interval="confidence")

pred.ac.conf






############# Prediction interval :  15.3522 13.37959 17.32481 ##################
############# Confidence interval :  15.3522 15.22096 15.48344 ##################


```

```{r}

par(mfrow = c(1,2))

contour(x = seq(50, 120, length.out = 100), 
        y = seq(30, 100, length.out = 100), 
        z = eta.so, xlab = "Preview Length (Seconds)", ylab = "Match Score (%)",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

points(x = convert.C.to.N(x = x.s[1,1], UH = 100, UL = 70),
       y = convert.C.to.N(x = x.s[2,1], UH = 79, UL = 49), 
       col = "red", pch = 16) 




points(x = 65, y = 82, pch = 16, col = "green")

legend("bottomright", legend = c("Preview Type:","TT"), lty = c(1,1), col=c("white", "white", "white"), cex = 0.8, bty = "n")
legend("bottomleft", legend = c("Theoretical","Practical"), lty = c(1,1), col=c("red","green", "green","white"), cex = 0.8, bty = "n")


contour(x = seq(50, 120, length.out = 100), 
        y = seq(30, 100, length.out = 100), 
        z = eta.so1, xlab = "Preview Length (Seconds)", ylab = "Match Score (%)",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

points(x = convert.C.to.N(x = x.s1[1,1], UH = 100, UL = 70),
       y = convert.C.to.N(x = x.s1[2,1], UH = 83, UL = 53), 
       col = "red", pch = 16)

points(x = 70, y = 77, pch = 16, col = "green")

legend("bottomright", legend = c("Preview Type:","AC"), lty = c(1,1), col=c("white", "white", "white"), cex = 0.8, bty = "n")

legend("bottomleft", legend = c("Theoretical","Practical"), lty = c(1,1), col=c("red","green", "green","white"), cex = 0.8, bty = "n")







```


```{r}


par(mfrow = c(1,2))


## plot for when preview type is:TT


## Preview length: 65 vs 70 vs 85 vs 100 vs 105
## Match Score:    44 vs 49 vs 64 vs 79  vs 84
plot(x = c(70,100,70,100,65,105,85,85,85),y = c(49,49,79,79,64,64,84,44,64), pch = 16, col = "green", type= "p", xlab="Preview Length (Seconds)", ylab="Match Score (%)", axes=TRUE,ylim=c(30,100),xlim=c(50,120) )

p = data.frame(X=c(-1,1,-1,1,-1.41,1.41,0,0,0),Y=c(-1,-1,1,1,0,0,1.41,-1.41,0))
points(x=convert.C.to.N(x =p$X, UH = 100, UL = 70), y=convert.C.to.N(x =p$Y, UH = 79, UL = 49), col="red")

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"),)
legend("bottomright", legend = c("Preview Type:","TT"), lty = c(1,1), col=c("white", "white", "white"), cex = 0.8, bty = "n")

legend("bottomleft", legend = c("Theoretical","Practical"), lty = c(1,1), col=c("red","green", "green","white"), cex = 0.8, bty = "n")

## plot for when preview type is:AC


## Preview length: 65 vs 70 vs 85 vs 100 vs 105
## Match Score:    48 vs 53 vs 68 vs 83  vs 88
plot(x = c(70,100,70,100,65,105,85,85,85),y = c(53,53,83,83,68,68,88,48,68), pch = 16, col = "green", type= "p", xlab="Preview Length (Seconds)", ylab="Match Score (%)", axes=TRUE,ylim=c(30,100),xlim=c(50,120) )

p = data.frame(X=c(-1,1,-1,1,-1.41,1.41,0,0,0),Y=c(-1,-1,1,1,0,0,1.41,-1.41,0))
points(x=convert.C.to.N(x =p$X, UH = 100, UL = 70), y=convert.C.to.N(x =p$Y, UH = 83, UL = 53), col="red")

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"),)
legend("bottomright", legend = c("Preview Type:","AC"), lty = c(1,1), col=c("white", "white", "white"), cex = 0.8, bty = "n")

legend("bottomleft", legend = c("Theoretical","Practical"), lty = c(1,1), col=c("red","green", "green","white"), cex = 0.8, bty = "n")





```

## PHASE IV: Confirmation

```{r}

## X = The design matrix from the original experiment with columns
## corresponding to each effect that was estimated (intercept, main
## effects, interactions, and quadratics).

#X_ <- data.matrix(data.frame(intercept=c(1,1,1,1,1,1,1,1,1), Prev.Length=c(-1,1,-1,1,-1.333,1.333,0,0,0), Match.Score=c(-1,-1,1,1,0,0,-1.333,1.333,0), Two.interacton = c(-1,1,-1,1,-1.333,1.333,0,0,0)*c(-1,-1,1,1,0,0,-1.333,1.333,0) , qx1= c(-1,1,-1,1,-1.333,1.333,0,0,0)^2 , qx2 = c(-1,-1,1,1,0,0,-1.333,1.333,0)^2))

X_ <- data.matrix(data.frame(intercept=c(1,1,1,1,1,1,1,1,1), Prev.Length=c(-1,1,-1,1,-1.333,1.333,0,0,0), Match.Score=c(-1,-1,1,1,0,0,-1.333,1.333,0),qx1= c(-1,1,-1,1,-1.333,1.333,0,0,0)^2 , qx2 = c(-1,-1,1,1,0,0,-1.333,1.333,0)^2, Two.interacton = c(-1,1,-1,1,-1.333,1.333,0,0,0)*c(-1,-1,1,1,0,0,-1.333,1.333,0)))




## making a x matrix:
## CR = prev.lenght = 65 and match.score = 82
## confirmation points: (65,82) , (70,87) , (70,78), (60,78), (60,87)
x.n = data.frame(Prev.Length=c(60,70,60,70,65), Match.Score=c(78,78,87,87,82), Prev.Type=c("TT","TT","TT","TT","TT"))


write.csv(x.n, "/cloud/project/20804524.CSV", row.names = F)

conf.points <- read.csv("/cloud/project/RESULTS_20804524_2021-08-15.csv", header = TRUE)

## x = The 'row' of X corresponding to the point in the factor space at
## which the confirmation runs are being compared to the RS. If one
## point is considered this is a row vector, if several points are
## consider x is a matrix with as many rows. Columns are ordered as
## main effects, quadratic effects and interaction effects
## (subscripts ascending). The units here must match the units of X
## (i.e, coded +/-1 vs natural).



x_ <- data.matrix(data.frame(intercept = c(1,1,1,1,1) ,    Prev.Length=convert.N.to.C(U=x.n$Prev.Length, UH = 100, UL = 70), Match.Score=convert.N.to.C(U=x.n$Match.Score, UH = 79, UL = 49), qx1 = convert.N.to.C(U = x.n$Prev.Length, UH = 100, UL = 70)^2 , qx2 = convert.N.to.C(U = x.n$Match.Score, UH = 79, UL = 49)^2 , Two.interaction = convert.N.to.C(U = x.n$Prev.Length, UH = 100, UL = 70)*convert.N.to.C(U = x.n$Match.Score, UH = 79, UL = 49)))

y.c_ <- t(data.matrix(data.frame(y1 = conf.points$Browse.Time[conf.points$Prev.Length==60 & conf.points$Match.Score==78 ], y2 = conf.points$Browse.Time[conf.points$Prev.Length==70 & conf.points$Match.Score==78 ], y3 = conf.points$Browse.Time[conf.points$Prev.Length==60 & conf.points$Match.Score==87 ], y4 = conf.points$Browse.Time[conf.points$Prev.Length==70 & conf.points$Match.Score==87 ],  y5 = conf.points$Browse.Time[conf.points$Prev.Length==65 & conf.points$Match.Score==82 ] )))

delta_ = c(0.25,0.25,0.25,0.25,0.25)

















# Function that calculates the commensurate probability
prob.comm <- function(X, k, model, x, y.c, delta, direction, CI){
  ## Inputs:
  ## X = The design matrix from the original experiment with columns
  ## corresponding to each effect that was estimated (intercept, main
  ## effects, interactions, and quadratics).
  ## k = The number of factors being considered.
  ## model = An lm() object corresponding to the RS as estimated by the
  ## original experiment.
  ## x = The 'row' of X corresponding to the point in the factor space at
  ## which the confirmation runs are being compared to the RS. If one
  ## point is considered this is a row vector, if several points are
  ## consider x is a matrix with as many rows. Columns are ordered as
  ## main effects, quadratic effects and interaction effects
  ## (subscripts ascending). The units here must match the units of X
  ## (i.e, coded +/-1 vs natural).
  ## y.c = A matrix whose entries correspond to the confirmation
  ## observations at the point x. Rows correspond to location and
  ## columns correspond to repeats.
  ##delta = The interval (-delta, delta) represents the range of differences
  ## between RS estimates that are deemed to be of no practical
  ## importance. This may be a vector if more than one location is
  ## considered.
  ##direction = Either "greater" or "less" indicating the direction of the
  ## inequality in the definition of the commensurate probability.
  ## For maximization use "greater", for minimization use "less".
  ## CI = A logical indicating whether a confidence interval is to be
  ## calculated
  ##
  ## Outputs:
  ##Theta = Estimate(s) of the commensurate probability at the confirmation
  ## location(s).
  ## LCL = Lower 95% confidence limits for the commensurate probability at
  ## the confirmation location(s).
  # Preparing the estimates associated with the original experiment
  if(is.null(dim(x))){
    n.l <- 1
  }else{
    n.l <- dim(x)[1]
  }
  theta <- rep(0, n.l)
  lcl <- rep(0, n.l)
  for(j in 1:n.l){
    if(n.l == 1){
      u <- matrix(x, nrow = 1)
      y <- y.c
    }else if(n.l > 1){
      u <- matrix(x[j,], nrow = 1)
      y <- y.c[j,]
    }
    beta <- model$coefficients # beta vector for original model
    vc <- vcov(model) # covariance matrix for the beta vector
    mu <- u %*% beta # this is predicted response at the confirmation
    #location
    sig2 <- as.numeric(summary(model)['sigma'])^2 # estimate of error
    #variance in original model
    vyhat <- sig2 * (u %*% solve(t(X) %*% X) %*% t(u))
    # Preparing the estimates associated with the confirmation runs
    mu.c <- mean(y)
    n.c <- length(y)
    if(n.c == 1){# single confirmation run
      sig2.c <- sig2
    }else if(n.c > 1){
      sig2.c <- var(y) # estimate of error variance in confirmation runs
    }
    vyhat.c <- sig2.c / n.c
    # Calculate the commensurate probability
    if(direction == "greater"){
      theta[j] <- 1 - pnorm((-delta[j] - (mu.c - mu))/sqrt(vyhat.c + vyhat))
    }else if(direction == "less"){
      theta[j] <- pnorm((delta[j] - (mu.c - mu))/sqrt(vyhat.c + vyhat))
    }
    if(CI == TRUE){
      # Calculate the lower confidence limit via bootstrapping
      library(mvtnorm)
      B <- 100000
      beta.boot <- rmvnorm(n = B, mean = beta, sigma = vc)
      mu.boot <- u %*% t(beta.boot)
      sig2.boot <- sig2 * rchisq(n = B, df = model$df.residual) /
        model$df.residual
      vyhat.boot <- sig2.boot * (u %*% solve(t(X) %*% X) %*% t(u))
      mu.c.boot <- rnorm(n = B, mean = mu.c, sd = sqrt(sig2.c))
      if(n.c == 1){
        sig2.c.boot <- sig2.boot
      }else if(n.c > 1){
        sig2.c.boot <- sig2.c * rchisq(n = B, df = (n.c-1)) / (n.c-1)
      }
      vyhat.c.boot <- sig2.c.boot / n.c
      theta.boot <- rep(0, B)
      if(direction == "greater"){
        for(i in 1:B){
          theta.boot[i] <- 1 - pnorm((-delta[j] - (mu.c.boot[i] -
                                                     mu.boot[i]))/sqrt(vyhat.c.boot[i] + vyhat.boot[i]))
        }
      }else if(direction == "less"){
        for(i in 1:B){
          theta.boot[i] <- pnorm((delta[j] - (mu.c.boot[i] -
                                                mu.boot[i]))/sqrt(vyhat.c.boot[i] + vyhat.boot[i]))
        }
      }
      lcl[j] <- as.numeric(quantile(theta.boot, 0.05))
    }
  }
  if(CI == TRUE){
    return(list(Theta = theta, LCL = lcl))
  }else{
    return(list(Theta = theta))
  }
}




```

```{r}
prob.comm <- prob.comm(X = X_ , k = 2 , model = model , x = x_ , y.c = y.c_ , delta=c(0.25,0.25,0.25,0.25,0.25), direction = "less", CI = FALSE)

prob.comm
```

```{r}
x1 <- convert.N.to.C(U=70  , UH= 100 , UL= 70)
x2 <- convert.N.to.C(U=78  , UH= 79 , UL= 49)
n.data <- data.frame(Prev.Length=x1, Match.Score=x2)

pred <- predict(model, newdata = n.data, type = "response", se.fit = TRUE)
pred


a = (x_[1,] %*% solve(t(X_) %*% X_) %*% t(x_[1,])
     
     ```
     
     ```{r}
     
     ## RSM model predicition in the confirmation locations
     model_prediction <- x_ %*% model$coefficients
     model_prediction
     
     average_observation <- rowMeans(y.c_) 
     average_observation
     
     
     ```
     
     
     
     ```{r}
     ## Confidence interval for the new optimum which we identified in confirmation phase
     
     x1 <- convert.N.to.C(U=70  , UH= 100 , UL= 70)
     x2 <- convert.N.to.C(U=78  , UH= 79 , UL= 49)
     n.data <- data.frame(Prev.Length=x1, Match.Score=x2)
     
     pred_new_opt <- predict(model, newdata = n.data, type = "response", se.fit = TRUE, interval = "confidence")
     pred_new_opt
     
     ````