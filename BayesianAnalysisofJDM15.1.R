#### Bayesian Analysis of JDMv15.1-Jan2020 data - primary hypothesis

library(rstan)
library(rstanarm)
library(shinystan)
library(psych)
library("poLCA")
library("reshape2")
library("plyr")
library("dplyr")
library("poLCA")
library("ggplot2")
library("ggparallel")
library("igraph")
library("tidyr")
library("knitr")

## data from running the JDMv15.1-Jan2020.R file (run first)
source("./JDMv15.1-Jan2020.R")
pub.out.simple <- lm(difi_team~dgOffer, data=data)
pub.out <- lm(difi_team~dgOffer+gender+age+education+country+income+winnerN+treatment, data=data)
summary(pub.out)
out <- stan_lm(difi_team~dgOffer+education+country, data=data, prior=R2(0.02))

shinystan::launch_shinystan(out)

hist(data$ownTeam_distance_before)
hist(data$otherTeam_distance_before)

hist(data$difi_team)
hist(data$difi_team_nonScale)
hist(data$difi_wholeGroup)
cor(data$difi_team,data$difi_wholeGroup)
ggplot(data,aes(difi_team,difi_wholeGroup)) + geom_smooth()

ggplot(data,aes(x=jitter(dgOffer),y=difi_team)) + geom_smooth()
hist(data$dgOffer)

### PEM analysis

psych::describe(data)
rm(dat)
dat <- data
dat$difi_otherTeam.dec <- as.numeric(as.character(cut(dat$difi_otherTeam,10,labels=F)))
dat$difi_ownTeam.dec <- as.numeric(as.character(cut(dat$difi_ownTeam,10,labels=F)))
dat$difi_wholeGroup.dec <- as.numeric(as.character(cut(dat$difi_wholeGroup,10,labels=F)))
table(dat$difi_otherTeam.dec)
table(dat$difi_ownTeam.dec)
table(dat$difi_wholeGroup.dec)

describe(dat[,25:27])

lca.dat <- dat[,25:27]
names(lca.dat) <- c("other","own","whole")
describe(lca.dat)


f <- with(lca.dat, cbind(other,own,whole)~1)

#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(f, lca.dat, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	
LCA_best_model
plot(LCA_best_model)
str(LCA_best_model)
dat$pred.class <- LCA_best_model$predclass
dat$pred.class.f <- as.factor(dat$pred.class)
lca.lm <- lm(dgOffer~pred.class.f,data=dat)
summary(lca.lm)

# class  Other   Own  Whole Perc  Parochialism  Universalism
#  1      L       M     M    .06       M             L
#  2      L       L     L    .08       L             L
#  3      H       H     H    .24       L             H
#  4      M       M     M    .16       L             L
#  5      M       H     H    .31       M             H
#  6      LM      H     HM   .16       H             M

## parochialism:  own - other
## universalism:  whole

## OK, so we need the probabilities of class 1 and 6 for parochialism
## we need the probabilities of classes 3 and 5 for Universalism

dat$prob.c1.p <- LCA_best_model$posterior[,1]
dat$prob.c6.p <- LCA_best_model$posterior[,6]
dat$prob.c3.u <- LCA_best_model$posterior[,3]
dat$prob.c5.u <- LCA_best_model$posterior[,5]

dat$offer.beta <- dat$dgOffer_nonScale/0.4

hist(dat$offer.beta)

dat$paroch <- dat$prob.c1.p + dat$prob.c6.p
dat$univer <- dat$prob.c3.u + dat$prob.c5.u

hist(dat$paroch)
hist(dat$univer)

cor(dat$paroch,dat$univer)
library(xkcd)
library(xkcdcolors)

ggplot(dat, aes(x=paroch, y=offer.beta)) + 
  geom_jitter() + 
  geom_smooth(method="lm") + 
  theme_xkcd() +
  labs(x="Parochialism", y="Proportion of 40 cents offered")

ggplot(dat, aes(x = offer.beta)) +
  geom_density(fill = "blue", alpha = .6) +
  theme_light() +
  labs(x = "Proportion of 40 cents offered",
       y = "Density")

stan_code <- "
data {
  int n;
  vector[n] x;
  vector<lower=0, upper=1>[n] y;
}

parameters {
  vector[4] coef;
}

transformed parameters {
  vector<lower=0,upper=1>[n] mu;
  vector<lower=0>[n] phi;
  vector[n] p;
  vector[n] q;
  for (i in 1:n){
    mu[i] = inv_logit(coef[1] + coef[2] * x[i]);
    phi[i] = exp(coef[3] + coef[4] * x[i]);
    p[i] = mu[i] * phi[i];
    q[i] = phi[i] - mu[i] * phi[i];
  }
}

model {
  y ~ beta(p, q);
  coef ~ normal(0,4);
}
"

dat <- dat[!is.na(dat$offer.beta),]
beta_normalize <- function(x) {
  x_ <- ((x - min(x)) / (max(x) - min(x)))
  (x_ * (length(x_) - 1) + 0.5) / length(x_)
}

dat$offer.beta.norm <- beta_normalize(dat$offer.beta)
range(dat$offer.beta)
range(dat$offer.beta.norm)

stan_dat.p <- list(n=nrow(dat), x=dat$paroch, y = dat$offer.beta.norm)
stan_dat.u <- list(n=nrow(dat), x=dat$univer, y = dat$offer.beta.norm)

mod.stan.p <- stan(model_code = stan_code, data=stan_dat.p, iter=1000, chains=4, cores=24)
mod.stan.u <- stan(model_code = stan_code, data=stan_dat.u, iter=1000, chains=4, cores=24)
round(summary(mod.stan.p)$summary[1:4,],2)
round(summary(mod.stan.u)$summary[1:4,],2)


library(bayesplot)
library(xkcd)
draws.p <- as.matrix(mod.stan.p)
draws.p <- draws.p[, c("coef[2]", "coef[4]")]
colnames(draws.p) <- c("mu", "phi")
mcmc_hist(draws.p, facet_args = list(labeller = label_parsed)) +
  ggtitle("Posterior densities for slope coefficients",
          "Proportion of 40 cents regressed on Parochialism") +
    theme_xkcd()

draws.u <- as.matrix(mod.stan.u)
draws.u <- draws.u[, c("coef[2]", "coef[4]")]
colnames(draws.p) <- c("mu", "phi")
mcmc_hist(draws.u, facet_args = list(labeller = label_parsed)) +
  ggtitle("Posterior densities for slope coefficients",
          "Proportion of 40 cents regressed on Parochialism") +
  theme_xkcd()


mod.stan.p.summary <- summary(mod.stan.p)$summary
mus.p <- mod.stan.p.summary[grepl("mu", rownames(mod.stan.p.summary)), "mean"]
mus_lo.p <- mod.stan.p.summary[grepl("mu", row.names(mod.stan.p.summary)), 4]
mus_hi.p <- mod.stan.p.summary[grepl("mu", row.names(mod.stan.p.summary)), 8]
ggplot() +
  geom_jitter(aes(x = dat$paroch, y = dat$offer.beta.norm)) +
  geom_line(aes(x = dat$paroch, y = mus.p)) +
  geom_line(aes(x = dat$paroch, y = mus_lo.p), linetype = 2) +
  geom_line(aes(x = dat$paroch, y = mus_hi.p), linetype = 2) +
  labs(x = "Parochialism", y = "E(Prop of 40 cents offered)")

mod.stan.u.summary <- summary(mod.stan.u)$summary
mus.u <- mod.stan.u.summary[grepl("mu", rownames(mod.stan.u.summary)), "mean"]
mus_lo.u <- mod.stan.u.summary[grepl("mu", row.names(mod.stan.u.summary)), 4]
mus_hi.u <- mod.stan.u.summary[grepl("mu", row.names(mod.stan.u.summary)), 8]
ggplot() +
  geom_jitter(aes(x = dat$univer, y = dat$offer.beta.norm)) +
  geom_line(aes(x = dat$univer, y = mus.u)) +
  geom_line(aes(x = dat$univer, y = mus_lo.u), linetype = 2) +
  geom_line(aes(x = dat$univer, y = mus_hi.u), linetype = 2) +
  labs(x = "Universalism", y = "E(Prop of 40 cents offered)")

#### so let's retrace our steps

## first, we wanted to do a better job predicting the following:

ggplot(dat,aes(x=offer.beta.norm)) + geom_histogram(fill="grey60") + theme_xkcd()

# suppose we rethink the original offer.beta as givers and non-givers

table(dat$offer.beta)

library(car)
dat$offer.binary <- recode(dat$offer.beta,"0=0;else=1",F)
table(dat$offer.binary)

# and they used some weird distance measure to determine if people were paroch or univer.
# let's consider them to be of a class rather than a degree, shall we?

ggplot(dat,aes(x=paroch)) + geom_histogram(fill="grey60") + theme_xkcd()
ggplot(dat,aes(x=univer)) + geom_histogram(fill="grey60") + theme_xkcd()

dat$paroch.bin <- 0
dat$paroch.bin[dat$paroch > .5] <- 1

dat$univer.bin <- 0
dat$univer.bin[dat$univer > .5] <- 1

table(dat$paroch.bin,dat$univer.bin)

dat$universalists <- NA
dat$universalists[dat$paroch.bin == 0 & dat$univer.bin == 1] <- 1
dat$universalists[dat$paroch.bin == 1 & dat$univer.bin == 0] <- 0

table(dat$universalists)
table(dat$universalists,dat$offer.binary)

## consider this problem now from a much simpler standpoint:

model_binGivers <- "
data {
  // Number of data points
  int n1;
  int n2;
  // Number of successes
  int y1[n1];
  int y2[n2];
}

parameters {
  real<lower=0, upper=1> UniGive;
  real<lower=0, upper=1> ParGive;
}

model {  
  UniGive ~ beta(.5, .5);
  ParGive ~ beta(.5, .5);
  y1 ~ bernoulli(UniGive);
  y2 ~ bernoulli(ParGive); 
}

generated quantities {
}
"

UniverGivers <- c(rep(1,1628),rep(0,1041))
ParochGivers <- c(rep(1,423),rep(0,534))
data_aslist <- list(y1 = UniverGivers, y2 = ParochGivers, n1 = length(UniverGivers), n2 = length(ParochGivers))

# Compiling and producing posterior samples from the model.
stan_runME <- stan(model_code = model_binGivers, data = data_aslist)

# Plotting and summarizing the posterior distribution
stan_runME
plot(stan_runME)

s.obj <- as.data.frame(stan_runME)
head(s.obj)

mean(s.obj$UniGive - s.obj$ParGive)*.4
hist((s.obj$UniGive - s.obj$ParGive)*.4, main="Universalists Giving vs. Parochialists Giving", xlab="Difference between the groups (Univ - Paroch)")


################### DON"T PAY MUCH ATTENTION TO THIS STUFF ----------------

#### Mclust approach
### https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html
library(mclust)

mclust.dat <- dat[,c(16:18)]
clPairs(mclust.dat)

## BIC model
BIC <- mclustBIC(mclust.dat)
plot(BIC)
summary(BIC)
mod1 <- Mclust(mclust.dat,x=BIC)
summary(mod1, parameters=T)
names(dat)
dat$pred.mclust <- mod1$classification
table(dat$pred.class,dat$pred.mclust) # compare clustuers to LCA
plot(mod1, what="uncertainty")
plot(mod1, what="classification")

## density estimation
mod2 <- densityMclust(mclust.dat)
summary(mod2)
plot(mod2, what = "BIC")
plot(mod2, what = "density", type= "hdr", data = mclust.dat, points.cex = 0.5)
plot(mod2, what = "density", type="persp")

## bootstrapped estimates
boot1 <- MclustBootstrap(mod2, nboot = 999, type = "bs")
summary(boot1, what = "se")

##### The best plot for the clusters
par(mfrow = c(4,9))
plot(boot1, what = "pro")
plot(boot1, what = "mean")
par(mfrow = c(1,1))

## dimenion reduction clustering...
mod1dr <- MclustDR(mod2)
summary(mod1dr)
plot(mod1dr, what = "pairs")

##### nested model comparison
dat$pred.mclust.f <- as.factor(dat$pred.mclust)
mclust.lm <- lm(dgOffer~pred.mclust.f, data=dat)
summary(mclust.lm)
anova(lca.lm, mclust.lm)
## given non-sig difference, I will stick with the lca results for parsimony


## testing
