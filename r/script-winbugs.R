#### Required packages ####

## devtools::install_github("fisabio/pbugs")
## remotes::install_github("fisabio/pbugs")
## install.packages("pacman")

pacman::p_load(foreign, readxl, faraway, R2WinBUGS, pbugs, spdep, ggplot2, 
               RColorBrewer, graphics, sp, ggpubr, leaflet, gridExtra,
               install = FALSE)

#### Figure 1: kappa[1:NCats] interpretation as cut points ####

x <- seq(from = -6.5, to = 6.5, length.out = 1000)
y <- dlogis(x)

df <- data.frame("x" = x, "y" = y)
lines <- data.frame("init" = c(x[300], x[425], x[575], x[700]),
                    "end" = c(y[300], y[425], y[575], y[700]))
p <- ggplot() + geom_line(data = df, mapping = aes(x = x, y = y)) + 
  geom_segment(data = lines, mapping = aes(x = init, y = 0, xend = init, yend = end)) +
  annotate("text", x = x[300], y = -0.01, label = expression(kappa[1]), size = 4) + 
  annotate("text", x = x[425], y = -0.01, label = expression(kappa[2]), size = 4) + 
  annotate("text", x = x[575], y = -0.01, label = expression(kappa[3]), size = 4) + 
  annotate("text", x = x[700], y = -0.01, label = expression(kappa[4]), size = 4) +
  
  annotate("text", x = x[250], y = 0.01, label = expression(pi[1]), size = 4) + 
  annotate("text", x = x[360], y = 0.05, label = expression(pi[2]), size = 4) + 
  annotate("text", x = x[500], y = 0.10, label = expression(pi[3]), size = 4) + 
  annotate("text", x = x[640], y = 0.05, label = expression(pi[4]), size = 4) + 
  annotate("text", x = x[755], y = 0.01, label = expression(pi[5]), size = 4) + 
  coord_cartesian(ylim = c(-0.0075, 0.25)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-7.5, 7.5, by = 1.5)) +
  labs(y = "Density")

# Warnings: do not pay attention
p

# # Saving the plot
# cutpoints <- p
# ggsave("cutpoints.eps", device = cairo_ps, fallback_resolution = 600,
#        path = file.path("images"), width = 15.38, height = 10, units = "cm")

### alpha[h] positive effect ###

df <- data.frame("x" = x, "y" = y)
effect <- 20
lines <- data.frame("initgrey" = c(x[300], x[425], x[575], x[700]),
                    "endgrey" = c(y[300], y[425], y[575], y[700]),
                    "initcol" = c(x[300 + effect], x[425 + effect], x[575 + effect], x[700 + effect]),
                    "endcol" = c(y[300 + effect], y[425 + effect], y[575 + effect], y[700 + effect]))
p <- ggplot() + geom_line(data = df, mapping = aes(x = x, y = y)) + 
  geom_segment(data = lines, mapping = aes(x = initgrey, y = 0, xend = initgrey, yend = endgrey),
               col = "gray62", lty = 2, lwd = 0.50) +
  
  # Effect
  geom_segment(data = lines, mapping = aes(x = initcol, y = 0, xend = initcol, yend = endcol), col = "red") +
  annotate("text", x = x[300 + effect], y = -0.01, label = expression(kappa[1] + alpha[h]), size = 4, col = "red") + 
  annotate("text", x = x[425 + effect], y = -0.01, label = expression(kappa[2] + alpha[h]), size = 4, col = "red") + 
  annotate("text", x = x[575 + effect], y = -0.01, label = expression(kappa[3] + alpha[h]), size = 4, col = "red") + 
  annotate("text", x = x[700 + effect], y = -0.01, label = expression(kappa[4] + alpha[h]), size = 4, col = "red") +
  
  annotate("text", x = x[250 + effect], y = 0.01, label = expression(pi[1]), size = 4, col = "red") + 
  annotate("text", x = x[360 + effect], y = 0.05, label = expression(pi[2]), size = 4, col = "red") + 
  annotate("text", x = x[500 + effect], y = 0.10, label = expression(pi[3]), size = 4, col = "red") + 
  annotate("text", x = x[640 + effect], y = 0.05, label = expression(pi[4]), size = 4, col = "red") + 
  annotate("text", x = x[755 + effect], y = 0.01, label = expression(pi[5]), size = 4, col = "red") +
  coord_cartesian(ylim = c(-0.0075, 0.25)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-7.5, 7.5, by = 1.5)) +
  labs(y = "Density")

# Warnings: do not pay attention
p

# # Saving the plot
# cutpointseffectpos <- p
# ggsave("cutpointseffectpos.eps", device = cairo_ps, fallback_resolution = 600, 
#        path = file.path("images"), width = 15.38, height = 10, units = "cm")

### alpha[h] negative effect ###

df <- data.frame("x" = x, "y" = y)
effect <- -20
lines <- data.frame("initgrey" = c(x[300], x[425], x[575], x[700]),
                    "endgrey" = c(y[300], y[425], y[575], y[700]),
                    "initcol" = c(x[300 + effect], x[425 + effect], x[575 + effect], x[700 + effect]),
                    "endcol" = c(y[300 + effect], y[425 + effect], y[575 + effect], y[700 + effect]))
p <- ggplot() + geom_line(data = df, mapping = aes(x = x, y = y)) + 
  geom_segment(data = lines, mapping = aes(x = initgrey, y = 0, xend = initgrey, yend = endgrey),
               col = "gray62", lty = 2, lwd = 0.50) +
  
  # Effect
  geom_segment(data = lines, mapping = aes(x = initcol, y = 0, xend = initcol, yend = endcol), col = "blue") +
  annotate("text", x = x[300 + effect], y = -0.01, label = expression(kappa[1] + alpha[h]), size = 4, col = "blue") + 
  annotate("text", x = x[425 + effect], y = -0.01, label = expression(kappa[2] + alpha[h]), size = 4, col = "blue") + 
  annotate("text", x = x[575 + effect], y = -0.01, label = expression(kappa[3] + alpha[h]), size = 4, col = "blue") + 
  annotate("text", x = x[700 + effect], y = -0.01, label = expression(kappa[4] + alpha[h]), size = 4, col = "blue") +
  
  annotate("text", x = x[250 + effect], y = 0.01, label = expression(pi[1]), size = 4, col = "blue") + 
  annotate("text", x = x[360 + effect], y = 0.05, label = expression(pi[2]), size = 4, col = "blue") + 
  annotate("text", x = x[500 + effect], y = 0.10, label = expression(pi[3]), size = 4, col = "blue") + 
  annotate("text", x = x[640 + effect], y = 0.05, label = expression(pi[4]), size = 4, col = "blue") + 
  annotate("text", x = x[755 + effect], y = 0.01, label = expression(pi[5]), size = 4, col = "blue") +
  coord_cartesian(ylim = c(-0.0075, 0.25)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-7.5, 7.5, by = 1.5)) +
  labs(y = "Density")

# Warnings: do not pay attention
p

# # Saving the plot
# cutpointseffectneg <- p
# ggsave("cutpointseffectneg.eps", device = cairo_ps, fallback_resolution = 600,
#        path = file.path("images"), width = 15.38, height = 10, units = "cm")

#### Data loading ####

rm(list = ls())
# Health Survey of the Region of Valencia (Spain) for the year 2016 (HSRV2016)
HSRV2016 <- read.spss(file.path("data", "ESCV2016_Adultos.sav"), 
                      use.value.labels = TRUE, to.data.frame = TRUE)

# Response variable: self-perceived health
levels(HSRV2016$P1) <- c("Very good", "Good", "Regular", "Bad", "Very bad")
y <- as.numeric(HSRV2016$P1)
# Number of categories
NCats <- length(table(y))
# Number of respondents
NResp <- length(y)

# Covariate sex: 1 = Male; 2 = Female
sexC <- HSRV2016$gSexo
levels(sexC) <- c("Male", "Female")
sex <- as.numeric(sexC)

# Covariate age group: 1 = [15,45); 2 = [45,65); 3 = [65,75); 4 = [75,85); 5 = [85,...)
ageC <- cut(HSRV2016$edad, breaks = c(15, 45, 65, 75, 85, 107), 
            include.lowest = TRUE, right = FALSE)
levels(ageC)[length(table(ageC))] <- "[85,...)"
age <- as.numeric(ageC)

# Number of respondents by sex and age group
table(sexC, ageC)

# Covariate dwelling stratum: 
# 1 = Dwellings with 1 minor (without residents over 74)
# 2 = Dwellings with 2 or more minors (without residents over 74)
# 3 = Dwellings with residents over 74
# 4 = Other dwellings
dwell <- as.numeric(HSRV2016$estrato)

# Number of levels of each (categorical) covariate
NSex <- length(table(sex))
NAges <- length(table(age))
NDwells <- length(table(dwell))

# Loading the population size of each municipality by sex and age group
# Source: https://www.ine.es/dynt3/inebase/index.htm?padre=6225
population <- readRDS(file = file.path("data", "population.rds"))

### R objects to be used in the WinBUGS model ###

# index.groupSA contains the number of respondents sorted by sex and age group
index.groupSA <- c(table(sex, age)[1, ], table(sex, age)[2, ])
index.groupSA <- as.vector(c(0, cumsum(index.groupSA)))

# groupSA contains the respondents sorted by sex and age group
groupSA <- c()
for (SexGroup in 1:NSex) {
  for (AgeGroup in 1:NAges) {
    groupSA <- c(groupSA, which(sex == SexGroup & age == AgeGroup))
  }
}
# Number of all sex-age group combinations
NGroupSA <- prod(dim(table(sex, age)))

# index.groupD contains the number of respondents sorted by dwelling strata
index.groupD <- as.numeric(table(dwell))
index.groupD <- as.vector(c(0, cumsum(index.groupD)))

# groupD contains the respondents sorted by dwelling strata
groupD <- c()
for (Dwell in 1:NDwells) {
  groupD <- c(groupD, which(dwell == Dwell))
}

### Preparation of maps: Option 1 (Option 2 below) ###

load(file.path("data", "cartography.RData"))

plot_map_neig <- function(neig) {
  plot(carto_muni)
  plot(carto_muni[neig, ], border = "black", col = "red", add = TRUE)
  plot(carto_muni[cv.nb[[neig]], ], border = "black", col = "pink", 
       add = TRUE)
}

# # Valencia neighboring municipalities
# par(mar=c(c(2, 0, 2, 0) + 0.1))
# plot_map_neig(526)
# # Alicante neighboring municipalities
# plot_map_neig(14)
# # Elche neighboring municipalities
# plot_map_neig(65)
# # Castellón de la Plana neighboring municipalities
# plot_map_neig(177)
# # Ademuz neighboring municipalities
# plot_map_neig(277)

### Preparation of maps: Option 2 (do not run if you have run Option 1) ###

# Cartography of the Region of Valencia
load(file.path("data", "CartoCV.Rdata"))
# Cartography is sorted by municipality code
order(carto_muni$INE_MUN)-1:542
# Neighborhood structure by contiguity
cv.nb <- poly2nb(carto_muni)

# Some extra neighborhoods are added for Rincón de Ademuz comarca
cv.nb[[277]] <- as.integer(sort(c(cv.nb[[277]], 312, 317, 517, 523, 508)))
cv.nb[[363]] <- as.integer(sort(c(cv.nb[[363]], 312, 317, 517, 523, 508)))
cv.nb[[364]] <- as.integer(sort(c(cv.nb[[364]], 312, 317, 517, 523, 508)))
cv.nb[[477]] <- as.integer(sort(c(cv.nb[[477]], 312, 317, 517, 523, 508)))

cv.nb[[312]] <- as.integer(sort(c(cv.nb[[312]], 277, 363, 364, 477)))
cv.nb[[317]] <- as.integer(sort(c(cv.nb[[317]], 277, 363, 364, 477)))
cv.nb[[517]] <- as.integer(sort(c(cv.nb[[517]], 277, 363, 364, 477)))
cv.nb[[523]] <- as.integer(sort(c(cv.nb[[523]], 277, 363, 364, 477)))
cv.nb[[508]] <- as.integer(sort(c(cv.nb[[508]], 277, 363, 364, 477)))

# Municipality codes
INE_MUN <- as.numeric(as.character(carto_muni@data$INE_MUN))
# Municipality of each respondent
muni <- HSRV2016$localidad
muni <- match(muni, INE_MUN)
# Number of (distinct) municipalities (542)
NMuni <- length(INE_MUN); rm(INE_MUN)

# Number of neighbors of each municipality
nadj <- card(cv.nb)
# Neighbors of each municipality
map <- unlist(cv.nb)
# Sum of all the neighbor numbers of all municipalities
nadj.tot <- length(map)
# Cumulative sums of the number of neighbors of each municipality
index <- c(0, cumsum(nadj))

#### A case study ####

# Code to be able to reproduce the results shown in the paper.

### Model code ###

model <- function() {
  
  # Likelihood
  for (Resp in 1:NResp) {
    y[Resp] ~ dcat(prlevels[Resp, 1:NCats])
    
    # Definition of the probabilities of each category as a function of the
    # cumulative probabilities
    prlevels[Resp, 1] <- p.gamma[Resp, 1]
    for (Cat in 2:(NCats-1)) {
      prlevels[Resp, Cat] <- p.gamma[Resp, Cat] - p.gamma[Resp, Cat-1]
    }
    prlevels[Resp, NCats] <- 1 - p.gamma[Resp, NCats-1]
    
    # Linear predictor
    for (Cat in 1:(NCats-1)) {
      logit(p.gamma[Resp, Cat]) <- kappa[sex[Resp], age[Resp], Cat] + 
        alpha[dwell[Resp]] + sd.theta * theta[muni[Resp]]
    }
  }
  
  # Prior distributions
  
  # kappa[1:NSex, 1:NAges, 1:NCats] cut points
  # Stick-breaking process
  for (SexGroup in 1:NSex) {
    for (AgeGroup in 1:NAges) {
      for (Cat in 1:(NCats-1)) {
        kappa[SexGroup, AgeGroup, Cat] <- logit(sum(delta[SexGroup, AgeGroup, 1:Cat]))
        omega[SexGroup, AgeGroup, Cat] ~ dbeta(1, aux[Cat])
      }
    }
  }
  
  for (Cat in 1:(NCats-1)) {
    aux[Cat] <- NCats - Cat
  }
  
  # Definition of delta[1:NSex, 1:NAges, 1:NCats] in logarithmic scale
  for (SexGroup in 1:NSex) {
    for (AgeGroup in 1:NAges) {
      # First piece of stick
      delta[SexGroup, AgeGroup, 1] <- omega[SexGroup, AgeGroup, 1]
      # More pieces of stick
      for (Cat in 2:(NCats-1)) {
        delta[SexGroup, AgeGroup, Cat] <- exp(sum(vect[SexGroup, AgeGroup, 1:(Cat-1)]) + log(omega[SexGroup, AgeGroup, Cat]))
      }
      # Last piece of stick
      delta[SexGroup, AgeGroup, NCats] <- exp(sum(vect[SexGroup, AgeGroup, 1:(NCats-1)]))
      
      # Auxiliary vector
      for (Cat in 1:(NCats-1)) {
        vect[SexGroup, AgeGroup, Cat] <- log(1 - omega[SexGroup, AgeGroup, Cat])
      }
    }
  }
  
  # alpha[1:NDwells] dwelling fixed effects (zero-sum constraint)
  alpha[1] <- -sum(alpha[2:NDwells])
  for (Dwell in 2:NDwells) {
    alpha[Dwell] ~ dflat()
  }
  
  # theta[1:NMuni] spatial random effect
  # LCAR distribution
  for (Muni in 1:NMuni) {
    theta[Muni] ~ dnorm(mean.theta[Muni], prec.theta[Muni])
    prec.theta[Muni] <- (1 - lambda + lambda * nadj[Muni]) # / (sd.theta * sd.theta)
    mean.theta[Muni] <- (lambda / (1 - lambda + lambda * nadj[Muni])) *
      sum(theta.map[(index[Muni] + 1):index[Muni + 1]])
  }
  
  for (NAdj in 1:nadj.tot) {
    theta.map[NAdj] <- theta[map[NAdj]]
  }
  
  # Hyperparameters of the spatial random effect
  lambda ~ dunif(0, 1)
  sd.theta ~ dunif(0, 10)
  
  # Zero-mean constraint for theta[1:NMuni]
  zero.theta ~ dnorm(mean.thetas, 10000)
  mean.thetas <- mean(theta[1:NMuni])
  
  # Stochastic restrictions in order to avoid (possible) spatial confounding problems
  # Required vectors
  for (Resp in 1:NResp) {
    theta.Resp[Resp] <- theta[muni[Resp]]
    theta.Resp.groupSA[Resp] <- theta.Resp[groupSA[Resp]]
    theta.Resp.groupD[Resp] <- theta.Resp[groupD[Resp]]
  }
  # Constraint for theta[1:NMuni] - (sex, age group) covariates
  for (GroupSA in 1:NGroupSA) {
    zero.theta.groupSA[GroupSA] ~ dnorm(mean.thetas.groupSA[GroupSA], 10000)
    mean.thetas.groupSA[GroupSA] <- mean(theta.Resp.groupSA[(index.groupSA[GroupSA] + 1):index.groupSA[GroupSA + 1]])
  }
  # Constraint for theta[1:NMuni] - dwelling stratum covariate
  for (Dwell in 1:NDwells) {
    zero.theta.groupD[Dwell] ~ dnorm(mean.thetas.groupD[Dwell], 10000)
    mean.thetas.groupD[Dwell] <- mean(theta.Resp.groupD[(index.groupD[Dwell] + 1):index.groupD[Dwell + 1]])
  }
  
}

### Initial values funtion ### 

inits <- function() {
  kk = rnorm(NMuni)
  kk = kk - mean(kk)
  list(omega = array(c(rbeta(NSex * NAges, 1, NCats - 1), 
                       rbeta(NSex * NAges, 1, NCats - 2), 
                       rbeta(NSex * NAges, 1, NCats - 3), 
                       rbeta(NSex * NAges, 1, NCats - 4)), 
                     dim = c(NSex, NAges, NCats - 1)),
       alpha = c(NA, runif(NDwells - 1)),
       theta = kk, lambda = runif(1), sd.theta = runif(1))
}

### Data to be loaded ###

data <- list(y = y, NResp = NResp, NCats = NCats, sex = sex, age = age, dwell = dwell, 
             NSex = NSex, NAges = NAges, NDwells = NDwells, 
             NGroupSA = NGroupSA, groupSA = groupSA, groupD = groupD,
             index.groupSA = index.groupSA, index.groupD = index.groupD,
             muni = muni, NMuni = NMuni, nadj = nadj, 
             nadj.tot = nadj.tot, index = index, map = map, 
             zero.theta = 0
             , zero.theta.groupSA = rep(0, NGroupSA), zero.theta.groupD = rep(0, NDwells)
)

### Parameters to be saved ###

parameters <- c("kappa", "alpha", "theta", "sd.theta", "lambda")

### WinBUGS call (working.directory path should be changed) ###

salwinbugs <- pbugs(model = model, data = data, inits = inits, parameters.to.save = parameters, 
                    n.chains = 5, n.iter = 8000, n.burnin = 500, n.thin = 15,
                    #working.directory = "~/.wine/drive_c/temp/Rtmp",
                    working.directory = "/home/beltran_mig/.wine/drive_c/users/beltran_mig/Temp/Area13/",
                    cluster = 5
                    #, debug = TRUE
                    , clearWD = TRUE
                    , DIC = FALSE
)

# saveRDS(salwinbugs, file = file.path("results", "a-case-study-winbugs.rds"))

#### Model results ####

salwinbugs <- readRDS(file.path("results", "a-case-study-winbugs.rds"))

#### Convergence assessment ####

salwinbugs$exec.time
summary(salwinbugs)

round(salwinbugs$summary, 4)[startsWith(labels(salwinbugs$summary[, 1]), "lambda"), ]

mean((salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "theta"), 8] > 1.10) | (salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "theta"), 9] < 100))
mean((salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "kappa"), 8] > 1.10) | (salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "kappa"), 9] < 100))
mean((salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "sd.theta"), 8] > 1.10) | (salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "sd.theta"), 9] < 100))
mean((salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "lambda"), 8] > 1.10) | (salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "lambda"), 9] < 100))
mean((salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "alpha"), 8] > 1.10) | (salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "alpha"), 9] < 100))

a <- labels(salwinbugs$summary[startsWith(labels(salwinbugs$summary[, 1]), "kappa"), 1])
traceplot(salwinbugs, var.names = a)
traceplot(salwinbugs, var.names = c("sd.theta", "lambda"))
traceplot(salwinbugs, var.names = c("theta[526]", "theta[14]", "theta[65]", "theta[177]"))

plot(density(salwinbugs$sims.array[1:500, 1, "lambda"]), ylim = c(0, 2.5))
lines(density(salwinbugs$sims.array[1:500, 2, "lambda"]), col = 2)
lines(density(salwinbugs$sims.array[1:500, 3, "lambda"]), col = 3)
lines(density(salwinbugs$sims.array[1:500, 4, "lambda"]), col = 4)
lines(density(salwinbugs$sims.array[1:500, 5, "lambda"]), col = 5)

plot(density(salwinbugs$sims.array[1:500, 1, "kappa[1,3,4]"]), ylim = c(0, 0.8))
lines(density(salwinbugs$sims.array[1:500, 2, "kappa[1,3,4]"]), col = 2)
lines(density(salwinbugs$sims.array[1:500, 3, "kappa[1,3,4]"]), col = 3)
lines(density(salwinbugs$sims.array[1:500, 4, "kappa[1,3,4]"]), col = 4)
lines(density(salwinbugs$sims.array[1:500, 5, "kappa[1,3,4]"]), col = 5)

plot(density(salwinbugs$sims.array[1:500, 1, "theta[526]"]), ylim = c(0, 4.5))
lines(density(salwinbugs$sims.array[1:500, 2, "theta[526]"]), col = 2)
lines(density(salwinbugs$sims.array[1:500, 3, "theta[526]"]), col = 3)
lines(density(salwinbugs$sims.array[1:500, 4, "theta[526]"]), col = 4)
lines(density(salwinbugs$sims.array[1:500, 5, "theta[526]"]), col = 5)

plot(density(salwinbugs$sims.list$lambda))

#### Supplementary Material: Dwelling stratum effect is not relevant ####

subtitles <- c("Dwellings with 1 minor (without residents over 74)",
               "Dwellings with 2+ (without residents over 74)",
               "Dwellings with residents over 74",
               "Other dwellings")

round(salwinbugs$summary, 4)[startsWith(labels(salwinbugs$summary[, 1]), "alpha"), ]
CIalpha <- apply(salwinbugs$sims.list$alpha, 2, quantile, probs = c(0.025, 0.975))

p <- list()
for (Dwell in 1:NDwells) {
  df <- data.frame("x" = density(salwinbugs$sims.list$alpha[, Dwell])$x, 
                   "y" = density(salwinbugs$sims.list$alpha[, Dwell])$y)
  p[[Dwell]] <- ggplot(df, aes(x = x, y = y)) + 
    geom_line() + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = CIalpha[, Dwell], linetype = "twodash", color = "red") +
    labs(title = subtitles[Dwell], x = "x", y = "Density") +
    theme_bw()
}

supplementary <- ggarrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 2, ncol = 2)
supplementary

# Saving the plot
ggsave("supplementary.eps", device = cairo_ps, fallback_resolution = 600,
       path = file.path("images"), width = 23, height = 16, units = "cm")

#### Figure 2: kappa[1:NSex, 1:NAges, 1:NCats] ####

round(salwinbugs$summary, 4)[startsWith(labels(salwinbugs$summary[, 1]), "kappa"), ]
kappamean <- salwinbugs$mean$kappa
kappaIC <- apply(salwinbugs$sims.list$kappa, 2:4, quantile, probs = c(0.025, 0.975))
x <- seq(from = -6.5, to = 6.5, length.out = 1000)

p <- list()
for (AgeGroup in 1:NAges) {
  AgeLevel <- levels(ageC)[AgeGroup]
  df <- data.frame("x" = x, 
                   "y" = dlogis(x))
  lines <- data.frame("intercepts" = as.numeric(kappamean[, AgeGroup, ]),
                      "Sex group" = rep(c("Male", "Female"), NCats - 1))
  ic <- data.frame("lower" = as.numeric(kappaIC[1, , AgeGroup, ]),
                   "upper" = as.numeric(kappaIC[2, , AgeGroup, ]),
                   "Sex group" = rep(c("Male", "Female"), NCats - 1))
  p[[AgeGroup]] <- ggplot() + 
    geom_line(data = df, mapping = aes(x = x, y = y)) + 
    geom_rect(data = ic, mapping = aes(xmin = lower, xmax = upper, ymin = -0.1, ymax = 0.30, 
                                       fill = Sex.group), 
              alpha = 0.25) +
    geom_vline(data = lines, 
               mapping = aes(xintercept = intercepts, 
                             linetype = Sex.group, 
                             color = Sex.group)) +
    scale_fill_manual(name = "95% CI", values = c("Male" = "blue", "Female" = "red")) +
    scale_color_manual(name = "Mean", values = c("Male" = "blue", "Female" = "red")) +
    scale_linetype_manual(name = "Mean", values = c("Male" = "dashed", "Female" = "twodash")) +
    labs(title = substitute(paste("Age group ", a), list(a = AgeLevel)), x = "x", y = "Density") +
    coord_cartesian(ylim = c(0, 0.25)) +
    #coord_cartesian(xlim = c(1.5, 5)) +
    theme_bw() +
    scale_x_continuous(breaks = seq(-7.5, 7.5, by = 1.5))
}

kappa <- ggarrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 2, ncol = 3, 
                common.legend = TRUE, legend = "bottom")
kappa

# # Saving the plot
# ggsave("kappa.eps", device = cairo_ps, fallback_resolution = 600,
#        path = file.path("images"), width = 24.62, height = 16, units = "cm")

table(y, sexC, ageC)

#### Figure 3 (a): Map of the RV - theta[1:NMuni] ####

# Total saved simulations
n.sims <- salwinbugs$n.sims
# Saved theta simulations
thetasim <- salwinbugs$sims.list$sd.theta * salwinbugs$sims.list$theta

# Discretization by nine equal-probability intervals
breaks <- c(min(apply(thetasim, 2, mean)) - 0.001, quantile(apply(thetasim, 2, mean), probs = seq(1/9, 8/9, length.out = 8)), max(apply(thetasim, 2, mean)))
breaks
breaks <- c(-0.60, round(breaks[2:9], 2), 0.48)
carto_muni@data$thetamean <- cut(apply(thetasim, 2, mean), breaks = breaks, include.lowest = FALSE, right = TRUE)

# We order the factor levels to match the colors appropriately
carto_muni@data$thetamean <- factor(carto_muni@data$thetamean, levels = 
                                      rev(levels(carto_muni@data$thetamean)))
levels(carto_muni@data$thetamean) <- c("Better", " ", "  ", "   ", "    ", 
                                               "     ", "      ", "       ", "Worse")

spplot(carto_muni,
       c("thetamean"),
       col.regions = colorRampPalette(brewer.pal(7,'BrBG'))(9)[9:1],
       cuts = 8,
       colorkey = list(key = list(labels = c("Better", " ", "  ", "   ", "    ", 
                                             "     ", "      ", "       ", "Worse")),
                       width = 1.5, cex = 1.5, height = 0.75),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

# Alternative: Legend with numerical values
carto_muni@data$thetamean <- cut(apply(thetasim, 2, mean), breaks = breaks, include.lowest = FALSE, right = TRUE)
# We order the factor levels to match the colors appropriately
carto_muni@data$thetamean <- factor(carto_muni@data$thetamean, levels = 
                                      rev(levels(carto_muni@data$thetamean)))
levels(carto_muni@data$thetamean) <- c("(0.17,0.48)", levels(carto_muni@data$thetamean)[2:9])

spplot(carto_muni,
       c("thetamean"),
       col.regions = colorRampPalette(brewer.pal(7,'BrBG'))(9)[9:1],
       cuts = 8,
       colorkey = list(key = list(labels = levels(carto_muni@data$thetamean)),
                       width = 1.5, cex = 1.5, height = 0.75),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

#### Figure 3 (b): Map of the RV - P(theta < 0 | y) ####

# Checking when theta is LESS than zero (worse self-perceived health)
stepsim <- matrix(nrow = n.sims, ncol = NMuni)
for (sim in 1:n.sims) {
  for (Muni in 1:NMuni) {
    stepsim[sim, Muni] <- ifelse(thetasim[sim, Muni] < 0, 1, 0)
  }
}

breaks <- c(-0.01, 0.05, 0.10, 0.20, 0.80, 0.90, 0.95, 1)
breaks
carto_muni@data$probmean <- cut(apply(stepsim, 2, mean), breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$probmean) <- c("[0,0.05]",
                                      levels(carto_muni@data$probmean)[2:6],
                                      "(0.95,1]")

spplot(carto_muni,
       c("probmean"),
       col.regions = colorRampPalette(brewer.pal(7,'RdYlGn'))(9)[c(9:7, 5, 3:1)],
       cuts = 8,
       colorkey = list(key = list(labels = levels(carto_muni@data$probmean)),
                       width = 1.5, cex = 1.5, height = 0.75),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

#### Interactive maps ####

# Does the municipality have any respondents?
carto_muni@data$index <- as.factor(as.numeric((1:542 %in% sort(unique(muni)))))
levels(carto_muni@data$index) <- c("No", "Yes")

# Number of respondents from each municipality
munif <- factor(muni, levels = 1:542)
carto_muni@data$Sampled <- table(munif)

# Municipalities without respondents
norepre <- which(!(1:542 %in% sort(unique(muni))))

labels1 <- sprintf("<strong> %s </strong> <br/>
   Municipality: %s <br/>
   Spatial effect: %s <br/>
   Respondents: %s",
                  1:NMuni,
                  carto_muni@data$NOMBRE_MUNI, 
                  carto_muni@data$thetamean,
                  carto_muni@data$Sampled) %>% lapply(htmltools::HTML)

labels2 <- sprintf("<strong> %s </strong> <br/>
   Municipality: %s <br/>
   Relevance: %s <br/>
   Respondents: %s",
                   1:NMuni,
                   carto_muni@data$NOMBRE_MUNI, 
                   carto_muni@data$probmean,
                   carto_muni@data$Sampled) %>% lapply(htmltools::HTML)

levels(carto_muni@data$thetamean) <- colorRampPalette(brewer.pal(7,'BrBG'))(9)[9:1]
levels(carto_muni@data$probmean) <- colorRampPalette(brewer.pal(7,'RdYlGn'))(9)[c(9:7, 5, 3:1)]

leaflet(carto_muni) %>%
  addTiles() %>%
  addPolygons(
    color = "black", fillColor = carto_muni@data$thetamean,
    fillOpacity = 0.65,
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "red",
      fillOpacity = 1,
      bringToFront = TRUE),
    label = labels1,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px", direction = "auto"
    )
  )

leaflet(carto_muni) %>%
  addTiles() %>%
  addPolygons(
    color = "black", fillColor = carto_muni@data$probmean,
    fillOpacity = 0.65,
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "red",
      fillOpacity = 1,
      bringToFront = TRUE),
    label = labels2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px", direction = "auto"
    )
  )

#### Figure 4: Maps of the RV - area population level percentages with post-stratification ####

# function1: 
# - (1) computes the n.sims simulated probabilities for each sex, age group and municipality
# - (2) post-stratifies these probabilities for each municipality and category

function1 <- function(salwinbugs) {
  
  n.sims <- salwinbugs$n.sims
  n.chains <- salwinbugs$n.chains
  p.gamma <- array(dim = c(n.sims, NMuni, NSex, NAges, NCats - 1))
  prlevels <- array(dim = c(n.sims, NMuni, NSex, NAges, NCats))
  prlevels.post <- array(dim = c(n.sims, NMuni, NCats))
  
  # Probabilities for each sex, age group and municipality
  for (sim in 1:n.sims) {
    for (SexGroup in 1:NSex) {
      for (AgeGroup in 1:NAges) {
        for (Muni in 1:NMuni) {
          for (Cat in 1:(NCats - 1)) {
            p.gamma[sim, Muni, SexGroup, AgeGroup, Cat] <- 
              ilogit(salwinbugs$sims.list$kappa[sim, SexGroup, AgeGroup, Cat] + 
                       salwinbugs$sims.list$sd.theta[sim] * salwinbugs$sims.list$theta[sim, Muni])
          }
          
          prlevels[sim, Muni, SexGroup, AgeGroup, 1] <- p.gamma[sim, Muni, SexGroup, AgeGroup, 1]
          prlevels[sim, Muni, SexGroup, AgeGroup, NCats] <- 1 - p.gamma[sim, Muni, SexGroup, AgeGroup, NCats - 1]
          
          for (Cat in 2:(NCats - 1)) {
            prlevels[sim, Muni, SexGroup, AgeGroup, Cat] <- 
              p.gamma[sim, Muni, SexGroup, AgeGroup, Cat] - p.gamma[sim, Muni, SexGroup, AgeGroup, Cat-1]
          }
        }
      }
    }
    
    # Post-stratification
    for (Muni in 1:NMuni) {
      for (Cat in 1:NCats) {
        prlevels.post[sim, Muni, Cat] <- sum(population[Muni, , ])^(-1) * sum(prlevels[sim, Muni, , , Cat] * population[Muni, , ])
      }
    }
    if (sim %in% c(1, seq(n.sims/n.chains, n.sims, n.sims/n.chains))) {
      cat(sim, "of", n.sims, "simulations", "\n")
    } else {}
  }
  
  return(list("prlevels" = prlevels, "prlevels.post" = prlevels.post))
}

# This may take a minute
result1 <- function1(salwinbugs)

### Post-stratified posterior means of the percentages for each municipality and category ###

percentagesmean <- apply(result1$prlevels.post, c(2, 3), mean) * 100

category <- percentagesmean[, 1]
breaks <- c(min(category) - 0.001, quantile(category, probs = seq(1/9, 8/9, length.out = 8)), max(category))
breaks
breaks <- c(14.16, round(breaks[2:9], 2), 31.53)
carto_muni@data$category1 <- cut(category, breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$category1) <- c("(14.16,17.23]", "(17.23,18.26]", "(18.26,19.10]",
                                       "(19.10,19.81]", "(19.81,20.81]", "(20.81,21.70]",
                                       "(21.70,22.87]", "(22.87,24.80]", "(24.80,31.53)")
category <- percentagesmean[, 2]
breaks <- c(min(category) - 0.001, quantile(category, probs = seq(1/9, 8/9, length.out = 8)), max(category))
breaks
breaks <- c(42.61, round(breaks[2:9], 2), 52.06)
carto_muni@data$category2 <- cut(category, breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$category2) <- c("(42.61,46.92]", "(46.92,47.93]", "(47.93,48.37]",
                                       "(48.37,48.96]", "(48.96,49.38]", "(49.38,49.74]",
                                       "(49.74,50.08]", "(50.08,50.45]", "(50.45,52.06)")
category <- percentagesmean[, 3]
breaks <- c(min(category) - 0.001, quantile(category, probs = seq(1/9, 8/9, length.out = 8)), max(category))
breaks
breaks <- c(14.42, round(breaks[2:9], 2), 29.40)
carto_muni@data$category3 <- cut(category, breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$category3) <- c("(14.42,18.37]", "(18.37,19.69]", "(19.69,20.58]",
                                       "(20.58,21.43]", "(21.43,22.37]", "(22.37,23.00]",
                                       "(23.00,23.77]", "(23.77,24.92]", "(24.92,29.40)")
category <- percentagesmean[, 4]
breaks <- c(min(category) - 0.001, quantile(category, probs = seq(1/9, 8/9, length.out = 8)), max(category))
breaks
breaks <- c(3.26, round(breaks[2:9], 2), 10.11)
carto_muni@data$category4 <- cut(category, breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$category4) <- c("(3.26,4.70]", "(4.70,5.29]", "(5.29,5.61]",
                                       "(5.61,6.00]", "(6.00,6.39]", "(6.39,6.67]",
                                       "(6.67,7.04]", "(7.04,7.66]", "(7.66,10.11)")
category <- percentagesmean[, 5]
breaks <- c(min(category) - 0.001, quantile(category, probs = seq(1/9, 8/9, length.out = 8)), max(category))
breaks
breaks <- c(1.18, round(breaks[2:9], 2), 3.93)
carto_muni@data$category5 <- cut(category, breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$category5) <- c("(1.18,1.78]", "(1.78,2.03]", "(2.03,2.17]",
                                       "(2.17,2.35]", "(2.35,2.51]", "(2.51,2.65]",
                                       "(2.65,2.80]", "(2.80,3.04]", "(3.04,3.93)")

### Mean age of each municipality ###

# Loading the population size of each municipality by sex and age group
# Source: https://www.ine.es/dynt3/inebase/index.htm?padre=6225
valencia <- read_excel(file.path("data", "todovalencia2016.xlsx"), col_names = TRUE)
castellon <- read_excel(file.path("data", "todocastellon2016.xlsx"), col_names = TRUE)
alicante <- read_excel(file.path("data", "todoalicante2016.xlsx"), col_names = TRUE)
region <- rbind(alicante, castellon, valencia)
rm(list = c("valencia", "alicante", "castellon"))

region <- region[order(region$Municipio), ]
region <- with(region, region[which(Sexo == "Ambos"), ])
region <- as.data.frame(region); region <- region[, -c(1, 2)]

# Average age five-year age group
age5 <- c(2, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97, 102) + 0.5
region <- data.frame(apply(region, 1, function(x) {x/sum(x)}))
# Mean age
agemean <- apply(region * age5, 2, sum)

breaks <- c(min(agemean) - 0.001, quantile(agemean, probs = seq(1/9, 8/9, length.out = 8)), max(agemean))
breaks
breaks <- c(36.38, round(breaks, 2)[2:9], 64.86)
carto_muni@data$agemean <- cut(agemean, breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$agemean) <- c("(36.38,40.95]", "(40.95,42.17]", "(42.17,43.18]",
                                     "(43.18,44.39]", "(44.39,45.67]", "(45.67,48.12]",
                                     "(48.12,50.51]", "(50.51,53.32]", "(53.32,64.86)")

### Plots ###

spplot(carto_muni,
       c("category1"),
       main = "Very good",
       col.regions = colorRampPalette(brewer.pal(7,'YlOrBr'))(9),
       cuts = 8,
       colorkey = list(key = list(labels = levels(carto_muni@data$category1)),
                       width = 1.75, cex = 1.5, height = 0.5),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

spplot(carto_muni,
       c("category2"),
       main = "Good",
       col.regions = colorRampPalette(brewer.pal(7,'YlOrBr'))(9),
       cuts = 8,
       colorkey = list(key = list(labels = levels(carto_muni@data$category2)),
                       width = 1.75, cex = 1.5, height = 0.5),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

spplot(carto_muni,
       c("category3"),
       main = "Regular",
       col.regions = colorRampPalette(brewer.pal(7,'YlOrBr'))(9),
       cuts = 8,
       colorkey = list(key = list(labels = levels(carto_muni@data$category3)),
                       width = 1.75, cex = 1.5, height = 0.5),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

spplot(carto_muni,
       c("category4"),
       main = "Bad",
       col.regions = colorRampPalette(brewer.pal(7,'YlOrBr'))(9),
       cuts = 8,
       colorkey = list(key = list(labels = levels(carto_muni@data$category4)),
                       width = 1.75, cex = 1.5, height = 0.5),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

spplot(carto_muni,
       c("category5"),
       main = "Very bad",
       col.regions = colorRampPalette(brewer.pal(7,'YlOrBr'))(9),
       cuts = 8,
       colorkey = list(key = list(labels = levels(carto_muni@data$category5)),
                       width = 1.75, cex = 1.5, height = 0.5),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

spplot(carto_muni,
       c("agemean"),
       main = "Mean age",
       col.regions = colorRampPalette(brewer.pal(7,'GnBu'))(9),
       cuts = 8,
       colorkey = list(key = list(labels = levels(carto_muni@data$agemean)),
                       width = 1.75, cex = 1.5, height = 0.5),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

#### Table 1: Model assessment ####

# Sample size of each municipality, sex and age group:
sample <- array(dim = c(NMuni, NSex, NAges))
for (Muni in 1:NMuni) {
  for (SexGroup in 1:NSex) {
    for (AgeGroup in 1:NAges) {
      sample[Muni, SexGroup, AgeGroup] <- sum(muni == Muni & sex == SexGroup & age == AgeGroup)
    }
  }
}

# function2: 
# - (1) Input: Estimates of the probabilities for each sex, age group and municipality 
# from function1; that is, result1$prlevels.
# - (2) We simulate values of the response variable for each sex-age group-municipality 
# combination from the corresponding posterior predictive distributions.
# - (3) The estimated number of individuals in each category and municipality is the 
# result of adding all the previous values.
# - (4) We compute posterior mean, 95% prediction intervals and observed value of the 
# percentage of respondents in each category and municipality

function2 <- function(prlevels, Muni) {
  
  predictive <- array(dim = c(n.sims, NSex, NAges, NCats))
  for (sim in 1:n.sims) {
    for (SexGroup in 1:NSex) {
      for (AgeGroup in 1:NAges) {
        predictive[sim, SexGroup, AgeGroup, ] <- as.numeric(
          rmultinom(n = 1, 
                    size = sum(sample[Muni, SexGroup, AgeGroup]) - sum(is.na(y[muni == Muni & sex == SexGroup & age == AgeGroup])), 
                    prob = prlevels[sim, Muni, SexGroup, AgeGroup, ]))
      }
    }
  }
  predictive.muni <- matrix(nrow = n.sims, ncol = NCats)
  predictive.muni <- apply(predictive, c(1, 4), sum)/(sum(sample[Muni, , ]) - sum(is.na(y[muni == Muni]))) * 100
  
  posteriormean <- round(apply(predictive.muni, 2, mean), 2)
  PInterval0.025 <- round(apply(predictive.muni, 2, quantile, prob = 0.025), 2)
  PInterval0.975 <- round(apply(predictive.muni, 2, quantile, prob = 0.975), 2)
  realvalue <- round(table(factor(y[muni == Muni], levels = 1:NCats))/(sum(sample[Muni, , ]) - sum(is.na(y[muni == Muni]))) * 100, 2)
  
  return(list("mean" = posteriormean,
              "PI" = list("lower" = PInterval0.025, "upper" = PInterval0.975),
              "real" = realvalue))
}

result2 <- function2(prlevels = result1$prlevels, Muni = 526)
result2$mean
result2$PI
result2$real

# Four municipalities with the largest population in the RV
Munis <- order(apply(population, 1, sum), decreasing = TRUE)[1:4]

result2 <- list()
for (Muni in 1:length(Munis)) {
  set.seed(3687241)
  result2[[Muni]] <- function2(prlevels = result1$prlevels, Muni = Munis[Muni])
}

# Saving the table
saveRDS(result2, file = file.path("results", "model-assessment-winbugs.rds"))
