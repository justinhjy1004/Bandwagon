#===============================================================================
# MODEL PARAMETERS
#===============================================================================
alpha   <- beta <- 1/2
R       <- 100                      # Number of Roots
F       <- 1000                     # Number of Firms
K0      <- 1                        # Initial Firm Capital
delta   <- 0.2                     # Depreciation
A       <- 1                        # Average TFP
s       <- 0.1                      # Percent of patents useful for future knowledge
w       <- 1                        # Wages
eta     <- 0.1                      # Spillover effect
Epi     <- 1                        # Expected profit per patent
tau     <- .2                       # Difficulty of Transferrability
fomo    <- .5                       # FOMO parameter
T       <- 10                       # Number of Generations in Sim
nruns   <- 100                      # Number of Monte Carlo Runs
set.seed(12345)

#===============================================================================
# MODEL FUNCTIONS
#===============================================================================
labor_demanded <- function (K) ((Epi * beta * A * K^alpha)/w)^(1/(1-beta))
production <- function(A, K, L) A * K[firm, t]^alpha * L^beta
cal_profit <- function(q, L) Epi*q - w*L

ring_distance <- function(a,b,r) min(abs(a-b), r - abs(a-b))

#===============================================================================
# INITIALIZE MATRICES
#===============================================================================
Q     <- matrix(0, nrow=F, ncol=T)
K     <- matrix(K0, nrow=F, ncol=T+1)
Labor <- matrix(1, nrow=F, ncol=T)
Roots <- matrix(0, nrow=F, ncol=T+1)
Profit <- matrix(0, nrow=F, ncol=T)

PiRoot <- matrix(0, nrow=R, ncol=T)
QRoot  <- matrix(0, nrow=R, ncol=T)

#Roots[,1] <- sample(1:R, F, replace=TRUE)
prob_weights <- rexp(R, rate=1)
Roots[,1] <- sample(1:R, F, replace=TRUE, prob=prob_weights/sum(prob_weights))

D <- matrix(0, nrow=R, ncol=R)
for (r in 1:R){
  D[r,] <- sapply(1:R, function(i) ring_distance(i,r,R))
}


#===============================================================================
# LOOP
#===============================================================================
for (t in 1:T){

  # Production
  for (firm in 1:F){
    Labor[firm, t] <- labor_demanded(K[firm,t])
    Q[firm, t] <- A * K[firm, t]^alpha * Labor[firm,t]^beta
    Profit[firm, t] <- cal_profit( Q[firm,t], Labor[firm,t] )
  }

  # Calculate Root Profit
  PiRoot[, t] <- sapply(1:R, function(r){
    pi <- sum(Profit[which(Roots[,t] == r), t])/length(which(Roots[,t] == r))
    if (is.na(pi) | is.infinite(pi)) return(0)
    return(pi)
  })
  QRoot[, t] <- sapply(1:R, function(r){
    sum(Q[which(Roots[,t] == r), t])
  })

  # Change in capital
  for (firm in 1:F){
    K[firm, t+1] <- (1-delta)*K[firm, t] + (1-eta)*s*Q[firm, t] + eta*s*QRoot[ Roots[firm,t], t]
  }

  for (firm in 1:F){
    # Discounted capital for switching roots
    K_discounted <- K[firm,t+1] / (exp(tau*D[Roots[firm,t],]))
    L_discounted <- labor_demanded(K_discounted)
    Q_discounted <- (A*(K_discounted^alpha)*(L_discounted^beta))

    profit_difference <- (1-fomo)*cal_profit(Q_discounted, L_discounted) + (fomo)*PiRoot[,t]
    best_option <- max(profit_difference)
    best_root <- which(profit_difference == best_option)

    if (length(best_root) != 1) {
      best_root <- sample(best_root,1)
    }

    if (best_root != Roots[firm,t]){
      Roots[firm,t+1] <- best_root
      K[firm, t+1]    <- K_discounted[best_root]
    } else {
      Roots[firm,t+1] <- Roots[firm,t]
    }
  }

}

#===============================================================================
# Regression Time
#===============================================================================
dat_K <- Reduce(rbind, lapply(1:T, function(t){
  data.frame(
    time = t,
    K = K[, t],
    Firm = 1:F
  )
}))

dat_L <- Reduce(rbind, lapply(1:T, function(t){
  data.frame(
    time = t,
    L = Labor[, t],
    Firm = 1:F
  )
}))

dat_Q <- Reduce(rbind, lapply(1:T, function(t){
  data.frame(
    time = t,
    Q = Q[, t],
    Firm = 1:F
  )
}))

dat_Root <- Reduce(rbind, lapply(1:T, function(t){
  data.frame(
    time = t,
    Root = Roots[, t],
    Firm = 1:F
  )
}))

library(dplyr)
left_join(dat_L, dat_K, by=c("time", "Firm")) %>%
  left_join(dat_Q, by=c("time", "Firm")) %>%
  left_join(dat_Root, by=c("time", "Firm")) -> dat

dat %>%
  group_by(Firm) %>%
  mutate(
    lagL = lag(L),
    lagK = lag(K)
  )->
  dat_firm

dat %>%
  group_by(Root, time) %>%
  summarize(
    K = sum(K),
    L = sum(L),
    Q = sum(Q)
  ) ->
  dat_root

dat %>%
  mutate(total = 1) %>%
  group_by(Root, time) %>%
  summarize(A = sum(total)) %>%
  left_join(dat_root, by = c("Root", "time")) %>%
  mutate(
    lagK = lag(K),
    lagL = lag(L),
    lagA = lag(A)
  ) ->
  dat_root

dat_root_drs <- dat_root %>%
  mutate(KpA = K/A) %>%
  filter(KpA <= 1)

dat_root_irs <- dat_root %>%
  mutate(KpA = K/A) %>%
  filter(KpA > 1)

summary(lm(log(Q) ~ log(A) + factor(Root), data=dat_root))$coefficient[2]

summary(lm(log(Q) ~ log(A) + factor(Root), data=dat_root_drs))$coefficient[2]

summary(lm(log(Q) ~ log(A) + factor(Root), data=dat_root_irs))$coefficient[2]

