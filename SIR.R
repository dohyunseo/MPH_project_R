#SIR deterministic
#Parameters: beta (transmission rate), gamma (recovery rate)
#State variables: S, I, R (compartments)

library(deSolve)
library(ggplot2)
library(data.table)

sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / Nsize
    dI <- beta * S * I / Nsize - gamma * I
    dR <- gamma * I

    return(list(c(dS, dI, dR)))
  })
}

#Intial conditions (values of S,I,R) as proportions: TO COMPLETE
Nsize=67000000
I0=100/Nsize
init <- c(S = Nsize - I0*Nsize, I = I0*Nsize, R = 0)

#Parameter values: TO COMPLETE
parameters <- c(beta = 0.15, gamma = 1/10)
times <- seq(0, 500, by = 1)

out <- as.data.table(ode(y = init, times = times, func = sir, parms = parameters))

out_l = melt(out, id.vars = "time", variable.name = "compartment", value.name = "value")
ggplot(out_l, aes(x = time)) + 
  geom_line(aes(y = value, color = compartment)) + 
  labs(x = "Time", y = "Number of S,I,R", title = "Deterministic SIR Model") + 
  theme_bw(14)

Nsize=1
I0=100/67000000
init <- c(S = 1 - I0, I = I0, R = 0)

out_prop <- as.data.table(ode(y = init, times = times, func = sir, parms = parameters))
out_prop_l = melt(out_prop, id.vars = "time", variable.name = "compartment", value.name = "value")
ggplot(out_prop_l, aes(x = time)) + 
  geom_line(aes(y = value, color = compartment)) + 
  labs(x = "Time", y = "Number of S,I,R", title = "Deterministic SIR Model") + 
  theme_bw(14)
