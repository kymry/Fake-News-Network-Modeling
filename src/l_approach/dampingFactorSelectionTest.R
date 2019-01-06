
x.seq.max <- 10
x <- seq(0, x.seq.max, by = .1)

# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = 1.5, sd = 5); y <- y*10


df = data.frame(x, y)
ggplot(data = df) +
    aes(x = x, y = y) +
    geom_line(color = '#0c4c8a') +
    theme_minimal()

## That's the shape i want to follow

# Now I want to scale it to tmax, where tmax = 48, give or take
tmax = 48

x <-x*tmax/x.seq.max # Scale x...
y <- y/max(y)# Scale y....

df = data.frame(x, y)
ggplot(data = df) +
    aes(x = x, y = y) +
    geom_line(color = '#0c4c8a') +
    theme_minimal()

# Got it