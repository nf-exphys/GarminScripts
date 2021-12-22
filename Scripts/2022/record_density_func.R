records <- list.files(path = "./Data/processed_fit/record/", pattern = "2021_", full.names = TRUE)

#425 was continuous tempo
#453 was cooldown today
#452 was 15 and 30 sec hill repeats with walking between
#445 was hills with jog between
#6 is 8 x 40 sec repeats

data <- read_csv(records[[425]])

plot <- ggplot(data = data) + geom_density(aes(x = speed)) 
plot
plot + geom_vline(xintercept = mean(data$speed), color = "blue") + 
  geom_vline(xintercept = median(data$heart_rate), color = "red") + 
  geom_vline(xintercept = fivenum(data$heart_rate)[2], color = "green") + 
  geom_vline(xintercept = fivenum(data$heart_rate)[4], color = "purple")

plot + geom_vline(xintercept = mean(data$heart_rate) + 2*sd(data$heart_rate)) + 
  geom_vline(xintercept = mean(data$heart_rate) - 2*sd(data$heart_rate))

#If data isn't normally distributed, would it be better to use median +/- IQR?
upper_ci <- mean(data$heart_rate) + 2*sd(data$heart_rate)
lower_ci <- mean(data$heart_rate) - 2*sd(data$heart_rate)

upper_ci - lower_ci

ggplot(data = data) +  geom_line(aes(x = time_elapsed, y = heart_rate))

diptest::dip.test(data$heart_rate, simulate.p.value = TRUE)

dens_max <- which.max(density(data$speed, bw = "sj")$y)

ggplot(data = data) + geom_density(aes(x = speed)) + 
  geom_vline(xintercept = density(data$speed, bw = "sj")$x[dens_max])

#test for normality with shapiro.test 
#then apply probability density function concepts?
#would probably need to smooth data a bit first

plot

test <- as_tibble(bkde(data$speed))

ggplot(data = test, aes(x = x, y = y)) + geom_line()

shapiro.test(test$x)

poly_lm <- lm(y ~ poly(x, 3), data = test)

summary(poly_lm)

res <- data.frame(x = test$x, pred = predict(poly_lm), actual = test$y)

ggplot(data = res, aes(x = x)) + geom_line(aes(y = pred), color = "red") + 
  geom_line(aes(y = actual), color = "blue")

coefs <- as.numeric(poly_lm$coefficients[2:4])

#use dmax code to fix this
deriv <- res %>% 
  mutate(y_deriv = coefs[3]*x^2 + coefs[2]*x + coefs[1])


ggplot(data = deriv, aes(x = x)) + geom_line(aes(y = pred), color = "red") + 
  geom_line(aes(y = actual), color = "blue") + 
  geom_line(aes(y = y_deriv), color = "green")