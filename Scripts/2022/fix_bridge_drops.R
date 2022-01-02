library(splines)
library(gsignal)

test <- smooth_record[[375]]

grad <- pracma::gradient(test$smooth_elev, h1 = test$time_elapsed)

grad2 <- pracma::gradient(grad, h1 = test$time_elapsed)

#Find errors in elevation data due to big drops
test_delta <- test %>% 
  mutate(delta_elev = (smooth_elev - lag(smooth_elev, 1))*10) %>% 
  mutate(grad_smooth = grad*10)

peaks <- pracma::findpeaks(abs(test_delta$grad_smooth), 
                           minpeakheight = 2.75,
                           minpeakdistance = 3)

if(!is.null(peaks)){
  peak_times <- as.data.frame(peaks)[2:4] %>% 
    rename(peak_time = V2, 
           start_peak = V3,
           end_peak = V4)
} else{peak_times <- data.frame(start_peak = 0, end_peak = 0)}

ggplot(data = test_delta, aes(x = time_elapsed, y = smooth_elev - smooth_elev[1])) + 
  geom_point(aes(x = time_elapsed, y = elevation  - elevation[1]), color = "light green") + 
  geom_bar(aes(x = time_elapsed, y = delta_elev), stat = 'identity', color = "pink") + 
  geom_line() + 
  # geom_line(aes(x = time_elapsed, y = grad_smooth_accel*200), color = "purple") +
  geom_line(aes(x = time_elapsed, y = grad_smooth), color = "orange") + 
  geom_hline(yintercept = 2.75) +
  geom_hline(yintercept = -2.75) +
  geom_vline(data = peak_times, aes(xintercept = start_peak)) +
  geom_vline(data = peak_times, aes(xintercept = end_peak))

#Make a copy of smoothed elevation and fix elevation on the copy
test_delta$fixed_elev <- test_delta$smooth_elev

zoo_test <- zoo::as.zoo(test_delta)

for (i in 1:nrow(peak_times)){
  
  idx_start <- which(as.numeric(zoo_test$time_elapsed) > peak_times[i,"start_peak"])
  idx_end <- which(as.numeric(zoo_test$time_elapsed) < peak_times[i, "end_peak"])
  
  set_to_na <- intersect(idx_start, idx_end)
  
  zoo_test[set_to_na,]$fixed_elev <- NA
  
  idx_start <- which(as.numeric(test_delta$time_elapsed) > peak_times[i,"start_peak"])
  idx_end <- which(as.numeric(test_delta$time_elapsed) < peak_times[i, "end_peak"])
  
  set_to_na <- intersect(idx_start, idx_end)
  
  test_delta[set_to_na,]$fixed_elev <- NA
  
}

x_na_zoo <- which(is.na(zoo_test$fixed_elev))
x_na <- which(is.na(test_delta$fixed_elev))

zoo_test$fixed_elev[x_na]
test_delta$fixed_elev[x_na]

#Highlights points to be fixed by spline
zoo_test_plotting <- as_tibble(zoo_test) %>% 
  mutate(timestamp = lubridate::with_tz(timestamp, tzone = "America/Chicago")) %>% 
  mutate(across(where(is.character), as.numeric))

ggplot(data = zoo_test_plotting, aes(x = time_elapsed, y = fixed_elev)) + 
  geom_line()

#Use stats::spline() to interpolate
# test_spline <- stats::smooth.spline(test_delta$time_elapsed, test_delta$fixed_elev)  

#hard code removal of random non-NA
test_delta$fixed_elev[426] <- NA
zoo_test$fixed_elev[426] <- NA

test_spline <- zoo::na.spline(zoo_test$fixed_elev[1:600], na.rm = "FALSE")

#format interpolated data
test_spline <- as_tibble(test_spline) %>% 
  mutate(across(where(is.character), as.numeric)) %>% 
  rename(spline_elev = value) %>% 
  mutate(time_elapsed = test_delta$time_elapsed[1:600])

ggplot(data = test_spline, aes(x = time_elapsed, y = spline_elev - spline_elev[1])) + 
  geom_point(data = test_delta[1:600,], aes(x = time_elapsed, y = fixed_elev - fixed_elev[1])) + 
  geom_line()

#Try RF?
df_imp <- test_delta %>% 
  select(-timestamp, -id, -elevation, -smooth_elev) %>% 
  as.data.frame()

df_imp_res <- missForest(df_imp, ntree = 200)

df_imp_res$ximp %>% 
  as_tibble() %>% 
  ggplot(data = ., aes(x = time_elapsed, y = fixed_elev)) + geom_line()

#Try rolling linear model




#find interpolated times
# fixed_times <- test_spline$time_elapsed

#fix elevation using values from splines
# test_delta$fixed_elev[fixed_times] <- test_spline$spline_elev

merge_test <- bind_cols(test_delta, test_spline, .name_repair = "minimal") %>% 
  pivot_longer(cols = c("fixed_elev", "spline_elev"), values_to = "calc_elev", names_to = "type")

ggplot(data = merge_test, aes(x = time_elapsed, y = calc_elev, colour = type), group = type) + 
  geom_line()

#matches original smoothed data very well
ggplot() + geom_line(data = test_spline, aes(x = time_elapsed, y = spline_elev - spline_elev[1]), color = "purple")

# mutate(moving_sum = slider::slide_sum(delta_elev, before = 10, after = 10, step = 1)*2) %>% 
# mutate(spline = smooth.spline())

plot(grad, col = 1)
