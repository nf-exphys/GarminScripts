

#Now calculate grade


# #Developing smoothed data through trial and error

#375 is Mississippi river data
#406 is Missouri run with big drop/rise
#408 is hill repeats
#250 is a bike home from the lab with some elevation errors


# 
# test <- record_list[[375]]
# 
# ggplot(data = test, aes(x = distance, y = elevation)) + 
#   geom_line() + 
#   ggtitle("Original Elevation Data")
# 
# init_elev <- test$elevation[1]
# 
# #Set up smoothed elevation with overlay of elevation difference
# test %>% 
#   mutate(smooth_elev = slide_dbl(elevation, .f = median, before = 5, after = 5, step = 1)) %>% #smooths out using the median
#   mutate(smooth_elev = slide_mean(smooth_elev, before = 3, after = 3, step = 1)) %>%  #smooth data a bit more with rolling mean
#   mutate(smooth_elev = slide_mean(smooth_elev, before = 15, after = 15, step = 1)) %>% #now take a wider pass
#   mutate(diff_smooth = smooth_elev - elevation) %>% 
#   mutate(diff_smooth = case_when(diff_smooth > 1.8 ~ diff_smooth, diff_smooth < -1.8 ~ diff_smooth, TRUE ~ 0)) %>% 
#   pivot_longer(cols = contains("elev"), names_to = "elev_type", values_to = "elev") %>% 
#   mutate(elev_type = as.factor(elev_type)) %>% 
#   ggplot(data = ., aes(x = distance, y = elev - init_elev, group = elev_type, colour = elev_type)) +
#   geom_bar(aes(x = distance, y = diff_smooth), alpha = 0.1, stat = 'identity', color = "grey") + 
#   geom_line() +
#   # ylim(265, 285) + 
#   ggtitle("All Elevation Data")
# 
# 
# #375 is Mississippi river data
# #406 is Missouri run with big drop/rise
# #408 is hill repeats
# #250 is a bike home from the lab with some elevation errors
# #356 is flapjack friday
# 
# smooth_record[[356]]
# 
# 
# test %>% 
#   mutate(rise = (smooth_elev - lag(smooth_elev,3)), #needed to find grade
#          run = (distance - lag(distance,3))) %>% #needed to find grade
#   mutate(elev_grade = round((rise / run) * 100, 3)) %>% #calculates grade
#   mutate(elev_grade = slide_mean(elev_grade, before = 5, after = 0, step = 1)) %>%  #smooths grade
#   ggplot(data = ., aes(x = distance, y = smooth_elev), color = "green") + 
#     geom_line() + 
#     geom_line(aes(x = distance, y = elev_grade), color = "blue")
# 
# 
# #Once the errors from bridges or rivers are corrected, go back and re-smooth
# #Can just use smooth_elevation with plot and impute set to false
