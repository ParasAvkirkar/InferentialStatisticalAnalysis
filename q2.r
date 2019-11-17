second_set = read.csv("~/workspace/Statistical Analysis/Statistical Analysis/dataset2.csv", quote="")

### Begin MANOVA test ###
result = manova(cbind(time, error) ~ menu, data=second_set)
result_summary = summary(result, test="Pillai")
"
  Have explored all the test variations of manova: Pillai, Wilks, Hotelling-Lawley, Roy
  For all the test methods p_value was less than 0.05
  Only for assignment/presentation purposes have shown Pillai test method  
"

p_value = result_summary$stats[[1,6]]
significance_level = 0.05
if (p_value < significance_level) {
  print("Reject the null hypothesis")
  print("There is variation in performance between menus in terms of time and error")
} else {
  print("Reject the alternative hypothesis")
  print("There is no difference between types of menus in terms of time and error")
}
### End MANOVA test ###

"
  After applying MANOVA we know that time and error dependent variables are not same across menus
  To identify which is different between time and error, we apply ANOVA again  
"
summary.aov(result)
aov_result = summary.aov(result)
time_p_value = aov_result$` Response time`$`Pr(>F)`[[1]]
error_p_value = aov_result$` Response error`$`Pr(>F)`[[1]]
if (time_p_value >= significance_level) {
  print("Among time and error dependent variables, time was not significant")
}
if (error_p_value >= significance_level) {
  print("Among time and error dependent variables, error was not significant")
}

