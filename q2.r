second_set = read.csv("~/workspace/Statistical Analysis/Statistical Analysis/dataset2.csv", quote="")

### Begin MANOVA test ###
result = manova(cbind(time, error) ~ menu, data=second_set)
result_summary = summary(result, test="Pillai")
result_summary
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
  print("Among time and error dependent variables, time was not significant between menu groups")
}
if (error_p_value >= significance_level) {
  print("Among time and error dependent variables, error was not significant between menu groups")
}

### Pairwise t-test ###
"
By now, we have just learned that some of the menus have
  different global population mean for error value.
  
  To infer which menus have better or similar performance, in terms of error,
  we have to perform pairwise t-test
"
t_test_result = pairwise.t.test(second_set$error, second_set$menu, paired=TRUE)
print(t_test_result)

"
  Our significance level is 0.05
  The p-value lower than significance level was found for:
  1. toolglass vs flowmenu (0.00117)
  2. toolglass vs controlmenu (0.00018)
  For other pairs, p-values was found greater than 0.05
  Hence, we reject the null hypothesis in case of:
  toolglass vs toolpalettte, toolpalette vs flowmenu, toolpalette vs controlmenu, flowmenu vs controlmenu
  For other pairs, we consider performance in terms of error is identical.
  
  Still, we have to figure out which one is better between: toolglass, toolpalette, flowmenu
  We will perform one-sided tail t-test for each above pair
"
toolglass_group = subset(second_set, menu == "toolglass")
toolpalette_group = subset(second_set, menu == "toolpalette")
controlmenu_group = subset(second_set, menu == "controlmenu")
flowmenu_group = subset(second_set, menu == "flowmenu")

one_side_t_test_result = t.test(toolglass_group$error, controlmenu_group$error, alternative = "less")
one_side_t_test_result
if (one_side_t_test_result$p.value < significance_level) {
  print("Toolglass menu shows less error than toolpalette group")
  print("Toolglass menu is better than toolpalette group")
}

one_side_t_test_result = t.test(toolglass_group$error, controlmenu_group$error, alternative = "less")
one_side_t_test_result
if (one_side_t_test_result$p.value < significance_level) {
  print("Toolglass menu shows less error than controlmenu group")
  print("Toolglass menu is better than controlmenu group")
}

one_side_t_test_result = t.test(toolglass_group$error, flowmenu_group$error, alternative = "less")
one_side_t_test_result
if (one_side_t_test_result$p.value < significance_level) {
  print("Toolglass menu shows less error than flowmenu group")
  print("Toolglass menu is better than flowmenu group")
}


### End of pairwise-t-test ###