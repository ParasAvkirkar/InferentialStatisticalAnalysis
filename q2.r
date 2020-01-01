second_set = read.csv("dataset2.csv", quote="")

### Begin MANOVA test ###
result = manova(cbind(time, error) ~ menu, data=second_set)
result_summary = summary(result, test="Pillai")
result_summary

p_value = result_summary$stats[[1,6]]
cat("P-value observed ", p_value, "\n")
### End MANOVA test ###

summary.aov(result)
aov_result = summary.aov(result)
time_p_value = aov_result$` Response time`$`Pr(>F)`[[1]]
error_p_value = aov_result$` Response error`$`Pr(>F)`[[1]]

### Pairwise t-test ###
### Pairwise t-test 

t_test_result = pairwise.t.test(second_set$time, second_set$menu, paired=TRUE, p.adjust.method = "bonferroni")
print(t_test_result)

t_test_result = pairwise.t.test(second_set$error, second_set$menu, paired=TRUE, p.adjust.method = "bonferroni")
print(t_test_result)

toolglass_group = subset(second_set, menu == "toolglass")
toolpalette_group = subset(second_set, menu == "toolpalette")
controlmenu_group = subset(second_set, menu == "controlmenu")
flowmenu_group = subset(second_set, menu == "flowmenu")

one_side_t_test_result = t.test(toolglass_group$error, controlmenu_group$error, alternative = "less", paired = TRUE)
one_side_t_test_result

one_side_t_test_result = t.test(toolglass_group$error, flowmenu_group$error, alternative = "less", paired = TRUE)
one_side_t_test_result

one_side_t_test_result = t.test(toolpalette_group$time, controlmenu_group$time, alternative = "greater", paired = TRUE)
one_side_t_test_result

one_side_t_test_result = t.test(toolpalette_group$time, flowmenu_group$time, alternative = "greater", paired = TRUE)
one_side_t_test_result

one_side_t_test_result = t.test(toolpalette_group$time, toolglass_group$time, alternative = "greater", paired = TRUE)
one_side_t_test_result

### End of pairwise-t-test ###