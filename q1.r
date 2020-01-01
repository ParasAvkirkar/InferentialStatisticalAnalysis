first_set = read.csv("dataset1.csv", quote="")
# View(first_set)

### Performing ANOVA test ###
result = aov(time ~ menu, data=first_set)
summary(result)
p_value = summary(result)[[1]]$Pr[[1]]
cat("P-value for difference in variance of menu usage timings ", p_value, "\n")

### End ANOVA test ###

### Pairwise t-test ###
"
By now, we have just learned that some of the menus have
  different global population mean for time value.
  By analysing just the sample and performing anova test,
  we could infer that for any kinds of users possible in the universe
  on average the performance in terms of time is not same for all menus.
  
  To infer which menus have better or similar performance,
  we have to perform pairwise t-test
"
all_pair_test = pairwise.t.test(first_set$time, first_set$menu, p.adjust.method = "bonferroni")
all_pair_test

toolglass_users = subset(first_set, menu == "toolglass")
toolpalette_users = subset(first_set, menu == "toolpalette")
controlmenu_users = subset(first_set, menu == "controlmenu")
flowmenu_users = subset(first_set, menu == "flowmenu")

### One-sided test to compare performance of toolpalette vs controlmenu
result = t.test(toolpalette_users$time, controlmenu_users$time, alternative = "greater")
result


### End of pairwise-t-test
  

