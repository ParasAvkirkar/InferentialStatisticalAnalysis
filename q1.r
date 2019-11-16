first_set = read.csv("~/workspace/Statistical Analysis/Statistical Analysis/dataset1.csv", quote="")
# View(first_set)

### Performing ANOVA test ###
significance_level = 0.05
result = aov(time ~ menu, data=first_set)
summary(result)
p_value = summary(result)[[1]]$Pr[[1]]
cat("P-value for difference in variance of menu performance ", p_value, "\n")
if (p_value < significance_level) {
  print("Null hypothesis is rejected")
  print("Some of the menus's exhibit different performance")
} else {
  print("Alternative hypothesis is rejected")
  print("Groups exhibit similar performance")
}

### End ANOVA test ###
"
By now, we have just learned that some of the menus have
  different global population mean for time value.
  By analysing just the sample and performing anova test,
  we could infer that for any kinds of users possible in the universe
  on average the performance in terms of time is not same for all menus.
  
  To infer which menus have better or similar performance,
  we have to perform pairwise t-test
"
toolglass_users = subset(first_set, menu == "toolglass")
flowmenu_users = subset(first_set, menu == "flowmenu")
toolpalette_users = subset(first_set, menu == "toolpalette")
controlmenu_users = subset(first_set, menu == "controlmenu")

# View(controlmenu_users)
# result = t.test(toolglass_users$time, flowmenu_users$time)
# result = t.test(toolpalette_users$time, controlmenu_users$time, alternative = 'less')

# names(result)
# result[["conf.int"]]
# View(result)

