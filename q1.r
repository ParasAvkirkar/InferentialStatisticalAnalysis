first_set = read.csv("~/workspace/Statistical Analysis/Statistical Analysis/dataset1.csv", quote="")
# View(first_set)

### Performing ANOVA test ###
significance_level = 0.05
result = aov(time ~ menu, data=first_set)
summary(result)
p_value = summary(result)[[1]]$Pr[[1]]
cat("P-value for difference in variance of menu acces timings ", p_value, "\n")
if (p_value < significance_level) {
  print("Null hypothesis is rejected")
  print("Some of the menus's exhibit different access timings")
} else {
  print("Alternative hypothesis is rejected")
  print("Menus exhibit similar access timings")
}

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
all_pair_test = pairwise.t.test(first_set$time, first_set$menu)
all_pair_test

### Individual pairwise test ###

toolglass_users = subset(first_set, menu == "toolglass")
flowmenu_users = subset(first_set, menu == "flowmenu")
toolpalette_users = subset(first_set, menu == "toolpalette")
controlmenu_users = subset(first_set, menu == "controlmenu")

glass_vs_flow = t.test(toolglass_users$time, flowmenu_users$time, var.equal = TRUE)
glass_vs_flow

glass_vs_palette = t.test(toolglass_users$time, toolpalette_group$time, var.equal = TRUE)
glass_vs_palette

glass_vs_control = t.test(toolglass_users$time, controlmenu_users$time, var.equal = TRUE)
glass_vs_control

flow_vs_palette = t.test(flowmenu_users$time, toolpalette_users$time, var.equal = TRUE)
flow_vs_palette

flow_vs_control = t.test(flowmenu_users$time, controlmenu_users$time, var.equal = TRUE)
flow_vs_control

palette_vs_control = t.test(toolpalette_users$time, controlmenu_users$time, var.equal = TRUE)
palette_vs_control

"
  Our significance level is 0.05
  The p-value lower than significance level was found only for
  toolpalette and controlmenu (0.034).
  For other pairs, p-values was found greater than 0.05
  Hence, we reject the null hypothesis in case of toolpalette vs controlmenu
  For other pairs, we consider time performance identical.
  Still, we have to figure out which one is better between toolpalette and controlmenu.
  
  We will perform one-sided tail t-test between toolpalette and controlmenu samples
  
"

result = t.test(toolpalette_users$time, controlmenu_users$time, alternative = "greater")
significance_level = 0.05
if (result$p.value < significance_level) {
  print("Reject Null Hypothesis")
  print("Toolpalette menu needs more access time compared to controlmenu")
  print("Controlmenu gives better performance")
} else {
  print("Reject alternative hypothesis")
  print("Controlmenu gives inferior or similar performance")
}

result = t.test(toolpalette_users$time, flowmenu_users$time, alternative = "greater")
significance_level = 0.05
if (result$p.value < significance_level) {
  print("Reject Null Hypothesis")
  print("Toolpalette menu needs more access time compared to flowmenu")
  print("flowmenu gives better performance")
} else {
  print("Reject alternative hypothesis")
  print("flowmenu gives inferior or similar performance")
}

result = t.test(toolpalette_users$time, toolglass_users$time, alternative = "greater")
significance_level = 0.05
if (result$p.value < significance_level) {
  print("Reject Null Hypothesis")
  print("Toolpalette menu needs more access time compared to toolglass")
  print("toolglass gives better performance")
} else {
  print("Reject alternative hypothesis")
  print("toolglass gives inferior or similar performance")
}

### End of pairwise-t-test
  

