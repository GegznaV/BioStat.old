library(tidyverse)

clerk_salary <- data.frame(salary = c(525,500,500,576,458,600,700,886,600),
                           gender = c("F", "F","F","F","F","F","M","M","M"))

clerk_salary$gender2 <- factor(clerk_salary$gender, levels = c("M", "F"))

unclass(clerk_salary)

boxplot(salary ~ gender, data = clerk_salary)

# Answers are different

DescTools::RunsTest(salary ~ gender, data = clerk_salary)
DescTools::RunsTest(salary ~ gender2, data = clerk_salary)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- data.frame(    x = c(52,  50, 50,  50, 45, 60, 70, 88, 60),
                 group = c("A", "A","A","B", "A","A","B","B","B"))
# df$group2 <- factor(df$group, levels = c("B", "A"))

DescTools::RunsTest(x ~ group,  data = df)
DescTools::RunsTest(x ~ group2, data = df)

df2 <- df %>%
    arrange(x) %>%
    mutate(nr = row_number(x))

duplicated_values(df2$x)

duplicated_values <- function(x) {
    unique(x[duplicated(x)])
}


