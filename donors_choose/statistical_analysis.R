library(MASS)
library(dplyr)

#Load the data
train <- read.csv('train_clean.csv')

#Change the year to a factor
train$year <- as.factor(train$year)

# Chi-square test to see if approval is not associated with the teacher prefix
teacher_table <- table(train$project_is_approved, train$teacher_prefix)
teacher_chi <- chisq.test(teacher_table)
print(teacher_chi)

# Chi-square test to see if approval is not associated with the state
school_table <- table(train$project_is_approved, train$school_state)
school_chi <- chisq.test(school_table)
print(school_chi)

# Chi-square test to see if approval is not associated with grade category
grade_table <- table(train$project_is_approved, train$project_grade_category)
grade_chi <- chisq.test(grade_table)
print(grade_chi)

# Chi-square test to see if approval is not associated with the month
month_table <- table(train$project_is_approved, train$month)
month_chi <- chisq.test(month_table)
print(month_chi)

# Chi-square test to see if approval is not associated with the year
year_table <- table(train$project_is_approved, train$year)
year_chi <- chisq.test(year_table)
print(year_chi)

# Chi-square test to see if approval is not associated with the project categories
cats_app <- train %>% filter(project_is_approved == 1) %>% select(Applied.Learning, Health...Sports, History...Civics, Literacy...Language, 
                                                                  Math...Science, Music...The.Arts,Special.Needs_x, Warmth.Care...Hunger_x)
cats_app_sums <- colSums(cats_app)

cats_rej <- train %>% filter(project_is_approved == 0) %>% select(Applied.Learning, Health...Sports, History...Civics, Literacy...Language, 
                                                                  Math...Science, Music...The.Arts,Special.Needs_x, Warmth.Care...Hunger_x)
cats_rej_sums <- colSums(cats_rej)

cat_df <- data.frame(cats_app_sums, cats_rej_sums)
colnames(cat_df) <- c("Approved","Rejected")
cat_chi <- chisq.test(cat_df)
print(cat_chi)

# Chi-square test to see if approval is not associated with the project subcategories
subcats_app <- train %>% filter(project_is_approved == 1) %>% select(Applied.Sciences, Character.Education, Civics...Government, College...Career.Prep,
                                                                     Community.Service, ESL, Early.Development,Economics,Environmental.Science,
                                                                     Extracurricular,Financial.Literacy,Foreign.Languages,Gym...Fitness,
                                                                     Health...Life.Science, Health...Wellness,History...Geography,Literacy,
                                                                     Literature...Writing,Mathematics,Music,Nutrition.Education,Other,Parent.Involvement,
                                                                     Performing.Arts,Social.Sciences,Team.Sports,Visual.Arts)
subcats_app_sums <- colSums(subcats_app)

subcats_rej <- train %>% filter(project_is_approved == 0) %>% select(Applied.Sciences, Character.Education, Civics...Government, College...Career.Prep,
                                                                     Community.Service, ESL, Early.Development,Economics,Environmental.Science,
                                                                     Extracurricular,Financial.Literacy,Foreign.Languages,Gym...Fitness,
                                                                     Health...Life.Science, Health...Wellness,History...Geography,Literacy,
                                                                     Literature...Writing,Mathematics,Music,Nutrition.Education,Other,Parent.Involvement,
                                                                     Performing.Arts,Social.Sciences,Team.Sports,Visual.Arts)
subcats_rej_sums <- colSums(subcats_rej)

subcat_df <- data.frame(subcats_app_sums, subcats_rej_sums)
colnames(subcat_df) <- c("Approved","Rejected")
subcat_chi <- chisq.test(subcat_df)
print(subcat_chi)

# ANOVA Test to see if there are not differences between the number of previously posted projects brokendown by project approval
aov_teach_proj <- aov(train$project_is_approved ~ train$teacher_number_of_previously_posted_projects)
summary(aov_teach_proj)

# ANOVA Test to see if there are not differences between the number of project resource word count brokendown by project approval
aov_proj_res_count <- aov(train$project_is_approved ~ train$proj_resouce_count_nostop)
summary(aov_proj_res_count)

# ANOVA Test to see if there are not differences between the number of project essay 1 word count brokendown by project approval
aov_essay_1_count <- aov(train$project_is_approved ~ train$essay1_count_nostop)
summary(aov_essay_1_count)

# ANOVA Test to see if there are not differences between the number of project essay 2 word count brokendown by project approval
aov_essay_2_count <- aov(train$project_is_approved ~ train$essay2_count_nostop)
summary(aov_essay_2_count)

# ANOVA Test to see if there are not differences between the number of project essay 3 word count brokendown by project approval
aov_essay_3_count <- aov(train$project_is_approved ~ train$essay3_count_nostop)
summary(aov_essay_3_count)

# ANOVA Test to see if there are not differences between the number of project essay 4 word count brokendown by project approval
aov_essay_4_count <- aov(train$project_is_approved ~ train$essay4_count_nostop)
summary(aov_essay_4_count)

# ANOVA Test to see if there are not differences between the number resources requested brokendown by project approval
aov_quantity <- aov(train$project_is_approved ~ train$quantity)
summary(aov_quantity)

# ANOVA Test to see if there are not differences between the total price of the resources requested brokendown by project approval
aov_price <- aov(train$project_is_approved ~ train$total_price)
summary(aov_price)