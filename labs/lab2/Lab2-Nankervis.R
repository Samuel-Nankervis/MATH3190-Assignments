#Math 3190 Lab 2
#Question 2
#a
library(math3150package) #loads tidyverse by extention
#b
employee_names = c("Alice", "Bob", "Charlie",  "Diana", "Edward")
#c
monthly_hours = c(160, 145, 180, 120,  155)
#d
work_data = data.frame(employee_names, monthly_hours)
#e
work_tibble = tibble(employee_names, monthly_hours)
#f
work_data[c(3,5),]
#g
one = filter(work_data, employee_names == "Alice")
one = work_data[work_data$employee_names == "Alice", ]
#h
hours = work_data$monthly_hours
mean(hours)
#mean is 152
#Question 3
#a
to_kelvin = function(ferenhight) 
  kelvin = (ferenhight - 32) / 1.8 + 273.15
  return (kelvin)

k = to_kelvin(100)
#310.9278
k = to_kelvin(0)
#255.3722
#b
fuel_cost = function(miles, mpg, price_per_gal) 
  cost = (miles / mpg) * price_per_gal
  return (cost)
ammount = fuel_cost (400, 32, 3.25)
#40.625

#question 4
mpg_filtered_data = filter(mpg, mpg$manufacturer == c("audi", "ford", "toyota"))
mpg_filtered_data2 = mutate(mpg_filtered_data, combined_mpg = (cty + hwy)/2)
mpg_filtered_data3 = mpg_filtered_data2[c(1:1, 2:2,5:5,11:11,12:12)]
mpg_filtered_data4 = group_by(mpg_filtered_data3, class)
print(mpg_filtered_data4 , n= 26)
mpg_summary = summarize(mpg_filtered_data4, avg_mpg = mean(combined_mpg),sd_mpg = sd(combined_mpg))
mpg_summary2 = arrange(mpg_summary, desc(avg_mpg))
#the best mpg is from compact cars (unsurprisingly)
#question 5
midwest
#a
ggplot(midwest, aes(x = poptotal, y = area)) +
  geom_point(size = 3) +
  labs(x = "total population", y = "Area",
       title = "population V.S Area") +
  theme_bw() +
  geom_smooth(method = "lm", se = F, formula = "y ~ x") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"))
#b
ggplot(midwest, aes(x = log(poptotal,10), y = area, color = state)) +
  geom_point(size = 3) +
  labs(x = "log total population", y = "Area",
       title = "population V.S Area") +
  theme_bw() +
  geom_smooth(method = "lm", se = F, formula = "y ~ x") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"))
#c
ggplot(midwest, aes(x = state, y = percollege)) +
  geom_boxplot(color = "black", fill = "yellow") +
  labs(x = "State", y = "Proportion College Educated",title = "Box Plot of College Education Proportion Split by State") +
  scale_fill_brewer() +
  theme(plot.title = element_text(size = 18, face = "bold"))
#d
ggplot(midwest, aes(x = percbelowpoverty, fill = state)) + 
  geom_density(alpha = 0.5) +
  scale_fill_brewer() +
  labs(x = "Proportion Below Poverty line", y = "Frequency",title = "Kernel Density Plots of Proportion in Poverty by State") +
  theme(plot.title = element_text(size = 18, face = "bold"))
#e
#gives number of counties per state
midwest2 = group_by(midwest, state)
midwest_summary = summarize(midwest2, count = n())
#makes a bar chart for the States
ggplot(midwest_summary, aes(x = state, y = count)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") + 
  labs(x = "State", y = "Number of Counties") +
  ggtitle("Number of Counties per State in the Midwest") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"))
#Question 6
#requires working directory to be set in the data folder
setwd("C:\\Users\\Samuel\\Desktop\\R-scripts-and-data\\MATH3190\\MATH3190-Assignments\\data")
#a
treeseeds = read.csv("treeseeds.txt")
head(treeseeds)
#b
blood_pressure = read.table("blood_pressure.txt",header = TRUE)
head(blood_pressure)
#c
Concrete_Data = read.table("Concrete_Data.txt",header = TRUE, sep = "\t")
head(Concrete_Data)
#question 7
#a
for (value in monthly_hours){
  if (as.numeric(value) > 150) {
    print(paste(as.character(value), " is overtime elegible."))}
  else{
    print(paste(as.character(value), " is on standard hours."))}
}
#b
work_data2 = mutate(work_data, overtime = ifelse(monthly_hours>150, "Overtime Elegeble","Standard Hours"))
