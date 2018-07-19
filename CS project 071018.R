library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggvis)
library(tidyr)
library(broom)
library(ggpubr)
pop_13_17 <- read_csv('data/State_pop_13_17.csv')
dim(pop_13_17)
# reading 2017 data
df_17 <- read_csv('data/State_Drug_Utilization_Data_2017.csv')
# EDA on the data
dim(df_17)
names(df_17)
glimpse(df_17)
summary(df_17)
# removing nas
df_17_nona <- na.omit(df_17)
summary(df_17_nona)
names(df_17_nona)
# subsetting data
df_17_nona_sub1 <- df_17_nona[c(2,6,7,8,10,11,12)]
names(df_17_nona_sub1)
# importing 2016 data
df_16 <- read_csv('data/State_Drug_Utilization_Data_2016.csv')
summary(df_16)
# removing nas
df_16_nona <- na.omit(df_16)
summary(df_16_nona)
# subsetting data
df_16_nona_sub1 <- df_16_nona[c(2,6,7,8,10,11,12)]
names(df_16_nona_sub1)
names(df_17_nona_sub1)
# importing 2015 data
df_15 <- read_csv('data/State_Drug_Utilization_Data_2015.csv')
summary(df_15)
# removing nas
df_15_nona <- na.omit(df_15)
summary(df_16_nona)
# subsetting data
df_15_nona_sub1 <- df_15_nona[c(2,6,7,8,10,11,12)]
names(df_15_nona_sub1)
names(df_17_nona_sub1)
# importing 2014 data
df_14 <- read_csv('data/State_Drug_Utilization_Data_2014.csv')
summary(df_14)
# removing nas
df_14_nona <- na.omit(df_14)
summary(df_14_nona)
# subsetting data
df_14_nona_sub1 <- df_14_nona[c(2,6,7,8,10,11,12)]
names(df_14_nona_sub1)
names(df_17_nona_sub1)

# importing 2013 data
df_13 <- read_csv('data/State_Drug_Utilization_Data_2013.csv')
summary(df_13)
# removing nas
df_13_nona <- na.omit(df_13)
summary(df_13_nona)
# subsetting data
df_13_nona_sub1 <- df_13_nona[c(2,6,7,8,10,11,12)]
names(df_13_nona_sub1)
names(df_17_nona_sub1)
# concatenating 5 years of data
df_13_17 <- rbind(df_17_nona_sub1,df_16_nona_sub1,df_15_nona_sub1,
                  df_14_nona_sub1, df_13_nona_sub1)
dim(df_13_17)
head(df_13_17)
# merging population data with it
df_13_17_pop <- merge(df_13_17,pop_13_17,  by = c("State", "Year"))
names(df_13_17_pop)
df_13_17_pop <- df_13_17_pop[-c(9:18)]
head(df_13_17_pop)
# making rds object out of it
saveRDS(df_13_17_pop, file = "df_13_17_pop.rds")
# Restore the object
data <- readRDS(file = "df_13_17_pop.rds")
dim(data)
# removing data with State == XX
data_A <- data %>% filter(State != "XX")
#changing Product_Name as Product
names(data_A)[names(data_A) == "Product Name"] <- "Product"
names(data_A)
# summarizing the data
data_B <- data_A %>% group_by(`State`, `Year`,`Quarter` ) %>% 
  summarize(tot_amt_reim = sum(`Total Amount Reimbursed`),
    tot_unt_reim = sum(`Units Reimbursed`),tot_nm_pres = sum(`Number of Prescriptions`),
    pop = mean(`Population`, na.rm = TRUE)) %>% mutate(unit_cost = tot_amt_reim/tot_unt_reim,
                      amt_capita = tot_amt_reim/pop)
head(data_B)
# filter this data only for TN
data_B_TN <- data_A %>% filter(State == "TN") %>% 
  group_by(`Year`,`Quarter` )%>% 
  summarize(tot_amt_reim = sum(`Total Amount Reimbursed`),
            tot_unt_reim = sum(`Units Reimbursed`),tot_nm_pres = sum(`Number of Prescriptions`),
            pop = mean(`Population`, na.rm = TRUE)) %>%
  mutate(unit_cost = tot_amt_reim/tot_unt_reim,amt_capita = tot_amt_reim/pop)

# Q1: Total cost over the years for all the states
Yearly_cost <- data_B %>% group_by(`Year`) %>% summarize(total_cost = sum(`tot_amt_reim`)) 
head(Yearly_cost)
# ggplot
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "blue", size = 16)
black.bold.italic.16.text <- element_text(face = "bold.italic", color = "black", size = 12)
gray.bold.italic.16.text <- element_text(face = "bold.italic", color = "gray4", size = 16)
# bar plot showing the increase in the cost
ggplot(Yearly_cost, aes(reorder( Year, total_cost),total_cost)) +
geom_col(width = 0.7, fill = "tomato3") +
  labs(title = "Medicaid cost over the years", x = "Year", y = "Total cost (billions)") +
  theme(title = blue.bold.italic.16.text, axis.title = black.bold.italic.16.text) + 
  scale_y_continuous(name = "Total cost (billions)", breaks = c(10000000000,
  20000000000,30000000000,40000000000,50000000000,60000000000 ),
                     labels = c(10,20,30,40,50, 60))
# Q1a: Total cost over the years for TN
Yearly_cost_TN <- data_B_TN %>% group_by(`Year`) %>% summarize(total_cost = sum(`tot_amt_reim`)) 
head(Yearly_cost_TN)
# ggplot bar plot
ggplot(Yearly_cost_TN, aes( Year, total_cost)) +
  geom_col(width=.5, fill = "green4")+ ggtitle("Total cost over the years") +
  labs(title = "Medicaid cost over the years in TN", x = "Year", y = "Total cost (billions)") +
  theme(title = gray.bold.italic.16.text, axis.title = black.bold.italic.16.text) +
  scale_y_continuous(name = "Total cost (billions)",breaks = c(100000000,200000000,400000000,600000000,800000000,1000000000,1200000000 ),
  labels = c(0.1,0.2,0.4,0.6,0.8,1.0,1.2))

# Last question:
# Least square regression for all the states
ggplot(data = Yearly_cost, aes(y = total_cost, x = Year)) +
  geom_point() + geom_smooth(method = "lm")
# prediction
mod <- lm(total_cost ~ Year, data = Yearly_cost)
new_data <- data.frame(Year = c(2018, 2019))
predict(mod, newdata = new_data)
# Visualizing new observations
exp_val <- broom::augment(mod, newdata = new_data)
ggplot(data = Yearly_cost, aes(y = total_cost, x = Year)) +
  geom_point() + geom_smooth(method = "lm") +
  geom_point(data = exp_val, aes(y = .fitted), size = 4, color = "red")+
  labs(title = "Medicaid cost over the years", x = "Year", y = "Total cost (billions)")+
  theme(title = blue.bold.italic.16.text, axis.title = black.bold.italic.16.text) +
  scale_x_continuous(name="Year",breaks=c(2013,2014,2015,2016,2017,2018,2019),
                     labels = c(2013,2014,2015,2016,2017,2018,2019))+
  scale_y_continuous(name="Total cost (billions)",breaks = c(10000000000,
  20000000000,30000000000,40000000000,50000000000,60000000000,80000000000, 100000000000 ),
                     labels = c(10,20,30,40,50, 60, 80,100))
# Least square regression for TN
ggplot(data = Yearly_cost_TN, aes(y = total_cost, x = Year)) +
  geom_point() + geom_smooth(method = "lm")
# prediction

mod_TN <- lm(total_cost ~ Year, data = Yearly_cost_TN)
predict(mod_TN, newdata = new_data) 

exp_val_TN <- broom::augment(mod_TN, newdata = new_data)

  ggplot(data = Yearly_cost_TN, aes(y = total_cost, x = Year)) +
  geom_point() + geom_smooth(method = "lm" , color = 'brown4') +
  geom_point(data = exp_val_TN, aes(y = .fitted), size = 4, color = "firebrick")+
  labs(title = "Medicaid cost over the years for TN", x = "Year", y = "Total cost (billions)")+
  theme(title = gray.bold.italic.16.text, axis.title = black.bold.italic.16.text) +
  scale_x_continuous(name="Year",breaks=c(2013,2014,2015,2016,2017,2018,2019),
                     labels = c(2013,2014,2015,2016,2017,2018,2019)) +
  scale_y_continuous(name="Total cost (billions)",breaks = c(100000000,200000000,
  400000000,600000000,800000000,1000000000,1200000000 ),labels = c(0.1,0.2,0.4,0.6,0.8,1.0,1.2))

# Q2: State wise total cost over the years CA on top
top_states <- data_B %>% group_by(`State`) %>% summarize(total_cost = sum(`tot_amt_reim`)) %>% 
  arrange(desc(total_cost))
head(top_states, 10)
#ggplot
head(top_states, 10) %>% ggplot(aes(reorder( State, total_cost),total_cost)) +
  geom_col(width=.5, fill = "red") +
  labs(title = "States with highest cost in last five years", x = "State", y = "Total cost (billions)") +
  theme(title = blue.bold.italic.16.text, axis.title = black.bold.italic.16.text) + 
  scale_y_continuous(name = "Total cost (billions)",
  breaks = c(5000000000,10000000000,20000000000,
             30000000000,40000000000 ),
            labels = c(0.5,10,20,30, 40))

# Q3: State wise total per capita cost over the years
# HI is on the top
top_st_per_cap <- data_B %>% group_by(`State`) %>% summarize(total_cost_per_cap = sum(`amt_capita`)) %>% 
  arrange(desc(total_cost_per_cap)) 
# ggplot
red.bold.italic.16.text <- element_text(face = "bold.italic", color = "red4", size = 16)
head(top_st_per_cap, 10) %>% ggplot(aes(reorder( State, total_cost_per_cap),total_cost_per_cap)) +
  geom_col(width=.5, fill = "purple4") + 
  labs(title = "States with highest cost per person", x = "State", y = "Cost per person") +
  theme(title = red.bold.italic.16.text, axis.title = black.bold.italic.16.text)

# Q3a: State wise total cost over the years(Bottom one) 
top_states <- data_B %>% group_by(`State`) %>% summarize(total_cost = sum(`tot_amt_reim`)) %>% 
  arrange(desc(total_cost))
tail(top_states, 10)
#ggplot
tail(top_states, 10) %>% ggplot(aes(reorder( State, total_cost),total_cost)) +
  geom_col(width=.5, fill = "green4") + labs(title = "States with lowest cost in last five years", x = "State", y = "Total cost (billions)") +
  theme(title = gray.bold.italic.16.text, axis.title = black.bold.italic.16.text) + 
  scale_y_continuous(name = "Total cost (billions)",
  breaks = c( 100000000,200000000,400000000,600000000,
                800000000,1000000000 ),
  labels = c(0.1 ,0.2,0.4,0.6,0.8,1.0))
  

# Q3b: State wise total per capita cost over the years(WY on the bottom)
top_st_per_cap <- data_B %>% group_by(`State`) %>% summarize(total_cost_per_cap = sum(`amt_capita`)) %>% 
  arrange(desc(total_cost_per_cap)) 
tail(top_st_per_cap, 10)
tail(top_st_per_cap, 10) %>% ggplot(aes(reorder( State, total_cost_per_cap),total_cost_per_cap)) +
  geom_col(width=.5, fill = "green4") + labs(title = "States with lowest cost per person", x = "State", y = "Cost per person") +
  theme(title = gray.bold.italic.16.text, axis.title = black.bold.italic.16.text)

# Q4 Top 10 drugs across all the states
data_Drg <- data_A %>% group_by(`Product`) %>% 
  summarize(tot_amt_reim = sum(`Total Amount Reimbursed`),
            tot_unt_reim = sum(`Units Reimbursed`),
            tot_nm_pres = sum(`Number of Prescriptions`),
            pop = mean(`Population`, na.rm = TRUE)) %>% 
  mutate(unit_cost = tot_amt_reim/tot_unt_reim) %>% 
  arrange(desc(tot_amt_reim))
names(data_Drg)
top10drugs <- head(data_Drg, 10)
top10drugs
top10drugs %>% ggplot(aes(Product, tot_amt_reim, size = tot_amt_reim)) +
  geom_point(aes(col = Product)) +
  scale_size_continuous(range = c(1,15)) +
  labs(title = "Top ten drugs costing the taxpayers", x = "Drugs_name",
       y = "Total_cost") +
  theme(title = gray.bold.italic.16.text, axis.title = black.bold.italic.16.text) +
  scale_y_continuous(name = "Total cost (billions)",
                     breaks = c(1000000000,3000000000,5000000000,7000000000),
                     labels = c(1.0,3.0,5.0,7.0)) + theme(legend.position="none")

  
# Q4 Top 10 drugs in TN
data_Drg_TN <- data_A %>% filter (State == 'TN') %>% 
  group_by(`Product`) %>% 
  summarize(tot_amt_reim = sum(`Total Amount Reimbursed`),
            tot_unt_reim = sum(`Units Reimbursed`),
            tot_nm_pres = sum(`Number of Prescriptions`),
            pop = mean(`Population`, na.rm = TRUE)) %>% 
  mutate(unit_cost = tot_amt_reim/tot_unt_reim) %>% 
  arrange(desc(tot_amt_reim))
names(data_Drg_TN)
top10drugs_TN <- head(data_Drg_TN, 10)
top10drugs_TN
top10drugs_TN %>% ggplot(aes(Product, tot_amt_reim, size = tot_amt_reim)) +
  geom_point(aes(col = Product)) + scale_size_continuous(range = c(1,15))+
  labs(title = "Top ten drugs costing the taxpayers in TN", x = "Drugs_name",
       y = "Total_cost") +
  theme(title = gray.bold.italic.16.text, axis.title = black.bold.italic.16.text) +
  scale_y_continuous(name = "Total cost (billions)",
                     breaks = c(50000000,100000000,200000000),
                     labels = c(0.05,0.1,0.2)) + theme(legend.position="none")

# making a dataframe for disease and product
dis_prod <- data.frame('Product'=c('HUMIRA - A','METHYLPHEN','INVEGA SUS','TRUVADA', 'HARVONI',
                                   'ABILIFY', 'ADDERALL X','ADVAIR DIS','SOVALDI 40', 'FLOVENT HF','VYVANSE',
                                   'PROVENTIL','LANTUS','FOCALIN XR','HUMALOG','LYRICA','SEROQUEL X'),
                       'Dis'=c('Arthritis','ADHD','Schizophrenia', 'AIDS','HCV', 'Schizophrenia', 'ADHD','Asthma', 'HCV', 'Asthma',
                               'BED', 'Asthma', 'Diabetes','ADHD','Diabetes', 'Fibromyalgia','Schizophrenia'),
                       'gen_cost' = c(1789,0.21,1300,7,1094,1,3,5,1000,10,16,5,20,1,22,5,6  ))
dis_prod

# merging data to get an hold of disease status in whole country
top10drugs
top10drugs_dis <- merge(top10drugs,dis_prod, by = "Product" )
# getting Total value with generic cost also
top10drugs_dis_generic <- top10drugs_dis %>% mutate (Tot_gen_cost = tot_unt_reim*gen_cost) %>% 
  select ('Product', 'tot_amt_reim','Dis','Tot_gen_cost')
# plotting data disease wise
top_diseases <- top10drugs_dis_generic %>% group_by(`Dis`) %>%
  summarize(total_cost = sum (`tot_amt_reim`), total_gen_cost = sum(`Tot_gen_cost`))

   top_diseases %>% ggplot(aes(Dis, total_cost, size = total_cost)) +
     geom_point(aes(col = Dis)) +
     scale_size_continuous(range = c(1,15)) + 
     labs(title = "Disease prevalence", x = "Disease_name",
          y = "Total_cost") +
     theme(title = gray.bold.italic.16.text, axis.title = black.bold.italic.16.text) +
     scale_y_continuous(name = "Total cost (billions)",
    breaks = c(2000000000,4000000000,6000000000, 8000000000,10000000000),
    labels = c(2.0,4.0,6.0, 8.0, 10.0)) + theme(legend.position="none")
   
   

   # merging data to get an hold of disease status in TN
   top10drugs_TN
   top10drugs_TN_dis <- merge(top10drugs_TN,dis_prod, by = "Product" )
   top10drugs_TN_dis
   # getting Total value with generic cost also
   top10drugs_TN_dis_generic <- top10drugs_TN_dis %>% mutate (Tot_gen_cost = tot_unt_reim*gen_cost) %>% 
     select ('Product', 'tot_amt_reim','Dis','Tot_gen_cost')
   top10drugs_TN_dis_generic
   # plotting data disease wise
   top_TN_diseases <- top10drugs_TN_dis_generic %>% group_by(`Dis`) %>%
     summarize(total_cost = sum (`tot_amt_reim`), total_gen_cost = sum(`Tot_gen_cost`))
   
   top_TN_diseases %>% ggplot(aes(Dis, total_cost, size = total_cost)) +
     geom_point(aes(col = Dis)) +
     scale_size_continuous(range = c(1,15)) + 
     labs(title = "Disease prevalence in TN", x = "Disease_name",
          y = "Total_cost") +
     theme(title = gray.bold.italic.16.text, axis.title = black.bold.italic.16.text) +
     scale_y_continuous(name = "Total cost (billions)",
                        breaks = c(2000000000,4000000000,6000000000, 8000000000,10000000000),
                        labels = c(2.0,4.0,6.0, 8.0, 10.0)) + theme(legend.position="none")
   
   
# changing the name of columns in top10drugs_dis_generic
   top10drugs_dis_generic
   names(top10drugs_dis_generic)[names(top10drugs_dis_generic) == "tot_amt_reim"] <- "Pres_cost"
   names(top10drugs_dis_generic)[names(top10drugs_dis_generic) == "Tot_gen_cost"] <- "Gen_cost"
  # making single column for pres_cost and gen_cost using gather function
   comp_Prod <- gather(top10drugs_dis_generic, Drug, Cost, c("Pres_cost","Gen_cost"))
   comp_Prod
 # Plotting  box plot showing difference
   ggboxplot(comp_Prod, x = "Drug", y = "Cost",
             color = "Drug", palette = "jco",
             add = "jitter") +
     stat_compare_means(method = "t.test", label.y = 50) +
     labs(title = "Prescription vs Generic") +
     theme(title = gray.bold.italic.16.text, axis.title = black.bold.italic.16.text)+
     scale_y_continuous(name = "Total cost (billions)", breaks = c(2000000000,4000000000,6000000000,
    8000000000,10000000000), labels = c(2.0,4.0,6.0, 8.0, 10.0)) +
     theme(legend.position="none")
     
   
   