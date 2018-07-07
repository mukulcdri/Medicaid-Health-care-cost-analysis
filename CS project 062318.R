library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggvis)
pop_17 <- read_csv('data/State_pop_2017.csv')
# reading 2017 data
df_17 <- read_csv('data/State_Drug_Utilization_Data_2017.csv')
# EDA on the data
dim(df_17)
names(df_17)
glimpse(df_17)
summary(df_17)
#Units reimbursed, no. of prescriptions, total amount reimbursed, 
# medicaid amount reimbursed have lots of NA and extreme values
head(df_17)
#utilization type 2 values FFSU: fee for service, MCOU: managed care organization
# subsetting data keeping state, quarter, product, units reimbursed no. of prescription total amount prescribed medicaisd and non-medicaid
df_17_sub1 <- df_17[c(2,6,7,8,10,11,12, 13, 14)]
names(df_17_sub1)
summary(df_17_sub1)
dim(df_17_sub1)
boxplot(df_17_sub1$`Units Reimbursed`)
head(df_17_sub1, n = 25)
# removing rows with na (same in units reimbursed, prescriptions, total amount)
df_17_sub1_nona <- na.omit(df_17_sub1)
dim(df_17_sub1_nona)
names(df_17_sub1_nona)
summary(df_17_sub1_nona)
# getting data for states
df_17_ST <- df_17_sub1_nona %>%
  filter(`State` != "XX") %>%
  group_by( `State`, `Year`) %>%
  summarize(tot_amt_reim = sum(`Total Amount Reimbursed`),
            tot_unt_reim = sum(`Units Reimbursed`),
            tot_nm_pres = sum(`Number of Prescriptions`)) %>%
  arrange (desc(`tot_amt_reim`))
head(df_17_ST)
# merging population data on State
df_17_ST_pop <- merge(df_17_ST,pop_17,by = "State")
df_17_ST_pop
# getting per capita
df_17_per_cap <- df_17_ST_pop %>% mutate(tot_amt_per_cap = tot_amt_reim/Population) %>% arrange(desc(`tot_amt_per_cap`))
df_17_per_cap
# Top10 sick states as per per capita (They r on top)
Bottom_10_17 <- head(df_17_per_cap, 10)
Bottom_10_17
Bottom_10_17_ord <- Bottom_10_17 <- Bottom_10_17[order(Bottom_10_17$tot_amt_per_cap), ]
# making a graph ordered bar chart
library(ggplot2)
theme_set(theme_bw())
# Draw plot
ggplot(Bottom_10_17, aes(x = State, y = tot_amt_per_cap, color = State,
        size = tot_amt_reim)) + geom_point()
ggplot(Bottom_10_17_ord, aes(reorder( State, tot_amt_per_cap),tot_amt_per_cap)) +
  geom_col(width=.5, fill="tomato3") 
# Top10 healthy states as per per capita
Top_10_17 <- tail(df_17_per_cap, 10)
Top_10_17 %>% ggvis(~State, ~tot_amt_per_cap, size = ~Population, fill = ~State) %>%
  layer_points
ggplot(Top_10_17, aes(reorder( State, tot_amt_per_cap),tot_amt_per_cap)) +
  geom_col(width=.5, fill="darkblue") 
# getting data for states for products
  df_17_ST_Product <- df_17_sub1_nona %>%filter(`State` != "XX") %>%
  group_by( `Product Name`,`Year`) %>%
  summarize(tot_amt_reim = sum(`Total Amount Reimbursed`),
            tot_unt_reim = sum(`Units Reimbursed`),
            tot_nm_pres = sum(`Number of Prescriptions`)) %>%
  arrange(desc(`tot_amt_reim`))
head(df_17_ST_Product)
dim(df_17_ST_Product)
ggplot(df_17_ST_Product, aes(reorder( Product, tot_amt_reim),tot_amt_reim)) +
  geom_col(width=.5, fill="darkblue")
#changing Product_Name as Product
names(df_17_ST_Product)[names(df_17_ST_Product) == "Product Name"] <- "Product"
head(df_17_ST_Product, 10)
# making a dataframe for disease and product
dis_prod <- data.frame('Product'=c('HUMIRA - A','METHYLPHEN','INVEGA SUS','GENVOYA','UNKNOWN',
                                   'EPCLUSA  4','ZEPATIER 1', 'VENTOLIN H','TRUVADA', 'HARVONI' ),
                       'Dis'=c('Arthritis','ADHD','Schizophrenia','AIDS','UNKNOWN', 'HCV', 'HCV',
                               'Asthma', 'AIDS','HCV' ))
# merging data
df_17_ST_Prod_dis <- merge(df_17_ST_Product,dis_prod, by = "Product" )
df_17_ST_Prod_dis
# Group by disesase
df_dis_2017 <- df_17_ST_Prod_dis %>% group_by(`Dis`) %>% summarize(Tot_amt = sum(`tot_amt_reim`)) %>%
  arrange(desc(Tot_amt)) 
df_dis_2017
# Drawing a graph
ggplot(df_dis_2017, aes(reorder( Dis, Tot_amt),Tot_amt)) +
  geom_col(width=.5, fill="red")

  

