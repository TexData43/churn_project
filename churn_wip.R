churn_read <- read.csv("churn_data.csv")

# rename columns becuase I don't like CamelCase
# long live snake_case

colnames(churn_read) <- to_any_case(colnames(churn_read), case = "snake")
#churn_df <- churn_df %>% mutate(monthly_spend = monthly_charges, tenure_length = tenure)

# generate a duplicate column of monthly_spend
churn_read <- churn_read %>% mutate(monthly_spend = monthly_charges, tenure_segment = tenure, total_charges = monthly_charges*tenure)

churn_df <- churn_read

churn_df2 <- churn_df 


glimpse(churn_df)

# apply tenure_seg function on each row of dataframe
# importantly, I tried doing this with 

# churn_df$tenure_segment <- sapply(churn_df$tenure_segment, tenure_seg)
# churn_df$tenure_segment <- sapply(churn_df$tenure,tenure_seg)

tenure_seg <- function(tenure){
  if (tenure >= 0 && tenure <= 12) {
    return('0-12 Month')
  }else if (tenure > 12 && tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 && tenure <= 36){
    return('24-36 Month')
  }else if (tenure > 36 && tenure <= 48){
    return('36-48 Month')
  }else if (tenure > 48 && tenure <= 60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

churn_df$tenure_segment <- churn_df$tenure_segment %>% map_chr(tenure_seg) 
churn_df$tenure_segment <- as.factor(churn_df$tenure_segment)
churn_df$tenure_segment <- factor(churn_df$tenure_segment, levels(churn_df$tenure_segment)[c(2, 7, 3:6, 1)])

churn_df2$tenure_segment <- cut(churn_df2$tenure_segment, 
                                breaks = c(0, 12, 24, 36, 48, 60, Inf), 
                                labels = c('0-12 Month', 
                                           '12-24 Month', 
                                           '24-36 Month', 
                                           '36-48 Month', 
                                           '48-60 Month', 
                                           '> 60 Month'))

churn_df <- churn_read

churn_df <- churn_df %>% 
  mutate(
    # use purrr to apply my segment function and create a new column
    tenure_segment = map_chr(tenure, tenure_seg),
    # convert the text into a factor
    tenure_segment = factor(tenure_segment),
    # and correctly order the levels of this factor
    tenure_segment = fct_relevel(tenure_segment, 
                                 '0-12 Month', 
                                 '12-24 Month', 
                                 '24-36 Month', 
                                 '36-48 Month', 
                                 '48-60 Month',
                                 '> 60 Month'))


levels(churn_df$tenure_segment)
#churn_df2 <- churn_df 

churn_df$tenure_segment <- sapply(churn_df$tenure,tenure_seg)
#churn_df2[[23]] <- churn_df2[[23]] %>% map_chr(tenure_seg) 

month_seg <- function(monthly_charges){
  if (monthly_charges >= 0 && monthly_charges <= 40){
    return('Low Spend')
  }else if(monthly_charges > 40 && monthly_charges <= 80){
    return('Medium Spend')
  }else if (monthly_charges >= 80 && monthly_charges > 80){
    return('High Spend')
  }
}

# apply tenure_seg function on each row of dataframe
# churn_df$monthly_charges <- sapply(churn_df$monthly_charges, month_seg)
# When I first tried to do this, I 
churn_df$monthly_spend <- sapply(churn_df$monthly_charges,month_seg)
churn_df$monthly_spend <- as.factor(churn_df$monthly_spend)
churn_df$monthly_spend <- factor(churn_df$monthly_spend, levels(churn_df$monthly_spend)[c(2, 3, 1)])

churn_df <- churn_read

churn_df <- churn_df %>% 
  mutate(monthly_segment = map_chr(monthly_charges, month_seg), 
         monthly_segment = factor(monthly_segment),
         monthly_segment = fct_relevel(monthly_segment, 
                                       'Low Spend', 
                                       'Medium Spend', 
                                       'High Spend'))

churn_df2$monthly_spend <- cut(churn_df2$monthly_spend, 
                               breaks = c(0, 40, 80, Inf), 
                               labels = c('Low Spend', 'Medium Spend', 'High Spend'))

head(churn_df2$monthly_spend)