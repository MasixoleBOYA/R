#Imported the two datasets for the two variables
df_income <- read.csv("C:/Users/STAFF/Desktop/Data Science/Viz/Assignment/Actual_data/cleaned_gdp_pcap.csv")
df_population <- read.csv("C:/Users/STAFF/Desktop/Data Science/Viz/Assignment/Actual_data/urban_population_percent_of_total.csv")

#Installed all necessary packages
install.packages('dplyr')
install.packages('ggplot2')
install.packages('viridis')
install.packages('ggpubr')
install.packages('stringr')

library(dplyr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(stringr)

#Hanlding missing values
missing_values_income <- sapply(df_income, function(x) sum(is.na(x)))
cat("The Sum of missing of the missing values in the Income dataset columns: ", missing_values_income, "\n")

missing_values_population <- sapply(df_population, function(x) sum(is.na(x)))
cat("The sum of missing values in the Population dataset: ", missing_values_population, "\n")

class(df_income) #ensuring it is a dataframe
names(df_income)

#Extracting only the columns for the year 2021
df1 <- df_income[c("country", "X2021")] 
df2 <- df_population[c("country", "X2021")]

#merging the two dataframes so I can plot easily
merged_df <- merge(df1, df2, by = "country")

names(merged_df)
names(merged_df) <- c("country", "df_income$2021", "df_population$2021")
View(merged_df)

# Summary statistics
summary_stats <- summary(merged_df)
summary_stats

# Created a data frame to allow me to easily map the countries to its continents
country_to_continent <- c(
  'Afghanistan' = 'Asia',
  'Angola' = 'Africa',
  'Albania' = 'Europe',
  'Andorra' = 'Europe',
  'UAE' = 'Asia',
  'Argentina' = 'South America',
  'Armenia' = 'Asia',
  'Antigua and Barbuda' = 'North America',
  'Australia' = 'Oceania',
  'Austria' = 'Europe',
  'Azerbaijan' = 'Asia',
  'Burundi' = 'Africa',
  'Belgium' = 'Europe',
  'Benin' = 'Africa',
  'Burkina Faso' = 'Africa',
  'Bangladesh' = 'Asia',
  'Bulgaria' = 'Europe',
  'Bahrain' = 'Asia',
  'Bahamas' = 'North America',
  'Bosnia and Herzegovina' = 'Europe',
  'Belarus' = 'Europe',
  'Belize' = 'North America',
  'Bolivia' = 'South America',
  'Brazil' = 'South America',
  'Barbados' = 'North America',
  'Brunei' = 'Asia',
  'Bhutan' = 'Asia',
  'Botswana' = 'Africa',
  'Central African Republic' = 'Africa',
  'Canada' = 'North America',
  'Switzerland' = 'Europe',
  'Chile' = 'South America',
  'China' = 'Asia',
  "Cote d'Ivoire" = 'Africa',
  'Cameroon' = 'Africa',
  'Congo, Dem. Rep.' = 'Africa',
  'Congo, Rep.' = 'Africa',
  'Colombia' = 'South America',
  'Comoros' = 'Africa',
  'Cape Verde' = 'Africa',
  'Costa Rica' = 'North America',
  'Cuba' = 'North America',
  'Cyprus' = 'Asia',
  'Czech Republic' = 'Europe',
  'Germany' = 'Europe',
  'Djibouti' = 'Africa',
  'Dominica' = 'North America',
  'Denmark' = 'Europe',
  'Dominican Republic' = 'North America',
  'Algeria' = 'Africa',
  'Ecuador' = 'South America',
  'Egypt' = 'Africa',
  'Eritrea' = 'Africa',
  'Spain' = 'Europe',
  'Estonia' = 'Europe',
  'Ethiopia' = 'Africa',
  'Finland' = 'Europe',
  'Fiji' = 'Oceania',
  'France' = 'Europe',
  'Micronesia, Fed. Sts.' = 'Oceania',
  'Gabon' = 'Africa',
  'UK' = 'Europe',
  'Georgia' = 'Asia',
  'Ghana' = 'Africa',
  'Guinea' = 'Africa',
  'Gambia' = 'Africa',
  'Guinea-Bissau' = 'Africa',
  'Equatorial Guinea' = 'Africa',
  'Greece' = 'Europe',
  'Grenada' = 'North America',
  'Guatemala' = 'North America',
  'Guyana' = 'South America',
  'Hong Kong, China' = 'Asia',
  'Honduras' = 'North America',
  'Croatia' = 'Europe',
  'Haiti' = 'North America',
  'Hungary' = 'Europe',
  'Indonesia' = 'Asia',
  'India' = 'Asia',
  'Ireland' = 'Europe',
  'Iran' = 'Asia',
  'Iraq' = 'Asia',
  'Iceland' = 'Europe',
  'Israel' = 'Asia',
  'Italy' = 'Europe',
  'Jamaica' = 'North America',
  'Jordan' = 'Asia',
  'Japan' = 'Asia',
  'Kazakhstan' = 'Asia',
  'Kenya' = 'Africa',
  'Kyrgyz Republic' = 'Asia',
  'Cambodia' = 'Asia',
  'Kiribati' = 'Oceania',
  'St. Kitts and Nevis' = 'North America',
  'South Korea' = 'Asia',
  'Kuwait' = 'Asia',
  'Lao' = 'Asia',
  'Lebanon' = 'Asia',
  'Liberia' = 'Africa',
  'Libya' = 'Africa',
  'St. Lucia' = 'North America',
  'Sri Lanka' = 'Asia',
  'Lesotho' = 'Africa',
  'Lithuania' = 'Europe',
  'Luxembourg' = 'Europe',
  'Latvia' = 'Europe',
  'Morocco' = 'Africa',
  'Monaco' = 'Europe',
  'Moldova' = 'Europe',
  'Madagascar' = 'Africa',
  'Maldives' = 'Asia',
  'Mexico' = 'North America',
  'Marshall Islands' = 'Oceania',
  'North Macedonia' = 'Europe',
  'Mali' = 'Africa',
  'Malta' = 'Europe',
  'Myanmar' = 'Asia',
  'Montenegro' = 'Europe',
  'Mongolia' = 'Asia',
  'Mozambique' = 'Africa',
  'Mauritania' = 'Africa',
  'Mauritius' = 'Africa',
  'Malawi' = 'Africa',
  'Malaysia' = 'Asia',
  'Namibia' = 'Africa',
  'Niger' = 'Africa',
  'Nigeria' = 'Africa',
  'Nicaragua' = 'North America',
  'Netherlands' = 'Europe',
  'Norway' = 'Europe',
  'Nepal' = 'Asia',
  'Nauru' = 'Oceania',
  'New Zealand' = 'Oceania',
  'Oman' = 'Asia',
  'Pakistan' = 'Asia',
  'Panama' = 'North America',
  'Peru' = 'South America',
  'Philippines' = 'Asia',
  'Palau' = 'Oceania',
  'Papua New Guinea' = 'Oceania',
  'Poland' = 'Europe',
  'North Korea' = 'Asia',
  'Portugal' = 'Europe',
  'Paraguay' = 'South America',
  'Palestine' = 'Asia',
  'Qatar' = 'Asia',
  'Romania' = 'Europe',
  'Russia' = 'Europe',
  'Rwanda' = 'Africa',
  'Saudi Arabia' = 'Asia',
  'Sudan' = 'Africa',
  'Senegal' = 'Africa',
  'Singapore' = 'Asia',
  'Solomon Islands' = 'Oceania',
  'Sierra Leone' = 'Africa',
  'El Salvador' = 'North America',
  'Somalia' = 'Africa',
  'Serbia' = 'Europe',
  'South Sudan' = 'Africa',
  'Sao Tome and Principe' = 'Africa',
  'Suriname' = 'South America',
  'Slovak Republic' = 'Europe',
  'Slovenia' = 'Europe',
  'Sweden' = 'Europe',
  'Eswatini' = 'Africa',
  'Seychelles' = 'Africa',
  'Syria' = 'Asia',
  'Chad' = 'Africa',
  'Togo' = 'Africa',
  'Thailand' = 'Asia',
  'Tajikistan' = 'Asia',
  'Turkmenistan' = 'Asia',
  'Timor-Leste' = 'Asia',
  'Tonga' = 'Oceania',
  'Trinidad and Tobago' = 'North America',
  'Tunisia' = 'Africa',
  'Turkey' = 'Asia',
  'Tuvalu' = 'Oceania',
  'Taiwan' = 'Asia',
  'Tanzania' = 'Africa',
  'Uganda' = 'Africa',
  'Ukraine' = 'Europe',
  'Uruguay' = 'South America',
  'USA' = 'North America',
  'Uzbekistan' = 'Asia',
  'St. Vincent and the Grenadines' = 'North America',
  'Venezuela' = 'South America',
  'Vietnam' = 'Asia',
  'Vanuatu' = 'Oceania',
  'Samoa' = 'Oceania',
  'Yemen' = 'Asia',
  'South Africa' = 'Africa',
  'Zambia' = 'Africa',
  'Zimbabwe' = 'Africa'
)

#Putting the "continent" column into the merged dataframe
merged_df_with_continent <- left_join(merged_df, data.frame(country = names(country_to_continent), continent = country_to_continent), by = "country")
names(merged_df_with_continent)

names(merged_df_with_continent)[2] <- "Income_2021"
names(merged_df_with_continent)[3] <- "Population_2021"

View(merged_df_with_continent)

continent_data <- summarise(
  group_by(merged_df_with_continent, continent),
  Average_Income = mean(`Income_2021`),
  Average_Population = mean(`Population_2021`)
)
continent_data <- continent_data[!is.na(continent_data$continent), ]

# Arranging the data in terms of the Average income, descending order
continent_data <- arrange(continent_data, desc(Average_Income))

# Created a bar plot to rank the continents
ggplot(data = continent_data, aes(x = reorder(continent, Average_Income), y = Average_Income, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Average_Income, 2)), vjust = -0.5, size = 4) + 
  labs(x = "Continent (Descending order)", y = "Average Income (International $)", fill = "Continent") +
  theme_minimal()+
  theme(panel.grid = element_blank(),axis.line = element_line(color = "black", linewidth = 1),
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 14, vjust = 0.5),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"))+ 
  coord_flip() + ggtitle(str_wrap("Ranking each continent by Average Income per person in 2021", width = 50))

ggsave(filename = 'C:/Users/STAFF/Desktop/Data Science/Viz/Assignment/Actual_data/Ranking.png')

#xxxxxxxxxxxxxxxxxx The relationship plot xxxxxxxxxxxxx

install.packages('dplyr')
install.packages('ggplot2')
install.packages('viridis')
install.packages('ggpubr')
install.packages('stringr')
install.packages("RColorBrewer")


library(dplyr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(stringr)
library(RColorBrewer)


names(merged_df) <- c('country', 'Income_2021', 'Population_2021')

merged_df$Income_2021 <- log10(merged_df$Income_2021)
merged_df$Population_2021 <- merged_df$Population_2021

african_countries <- c(
  'Algeria', 'Angola', 'Benin', 'Botswana', 'Burkina Faso', 'Burundi', 'Cabo Verde', 'Cameroon', 'Central African Republic', 'Chad',
  'Comoros', 'Congo, Dem. Rep.', 'Congo, Rep.', 'Cote d\'Ivoire', 'Djibouti', 'Egypt', 'Equatorial Guinea', 'Eritrea', 'Eswatini',
  'Ethiopia', 'Gabon', 'Gambia', 'Ghana', 'Guinea', 'Guinea-Bissau', 'Kenya', 'Lesotho', 'Liberia', 'Libya', 'Madagascar', 'Malawi',
  'Mali', 'Mauritania', 'Mauritius', 'Morocco', 'Mozambique', 'Namibia', 'Niger', 'Nigeria', 'Rwanda', 'Sao Tome and Principe', 'Senegal',
  'Seychelles', 'Sierra Leone', 'Somalia', 'South Africa', 'South Sudan', 'Sudan', 'Tanzania', 'Togo', 'Tunisia', 'Uganda', 'Zambia',
  'Zimbabwe'
)

african_data_2021 <- merged_df[merged_df$country %in% african_countries, ]

lm_model <- lm(Population_2021 ~ Income_2021, data = african_data_2021)
rsquared <- summary(lm_model)$r.squared
pvalue <- format(summary(lm_model)$coefficients[2, 4], digits = 4)

ci <- confint(lm_model)

# Extract the lower and upper bounds of the confidence intervals for Population_2021
ci_lower <- ci["Income_2021", "2.5 %"]
ci_upper <- ci["Income_2021", "97.5 %"]


ggplot(data = african_data_2021, aes(x = Income_2021, y = Population_2021, color = country)) +
  geom_point(alpha = 0.7, size = 5, stroke = 1) +
  geom_smooth(method = 'lm', se = TRUE, color = 'red', size = 1) + # Add correlation line
  scale_color_viridis(discrete = TRUE) +
  labs(title = str_wrap('How Percentage of the population living in urban areas changes for African Countries (Log Scales)',width = 50),
       x = str_wrap('Log Average Income per person for the year 2021 (International $)',width = 30),
       y = str_wrap('Percetage of total Population that lived in urban areas in 2021 (%)',width = 45)) +
  theme_minimal() +
  theme(legend.position = 'right',axis.line = element_line(color = "black"), plot.title = element_text(size = 22, face = "bold"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 14, vjust = 0.5),
        axis.ticks = element_line(color = "black"),  
        axis.ticks.length = unit(0.2, "cm")) +
  guides(color = guide_legend(title = 'Country')) +
  scale_x_continuous(limits = c(min(african_data_2021$Income_2021), max(african_data_2021$Income_2021))) +
  scale_y_continuous(limits = c(0,100), breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  coord_cartesian(expand = FALSE) +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 10))+
  annotate("text", x = 3.2, y = 90, label = paste("R-squared =", round(rsquared, 2)), size = 5, color = 'red') +
  annotate("text", x = 3.2, y = 85, label = paste("p-value =", pvalue), size = 5, color = 'red')+
  annotate("text", x = 4.2, y = 85, label = paste("95% CI:", round(ci_lower, 2), "-", round(ci_upper, 2)), size = 5, vjust = -1)

ggsave(filename = 'C:/Users/STAFF/Desktop/Data Science/Viz/Assignment/Actual_data/Relationship_final.png')


  

