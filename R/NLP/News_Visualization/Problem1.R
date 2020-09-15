library(dplyr)
library(ggplot2)
Dataset <- datasets_494724_1248313_COVID19_line_list_data #rename the variable of the dataset

# China

China <- Dataset %>%  filter(country == 'China')


ggplot(China, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as avearge and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.

# Japans

Japan <- Dataset %>%  filter(country == 'Japan')
Japan <- Japan[!is.na(Japan$gender),] #Remove NA of gender


ggplot(Japan, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as avearge and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.


# South Korea

South_Korea <- Dataset %>%  filter(country == 'South Korea')
South_Korea<- South_Korea[!is.na(South_Korea$gender),] #Remove NA of gender


ggplot(South_Korea, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as avearge and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.

# U.S
US <- Dataset %>%  filter(country == 'USA')
US <- US[!is.na(US$gender),] #Remove NA of gender


ggplot(US, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as avearge and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.

#France

France <- Dataset %>% filter(country == 'France')
France <- France[!is.na(France$gender),] #Remove NA of gender


ggplot(France, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as avearge and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.

# Singapore

Singapore <- Dataset %>% filter(country == 'Singapore')
Singapore <- Singapore[!is.na(Singapore$gender),] #Remove NA of gender


ggplot(Singapore, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as avearge and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.

# Iran

Iran <- Dataset %>%  filter(country == 'Iran')
Iran$gender <- 'Patient' #gender of Iran dataset is all NA, so NA needs to be changed with another name
sum(is.na(Iran$age)) #all age data of Iran is NA

ggplot(Iran, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as avearge and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.

#Because of two problems of gender and age indicated above, a proper graph cannot be created

# Spain

Spain <- Dataset %>%  filter(country == 'Spain')
Spain <- Spain[!is.na(Spain$gender),]
ggplot(Spain, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as avearge and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.

# Hong Kong

HK <- Dataset %>%  filter(country == 'Hong Kong')

ggplot(HK, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as average and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.

# Germany

Germany <- Dataset %>%  filter(country == 'Germany')
Germany <- Germany[!is.na(Germany$gender),] # remove NA from gender
ggplot(Germany, aes(x=gender, y=age, color=gender)) +  #x-axis variable is gender and y-axis variable is age # Set the colors by gender
  theme_bw() + #Set the classic dark-on-light ggplot2 theme
  scale_fill_manual(guide = FALSE) + #Set color
  ggtitle("Box plot for distribution of Patient age by each gender in China") + #Writing a title of the graph
  geom_violin() + #Making a graph as violin graphs
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) #Set the y-axis variable as average and expressing it as a dot on the graph. The shape and size of it is 23 and 2 respectively.
