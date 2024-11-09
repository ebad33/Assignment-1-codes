# "readr" package was installed to read the data obtained from BOLD database

install.packages("readr")
library(readr)

# dfBOLD was used to store the data obtained from BOLD

dfBOLD <- read_tsv(file = "../data/Bryzoa_BOLD_data.tsv")

#set.seed was utilized to get consistent "runs"

set.seed(123)

#Section 1: during this section data of bryozoan species was filtered, through using common statistics. Also, filtered the countries and names of species of the target organism.

names(dfBOLD)
summary(dfBOLD)
length(dfBOLD)
dfBOLD.sub <- dfBOLD[, c("processid", "bin_uri", "species_name", "country", "lat", "lon")]
dfBOLD.sub
my.table <- table(dfBOLD.sub$country)
my.table 
names(my.table)
mean(my.table)
median(my.table)
range(my.table)
my.table[9]
my.table[55]

#Barplot was generated of Canada and United States and their respective "Bin" numbers, to see how many bins each country had.

my.table <- data.frame(
  country = c("Canada", "United States"),
  count = c(583, 715)
)
barplot(height = my.table$count, 
        names.arg = my.table$country, 
        xlab = "Country", 
        ylab = "Count of BOLD Records per Country", 
        col = "lightgreen",
        main = "BOLD Records by Country")


#The Bryozoan species in obtained from the BOLD data base was filtered out and barplot was generated to see the names and amount of particular species in the BOLD data base. Basic statistics were also conducted.

my.table <- table(dfBOLD.sub$species_name)
my.table
barplot(my.table,
        main = "Bold Record of Bryozoan Species",
        xlab = "Bryozoan Species",
        ylab = "Number of Bold Records")
names(my.table)
mean(my.table)
median(my.table)
range(my.table)

#Section 2: During this section latitude and longitude of the data was generated,so we can filter the lat and long of Canada and United States.


my.table <- table(dfBOLD.sub$bin_uri)
my.table
my.table <-table(dfBOLD.sub$lat)        
my.table
my.table <- table(dfBOLD.sub$lon)        
my.table
library(dplyr)

dfBOLD_United_States <- dfBOLD.sub %>%
  filter(country == "United States")

table_lat <- table(dfBOLD_United_States$lat)

table_lat
library(dplyr)

dfBOLD_United_States <- dfBOLD.sub %>%
  filter(country == "United States")

table_lon <- table(dfBOLD_United_States$lon)

table_lon

#The BOLD records of Bryozoan species per country, who had high abundace was sorted from highest to lowest, to get a good idea of which country had high abundance. I had figured it was United States and Canada, however I wanted to make sure.

plot(sort(table(dfBOLD.sub$country), decreasing = TRUE)[1:5],
     main = "Bold Records of Bryozoan Species per Country",
     xlab = "Countries",
     ylab = "Number of Bold Records")

# A table was generated in effort to filter the Bryozoan species with their respective conutries to further filter out the data to answer my research question.

y <- table(dfBOLD.sub$country,dfBOLD.sub$species_name)
y
dfBOLD_canada <- dfBOLD.sub[dfBOLD.sub$country =="Canada",] 
library(dplyr)
dfBOLD_canada <- dfBOLD.sub %>% filter(country == "Canada")
ls()
y <- table(dfBOLD_canada$species_name)
y

#The Abundance of North American Bryozoan spcies were graphed.

barplot(y,
        main = "The Abundance of Bryozoan Species",
        xlab = "Bryozoan Species",
        ylab = "The Abundance")
counts <- c(9, 6, 4, 3, 1)
names <- c("Membranipora membranacea", "Primavelans insculpta", "Phidolopora pacifica", "Schizoporella japonica", "Tubulipora tuba")

# Section 3: During this section Bryozoan species found in Canada were plotted using ggplot. This plot highlights the species found in highhest abundance in Canada, and also, the species that contained bioactive compounds,which contribute to cancer. The first three species are highlighted as contribution to cancer treatment.


library(ggplot2)
df <- data.frame(
  species = c("Membranipora membranacea", "Primavelans insculpta", "Phidolopora pacifica", "Schizoporella japonica", "Tubulipora tuba"),
  
  count = c(9,6,4,3,1),
  contribution = c("Cancer contribution", "Cancer contribution", "Cancer contribution", "Non Cancer contribution", "Non Cancer contribution")
)
ggplot(df,aes(reorder(species, count), y = count, fill = contribution)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Bryozoan Species Found in Canada", 
    x = "Bryozoan Species",
    y = "Number of Species"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20,face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.5,"lines")
  ) +
  scale_fill_manual(values = c("lightgreen", "lightblue"))


#  The Countries were filtered out and then particular data pertaining to United States, such as the species found and their abundance was filtered out. Then barplot was generated to see the highest abundance of the target species found in United States and also highlighting the species that contribute to cancer treatment. The first three species were highlighted.

        
unique(dfBOLD$country)
dfBOLD_United_States <- dfBOLD.sub[dfBOLD.sub$country =="United States",]
library(dplyr)
dfBOLD_United_States <- dfBOLD.sub %>% filter(country == "United States")
ls()
y <- table(dfBOLD_United_States$species_name)
y
counts <- c(85,35,33,29,25)
names <- c("Watersipora subtorquata","Membranipora chesapeakensis","Membranipora membranacea","Watersipora sp. COI group A","Watersipora sp. Santa Cruz Harbour ")

library(ggplot2)
df <- data.frame(
species = c("Watersipora subtorquata","Membranipora chesapeakensis","Membranipora membranacea","Watersipora sp. COI group A","Watersipora sp. Santa Cruz Harbour"),

count = c(85,35,33,25,29),
contribution = c("Cancer contribution", "Cancer contribution", "Cancer contribution", "Non Cancer contribution", "Non Cancer contribution")
)
ggplot(df,aes(reorder(species, count), y = count, fill = contribution)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Bryozoan Species Found in United States", 
    x = "Bryozoan Species",
    y = "Number of Species"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20,face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.5,"lines")
  ) +
  scale_fill_manual(values = c("lightgreen", "lightblue"))


#Section 4: During this section the Bryozoan species from Canada and United States, which also contribute to cancer treatment as found was the previous graphs plotted, were graphed together. This was done in effort to compare between the two countries the species in abundance and to answer my research question in a more clearer way. The particular abundance of target species found in United states and Canada can now be clearly intreperated.


dfBOLD.sub <- data.frame(
  species_name = c("Membranipora membranacea","Membranipora membranacea","Watersipora subtorquata","Primavelans insculpta"),
  country = c("United States", "Canada", "United States","Canada"),
  contribution = c(35,9,85,6))

library(dplyr)
df_combined <- dfBOLD.sub %>%
  filter(country %in% c("United States", "Canada")) %>%
  group_by(species_name, country) %>%
  summarise(contribution = sum(contribution)) %>%
  ungroup()

library(ggplot2)

ggplot(df_combined, aes(x = "species name",y = contribution,fill = country)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),width = 0.4) +
  labs(title = "The Abundance of Membranipora membranacea Species Found in United States and Canada",
       x = "Membranipora membranacea",
       y = "Number of species") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
  )

# A stacked bar graph was generated to make the intreperation even more precice and clear, as the graph indicated United States has a higher abundance of target species, which contribute to cancer treatment.

ggplot(df_combined, aes(x = species_name, y = contribution, fill = country)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Cancer Contributing Bryozoan Species Found in United States and Canada",
    x = "Bryozoan Species",
    y = "Number of Species"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.5,"lines")
  ) +
  coord_flip()  


#Section 5: Since we found that United States has a higher abundance in comparison to Canada, map plot was generated to see which part of the country the species are found and in what abundance.


library(ggplot2)
library(maps)

us_map <- map_data("state")

df <- data.frame(
  species = c("Watersipora subtorquata", "Membranipora chesapeakensis", 
              "Membranipora.mem", "Watersipora sp. COI group A", 
              "Watersipora sp. Santa Cruz Harbour"),
  count = c(85, 35, 33, 25, 29),
  contribution = c("Cancer contribution", "Cancer contribution", 
                   "Cancer contribution", "Non Cancer contribution", 
                   "Non Cancer contribution"),
  lat = c(37.7749, 34.0522, 40.7128, 36.1699, 39.7392),
  lon = c(-122.4194, -118.2437, -74.0060, -115.1398, -104.9903) 
)

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgreen", color = "white") +
  geom_point(data = df, aes(x = lon, y = lat, color = contribution, size = count), alpha = 0.7) +
geom_text(data = df, aes(x =lon,y = lat, label = species), hjust = -0.1, vjust = -0.5, size = 4.5) +
  scale_size(range = c(3, 8)) + 
  labs(
    title = "Distribution of Bryozoan Species in the United States",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
  ) +
  scale_color_manual(values = c("blue", "red"))








