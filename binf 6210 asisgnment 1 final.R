# Editor: small grammatical fixes were done throughout the code comments

library(readr)
library(dplyr)
library(ggplot2) 
library(maps) #Editor: added all libraries to the top of the file, pulled from throughout the code repeatedly, redundant. Also removed the code for install, can trust those using your code can intuit that.

# dfBOLD was used to store the data obtained from BOLD

dfBOLD <- read_tsv(file = "Bryzoa_BOLD_data.tsv")

#set.seed was utilized to get consistent "runs"

set.seed(123)

checkdata <- function(x) {
  print("names")
  print(names(x))
  print("range")
  print(range(x))
  print("mean")
  print(mean(x))
  print("median")
  print(median(x))
}
#Editor: this check was done multiple times in the code, added a function for readability and ease of use

top5 <- function(x, y) {
  x[order(x$y, decreasing = TRUE),]
}

#Section 1: during this section data of Bryozoan species was filtered, through using common statistics. Also, filtered the countries and names of species of the target organism.

#Editor: several instances of 'my.table' as identifier names, going down the code I have given them clearer, individual names to reduce confusion.

names(dfBOLD)
summary(dfBOLD)
length(dfBOLD)
dfBOLD.sub <- dfBOLD[, c("processid", "bin_uri", "species_name", "country", "lat", "lon")] |>
  filter(!is.na(species_name)) |>
  filter(!is.na(country))
#added filtering steps to ensure data is clean and representative
dfBOLD.sub
country.table <- table(dfBOLD.sub$country)
country.table
checkdata(country.table)
country.table[8]
country.table[47]
#when removing only the species names and countries which are listed as NA the number of samples in Canada drops drastically, this may change downstream analysis but will give more proportional results

#Barplot was generated of Canada and United States and their respective "Bin" numbers, to see how many bins each country had.

us.canada.comp <- data.frame(
  country = c("Canada", "United States"),
  count = c(75, 579)
)
barplot(height = us.canada.comp$count, 
        names.arg = us.canada.comp$country, 
        xlab = "Country", 
        ylab = "Count of BOLD Records per Country", 
        col = "lightgreen",
        main = "BOLD Records by Country")


#The Bryozoan species in obtained from the BOLD data base was filtered out and barplot was generated to see the names and amount of particular species in the BOLD data base. Basic statistics were also conducted.

species.table <- table(dfBOLD.sub$species_name)
species.table
barplot(species.table,
        main = "Bold Record of Bryozoan Species",
        xlab = "Bryozoan Species",
        ylab = "Number of Bold Records")
checkdata(species.table)

#Section 2: During this section latitude and longitude of the data was generated,so we can filter the lat and long of Canada and United States.

bin.table <- table(dfBOLD.sub$bin_uri)
bin.table
lat.table <-table(dfBOLD.sub$lat)        
lat.table
lon.table <- table(dfBOLD.sub$lon)        
lon.table

dfBOLD_United_States <- dfBOLD.sub %>%
  filter(country == "United States")

table_lat <- table(dfBOLD_United_States$lat)

table_lat
#Editor: deleted redundant code, variable is already established

table_lon <- table(dfBOLD_United_States$lon)

table_lon

#The BOLD records of Bryozoan species per country, who had high abundance was sorted from highest to lowest, to get a good idea of which country had high abundance. I had figured it was United States and Canada, however I wanted to make sure.

plot(sort(table(dfBOLD.sub$country), decreasing = TRUE)[1:5],
     main = "Bold Records of Bryozoan Species per Country",
     xlab = "Countries",
     ylab = "Number of Bold Records")

# A table was generated in effort to filter the Bryozoan species with their respective countries to further filter out the data to answer my research question.

filter.species <- table(dfBOLD.sub$country,dfBOLD.sub$species_name)
filter.species
dfBOLD_canada <- dfBOLD.sub %>% filter(country == "Canada")
ls()
species.table.can <- table(dfBOLD_canada$species_name)
species.table.can
#Editor: removed first dfBOLD_canada filter as the first one retained the NA files

#The Abundance of North American Bryozoan species were graphed.

barplot(species.table.can,
        main = "The Abundance of Bryozoan Species in Canada", #more representative name
        xlab = "Bryozoan Species",
        ylab = "The Abundance")
counts <- c(9, 6, 4, 3, 1)
names <- c("Membranipora membranacea", "Primavelans insculpta", "Phidolopora pacifica", "Schizoporella japonica", "Tubulipora tuba")

species_count_canada <- dfBOLD_canada %>%
  group_by(species_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(species_count_canada)[1:5,]
#Editor: pulling the organized number of species by most abundant, there is a difference than the ones used in downstream analysis. As the cancer/noncancer status was determined externally, I will not be able to update the status of these species moving forward but it will need to be done if the data is to be implemented.

# Section 3: During this section Bryozoan species found in Canada were plotted using ggplot. This plot highlights the species found in highest abundance in Canada, and also, the species that contained bioactive compounds,which contribute to cancer. The first three species are highlighted as contribution to cancer treatment. 

dfSpecies <- data.frame(
  species = c("Membranipora membranacea", "Primavelans insculpta", "Heteropora pacifia", "Alcyonidium pedunculatum", "Dendrobeania murrayana"),
  count = c(9,6,6,4,4),
  contribution = c("Cancer contribution", "Cancer contribution", "Unknown", "Unknown", "Unknown")
)

ggplot(dfSpecies,aes(reorder(species, count), y = count, fill = contribution)) +
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
dfBOLD_United_States <- dfBOLD.sub %>% filter(country == "United States") #SAMESAME
ls()
table.species.us <- table(dfBOLD_United_States$species_name)
table.species.us
counts <- c(85,35,33,29,25)
names <- c("Watersipora subtorquata","Membranipora chesapeakensis","Membranipora membranacea","Watersipora sp. COI group A","Watersipora sp. Santa Cruz Harbour ")

#Editor: removed the first dfBOLD_United_States code as it wasn't used and contained NA values

dfSpeciesUS <- data.frame(
speciesUS = c("Watersipora subtorquata","Membranipora chesapeakensis","Membranipora membranacea","Watersipora sp. COI group A","Watersipora sp. Santa Cruz Harbour"),

countUS = c(85,35,33,25,29),
contributionUS = c("Cancer contribution", "Cancer contribution", "Cancer contribution", "Non Cancer contribution", "Non Cancer contribution")
)
ggplot(dfSpeciesUS,aes(reorder(speciesUS, count), y = countUS, fill = contributionUS) +
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


#Section 4: During this section the Bryozoan species from Canada and United States, which also contribute to cancer treatment as found was the previous graphs plotted, were graphed together. This was done in effort to compare between the two countries the species in abundance and to answer my research question in a more clearer way. The particular abundance of target species found in United states and Canada can now be clearly interpreted.


dfBOLD.sub.mod <- data.frame(
  species_name = c("Membranipora membranacea","Membranipora membranacea","Watersipora subtorquata","Primavelans insculpta"),
  country = c("United States", "Canada", "United States","Canada"),
  contribution = c(35,9,85,6))

df_combined <- dfBOLD.sub.mod %>%
  filter(country %in% c("United States", "Canada")) %>%
  group_by(species_name, country) %>%
  summarise(contribution = sum(contribution)) %>%
  ungroup()

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

us_map <- map_data("state")

dfUS <- data.frame(
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
  geom_point(data = dfUS, aes(x = lon, y = lat, color = contribution, size = count), alpha = 0.7) +
geom_text(data = dfUS, aes(x =lon,y = lat, label = species), hjust = -0.1, vjust = -0.5, size = 4.5) +
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
