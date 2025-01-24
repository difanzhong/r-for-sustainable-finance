library("bibliometrix")
library("openxlsx")
library("bibliometrixData")
library("readxl")
library("dplyr")
library("stringr")
library(ggplot2)
library(pander)

# 1. convert original data that downloaded from Scopus database to data frame
scopus_data <- convert2df("data/scopus-final.bib", dbsource="scopus", format="bibtex")
dim(scopus_data) #check result

# 2. convert original data that downloaded from Web of Science to data frame
# 2.1. combine partial web of science data into one file
wos_data <- c(readLines("data/wos_final_1.txt"), "\n", readLines("data/wos_final_2.txt"))
writeLines(wos_data, "data/wos_final_combined.txt")

# 2.2. convert the combined file into data frame
wos_final_data <- convert2df("data/wos_final_combined.txt")
dim(wos_final_data) #check result

# 3. combine converted dataframe from both Web of Science and Scopus into one as final
#    with this combination, overlapping data in both Web of Science and Scopus will be merged
combined_final <- mergeDbSources(scopus_data, wos_final_data, remove.duplicated =TRUE)
dim(combined_final) #check result
write.xlsx(
  combined_final[c("PY","TI","AU","AB","SO","UT")], 
  "data/combined_final_data.xlsx"
) #output combined data with specific columns (publication year, authors, title, etc.)

# 4. refine data
# 4.1. remove irrelevant data (reviewed manually) with an array of row number  
data <- combined_final[-c(52,54,82,86,118,139,153,160,165,196,212,232,240,247,251,254,255,269,279,282,286,287,296,300,310,318,320,324,332,344,349,372,375,376,378,381,385,398,406,408,414,444,449,451,458,482,489,499,504,523,527,532,540,541,554,558,572,579,586,588,591,593,594,605,610,611,612,615,617,623,624,625,633,636,638,642,644,645,646,652,656,666,674,681,695,712,713,721,724,733,734,745,746,775,778,779,780,785,788,792,796,813,817,823,827,829,830,844,852,854,864,870,873),]
write.xlsx(
  data[c("PY","TI","AU","AB","SO","UT")], 
  "data/filtered_combined_final_data.xlsx"
) #ouput filtered data

# 4.2. convert publication year format from number to string
data$PY <- as.character(data$PY) 

# 4.3. remove meaningless keywords
data <- data %>%
  mutate(DE = str_remove_all(DE, "G11|Q01|Q56|G23|G28|Q54"))

# 5. ploting
# 5.1. Sankey Diagram
threeFieldsPlot(data, fields = c("DE", "PY", "SC"), n=c(16,20,12))

# 5.2. keywords co-occurrences 
Netmatrix2 = biblioNetwork(data, analysis = "co-occurrences", network = "author_keywords", sep = ";")
networkPlot(Netmatrix2, normalize = "association", weighted = T, n = 50, Title = "Keyword Co-occurrences",
                  type = "auto",size=22,  size.cex=T,label.cex = T, edgesize = 20, labelsize = 8)

# 5.3. thematic map
data$PY <- as.numeric(data$PY)
Map = thematicMap(data, field = "DE", n = 1000, minfreq = 5, stemming = F, size = 0.1,
                  n.labels = 5, repel = TRUE)
plot(Map$map)

# 5.4. Anual Scientific Production 
res1 = biblioAnalysis(data, sep=";")
p1 = plot(res1, pause = FALSE)
p1[[3]] + theme_bw(base_size = 15)