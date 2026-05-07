library(readxl)
library(readr)
library(tibble)
library(Momocs)
library(dplyr)
theattempt2<-import_jpg() #The big one 3
nameslistleaf<-as.list(names(theattempt2))
View(nameslistleaf)
#if there are any duplicates or typing error in your labels fix them now.
write.table(nameslistleaf, file = "~/Desktop/Output/nameslistleaf.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))

#============================================================================attach metadata



# Load required libraries
library(dplyr)
library(readxl)  # For reading Excel files
library(readr)   # For writing CSV files

# Set the working directory
setwd("/Users/ryantraband1/Desktop/Output")

# Read the input files
nameslistleaf <- read_excel("nameslistleaf.xlsx", col_names = "SampleID", sheet = "Sheet1")
metadata <- read_csv("Metadata250.csv")

# Step 1: Extract variety name and number from SampleID
# Remove everything after the first dash (e.g., -10001_001)
nameslistleaf <- nameslistleaf %>%
  mutate(VarietyName = sub("^(.*?)-.*", "\\1", SampleID))

# Step 2: Match VarietyName with metadata to assign Category
# Ensure VarietyName in nameslist matches ID in metadata
nameslistleaf <- nameslistleaf %>%
  left_join(select(metadata, ID, Category), by = c("VarietyName" = "ID"))

# Step 3: Write the updated dataframe to a new CSV file
write_csv(nameslistleaf, "nameslistleaf_updated.csv")

# Print a message to confirm completion
cat("Processing complete. Output saved to nameslistleaf_updated.csv\n")

#============================================================================bring the file back
#some editing by hand was done to make this metadata work and labels fit properly
nameslistleafupdated<-read_csv("/Users/ryantraband1/Desktop/Output/nameslistleaf_updated copy2.csv")
okay<-names(theattempt2)
names(tibbleleafbig1[[1]])
write.table(okay, file = "~/Desktop/Output/okay.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))
#============================================================================attach to data


tibbleleafbig1<-tibble(leaf_number=1,SampleID=nameslistleafupdated$SampleID,Variety=nameslistleafupdated$VarietyName,Category=nameslistleafupdated$Category)
dim(tibbleleafbig1)
leafbigout<-Out(theattempt2, fac=tibbleleafbig1)
leafbigout$Variety
#============================================================================run the pca
length(leafbigout)
leafbigout$fac
dim(theattempt2)
dim(tibbleleafbig1)
print(leafbigout[[1]])
efleafbig<-efourier(leafbigout)
efleafbig$fac
print(efleafbig)
pcaleafbig3<-PCA(efleafbig)
pcaleafbig1$fac
pcaleafbig1$rotation
PC.contrib(pcaleafbig3)
PCcontrib(pcaleafbig1)
plot_PCA(pcaleafbig1, ~Category,axes=c(3,4),labelpoints =FALSE,center_origin = TRUE,zoom = 0.5)
plot_PCA(pcaleafbig3, ~Category,axes=c(1,2),labelpoints =FALSE,legend = FALSE,chull = FALSE,points = FALSE,center_origin = TRUE,zoom = 2.0, morphospace = TRUE)
?plot_PCA
plot_LDA(pcaleafbig3, ~Category,center_origin = TRUE,title = "LDA")
?plot_LDA
