## Get basic information from txt MQ folder ------

# Load packages ----

library(tidyverse)

# Load data ----

ptg <- read.delim("proteinGroups.txt")
evi <- read.delim("evidence.txt")
pept <- read.delim("peptides.txt")
msms <- read.delim("msms.txt")

# Get number of protein groups ----

dim(ptg)[1]
table(ptg$Reverse)

scrampr <- filter(ptg,
                  str_detect(Protein.IDs, "SCRAMBLED"))

length(scrampr$Protein.IDs)

# Get number of identified peptides ----

dim(pept)[1]

names(pept)
table(pept$Reverse)
table(evi$Reverse)

table(str_detect(evi$Proteins, "SCRAMBLED"))

