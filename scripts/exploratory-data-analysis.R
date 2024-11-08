

# ---------------- Load libraries for data exploration ------------------------
# the purpose of the exploration is to find trends in the data that 
# could help with later analysis using PCA and CA. For example, we look for 
# trends in methods used for parchment damage assessment (PDA), trends 
# conservator and lab involvement in projects, etc.

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggthemes)

# --------- Set working directory so we can use relative file paths -----------
setwd("C:/Users/chris/Downloads/MappingFieldParchmentDamageAssessment")

# --------------------- Read in data and clean data ----------------------------
#The data exists in an Excel file, so use read.csv().
data <- read.csv("simulated-analysis-data.csv")
data # We noticed that some letters with diacritics did not load in properly, 
# unfortunately, but that is not a big problem with starting data analysis. 

# Check heading names are suitable.
colnames(data) #When importing, R had converted any spaces in our headings into
#dots, which makes the headings look very messy and complicated. Let's rename
#the headings into shorter, sensible headings so that graphing and analysis is
#clearer.

data$Year <- as.numeric(data$Year) 
#define that columns 8 to 49 in our CSV contain the methods - the other columns are irrelevant to us for now.
methods_columns <- c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49)
methods_data <- data[, methods_columns] #putting the data into the columns
methods_data #this outputs all of the columns with methods data, including imaging,thermal, spectroscopy, biological-chemical, and visual.

#Rename the column titles to something more sensible and short.
methods_data <- methods_data %>%
  rename(
    "Light microscopy etc." = `Light..optical..microscopy...polarized.light.microscopy..fluorescence.microscopy..light.transmission.analysis..LTA....thermography..with.a.stereomicroscope.`,
    "NLO microscopy etc." = `Nonlinear.optical..microscopy..NLO.`,
    "CT" = `Computed.tomography...CT.`,
    "OCT" = `Optical.coherence..tomography..OCT.`,
    "AFM etc." = `Atomic.force.microscopy..AFM...including.micro.thermal.analsysis.with.modified.AFM.probe`,
    "Electron microscopy etc." = `Electron.microscope..scanning.electron.microscopy..SEM...and.SEM.with.energy.dispersive.x.ray.spectroscopy..SEM.EDS..aka.EDX.`,
    "MSI" = `Multispectral.imaging..MSI.`,
    "Colourimetry" = `Colourimetric.measurements..CIE.Lab.colour.change..and.RGB.imagery.`,
    "DIC" = `X3D.Digital.Image.Correlation..DIC..for.strain.and.displacement.monitoring`,
    "XPS" = `X.ray.photoelectron.spectroscopy..XPS.`,
    "DSC etc." = `Differential.scanning.calorimetry..DSC...microDSC..and.adiabatic.scanning.calorimetry..ASC.`,
    "DMA etc." = `Dynamic.mechanical.analysis..DMA...tensile.strength.tests`,
    "TG etc." = `Thermogravimetry..thermogravimetric.analysis...TG...and.derivative..thermogravimetry..DTG.`,
    "MHT etc." = `Micro.hot.table...MHT...hot.table.`,
    "RH etc." = `Humidity.sorption..RH...and.hygroscopicity`,
    "DTA" = `Differential.thermal.analysis..DTA.`,
    "IRT" = `Infrared.thermography`,
    "FTIR etc." = `Fourier.transform.infrared.spectroscopy..FTIR....infrared.spectroscopy..Attenuated.total.reflectance.FTIR..ATR.FTIR...synchotron.IR`,
    "RMS etc." = `Raman.spectroscopy..RS...polarised.Raman.spectroscopy..and..Raman.microspectroscopy..RMS.`,
    "FORS" = `Fibre.optic.reflectance.spectroscopy..FORS.`,
    "UV-VIS-NIR spectroscopy etc." = `UV.VIS.NIR.spectroscopy..incl..fluorescence...UV.VIS.spectroscopy..incl..fluorescence...nanoIR.spectroscopy..incl..fluorescence...VIS.spectroscopy..incl..fluorescence..`,
    "XRF etc." = `X.ray.fluorescence..XRF...energy.dispersive.XRF`,
    "NMR spectroscopy" = `Nuclear.magnetic..resonance..NMR..spectroscopy`,
    "XRD etc." = `X.ray.diffraction..XRD....all.angles..e.g.including.wide.angle.X.ray.diffraction..WAXD..`,
    "SF" = `Synchronous.fluorescence..SF.`,
    "LIBS" = `Laser.induced.breakdown.spectroscopy..LIBS.`,
    "SAXS" = `X.ray.scattering..SAXS.`,
    "EPR" = `Electron.paramagnetic.resonance..EPR..spectroscopy..also.called.Electron.spin.spectroscopy..ESR..`,
    "MEES" = `Multiphoton.electron.extraction.spectroscopy`,
    "AAA" = `Amino.acid.analysis..AAA.`,
    "GC etc." = `Gas.chromatography..GC...liquid.chromatography..LC...liquid.chromatography...mass.spectrometry..LC.MS...size.exclusion.chromatography`,
    "MA" = `Microbial.bacterial.analysis...MA...e.g..through.PCR.based.methods.or.in.combination.with.microscopy...and.fungal.analysis`,
    "PH" = `Surface.pH..measurement`,
    "GE" = `Gel.electrophoresis..GE.`,
    "PME" = `Peptide.mass.fingerprinting`,
    "MALDI etc." = `Matrix.assisted.laser.desorption..ionisation.mass.spectrometry..for.proteomics`,
    "LCD" = `Lipid.content.determination.with.gravimetry`,
    "NGS" = `Next.generation.sequencing..NGS...metagenomic.analysis`,
    "EISD" = `Enzymatic.in.solution.digestion`,
    "RP" = `Roughness.profiling`,
    "VA" = `Visual.analysis..unaided.by.equipment..particularly.surface.examination.for.wrinkles.tears.fading.of.inks.and.pigments..texture.assessment..and.colour.change..mechanics..etc..as.observed..by.the.naked.eye.`
  )

#Delete the column about destructive, non-destructive etc. as it is not part of 'methods.'
methods_data <- methods_data %>%
  select(-`Destructive..0...non.destructive..1....invasive..2....non.invasive..3...not.mentioned..4.`)

# So now we have a cleaner version of methods_data to work with.

# --- Exploration 1: Method popularity: Frequency of each damage assessment ---
# ----------------- method across the whole dataset ---------------------------------

#Create a new variable called method_counts that counts every time a cell
#in a method's column says '1.' I.e. if it says 1, count it up!
method_counts <- colSums(methods_data == 1) 

#Sort the counts from greatest to least, so we can see which method is 
#use most often, to least often, in terms of whole numbers.
method_counts_sorted <- sort(method_counts, decreasing = TRUE)
#Print the results
print(method_counts_sorted) #e.g. we are told 'light microscopy etc.' is used 22 times.

#Graph these absolute counts of each method on a bar graph.

plot <- ggplot(data = data.frame(Method = names(method_counts_sorted), Count = method_counts_sorted), 
       aes(x = reorder(Method, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Method usage count (absolute)", x = "Method name", y = "Count")
plot + theme_minimal() 

#Now graph as a percentage, to see how they compare to each other relatively
#E.g. seeing that 80% of studies use on method might indicate it's a preferred
#or standard approach.

total_observations <- nrow(methods_data) #finding total number of rows to figure out what to divide by 
method_counts
method_percentages <- (method_counts / total_observations) * 100 
method_percentages
method_percentages_sorted <- sort(method_percentages, decreasing = TRUE) #sorting from greatest to least percentage of method usage 

print(method_percentages_sorted) #VA is used the most, FTIR the second most, v.v.

#plot it!
percentage_plot <- ggplot(data = data.frame(Method = names(method_percentages_sorted), 
                                            Percentage = method_percentages_sorted), 
                          aes(x = reorder(Method, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Method usage count (percentage)", x = "Method name", y = "Usage percentage (%)") +
  theme_minimal()

percentage_plot

# ------ Exploration 2: Which methods tended to be used together in a paper? ---
# Very few of the methods seen in our CSV or exploration 2 were used on their own
# in a paper. They were often used in conjunction with another or several other techniques
# to derive a parchment damage assessment finding. So we ask, 
# which methods tended to be used together in a paper? 
# Note: using pairwise co-occurances analysis will not work here as
# sometimes methods are not used just in pair, but with two other or
# three other methods. Cluster analysis is better suited for this. 
# Specifically, hierarchical clustering will help us explore
#which methods are used together. Visualize with a dendrogram too.


#Each row in the methods_data_clean is a different paper; each column is a different
#method.

#remove column headers so we only have binary data to cluster

colnames(methods_data_clean) <- as.character(unlist(methods_data_clean[1, ]))

methods_data_clean <- methods_data_clean[-1, ]

#make sure our methods_data_clean is numeric so we can cluster
methods_data_clean <- as.data.frame(lapply(methods_data_clean, as.numeric))

# Calculate Jaquard distance matrix 
distance_matrix <- dist(methods_data_clean, method = "binary")

# Perform hierarchical clustering
hc <- hclust(distance_matrix, method = "complete") #using "complete linkage" method 
# here as it is suitable for distinct clusters with strong separation. Other 
# options may include choosing Ward's method, etc.

dendrogram <- plot(hc, main = "Dendrogram of parchment damage assessment methods",
     xlab = "Papers", ylab = "Similarity/dissimilarity", sub = "")

#In my dendrogram image, each "hair" at the bottom represents a single
#paper. Papers using similar combos of methods are lumped together. The height
#of the branches represents how similar to dissimilar those papers' methods
#are. E.g. papers more similar will have branches connected together lower
#in the plot; papers that are more dissimilar will have branches
#connected further up. See this video for an explanation of how 
#this works: https://www.youtube.com/watch?v=ijUMKMC4f9I

#Clean up the dendrogram and make interpretation easier, by creating a legend
# that we can add below the dendrogram (in our published paper) to clarify that 
# paper 1 is titled ... paper 2 is titled ... v.v.

paper_titles <- data[, 1]  
paper_titles

paper_legend_for_dendrogram_table <- data.frame(
  Paper = paste("Paper", seq_along(paper_titles)),  # Generate "Paper 1", "Paper 2", ...
  Title = paper_titles  # The titles extracted
)

#export this table into the working dir so we can insert it below our dendrogram in
#the final published paper - it now exists in dir as a csv file called
#"paper_legend_for_dendogram_table"

write.csv(paper_legend_for_dendrogram_table, "paper_legend_for_dendrogram_table.csv", row.names = FALSE)


# ------- Exploration 3: Columns "Conservator involved?" or "Lab involved?" ----


# I can also perform a Chi-square test to see if there's a statistically significant relationship between method use and involvement of conservators/labs.
#e.g. visual methods and msi might be much more preferred by conservators


