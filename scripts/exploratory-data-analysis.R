

# ---------------- Load libraries for data exploration ------------------------
# The purpose of this exploration script is to find trends in the data that 
# could help with later analysis in the text. For example, we look for 
# trends in methods used for parchment damage assessment (PDA), trends of
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

# Check that the heading names of the columns are suitable.
colnames(data) #When importing, R had converted any spaces in our headings into
#dots, which makes the headings look very messy and complicated. Let's rename
#the headings into shorter, sensible headings so that graphing and analysis is
#clearer.

data$Year <- as.numeric(data$Year) 
#Define that columns 8 to 49 in our CSV contain the methods - the other columns
#are irrelevant to us for now. E.g. they contain bibliographical information.
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

methods_data 
#We accidentally have a blank row of cells at the bottom of our methods_data
# so let's remove it.
methods_data <- methods_data[-nrow(methods_data), ]

#### this bit of code is only relevant for the simulated data, it replaces
#### any NAs with 0 (method not present) because we are still waiting on LB
#### to finalize our data for analysis. In the true data please remove this 
#### code as there should be no NAs in the methods_data. ##################

any(is.na(methods_data))
na_rows <- which(apply(methods_data, 1, function(row) any(is.na(row))))
na_rows

methods_data_cleaned<- methods_data
methods_data_cleaned[is.na(methods_data_cleaned)] <- 0
methods_data_cleaned
methods_data <- methods_data_cleaned

#### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

# So now we have a cleaner version of methods_data to work with. It's called
# methods_data.

# --- Exploration 1: Method popularity: Frequency of each damage assessment ---
# ----------------- method across the whole dataset ---------------------------------

#Create a new variable called method_counts that counts every time a cell
#in a method's column says '1.' 
method_counts <- colSums(methods_data == 1) 
method_counts
write.csv(method_counts, "1. methodcountsabsolute.csv", row.names = FALSE) #We 
# will use the 1. methodcountsabsolute.csv in our published paper.

#Now sort the counts from greatest to least, so we can see which method is 
#use most often, to least often, in terms of whole numbers.
method_counts_sorted <- sort(method_counts, decreasing = TRUE)
method_counts_sorted #e.g. we are told 'light microscopy etc.' is used 22 times.

#Plot these absolute counts of each method on a bar graph.

plot_absolute_method_counts <- ggplot(data = data.frame(Method = names(method_counts_sorted), Count = method_counts_sorted), 
       aes(x = reorder(Method, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Method usage count (absolute)", x = "Method name", y = "Count")
plot + theme_minimal() 

plot_absolute_method_counts

#Now graph as a percentage, to see how they compare to each other relatively
#E.g. seeing that 80% of studies use on method might indicate it's a preferred
#or standard approach.

total_observations <- nrow(methods_data) #finding total number of rows to figure out what to divide by 
method_counts
method_percentages <- (method_counts / total_observations) * 100 
method_percentages
method_percentages_sorted <- sort(method_percentages, decreasing = TRUE) #sorting from greatest to least percentage of method usage 

print(method_percentages_sorted) #VA is used the most, FTIR the second most, v.v.

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
methods_data
colnames(methods_data) <- as.character(unlist(methods_data[1, ]))

methods_data <- methods_data[-1, ]
methods_data

#make sure our methods_data_clean is numeric so we can cluster
methods_data <- as.data.frame(lapply(methods_data, as.numeric))

# Calculate Jaquard distance matrix 
distance_matrix <- dist(methods_data, method = "binary")

# Perform hierarchical clustering
hc <- hclust(distance_matrix, method = "complete") #using "complete linkage" method 
# here as it is suitable for distinct clusters with strong separation. Other 
# options may include choosing Ward's method, etc.
hc

dendrogram <- plot(hc, main = "Dendrogram of parchment damage assessment methods",
     xlab = "Papers", ylab = "Similarity/dissimilarity", sub = "")




#In my dendrogram image, each "hair" at the bottom represents a single
#paper. Papers using similar combos of methods are lumped together. The height
#of the branches represents how similar to dissimilar those papers' methods
#are. E.g. papers more similar will have branches connected together lower
#in the plot; papers that are more dissimilar will have branches
#connected further up. See this video for an explanation of how 
#this works: https://www.youtube.com/watch?v=ijUMKMC4f9I

hc$height #Looking at the dendrogram and seeing the heights tells me:
#e.g. two papers grouped together lower, like 89 and 93
#says that these two use very similar method combos. 
#e.g. a paper that is low that is connected to one higher up 
#means there is a significant different between the method combos
#used in those two but they still have some similarity. e.g. 93 and 90, or 22 and 
#68.


#Create a legend that we can add below the dendrogram (in our published paper) 
#to clarify that paper 1 is titled ... paper 2 is titled ... v.v.

paper_titles <- data[, 1]  
paper_titles

paper_legend_for_dendrogram_table <- data.frame(
  Paper = paste("Paper", seq_along(paper_titles)),  # Generate "Paper 1", "Paper 2", ...
  Title = paper_titles  # The titles extracted
)

paper_legend_for_dendrogram_table

#export this table into the working dir so we can insert it below our dendrogram in
#the final published paper - it now exists in dir as a csv file called
#"paper_legend_for_dendrogram_table"

write.csv(paper_legend_for_dendrogram_table, "5. Dendrogram_part_2_legend.csv", row.names = FALSE)

#In my paper, now I can write that papers 89 and 93 use similar method combos.
#Papers 44 and 45 use similar method combos. Etc. 


# [After running validated data (not sim.), insert writtten analysis here, see example below]
# [-> Cluster overview and key findings: ]
# [---> Cluster 1: this group predominantly includes studies that apply spectro-
#       scopic techniques and imaging tehcniques. Papers in this cluster, such 
#       as X and Y, all explore the structural and chemical composition of 
#       parchment using blahblah. These methods are all also notably non-
#       mally destructive, as they do not take even microsamples from the parch-
#       ment. This close structuring suggests a strong correlation between spec-
#       troscopic techniques and studies focused on structural assessment.]
# [---> Cluster 2: blahblah "similar format to cluster 1's analysis"]
# [---> Cluster 3: blahblah ""]
# [-> Interpretation of clustered method combinations: ]
# [---> The dendrogram reveals a clear methodological divide: studies focusing 
# on material composition and structural integrity tend to cluster separately
# from those analyzing blahblah and blahblah methodologies blahblah. For 
# instance, clusters one and two align with methods that directly assess
# thermal properties, while clusters three and four explore blahblah...]
# [---> This clustering pattern indicates potential research areas that could
# benefit from integrative approaches. For instance, combining blah methods
# with computational modeling (bridging clusters 2 and 4) could enhance 
# blahblahireallydefinemyworkinfuturehere blahblah.]
# [---> What's not captured in the clustering: a cluster of [15] papers all
# use visual damage assessment techniques. Yet there is great variance in between
# all the VDAT each paper uses: some use naked eye, some use raking light, etc.
# Keep blahblahblahdescroiptionaboutvariancesinvisualtechniques]

# ------- Exploration 3: Columns "Conservator involved?" or "Lab involved?" ----
# How often did conservators and labs work together?
# Which methods were used by conservators, or labs, or both? 


# ------- Exploration 4: Historical vs. new parchment  --------------------------------
# ------- Exploration 5: Invasiveness analysis  -------------------------
