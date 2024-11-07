# Load libraries for data exploration - the purpose of the exploration
# is to find trends in the data that could help with later analysis
# using PCA and CA. E.g. trends in methods used for parchment damage assessment
# (PDA), trends in conservator and lab involvement in projects, etc.

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggthemes)

# Set working directory so we can use relative file paths
setwd("C:/Users/chris/Downloads/MappingFieldParchmentDamageAssessment")

# Read in data
data <- read.csv("simulated-analysis-data.csv")
data # We noticed that some letters with diacritics did not load in properly, 
# unfortunately, but that is not a big problem with starting data analysis.

# Check heading names
colnames(data) #OK, R formatted them, turning spaces into dots, we will fix it
#in a few steps

# Preprocessing
data$Year <- as.numeric(data$Year) 
methods_columns <- c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49)
methods_data <- data[, methods_columns]
methods_data #this is all of the columns with methods data, including imaging,thermal, spectroscopy, biological-chemical, and visual.

#rename the column titles to something more sensible and short so the graphs make sense to look at


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

# (exploration 1) Method popularity: Frequency of each method across the whole dataset

method_counts <- colSums(methods_data == 1)

method_counts_sorted <- sort(method_counts, decreasing = TRUE)

print(method_counts_sorted) #gives us a count of how often each technique is used, e.g. 'light microscopy etc.' is used 22 times.


plot <- ggplot(data = data.frame(Method = names(method_counts_sorted), Count = method_counts_sorted), 
       aes(x = reorder(Method, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Method usage count", x = "Method name", y = "Count")
plot + theme_minimal() 

# (exploration 2)  Which methods tended to be used together in a paper?
# (exploration 3) Any trends over the years if any method tended to be used more in certain years?
# (exploration 4) Columns "Conservator involved?" or "Lab involved?" 
# I can also perform a Chi-square test to see if there's a statistically significant relationship between method use and involvement of conservators/labs.

