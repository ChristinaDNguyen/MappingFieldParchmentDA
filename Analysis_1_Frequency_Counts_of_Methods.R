# ------------ Analysis 1: Frequency counts of methods ------------
# ------------ How many times is each method used? and then calculate this as a percentage and plot it. ------------

# Load in the packages to read Excel files, select/manipulate data, tidy data, and deal with strings/text.
library(readxl) 
library(dplyr)
library(tidyr)
library(stringr)

# Set the path to the Excel file in the working directory.
file_path <- "2_raw_excel_methods_only.xlsx"
file_path

# Read in row 2 as column headers, which define technique name (skip row 1)
technique_headers <- read_excel(file_path, skip = 1, n_max = 1, col_names = FALSE)
technique_headers <- as.character(technique_headers[1, ])
technique_headers <- str_squish(gsub("\n", " ", technique_headers))  # Clean up line breaks and spacing
technique_headers #checking that headers (technique names) are correctly read in

# Read the main dataset starting from row 3 (data entries of 0 and 1 indicating presence of technique or not)
raw_data <- read_excel(file_path, skip = 2, col_names = FALSE) #return the dataset of 0s and 1s showing which techniques were used in each paper
colnames(raw_data) <- technique_headers #add the column headings (technique_headers) back onto the raw_data

raw_data #check how raw data looks

# Rename the first column to "Paper"
colnames(raw_data)[1] <- "Paper"

# Convert method columns to numeric so I can process it properly later
data_numeric <- raw_data %>%
  mutate(across(-Paper, ~as.numeric(as.character(.))))

# Count how many papers used each method
technique_counts <- data_numeric %>%
  summarise(across(-Paper, sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "Count") %>%
  arrange(desc(Count))

# View the result, and turn it into percentages
technique_counts #this looks good - I can see that there are 41 techniques in total and each has a count of how many papers used them.

total <- sum(technique_counts$Count, na.rm = TRUE)
technique_counts_percentage <- technique_counts %>%
  mutate(Percentage = (Count / total) * 100)
technique_counts_percentage


# Export the counts as CSV
write.csv(technique_counts, "analysis_1_technique_usage_counts.csv", row.names = FALSE)

# Graph it, and reduce long technique names to abbreviations

abbreviations <- c(
  "Visual analysis (unaided by equipment, particularly surface examination for wrinkles/tears/fading of inks and pigments, texture assessment, and colour change, mechanics, etc. as observed by the naked eye)" = "Visual analysis, etc.",
  "Fourier-transform infrared spectroscopy (FTIR), infrared spectroscopy, Attenuated total reflectance FTIR (ATR-FTIR), synchotron-IR" = "FTIR, etc.",
  "Light (optical) microscopy, Polarized light microscopy, Fluorescence microscopy, Light transmission analysis (LTA), stereomicroscope (non-thermography)" = "Light microscopy, etc.",
  "Electron microscope, scanning electron microscopy (SEM), and SEM with energy dispersive x-ray spectroscopy (SEM-EDS, aka EDX)" = "EM, SEM, etc.",
  "Differential scanning calorimetry (DSC), microDSC, and adiabatic scanning calorimetry (ASC)" = "DSC, etc.",
  "Raman spectroscopy (RS), polarised Raman spectroscopy, and Raman microspectroscopy (RMS)" = "RS, etc.",
  "Micro hot table (MHT), hot table" = "MHT, etc.",
  "Colourimetric measurements (CIE*Lab colour change, and RGB imagery)" = "Colourimetry, etc.",
  "UV-VIS-NIR spectroscopy (incl. fluorescence), UV-VIS spectroscopy (incl. fluorescence), nanoIR spectroscopy (incl. fluorescence), VIS spectroscopy (incl. fluorescence)," = "UV, VIS, and NIR spectroscopy",
  "Humidity sorption (RH), and hygroscopicity" = "RH, etc.",
  "X-ray diffraction (XRD), all angles (e.g including wide angle X-ray diffraction (WAXD))" = "XRD, etc.",
  "Microbial/bacterial analysis (MA) (e.g. through PCR-based methods or in combination with microscopy), and fungal analysis" = "MA, etc.",
  "Nuclear magnetic resonance (NMR) spectroscopy" = "NMR",
  "Thermogravimetry (thermogravimetric analysis) (TG), and derivative thermogravimetry (DTG)" = "TG, etc.",
  "Multispectral imaging (MSI)" = "MSI",
  "Dynamic mechanical analysis (DMA), tensile strength tests" = "DMA, etc.",
  "Gas chromatography (GC), liquid chromatography (LC), liquid chromatography - mass spectrometry (LC-MS), size-exclusion chromatography" = "GC, etc.",
  "Amino acid analysis (AAA)" = "AAA",
  "Nonlinear optical microscopy (NLO)" = "NLO",
  "X-ray fluorescence (XRF), energy dispersive XRF" = "XRF, etc.",
  "Atomic force microscopy (AFM), including any modified AFM probe" = "AFM",
  "Thermography (with a stereomicroscope)" = "TS",
  "X-ray scattering (SAXS)" = "SAXS",
  "Surface pH measurement" = "PH",
  "Gel electrophoresis (GE), polyacrimide gel electrophoresis (SDS-PAGE)" = "GE, SDS-PAGE, etc.",
  "Fibre optic reflectance spectroscopy (FORS)" = "FORS",
  "Synchronous fluorescence (SF)" = "SF",
  "Laser-induced breakdown spectroscopy (LIBS)" = "LIBS",
  "Optical coherence tomography (OCT)" = "OCT",
  "3D Digital Image Correlation (DIC) for strain and displacement monitoring" = "DIC",
  "Differential thermal analysis (DTA)" = "DTA",
  "Infrared thermography" = "IRT",
  "Electron paramagnetic resonance (EPR) spectroscopy (also called Electron spin spectroscopy (ESR))" = "EPR",
  "Multiphoton electron extraction spectroscopy (MEES)" = "MEES",
  "X-ray photoelectron spectroscopy (XPS)" = "XPS",
  "Peptide mass fingerprinting" = "PMF",
  "Matrix-assisted laser desorption/ ionisation (mass spectrometry) for proteomics" = "MALDI",
  "Lipid content determination with gravimetry" = "LCDG",
  "Next generation sequencing (NGS), metagenomic analysis" = "NGS",
  "Roughness profiling" = "RP",
  "Computed tomography (CT)" = "CT"
)

technique_counts$Method <- sapply(technique_counts$Method, function(x) abbreviations[x] %||% x)


#Graph the counts with the abbreviated method methods

library(ggplot2)
library(dplyr)

ggplot(technique_counts_percentage, aes(x = reorder(Method, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Percentages of techniques used in papers",
    x = "Technique used",
    y = "Percentage of papers (%)"
  ) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Rotate x-axis labels for better readability

#Looks good!

