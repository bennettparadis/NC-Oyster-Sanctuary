# NC-Oyster-Sanctuary
R scripts used to analyze data collected from monitoring NC's network of oyster sanctuaries

Each year monitoring efforts collect ecological data on the oyster sanctuaries throughout Pamlico Sound. In 2018, this methodology was revamped with a pilot study that was cut short by hurricane Florence. From 2019 onward this robust monitoring procedure has been used to collect standardized samples and record relevant ecological data pertaining to the performance of artificial reefs with the goal of restoring subtidal oyster populations. This repository is organized by year and also includes a branch with the entire data set from 2019-present. The main output of yearly analyses are histogram plots which visualize the population structure of each sanctuary over time. In the 2019-present folder you will find R scripts used to analyze the entire time series of oyster sanctuary monitoring, including reproductive potential output, density estimates for each size class (spat, sublegal, legal), and animated plots of the full time series. 

Below is a general description of the program and the methodology used during data collection.


General Description						February 22, 2023

North Carolina’s Division of Marine Fisheries has been permitting, constructing, and monitoring oyster sanctuaries since the late 1990s, following a recommendation from the 1995 Blue-Ribbon Advisory Council on Oysters to rehabilitate eastern oyster (Crassostrea virginica) populations in North Carolina. In pursuit of this goal, NCDMF created Program 611 in order to monitor the effectiveness of the creation and preservation of protected artificial oyster reef habitat within the Pamlico Sound Oyster Sanctuary Network. In an annual effort, SCUBA certified biologists and technicians conduct quantitative surveys on each oyster sanctuary, collecting data on the density and population structure of oysters at each site, as well as other species present and relevant abiotic data. These surveys provide insight into the degree of sanctuary persistence and are used to refine site selection and construction methods for future sanctuary sites and habitat enhancement.

The creation and preservation of oyster sanctuaries represents both a long-term ecological restoration project as well as a long-term fisheries investment to the state of North Carolina. While artificial reefs and oyster sanctuaries can be relatively small in comparison to natural reefs, they provide ecosystem services that improve the quality of habitat throughout Pamlico Sound. Prohibited from oyster harvest, sanctuaries encourage the growth of large, healthy oyster populations that act as brood stock for the rest of North Carolina’s waters. This strategy has been found to support high reproductive potential within sanctuaries, thereby providing larvae to natural reefs. Sanctuaries also act as nursery habitat for other species, increasing their abundance for commercial and recreational fishing; provide refuge and forage habitat for marine life; provide travel corridors for transient finfish; and increase water filtration, reducing turbidity and excess nutrients in the estuary. 

Today DMF maintains and manages 15 oyster sanctuaries throughout Pamlico Sound. The sanctuaries encompass 566.22 acres total, with over 223,643 tons of material deployed for oyster habitat. 

Program Objectives
1.	Evaluate material performance as oyster habitat.
2.	Evaluate material long-term durability and stability.
3.	Evaluate overall material performance for reproductive potential and larval recruitment.
4.	Evaluate oyster demographic trends at each sanctuary over time.


General Overview of Sampling Methodology

Annual sanctuary monitoring for this study is conducted from June 15th through October 31st and depends heavily on weather and logistical constraints.

1.	Site Selection

Due to the logistical and financial feasibility of large scale monitoring, attempts have been made to keep sample replicates to a minimum while still maintaining statistical validity. The survey sample size for each sanctuary is defined by a representative sample that accurately reflects the total amount of each material within a sanctuary. Using the most recent side-scan imagery for each sanctuary, deployed material is digitized in ArcGIS to identify true footprint acreage of each material patch. These material patches are then grouped by material type (Table 1). 

Using the random point-generating tool within the ArcGIS toolbox, n randomly selected sampling points are generated based on the total area of each grouped material layer, labeled, and displayed on a field sampling map (Table 2, Table 3, Figure 2). Each site will act as a replicate for the material type sampled. Additionally, sampling points should also be exported as a .gpx file, and uploaded on the vessel chart plotter prior to sampling. 

2.	Field Operations

Sample types

For investigating the performance of sanctuary sites, there are two types of procedures followed for data collection: observational and excavated. The procedure to be followed at a site is determined primarily by the material type—can the material be excavated easily? If material and the settled oysters can be dug up easily by divers using only their hands, then divers will follow the procedure for excavation. If the material is too large to fit in baskets and cannot easily be removed, then the divers will follow an observational approach. These procedures are described in detail below.


Methodology for all sample types (both observational and excavated)

Upon arrival, discrete environmental and habitat data are measured and recorded. These parameters include: date, site name, material type, weather description, surface and bottom temperature (°C), surface and bottom salinity (ppt), surface and bottom dissolved oxygen (DO; mg/L), secchi depth (cm), wind speed (knots), wind direction, latitude, and longitude. Once parameters have been recorded, marker buoys attached to a weighted anchor and an adequate scope of line are then deployed at each of the sampling locations at a given sanctuary. SCUBA divers with PVC quadrat(s) in hand will descend down the marker buoy line to the anchor on the bottom in place of the anchor on the site to be investigated. 

Percent area coverage of oysters, mussel, and algae are collected for both excavated and observational sampling. For sites that will be excavated, this procedure is done underwater, prior to removing any material. SCUBA divers will place a quadrat frame with a 6 by 6 grid formed by 5 strings vertically and 5 strings horizontally (Figure 3). The intersection of lines, nodes, within the grid are used for percent cover estimates. If an oyster, mussel, or algae are present at/under the node of the grid, then that is considered presence. The presence/absence of oysters, mussel, and algae is recorded for each, and noted in situ underwater prior to excavating. At the surface, this number is then multiplied by four to get a percent area coverage estimate on the data sheet (Figure 4; Figure 5). 

Additionally, for both excavated and non-excavated sites, divers will record a categorical boring sponge rating (1=absent, 2=evidence of sponge (boring), 3=sponge present in beta (encrusting), 4=prolific sponge cover (massive).  Sediment type and rating (1=none if no sediment is found either visually or collected by finger sweep), (2=light if sample shows visual evidence of sediment, but findings are only enough to collect discoloration at finger tips), (3=medium if sample shows visual evidence of sediment and findings are less than ¼ inch or thick layers of sediment on sample), (4=heavy if sample shows clear visual evidence of sediment and findings of 0.25 inch or thicker layer sediment on sample) is recorded as well. 
Divers should also take note of any finfish or invertebrates that they might see during their dive on each site and record that in the notes on the respective data sheet (Table 5). 


Excavated sample site procedure (oyster census)

After acquiring percent coverage estimates, divers with place an open 0.25 m² quadrat frame in the same general location where the grid PVC quadrate was placed. Divers will then gather material within each quadrat, excavating down to 15 cm (about the length of a hand) and placed into a submerged bushel basket. If a piece of material is on the boundary of the PVC quadrat—that is partly within the frame and partly outside—an attempt to excavate the piece of material should be made within reason. If the material is cemented in place or too large for the basket, then it does not need to be included. The material within the quadrat is the priority. However, it should be noted that divers would ideally select a quadrat site where material can easily be excavated (ie, not cemented in place). Once the material has been placed in the basket, it is then hauled to the surface, brought aboard the vessel, and labeled for subsequent evaluation. 

On board the research vessel, each piece of excavated material is visually inspected and all live oysters measured (LVL, mm) and recorded for a given basket (Figure 4). Here boring sponge data can be recorded as well if not already done so underwater. If an oyster is thought to be dead (hollow and missing the meat; aka a “box”), then it should be measured and recorded at the bottom of the data sheet (Figure 4). Additionally, each rock that came to the surface in the basket should also be measured—record the approximate length, width, and height of each rock. These dimensions should only reflect the rock material itself and not include the length of any oysters attached or protruding from the material. Once measurements and counts have been taken, both oysters and material are then returned onto the sanctuary. 

Subsampling

Beginning in 2023, a subsample protocol will be in effect. If a sample is thought to have more than 400 oysters, only the first 400 will be measured. All oysters will still be counted within an excavated sample. The reasoning behind this is to reduce sampling fatigue and maintain data accuracy during long field days. This threshold was determined by using the largest sample collected between 2019 and 2022, which was a quadrat consisting of 1,049 oysters. A series of random subsamples of various sizes (n = 300, 350, and 400) were randomly selected without replacement. Following a bootstrap approach, that subsample was used to generate 1,000 simulated subsamples with replacement for each iteration. A Kolmogorov-Smirnov test was then used to determine whether there was a statistically significant difference in the distributions between the true dataset and each bootstrap dataset. This approach consistently yielded more that 95% of the bootstrap datasets did not have significantly different distributions that the truth dataset.

The trade off here is collecting accurate data at the expense of collecting a full census for length frequency plots. There is a rate of diminishing return for oyster measured once sampling fatigue is introduced. Because all oysters will still be counted using this subsampling approach, density estimates will be unaffected by the protocol. To compensate for subsampling when generating length frequency plots, bootstrapping with replacement will be utilized in the analysis of data. As the total count will have been collected, bootstrapping can be used to fill in the gap while maintaining a confidence level above 95%. 

In order to limit bias and randomize which oysters are measured, the sampling team can arrange the rock material and use the random number table (RNT) on the data sheet (Figure 4). For example, divers come up with a basket that contains 10 rocks. The team suspects that there are more than 300 oysters in this sample. The rocks can be arranged in a line and assigned a number in sequential order (0-9). The scribe can then close their eyes and place their pencil on the RNT and the corresponding rock will then be used for measuring oysters. The process can be repeated until 300 oysters have been measured. The remaining rocks would then be used to only count how many oysters are on them. 


Primary Data Analysis

Oyster Density Estimates 

Total mean oyster densities and standard deviations for each sanctuary are calculated using oyster counts captured from all excavated sample 0.25m2 quadrats. Legal (>75mm), sub-legal (75mm ≥ x > 25mm), and recruit (≤ 25mm) mean densities will also be calculated for each sanctuary. For each mean oyster density comparison (total, legal, recruit), a one-way ANOVA will be used to test for differences between sanctuaries. Post hoc pairwise comparisons will also be performed on any significant effects.

Size Frequency Plots

Oyster lengths measured at each sanctuary will be separated into 5mm size class intervals, standardized by area (m2), and their counts plotted. Modal mean ± SD, length, and density (N/m2) estimates will also also calculated from size frequencies for multi-year visual trend comparisons and cohort identification. Modes in each annual size frequency distribution were identified using NORMSEP (FiSAT; Puckett et al. 2012). The number of distributions (i.e., modes) was specified a priori at one and increased in number until the separation index of modal means was <2 (∼2 SD; Jennings et al. 2001 and references therein). Each mode is assumed to represent a distinct cohort of oysters. A cumulative mode derived from each cohort mode is generated based on each cohort mode and provides a linearized representation of each sanctuaries oyster demographics. 

















