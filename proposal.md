P8105 Final Project Proposal
================
2023-11-11

## Group members

- Stephanie Calluori - sdc2157
- Zander De Jesus - agd2159
- Sharon Kulali - sjk2254
- Christine Kuryla - clk2162
- Grace Santos - gvs2113

## Project title

**Operation Lorax - Data Speaks for the Trees!**

## Motivation

There is a growing body of evidence demonstrating the beneficial effects
of green space exposure on human health ([Yang et al.,
2021](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8479545/)). To better
inform public health interventions, researchers are diving deeper into
the specific aspects of nature, such as urban trees, and their effects
on health. Most studies have observed positive effects of urban trees on
physical activity and a collection of health outcomes, including
respiratory, cardiovascular, and mental health ([Wolf et al.,
2020](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7345658/)). However,
validation of these results and further exploration of additional health
outcomes is needed. In addition, few studies have examined how urban
tree exposure may differ across sociodemographic groups ([Wolf et al.,
2020](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7345658/)). Here, we
focus on New York City using tree data from 2015 (most recent dataset).
In this study, we aim to examine the geographic distribution of trees
across NYC neighborhoods and potential associations with
sociodemographic variables and health outcomes, primarily at the NTA
(neighborhood tabulation area as defined by the 2010 census) resolution.
Such research will aid in informing public health interventions to
promote equitable access to urban trees and improve health. While our
analyses are retrospective, our work will lay a foundation that can be
used as a comparison point when the next tree census is realeased in
2025, allowing researchers to see how trends may have changed between
2015 and 2025.

## Final products

**1. Report**

A collaborative report will be written by our team to give specific
details about how and why we completed our project. This report will
include some guiding questions that our work will be centered around
with a common goal of yielding credible answers to each. There will be
descriptive sections on the motivation for the project, our data
collection and cleaning steps, how we analyzed the data and what we can
conclude from our results and findings. It will detail the challenges
and limitations of our analysis and approaches and will also describe
our findings and how they can be interpreted from a public health
perspective.

**2. Website**

An interactive webpage will be created to include the scope of our
project and our choice of data sources. There will be supplementary
pages within the webpage to detail data processing, exploratory data
analysis, visualizations and geographic mapping, and our overall
findings to attempt to find linkages between the health of trees and
humans. We hope that this webpage yields an accessible and interesting
platform to inform the eco-curious public about the necessity of trees
in New York City.

**3. Screencast Video**

A 2-minute narrated video screencast will be available on the website to
view in order to further display all aspects of our webpage project and
teach viewers how to navigate to each section.

## Data sources

We found localized Street Tree Survey data collected by the NYC Parks
and Recreation Department and volunteer partnerships with community
organizations. This dataset provides street addresses for surveyed
trees, evaluates species and metrics for tree health, and can be grouped
by Borough, Neighborhood Tabulation Area (NTA) demarcated by 2010
Census, “zip_city” zip code neighborhood level, and as well as latitude
and longitude coordinates available for specific point mapping.

The first data resolution of greatest interest to us is the
**Neighborhood Tabulation Area (NTA)** level neighborhood. These
standardized neighborhood level names are built on the combination of
2010 census tracts, and also nest into larger PUMA regions across New
York City. The 1995 dataset Street Tree Survey also has NTA regions
brought from the 2010 census for comparison. Many of our auxiliary
datasets for comparison on sociodemographic or environmental health data
also have 2010 NTA codes.

The second resolution of interest was zipcode / ZCTA, but this data
resolution has a few inconsistencies that would need to be cleaned. The
tree survey splits zipcode into two columns - “postcode” and associated
“zip_city” neighborhood name, but some of these neighborhoods are
specific while others are broader borough names of different scales.

To potentially standardize this, we have found:

- Raw zip code ids (ZCTA) for standardizing NYC and Neighborhood Names
  Provided by NYCHealth:
  <https://nychealth.github.io/covid-maps/modzcta-geo/about.html>

- We could match the neighborhood names associated with these ZCTA codes
  using joins, and then group tree data by these cleaned ZCTA
  neighborhood names.

The Street Tree survey appears to be collected citywide every 10 years,
and the primary dataset of interest was 2015 data. Previous surveys from
2005 and 1995 are also available and could be used for comparison
overtime. We are currently exploring what local sociodemographic
datasets are available at neighborhood resolution to think about how
tree health and abundance associates with variables like household
income, ethnic demographic, surveyed mental wellbeing, etc.

**Main Dataset (NYC Street Tree Survey):**

- 2015 Street Trees:
  <https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh>
  - 2005
    <https://data.cityofnewyork.us/Environment/2005-Street-Tree-Census/29bw-z7pj>
  - 1995
    <https://data.cityofnewyork.us/Environment/1995-Street-Tree-Census/kyad-zm4j>

*Primary NTA Level Datasets for Sociodemographics and Environmental
Health:*

- NYC Population Breakdown from 2010 Census NTA
  - <https://www.nyc.gov/site/planning/planning-level/nyc-population/census-2010.page>
  - <https://www.nyc.gov/site/planning/data-maps/open-data/census-download-metadata.page>
- NYC Department of Environmental Health NTA Metrics:
  - <https://a816-dohbesp.nyc.gov/IndicatorPublic/beta/data-stories/geographies/>
  - Asthma Emergency Visits in NYC
    <https://a816-dohbesp.nyc.gov/IndicatorPublic/beta/data-explorer/asthma/?id=2384#display=summary>

*Additional Comparative Datasets (Supplemental Depending on the needs of
our Research Questions):*

- US Census/ ACS demographic data by County:
  <https://data.census.gov/table/ACSDP1Y2022.DP05?q=demographic&g=050XX00US36005,36047,36061,36081,3608>

  - By Zip Code:
    <https://data.census.gov/table/ACSDP5Y2021.DP05?g=040XX00US36$8600000>

- Demograpic Youth data by Zip Code:
  <https://data.cityofnewyork.us/City-Government/Demographic-Statistics-By-Zip-Code/kku6-nxdu>

- 2015 Community Health profile (has percentages of diff outcomes for
  each borough):
  <https://www.nyc.gov/site/doh/data/data-publications/profiles-2015-community-health-profiles.page>

- Environmental Health Data Portal:
  <https://a816-dohbesp.nyc.gov/IndicatorPublic/beta/data-explorer/>

- Neighborhood name matching / conversion across other resolutions:
  PUMA, Council District, Census Tracts, Neighborhood Tracking Area

  - <https://guides.newman.baruch.cuny.edu/nyc_data/nbhoods>
  - <https://a816-dohbesp.nyc.gov/IndicatorPublic/beta/data-stories/geographies/>

## Planned analyses / visualizations / coding challenges

### Analyses and visualizations

#### Overall tree distributions in NYC

- EDA: Summary statistics (mean, median, standard deviation, minimum,
  maximum) for various metrics like tree size (measured by Diameter at
  Breast Height, DBH, sometimes seen as a proxy for tree age) and
  health, both overall and segmented by borough and neighborhood.
- Distributions: Bar charts will visualize frequency counts for
  categorical variables like tree species, and histograms as well as
  violin plots will be used to explore distributions (sometimes faceted
  by borough) and identify outliers.
- Species Analysis and Biodiversity: Identify the most common tree
  species across different neighborhoods and boroughs, assessing their
  health and distribution. Additionally, we will assess biodiversity by
  counting different species, and analyzing species richness and
  evenness. This can then be used to correlate with health and
  environmental variables.
- Tree Status: Assess whether trees being along or offset from the curb
  or protected by the guardrail are related to their health, damage
  amount, or whether they are alive. Sneakers amount by borough
  visualized by a bar chart, both with the gross number of sneakers in
  the trees and with a normalized value.
- Additional Visualizations: Latitude/longitude maps of various tree
  variables such as tree health across NYC. Latitude longitude plot of
  top tree species colored by species. Tree density by neighborhood.
  Stacked bars for tree species/health/relative sidewalk position by
  neighborhood or borough.

#### Sociodemographic associations

- Correlation and Exploratory Analysis: Examine the relationships
  between tree variables (density, health, species prevalence) and
  sociodemographic factors (such as population density). Example: tree
  density and poverty level.
- Visualizations: Scatterplots illustrating the relationship between
  tree density and demographic variables like population density, with
  color or size variations to represent different demographic or
  health-related outcomes.

#### Health outcomes

- Public Health Correlations: Investigate the link between tree presence
  and public health outcomes such as asthma by correlating tree density
  or health with emergency room asthma visits per neighborhood unit.
  - Tree species compared to emergency room asthma visits as well as
    asthma prevalence (certain tree species may exacerbate allergies,
    leading to asthma attacks).
  - Association between heat vulnerability index and tree density,
    including a corresponding box plot.
  - Regression on physical activity data and tree density, including a
    visualization with a scatterplot and regression line.
- Environmental Impact Analysis: Assess how tree density or health
  correlates with environmental factors like air quality or temperature
  (data pending).

### Coding challenges

- Data Integration: Merging datasets with different location
  descriptions (ZIP code, neighborhood name, latitude/longitude) into a
  coherent format, which may be different for different analyses.
- Time Windows: Dealing with tree data collected every ten years and
  sociodemographic data collected annually, requiring aggregation or
  alignment.
- Data Wrangling: Ensuring that comparisons like tree density are
  meaningful by adjusting for area size differences across
  neighborhoods.

## Planned timeline

| Dates           | Tasks                                  | Due Dates  |
|-----------------|----------------------------------------|------------|
| Oct 30 - Nov 5  | Brainstorm project ideas               | \-         |
| Nov 6 - Nov 12  | Finalize dataset and draft proposal    | Nov 8      |
|                 | Finalize proposal                      | Nov 10     |
|                 | Proposal due                           | **Nov 11** |
| Nov 13 - Nov 19 | Project review meeting                 | TBD        |
|                 | Divide up the work                     | \-         |
|                 | Create ‘foundation’ documents          | \-         |
|                 | Start data tidying and manipulation    | \-         |
| Nov 20 - Nov 26 | Finalize data tidying and manipulation | Nov 26     |
| Nov 27 - Dec 3  | Start data analyses and visualizations | \-         |
|                 | Start building the website             | \-         |
|                 | Draft the report                       | \-         |
|                 | Video script and/or slides             | \-         |
| Dec 4 - Dec 10  | Record video                           | Dec 4      |
|                 | Finalize all components                | Dec 8      |
|                 | Project due                            | **Dec 9**  |
|                 | \- Written report                      | \-         |
|                 | \- Webpage                             | \-         |
|                 | \- Explanatory video                   | \-         |
|                 | \- Peer assessment                     | \-         |
| Dec 11 - Dec 17 | Prepare for class discussion           | Dec 11     |
|                 | In-class discussion of projects        | **Dec 14** |
