# Population changes not related to births and deaths
## Internal migration flows animated chordplot example

Changes in local populations can be driven by births and deaths as well as internal and international migration. The Office for National Statistics produces annual estimates of the resident population of England and Wales as at 30 June every year. The estimates are broken down by local authority, sex and age. The most comprehensive estimates come from the census, which takes place every 10 years in the UK. Population estimates are then updated every year until the next census.

Latest data for the UK suggests that net migration in 2018 was a bigger driver of population change than natural change (births, deaths and ageing) for the fifth year in a row, with net migration at 275,000.

However, migration is the most difficult part of the estimate process to measure precisely because the UK has no population register. These estimates are primarily based on data that flags up when people change their GP as they change address. Since most people re-register with a new doctor after moving, these data are considered to provide a good proxy indicator of migration. Similar data sources are used both for cross-border flows and moves within England and Wales.

This repository focuses mostly on migration flow and on data provided by the ONS (Residential moves are gathered from the NHS Patient Register, the NHS Central Register (NHSCR) and the Higher Education Statistics Agency (HESA));

* Internal migration is defined as residential moves between different local authorities (LAs) in the UK, including those that cross the boundaries between England, Wales, Scotland and Northern Ireland. However, only moves affecting LAs in England and Wales are included in the LA level release. Moves that occur solely within Scotland and/or Northern Ireland are excluded. The statistics also exclude any moves within a single LA and any international moves either into or out of the UK. The local authority and age/sex tables use this definition.														

* The regional table follows the same principles, except that it only includes moves that cross the boundaries of the English regions or the boundaries between the four UK nations. Any moves occurring within a single English region, or within Wales, are excluded. 								

For a comprehensive methodology of the population estimates see: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/methodologies/methodologyguideformid2012tomid2016ukpopulationestimatesenglandandwalesmarch2018						

## Output: migration flow infographics - including chordplots

This project shows the workflow of four concepts in R:

* Download and compile each year of data into a single file
* Create new folders in the working directory
* Create figures showing the number of moves between areas in a chordplot
* Create animated gifs of changes in migration flows over time

## There are three R scripts in this repository:

Script name | Comment
------------| -------------
1 - ONS population change data | This script downloads data from ONS for LA level migration and creates some summary analyses of population change
Chordplot example | This script shows an example of global migration flows adapted from a tutorial by Guy Abel (The original post is available here: https://guyabel.com/post/animated-directional-chord-diagrams/).
Internal Migration flows ONS | This script creates chordplots from ONS migration data
