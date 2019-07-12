# Population changes not related to births and deaths
## Internal migration flows animated chordplot example

Changes in local populations can be driven by births and deaths as well as internal and international migration.

In the UK, net migration was a bigger driver of population change than natural change (births and deaths) for the fifth year in a row, with net migration (275,000) was a bigger driver of population change than natural change (121,000; see Figure 2). However, population change over the last two years has remained stable as increasing net international migration has roughly matched increasing emigration and lower natural change.

The Office for National Statistics produces annual population estimates based on

This repository focuses mostly on migration flow and on data provided by the ONS (Residential moves are gathered from the NHS Patient Register, the NHS Central Register (NHSCR) and the Higher Education Statistics Agency (HESA));
* Internal migration is defined as residential moves between different local authorities (LAs) in the UK, including those that cross the boundaries between England, Wales, Scotland and Northern Ireland. However, only moves affecting LAs in England and Wales are included in the LA level release. Moves that occur solely within Scotland and/or Northern Ireland are excluded. The statistics also exclude any moves within a single LA and any international moves either into or out of the UK. The local authority and age/sex tables use this definition.														

* The regional table follows the same principles, except that it only includes moves that cross the boundaries of the English regions or the boundaries between the four UK nations. Any moves occurring within a single English region, or within Wales, are excluded. 														

## Output: migration flow infographics - including chordplots

This project shows the workflow of four concepts in R:

* Download and compile each year of data into a single file
* Create new folders in the working directory
* Create figures showing the number of moves between areas in a chordplot
* Use of magick package to create animated gifs of changes in migration flows over time

## There are three R scripts in this repository:

Script name | Comment
------------| -------------
1 - ONS population change data | This script downloads data from ONS for LA level migration and creates some summary analyses of population change
Chordplot example | This script shows an example of global migration flows adapted from a tutorial by Guy Abel (The original post is available here: https://guyabel.com/post/animated-directional-chord-diagrams/).
Internal Migration flows ONS | This script creates chordplots from ONS migration data
