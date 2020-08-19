# Purpose of this project
#### Discover statistical patterns or other computational regularities in indecipherable, symbolic data
#### Comparative analysis of symbolic languages with other languages
#### Enhance status quo NLP techniques with image processing
#### Create a universal methodology for analyzing known/unknown data, regardless of the nature and source of these data. 
#### Address of shiny dashboard to support analysis of Linear A

https://acottman1.shinyapps.io/Dasboard_lin_A/


to run the R files ans see the symbols:
1) install NotoSansLinearA-LinearB.ttf to your computer.  
2)  Run the following commands: they are included in the "Symbol Parsing_modified.R" file, but commented out. 

install.packages('extrafont')
library(extrafont)
font_import()
loadfonts()

linearA_EDA.R is the exploratory phase of the project.  It was also used to initially write out the comparisons of the NLP model to the academic categorizations for the documents. 

Symbol Parsing_modified.R is where the main processing and modeling happening in the project.  

Lin_A_text_mining.R segements the symbols into various patters.  The intent is to set up the data to attempt to predict the symbols taht would appear before and after an unknown character based on the patterns in the data.  The next step would be to do a baysian analysis and display the conditional probabilities for each symbol within a sequense, or pair.   A problem with this approach is the small size of the corpus.  It is also not known what the true distribution the symbols follow. 

compare_tm_to_academic.R is the function that creates the table of ratios of topic model and academic groupings.  

clustering.R pulls thte clustering component out of the EDA and is used to grate the k-means clusters for comparison. A next step would be to creater silhouette graphs to help evaluate the quality of th the clustering.

# Resources

## Linear A
Haghia Triadha texts coded numerically and syllabic http://y.deliyannis.free.fr/linearA/catalog.php?id=HT#
Ideograms, Logograms and vocabulary http://www.people.ku.edu/~jyounger/LinearA/#13. Data comes from https://github.com/mwenge/LinearA

## Unicode for ancient texts
https://dn-works.com/ufas/

## Entropy Analyses

Letter Frequencies in Different Languages: http://www.cryptogram.org/downloads/words/frequency.html#L5 and http://pi.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html (English)

