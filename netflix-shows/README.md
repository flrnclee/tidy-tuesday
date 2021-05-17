# Visualizing Netflix Titles
 
## Submission
![](https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/netflix-shows/netflix16052021.png)

## Notes
* Titles include both TV shows and movies.   
* Genres are not mutually exclusive. Titles are often classified with up to three genres.   
* The *Children & Family* category includes Kids titles in addition to Children & Family titles.  
* The *International* category includes all non-U.S. titles (e.g., British, Spanish-language, Korean).  
* The *Documentaries* category includes both documentaries and docuseries.   
* Ambiguous genres were removed from the genre category. These include genres like "Movies", "TV", "Shows".  
* Multi-series shows are only listed once in the dataset. For instance, *The Crown*, which is in its 4th season, is only listed once---in 2020 based on the release date of the latest season. 

## Data 

**Data source:** Kaggle  
**Dimensions:** 7,787 rows x 12 columns

## Helpful Packages & Musings

* `extrafont` 
* `glue` (why have I been using `paste`!?)
* `ggtext` 
* Love `ifelse`-ing inside of ggplot
* `geom_textbox` is awesome but need some work with sizing in final PNG
* Graphics from [The Noun Project](https://www.thenounproject.com)
* Strip plots/horizontal tile plots look cool (and save space) but don't allow for precise extraction of numbers



