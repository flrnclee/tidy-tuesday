# Visualizing MarioKart64 World Records
 
## Submission
![](https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/mario-kart/mariokart20210601.png)

## Notes
* World records are for the three-lap, non-shortcut runs.   
* Four world records belong to players that do not have an identified country. These world records were excluded from analysis.
* The first world record was set in March 2017, and the latest world records in the dataset are from 2021.
* Tracks in the tile plot are sorted by relative difficulty, where more difficult tracks are those that have seen the move improvements in world record time since the first world record. **D.K.'s Jungle Parkway**, **Rainbow Road**, **Wario Stadium** were among the most difficult.  

## Data 

**Data source:** [Mario Kart World Records](https://mkwrs.com)  
**Dimensions:** 

* records: 2,334 rows x 9 columns  
* players: 2,250 rows x 6 columns

## Helpful Packages & Musings

* First time using `ggfx()` for dropshadow. `with_blend()` didn't work out so well, but maybe next time.  


