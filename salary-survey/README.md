# Visualizing Salaries from Ask A Manager Survey

## Submission
![](https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/salary-survey/salarydisp20210523.png)

## Notes
* Career stage is assumed to be based on age. **Early stage** is < 35 years, **mid stage** is 35 to 54 years, and **late stage** is 55 and over.  
* Analyses were limited to U.S. professionals (annual salary reported in USD).
* Respondents were from a convenience sample of [Ask A Manager blog](https://https://www.askamanager.org/) readers. Findings cannot be applied to the larger population. Respondents were predominantly identified as female, white, and early-to-mid career.  
* Even though there were respondents representing more than these 12 industries, this analysis only include those industries that had 3+ respondents for each gender and career stage combination. The cut off of 3 was determined by the minimum number of points to report a somewhat reasonable median salary.  
* Respondents were able to report genders other than "Man" and "Woman". This analysis only reports findings for respondents who identified as either a man or a woman due to small counts.

## Data 

**Data source:** [Ask A Manager Salary Survey 2021](https://www.askamanager.org/2021/04/how-much-money-do-you-make-4.html)  
**Dimensions:** 26,232 rows x 18 columns

## Random Musings
* [ColorHunt](https://colorhunt.co) is a great resource for finding nice color combos.  
* First time using `annotate()`, and it was pretty easy. Not so easy to annotate when you have continuous values mapped to a categorical variable.  
* `ggtext` made embedding legends pretty managable. Tried to use `fontawesome` for the icons but couldn't get it to work in time. Used `Wingdings` and `Webdings` in the meantime, since the symbols were basic.  
* Dotplot design inspired by [this Pudding.cool article](https://pudding.cool/2019/04/eu-regions/) on why regions change their borders in Europe.  


