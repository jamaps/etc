A simple procedure for classifying households when only given the ages of each member in the household

First, it takes a table of individuals with a household key and generates lists for each household with the ages of its members. e.g. `[4,8,38,39,67]`

Then it loops over each list, returning the percent of household members in different age groups (e.g. 0-17, 18-30, etc.). Finally households are categorized into the following:

- 'single_person' - single person living alone
- 'one_gen' - two or more people of the same generation living together (e.g. couple, roommates)
- 'two_gen', - two generations in the same household (e.g. typical family with children)
- 'mutli_gen', - three generations in the same household (e.g. children, parents, and grandparents)
- 'other' - anything else
