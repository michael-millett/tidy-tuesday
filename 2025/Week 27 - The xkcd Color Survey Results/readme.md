# The xkcd Color Survey Results

In 2010, the [xkcd Color Survey](https://blog.xkcd.com/2010/05/03/color-survey-results/) asked hundreds of thousands of people to name colors they saw, revealing the different ways in which people perceive and label colors.

> Color is a really fascinating topic, especially since we’re taught so many 
> different and often contradictory ideas about rainbows, different primary 
> colors, and frequencies of light.

Thank you to [Nicola Rennie](https://github.com/nrennie) for curating this week's dataset.

# Plot
![Color Survey Plot](color_plot.png)

# Data Dictionary

### `answers.csv`

|variable |class     |description                           |
|:--------|:---------|:-------------------------------------|
|user_id  |double    |The id of the user who gave the answer. |
|hex      |character |Hex code of the color shown to a user. |
|rank     |double    |The rank of the color that the user gave as the name of the color they were shown (join with `color_ranks`to get the color name answer given by the user). Note that this table is a subset of the full answers data where the `color_name_answer` was one of the names of the 5 top ranked colors in the `color_ranks` data. |

### `color_ranks.csv`

|variable |class     |description                           |
|:--------|:---------|:-------------------------------------|
|color    |character |The name of the color (for the 954 most common RGB monitor colors only). |
|rank     |double    |The rank of the color. |
|hex      |character |The hex code of the color. |

### `users.csv`

|variable     |class     |description                           |
|:------------|:---------|:-------------------------------------|
|user_id      |double    |The id of the user. |
|monitor      |character |The user's monitor type. |
|y_chromosome |double    |Whether or not the user reported having a Y chromosome. The data was recorded in this way since chromosomal sex is related to colorblindness. |
|colorblind   |double    |Whether or not the user reported being colorblind. |
|spam_prob    |double    |Probability of the user being a spam user. |
