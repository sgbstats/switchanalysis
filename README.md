# Switch analysis


The app is available at
[aldc.shinyapps.io/switchanalysis](aldc.shinyapps.io/switchanalysis)

This tool was built to make interpreting switch analysis easier.

If you are a candidate, stop here and go and deliver some more leaflets
and knock on more doors. This is for campaign managers and backroom
staff. If you are unsure what switch analysis is, there is a guide on
the next tab.

As with all switch analyses, a large amount of data is needed. You get
the best results when you have several cycles of data but can still draw
inferences in a start up ward.

### Build your list

1.  Create a list on Connect

    - Select people who have responded to a vote intent question in a
      period
    - Make sure you have filtered by ward/constituency/etc
      - It can cope with multiple wards
    - You can pick a larger pool of people but you risk having a
      substantial number of people with no vote intent

2.  Run your list

3.  Click counts and then quick counts and cross tabs

    <img src="app/images/paste-7.png" width="300" />

### Build your Crosstabs

1.  In column 1, select your previous MPID by selecting the target pools
    option and picking the correct year

    - There are several options you can choose from. If you had locals
      in 2025 then pick MPID 2025, if you have locals in 2024 then 2024,
      etc etc.
      - These are in the target pools section

        <img src="app/images/paste-9.png" width="250" />
    - If you want to look at multiple wards, then choose ward in column
      1 and you previous MPID in column 2

2.  In crosstab 1, choose master questions, vote intent, recent MPID

    <img src="app/images/paste-8.png" width="300" />

3.  Refresh results, export to excel, ignore warnings and save

### Running the analysis

1.  Load the file. The file can be the raw ‘xls’ file from Connect (that
    is actually an HTML file!!) or xlsx file that you have otherwise
    saved.

<img src="app/images/paste-1.png" width="300" />

2.  This will load a familiar looking table, on the left is previous
    voter ID and across is the recent voter ID

    <img src="app/images/paste-2.png" width="300" />

3.  You can expand each row to reveal any subgroups

    <img src="app/images/paste-3.png" width="300" />

4.  If there are multiple groups within your Recent MPID, then tick
    expand columns to show these (although it will collapse all rows
    that are already open).

5.  Tick the Show % box to express each row as a percentage.

    - Each row will be relative to its own number in the denominator and
      so will the aggregate rows
    - Be careful for small numbers in a row distorting percentages.

### Sankey Plot

#### The theory

A sankey plot will show the flow between two sets of states. The wider
the path the more people.

What we need to be careful of here is that there may be different
numbers of people in each group which could distort the size of the bar.

Without adding assumptions, you could be at risk making wrong
interpretation.

#### How to use

When you open the Sankey Plot tab, you will see the assumptions menu.

<img src="app/images/paste-4.png" width="300" />

In here you can add weights to the bars so that the left side is
proportion to your own circumstances.

Currently, I would suggest using your previous election results and then
in new voters add the turnover in your ward.

This defaults to equal weights so you can see how each party is breaking
(but remember there may be distortions due to small numbers).

While no data includes, latent voters (those that have voted before but
don’t have any data) and new data. It is yet to be found whether these
different groups of breakdown differently.

Obviously, if you are starting from further back you are going to need
to switch more voters than if it is very close or you won previously.

The Sankey diagram can be slow to update.

This may be indicative of a prediction but currently is not valid as
one.

You can untick the weighted diagram box but this will distort the data.

#### Crosstabs

<img src="app/images/paste-5.png" width="300" />

If you have an analysis that has extra crosstabs, it will give you the
option here to split by each the different options in the first cross
tab.

## Examples

There are examples of data available to [download from
github](https://github.com/sgbstats/switchanalysis)

### What is switch analysis?

Switch analysis is a toll that we can use to analyse our results from
canvassing. It gives us a detailed breakdown of how voter behave over
time.

#### Why can’t I just count how many definites (Ds) and probables (Ps) there are?

When you knock on doors, the people who answer the door aren’t random
and probably aren’t representative of your ward as a whole. You might go
knocking in your best polling district, get loads of Ds & Ps and then
think that you are going to win by a mile and then be sorely
disappointed come polling day. Conversely, certain types of voters are
more likely to answer the door (even if you did knock on every single
door in your ward) and this may skew the results of just taking the
percentages from the voter intent question.

#### What can switch analysis tell us?

If you have data from at least one, if not several cycles, you can see
how people who have answered a vote intent question multiple times move
between different parties.

### How to interpret switch analysis

<img src="app/images/paste-6.png" width="500" />

In a switch analysis, the previous vote intent is always on the left and
the recent vote intent is always on the top.

What we can see
