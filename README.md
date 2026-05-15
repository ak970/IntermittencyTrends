# IntermittencyTrends
This repository contains data and analysis looking at long-term changes in stream intermittency across the continental United States, which is published in [Zipper et al. (2021) *ERL*](https://doi.org/10.1088/1748-9326/ac14ec).

If you use this code or data in your own manuscript, please cite the following publication:
	Zipper, SC, et al. (2021). Pervasive changes in stream intermittency across the United States. *Environmental Research Letters.* doi: [10.1088/1748-9326/ac14ec](https://doi.org/10.1088/1748-9326/ac14ec)

This work was supported by the NSF-funded [DryRiversRCN](https://www.dryriversrcn.org/). 


My methodology

Python files - data aggregate from daily scale to annual
Master data created

R code - IntermittencyTrends git
Code->Mine
1. trend_updatedFinal - it will take input the master data and the region file (AI region, also created in Peninsular Rivers repo) and will output master dataset updated with region column, and train-test column, and also a trend file output
2. RandomForest_Preliminary...file



ORDER OF EXECUTION AND CATEGORIES

Here is the Official, Sequential Execution Order for the entire repository, divided into logical phases. (I have added checkmarks ✅ next to the ones we have already adapted and completed together!)



PHASE 1: Data Preparation & Core Trends
These scripts build the foundational datasets that every other script relies on.

1. 000_CalculateMetrics.R & 00b...
What it does: Converts daily data to annual metrics.
Status: ✅ Skipped (You brilliantly built your own Master_Annual_Dataset.csv instead).

2. 00a_SelectGagesForAnalysis.R
What it does: Runs the Mann-Kendall and Poisson trends, and splits the data into Train/Test subsets.
Status: ✅ Completed (Output: gauge_trends.csv and gauges_annual_summary_out.csv).

3. RedundancyAnalysis.R
What it does: Uses shapefiles to find physically nested catchments to prevent double-counting.
Status: ✅ Completed (Output: RedundancyAnalysis_RedundantGages.csv).



PHASE 2: "State" Machine Learning
These scripts determine why rivers went dry in specific years.

1. 01_RandomForest_PreliminaryVariableImportance.R
What it does: The "Scout". Tests all 100+ variables. ✅ Completed

2. 02_RandomForest_FigureOutNumPredictors.R
What it does: The "Optimizer". Finds the perfect number of variables to prevent overfitting. ✅ Completed

3. 03_RandomForest_TuneHyperparameters.R
What it does: The "Tuner". Finds the best tree depth and splits. ✅ Completed

4. 04_RandomForest_RunModels.R
What it does: Runs the final models and outputs predictions and PDPs. ✅ Completed
(Once Phase 2 is done, you can run RandomForest_Validation.R and RandomForest_VariableImportance.R anytime!) ✅


PHASE 3: "Trend" Machine Learning (Optional, but in the repo)
These scripts determine why rivers are drying up over a 40-year period.
(Note: We haven't run these yet! These use the same ML logic as Phase 2, but they predict the mk_tau slopes instead of annual days).

1. RandomForestTrends_01_VariableImportance+NumPredictors.R

2. RandomForestTrends_02_TuneHyperparameters.R

3. RandomForestTrends_03_RunModels.R

PHASE 4: Generating the Figures (Can be run in any order!)
Once the phases above are done, the scripts in the figures_manuscript/ folder do not depend on each other. You can run them in any order you want.

Trend Figures (Relies on Phase 1)
- Trends_MannKendall+MannWhitney.R ✅ (Histograms & P-Value Maps)
- Figure_TrendMap.R ✅ (Sen's Slope Maps)
- Trends_Violins.R ✅ (River drying by latitude/region)
- Trends-Climate_Violins.R ✅ (Climate changes by latitude/region)
- Trends_CompareTrendsToDrivers.R ✅ (Is climate causing the drying?)
- Redundancy_CompareTrends.R ✅ (Did deleting nested catchments change the results?)

Machine Learning Figures (Relies on Phase 2 & 3)
- RandomForest_Validation.R ✅ (Scatterplots of R-Squared)
- RandomForest_VariableImportance.R ✅ (Bar charts & Partial Dependence curves)
- RandomForestTrends_Validation.R (We haven't done this yet)
- RandomForestTrends_VariableImportance.R (We haven't done this yet)













=====================================================
Files and the interpretation of their outputs
=====================================================

RandomForest_Validation.R: Creates scatter plots showing how perfectly your ML models predicted the actual streamflow data.

This script is going to produce some of the most important visuals for your paper! It will create Scatter Plots showing exactly how closely your model's predictions hugged the 1:1 line (Predicted vs. Observed), and it will output a CSV containing all the formal validation statistics (like RMSE, KGE, and 
R2) to put into your manuscript tables.

RandomForest_Validation-FitTable-Test.csv: Check this CSV first! It will show you the exact performance gap between your National and Regional models. This is perfect for copy-pasting directly into your manuscript.


RandomForest_Validation-CompareNationalRegionFit.png: This is a brilliant visualization showing exactly what we talked about earlier. Any points that fall above the dashed 1:1 line are regions where the custom regional model significantly outperformed the clunky National model!


RandomForest_Validation...Train+Test.png: Beautiful 6-panel scatter plots. A perfect 1:1 line means flawless prediction. If you see horizontal or vertical clusters of points near zero, it confirms the "zero-inflation" problem we discussed earlier!

=====================================================

RandomForest_VariableImportance.R: Creates the beautiful bar charts/heatmaps showing which climate or topography variables are driving the intermittency.




=====================================================

Figure_TrendMap.R: Creates a spatial map of your region with upward/downward pointing arrows showing exactly where rivers are drying up or getting wetter.


How to interpret this map:
Unlike the correlation maps, the color bar here represents real days.
If a dot is solid red, and the color scale says 2, it means that river has historically lost an additional 2 flowing days every single year (e.g., losing 20 days per decade).
If a dot is hollow (an empty circle), it means the trend exists, but it wasn't strong enough to pass the 95% statistical significance test.

=====================================================

Trends_Violins.R: Creates violin plots grouping your catchments by Arid, Humid, etc., to see which climates are changing the fastest.
It uses Violin Plots (which are like boxplots, but they show the full distribution curve of the data) to compare how trends vary across different climates and latitudes.

How to read this output for your paper:
The Regional Plot (Left side): Look for regions where the "belly" of the violin is sitting either entirely above or below the 0 line. If the belly is way above the line for No-Flow Days, it means practically every river in that climate zone is rapidly drying up.
The Latitude Plot (Right side): Because we flipped the coordinates (coord_flip), the Y-axis literally represents North-to-South geography! You can look at this and instantly say: "Catchments in the deep south (10°N - 14°N) are seeing a decrease in zero-flow days, while northern catchments (22°N - 26°N) are drying rapidly."

=====================================================

Trends_MannKendall+MannWhitney.R

It takes the trends you calculated in Path A and plots them on a geographic map. It also calculates exactly what percentage of your rivers are drying up versus getting wetter.

What to expect:
When you run this, watch your console at the very bottom! It will automatically print out sentences like: Total gauges: 140 and % Drying (More Zero-Flow Days): 35%. This gives you the exact statistics you need to type into your results section!
Check Trends_MannKendall-Maps.png. You will see the beautiful outline of Peninsular India, with red dots highlighting the stations that are rapidly drying up, and blue dots highlighting the ones that are getting wetter.


=====================================================

Trends_CompareTrendsToDrivers.R

This script explores a very important hydrological question: "Are the trends in stream drying caused by trends in climate?"
For example, if a river is gaining 2 zero-flow days per year, is it because precipitation is dropping by 5mm per year? Or is it something else?



The Master Driver
The original authors used p.pet_cy (Aridity Index) as their master climate driver for these plots. I have set the updated script to use p (Precipitation) as your master driver. If you'd rather compare against tmax, pet, or aet, just change the main_driver variable at the top of the code!


What to look for in your Console Output:
When you run this script, it will print three very exciting things into your console:

Trend Correlations: Is the correlation between tau_p and tau_Zero_Flow_Days_sum negative? It should be! If it's ~ -0.5, it means: As precipitation increases, zero-flow days significantly decrease.

Top 5 Static Variables: The code mathematically removes the effect of precipitation, and then scans all your static variables (like num_dams or cwc_area) to see what explains the remaining trend. It will print the Top 5. (For example, if num_dams is #1, it means dams are the biggest secondary cause of rivers drying up!).

Multiple Linear Regression: It takes your climate driver + those top 2 static drivers and runs a traditional statistical regression. Look at the Adjusted R-squared at the bottom of the summary—this tells you exactly how much of your drying trend is explained by those three factors combined!


------ new addition ------

Here is the automated "Best Driver Finder" loop:
You can just copy and paste this entire block at the bottom of your current Trends_CompareTrendsToDrivers.R script.
It will automatically scan all your tau_ columns, test them region-by-region, and print a clean summary table showing the #1 driver for every single zone!

What this code does:
Gathers Candidates: It looks at your master dataframe and automatically identifies every single column we calculated a trend for (e.g., tau_p, tau_tmax, tau_spei12, etc.).
The Nested Loop: It groups your data by region. Then, it runs a correlation test between tau_Zero_Flow_Days and every single candidate.
The abs() Check: Because a correlation can be strongly negative (e.g., more rain = fewer dry days), or strongly positive (e.g., more heat = more dry days), it uses abs(current_cor) to find the most powerful driver regardless of direction.
The Output: It spits out a clean table. You might see output like this:
Arid | Zero_Flow_Days | pet | 0.81
Humid | Zero_Flow_Days | p | -0.76
Coastal | Zero_Flow_Days | urban_pct | 0.65



DIFFERENCE BETWEEN RANDOM FOREST VARIABLE IMPORTANCE RESULTS AND THE CURRENT RESUTLS (MAJOR DRIVER)

If the Random Forest already told us that Precipitation is the most important variable, why do we need to do this separate linear correlation analysis on the trends?
The answer lies in the difference between Inter-annual Variability (State) and Long-Term Change (Trend). They answer two completely different physical questions.
Here is the breakdown of the differences and the benefits of using both:
1. The Question Being Answered
Random Forest (State): Answers the question: "Why did this river go dry in 1995, but flow perfectly in 1996?"
It looks at absolute values year-by-year. If 1995 had low precipitation, the RF learns that low rain = dry river.
Trend Correlation (Driver Analysis): Answers the question: "Why is this river drying up by +2 days per year over the last 40 years?"
It does not look at individual years. It looks at the slopes (τ). It proves whether the long-term drying of the river is being caused by the long-term climate change of the region.
2. The Math (Non-Linear vs. Linear)
Random Forest: Is a complex, non-linear algorithm. It is brilliant at finding thresholds. For example, it might learn that temperature doesn't matter at all... until it hits 35°C, and then the river instantly dries up.
Trend Correlation: Is a simple linear math equation (y=mx+b). It is brilliant at finding proportional relationships. It proves that for every 1 unit of precipitation lost per decade, the river loses exactly 5 days of flow.
3. The "Baseline" Problem (Spatial vs Temporal)
Random Forest: Compares all stations against each other. If you have a station in the arid Deccan Plateau (100 dry days/year) and a station in the wet Western Ghats (0 dry days/year), the RF uses their massive differences in baseline climate to build its rules.
Trend Correlation: Eliminates the baseline entirely! It only looks at the rate of change. If the arid station is losing 2 days of flow per year, and the wet station is also losing 2 days of flow per year, this analysis connects them and says, "Aha! Even though they are in different climates, they are reacting to the exact same regional trend in climate change."
The Benefit: Why your paper needs both!
Imagine you discover that your rivers are gaining +5 Zero-Flow Days per year (they are drying up fast). You need to tell policymakers why.
Scenario A (The RF matches the Trend):
The Random Forest says: "Precipitation controls the river."
The Trend Correlation says: "The trend in Precipitation perfectly matches the trend in the river drying."
Your Conclusion: "The rivers are drying up strictly because of climate change (declining rainfall)."
Scenario B (The ultimate scientific discovery):
The Random Forest says: "Precipitation controls the river."
The Trend Correlation says: "Wait, Precipitation has NO long-term trend! The rainfall is exactly the same as it was in 1980!"
Your Conclusion: "If the rain hasn't changed, but the rivers are drying up, human activity must be to blame."
This is exactly why the code I just gave you ran the Residual Analysis! If you remove the climate trend, and suddenly Total_Dams or urban_pct strongly correlates with the leftover river trend, you have just mathematically proven that human development is causing the rivers to dry up despite a stable climate!
Using both methods allows you to say: "Climate governs the day-to-day flow (Random Forest), but Human Activity is driving the long-term decadal drying (Trend Analysis)."


=====================================================

Trends_CompareTrendsToDrivers.R

Trends-Climate_Violins.R

This script is the perfect companion to the previous Violin plot!
While the first Violin plot showed you where the rivers are drying up, this script plots the trends for your climate drivers to see if the climate itself is getting hotter or drier in those exact same regions and latitudes.
What I changed for your dataset:
The Climate Metrics: The original authors used their specific climate columns (p_mm_cy, T_max_c_cy, etc.). I swapped these out for your equivalents: p (Precipitation), pet (Potential Evapotranspiration), tmax (Max Temp), and tmin (Min Temp). (If you want to plot different ones, like spei12_drought_count, just change the metrics list at the top!)

----------------

What to look for when you open the plot:
Because you just generated the "Intermittency" (Zero-Flow) violins a few scripts ago, put the two images side-by-side!
If you see that the "Arid" region's No-Flow Days violin is pointing upwards (positive trend = drying), and then you look at this new plot and see the Arid region's Precipitation violin is pointing downwards (negative trend = less rain), you have a perfect, visual, undeniable proof of your driver!


=====================================================

Redundancy_CompareTrends.R

This script addresses a very specific peer-review question: "Are your results biased because some of your stream gauges are located on the exact same river, right next to each other?"

To test this, the authors identified "Redundant" gauges (gauges that are too close together or capture the exact same watershed), temporarily removed them, and re-ran the trends to prove their median regional trends didn't artificially inflate.


How to interpret Plot 3:
When you look at Redundancy_CompareMedianTrend.png, you will see a dashed 1:1 diagonal line.
If your points land exactly on the line, it means your results are robust! Keeping or deleting those overlapping gauges didn't change the regional median trend at all.
If your points drift far away from the line, it means your regional trends were heavily biased by a cluster of gauges on the same river acting identically.


=====================================================

RandomForestTrends_01_VariableImportance+NumPredictors.R

In Phase 3, we are predicting the Long-Term Trend (Kendall τ).
Because a river only has one trend over its 40-year history, this script completely collapses your dataset down to just 1 row per gauge. It calculates the 40-year average of your climate (e.g., Mean Precipitation from 1980-2020) and uses it to predict the 40-year slope of the river.


What I updated for you:
Dynamic Lists & Sanitization: Brought in your exact predictors_climate, human, and static lists, and added make.names().
Stability Fixes: I forced conditional = FALSE and ncores = 1 so this runs quickly and mathematically securely on the smaller dataset without throwing -11 or 400+ importance errors!
Modern Tidymodels: Updated the deprecated pull_workflow functions.
Dynamic Elbow Finding: The script automatically finds the optimal number of predictors instead of hardcoding c(27, 23, 16).


RandomForestTrends_02_TuneHyperparameters.R




RandomForestTrends_03_RunModels.R

This script takes the best variables (from Script 01) and the best hyperparameters (from Script 02) and builds the final, highly-optimized model to predict the 40-year drying trends of your rivers.
What I upgraded for you:
Dynamic Variables: Just like before, I removed the hardcoded c(27, 23, 16) and replaced it with a dynamic CSV reader that grabs the exact optimal numbers from your Script 01 output.
Fixed an Original Author Bug: The original authors made a typo in this script! In the previous script, they saved the tuning results under the name tau_annualnoflowdays. But in this script, they try to search for the tuning results using just annualnoflowdays (without the tau_). I fixed this mismatch so it correctly finds your tuned hyperparameters!
Modern Tidymodels: Updated the deprecated pull_workflow_fit() functions to modern extract_fit_engine().
File Paths & Column Names: Swapped gage_ID to gauge_id and routed everything to results/Mine.


=====================================================
RandomForestTrends_Validation.R

This script validates your Trend machine learning models. Instead of predicting the number of dry days, it tests how accurately the model was able to guess the long-term Kendall 
τ
τ
 slope of each river.
What I fixed/upgraded for you:
The Missing R2 Function: Just like in the previous validation script, the original authors didn't include the R2 function formula. I injected it back in so the script doesn't crash when trying to calculate the R-Squared.
Fixed a Facet Labeller Bug: In the original code you pasted, the authors made a typo in their facet_grid labeller. The metric in the dataframe is named "tau_annualnoflowdays", but they tried to label "annualnoflowdays". This would cause their graph labels to fail and just print the raw variable names! I fixed this and updated it to your specific metrics (tau_Zero_Flow_Days_sum, etc.).
ggsave Syntax: Moved ggsave to its own line so ggplot2 doesn't throw an error.
Added Dynamic Colors: Automatically generates the region colors.



RandomForestTrends_VariableImportance.R


It creates the exact same style of Bar Charts we made earlier, but instead of showing what drives the annual zero-flow days, it shows what drives the 40-year drying trends (e.g., "Rivers are drying up primarily because the long-term average Precipitation is low").
What I upgraded for you:
Dynamic df_pred Table: Just like in the previous Variable Importance script, the authors hid the df_pred table in their helper files. I recreated it here using your exact sanitized predictor lists so the colors map perfectly to Climate, Land Use, and Physiography.
Removed Hardcoded Axis Limits: The original authors hardcoded their X-axis to stop at 9% (breaks = c(0, 0.03, 0.06, 0.09)). If your variables have a 25% importance, their code would literally crop the bars out of the graph! I removed this so the axes automatically scale to your true data.
Fixed Factor Ordering: The original code had a bug where the bars wouldn't sort strictly from highest to lowest. I fixed this using rev(long_name).

