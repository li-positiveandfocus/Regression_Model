# Regression_Model
Built regression model to identify the factors of determining house price with R programming

This project develops a regression model with Square feet living space, Building quality grade, Age of house (year_built), View, Waterfront dummy, and Condition that is capable of identifying the most relevant factors of determining house prices. 

This project also intends to understand the causal relationship of these factors to answer our business question and hypothesis. In particular, the model shows how living space size impacts house prices. 

Hypothesis Question:
Is living space size a factor in determining house prices?

Ho: There is no relationship between living space size and house price.

Ha: Larger living space size generally increases house prices.\
       Dependent variable: House price\
       Independent variable:\
            Variable of interest: sqrt_lving - square feet of living space\
	 Control variables: sqrt_lot, floors, # of bedrooms, # of bathrooms, waterfront&views.

Our final model is:

House price = 5,312,656 + 163.284 * sqft_living + 136,002.8 * grade - 3,154.557 * yr_built + 48,114.35 * view + 596,225.8 * waterfront + 14,173.64 * condition

Based on the model, we can say that： For every extra square footage, the house price will increase by 163.284 dollar on average. 

Validity & Limitations

Threats to Internal Validity:
-	Omitted variable Bias : Add control variables till the estimated β1 was consistent.
-	Misspecification of the functional form: Dependent variable is continuous.
-	Measurement errors:Could be present, not enough information to account for it.

Threats to External Validity:
-	Differences in population: Estimated house sale price using the data from King county, Washington, USA. This may not hold true for other counties in the USA or in other countries.
-	Differences in setting: The method evaluating the condition or grade level of the house may vary.
