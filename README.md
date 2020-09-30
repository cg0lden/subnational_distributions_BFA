# subnational_distributions

Calculation of Burden of disease for fish consumption reduction/increase

Initial thoughts:
The approach I suggest is a result of the following points:
The calculation is a country by country global analysis
DALYs is the preferred metric because it allows us to sum burdens and also compare overall results with the GBD global metrics.
We should try to use existing data and simplify our calculations. Trying to build new relative risk curves and calculating DALYs by our own and per country is time consuming and complicated. Lets try to leverage existing referenced data.


I suggest using the GBD project global values as they are the best country by country estimate of the global burden of disease we have.
The GBD project (Afshin 2019 and James 2018) has per country DALYs for vitamin A, iodine and iron deficiencies (and other nutrient deficiencies) and low seafood consumption (PUFA->heart disease) - four risk factors associated with fish. While not comprehensive of all outcomes we combed from the literature, it seems to include both micronutrient contribution and low PUFA derived from fish. Another potential source of micronutrient deficiency DALY burden per country is this.
We then perform the following calculation per country, and for each of the four risks, deriving the delta of DALYs (changes in health burden) as response to moving from the reference scenario to the alternative scenario:

                                        ∆DALY=DALY*(RRalt/RRref-1)

Where DALY is the value derived from the GBD per country and per health risk, and RRalt and RRref are the population level relative risk for the alternative (“shocked”) and reference (“baseline”) scenarios, respectively. RR is equal to:

                             RRref/alt = ∫(Intake_distributionref/alt*rr_curve)

rr_curve is the relative risk curve for each of the 4 risk factors. For PUFA it can be derived from the GBD source or any other one we want to use (for example the Thomsen paper used the Mozaffarian and Rimm 2006 curve and simplified it to a descending linear curve, until a consumption level of 250 mg/cap/d EPA+DHA).  
For the micronutrient calc, I suggest building the rr_curve by taking country level EAR value and constructing a descending risk curve based on the CDF (cumulative distribution function) of a normal distribution. IOM suggest using a CV of 10%-15% when there is no sufficient data on the requirements. Multiplying intake distribution with this rr_curve and integrating will result in the prevalence of inadequacy (probability method for calculating deficiency), in effect a population-level risk (RR). As the above reference indicates, as long as the requirement distribution is symmetrical (not for iron) the results are insensitive to the shape and SD of the requirement curve. 
As for intakes, I suggest taking the average nutrients/PUFA from the FAO model (for both scenarios) and building a gamma curve around it; using Monte Carlo we can then assess how changes in the tail of this distribution affect the overall calcs.  

Overall health burden resulting from the perturbation in consumption will be the sum of all burdens examined for each country: ∑∆DALYi.

This approach allows comparison across the different risks and assess their relative contribution to the total health burden following a shock. For example in developing countries one might expect that the DALY values from micronutrient deficiencies will be sensitive to shocks more than in developed countries and possibly on par with changes in EPA+DHA contribution to health. 
