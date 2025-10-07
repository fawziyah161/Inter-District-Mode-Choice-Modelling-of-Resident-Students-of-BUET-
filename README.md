**Inter-District Mode Choice Modelling of Resident Students of BUET**


**//Short description//**

Discrete choice study of inter district travel by BUET resident students.
Survey size 241. Alternatives bus, AC bus, train, personal car, rental.
Estimated with R and Apollo across thirteen specifications.
Final model shows strong fit and clear behavioral signals.

**//Overview//**

This thesis examines how resident students of BUET choose among five inter district modes from Dhaka. The analysis quantifies tradeoffs among monetary cost, travel time, distance, perceived comfort, and income. I designed and piloted a revealed preference questionnaire, collected 241 valid responses, built alternative specific attributes and availability flags, normalized continuous variables, and mapped every survey field to a utility term or control.

Modeling was conducted in R with the Apollo package. I developed thirteen multinomial logit specifications, starting from constants only and progressing to richer formulations with cost, time, distance, comfort, and income terms and targeted interactions. I kept a model only when log likelihood and information criteria improved. I tracked standard errors and t statistics across the ladder and checked signs and magnitudes. The final model reaches adjusted rho squared near 0.47 and provides coherent behavioral signals across retained parameters


**//Key findings//**

1. Bus and AC bus are price-sensitive. Higher fares reduce choice probability.

2. Travel time carries negative utility for the AC bus, train, and rental.

3. Greater distance shifts riders toward the AC bus and train, and away from the rental.

4. Family income aligns with personal car choice. Self-income supports rental.

5. Comfort improves the utility of the AC bus and personal car and is positive for rental.



**//Research implications//**

**Fare and discounts** :
Student fare instruments can move shares for the bus and AC bus in a predictable way.

**Reliability and comfort**:
Long-distance services gain with punctuality and seat quality, especially AC buses and trains.

**Short-range access**:
Rental behaves as a short-range solution. Design safe and convenient pick-up zones near campus and terminals.

**Segmented messaging**:
Income and car access segment behavior. Tailor communication and programs accordingly.




**//Data description//**

**Unit of observation**:
Individual student choice among available inter-district modes for a Dhaka origin trip.

**Alternatives**:
Bus, AC bus, train, personal car, or rental. Rare modes were screened out during preparation.




**//Core variables//**

Cost- continuous, normalized

Time- continuous, normalized

Distance- continuous, normalized

Comfort- Likert-style score, recoded and centered

Family income- bracketed and encoded

Self income- bracketed and encoded

Car ownership and basic demographics

Per alternative availability flags





**//Methods//**

Multinomial Logit estimated with Apollo in R

Thirteen ascending specifications from constants only to full attribute sets and interactions

Selection guided by log likelihood, AIC, BIC, adjusted rho squared, and t tests

Variable scaling for numerical stability, availability applied per respondent and per alternative

Diagnostics include sign checks and sensitivity to rescaling





**//Results summary//**

Observations 241

Final model adjusted rho squared about 0.47

Information criteria improved step by step across the retained ladder

Coefficients have expected signs. Effects near the threshold are labeled as suggestive and not stated as firm at the five percent level
