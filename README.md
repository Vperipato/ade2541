# **Data from: Over 10,000 Pre-Columbian earthworks are still hidden throughout Amazonia** <br />
This set of R computer codes was used to create the predictive model, figures and develop analysis on the manuscript "Over 10,000 Pre-Columbian earthworks are still hidden throughout Amazonia" published on Science research article (DOI: ...).

## **Citation** <br />
É necessário? Talvez remover

## **Abstract** <br />
Pre-Columbian societies are known to have occupied the entire Amazon basin for over 12,000 years, but the scale of their impact on Amazonian forests remains uncertain. Using Light Detection and Ranging information from across the basin, we report the discovery of 24 previously undetected pre-Columbian earthworks beneath the forest canopy. Modeled distribution and abundance of large-scale archeological sites across Amazonia suggest that 10,272–23,640 sites remain to be discovered, and that most will be found in the southwest. We also identified 53 domesticated tree species significantly associated with earthwork occurrence, suggesting past forest management practices. Closed-canopy forests across Amazonia are likely to contain thousands of new archaeological sites around which pre-Columbian societies actively modified forests, a discovery that opens new opportunities for understanding the magnitude of ancient human influence on Amazonia.

## **Earthwork Predictive Model** <br />
The Inhomogeneous Poisson Process (IPP) model fit was performed using the ‘fit_bayesPO’ function of the ‘<a href="https://github.com/GuidoAMoreira/bayesPO" target="_blank">bayesPO</a>’ library in R version 4.0.2. The model was developed by the author of the package <a href="https://github.com/GuidoAMoreira" target="_blank">Guido Alberti Moreira</a>.

### **Figure Files** <br />
<a href="https://github.com/Vperipato/ade2541/blob/main/MainText_figures/Fig02B_IPPResult_EarthworkPredictDist.r" target="_blank">Fig02B_IPPResult_EarthworkPredictDistribution</a> preview: <br />
<img src="https://user-images.githubusercontent.com/65520358/222964131-742e796e-bd79-45ab-860b-f453baae609d.png" width="400">

<a href="https://github.com/Vperipato/ade2541/blob/main/MainText_figures/Fig02C_IPPResult_VariableContribution.r" target="_blank">Fig02C_IPPResult_VariableContribution</a> preview: <br />
<img src="https://user-images.githubusercontent.com/65520358/222964151-62988489-aca5-479c-8c48-b53e5d2eb3b7.png" width="400">

<a href="https://github.com/Vperipato/ade2541/blob/main/MainText_figures/Fig02D_IPPResult_ResponseCurves.r" target="_blank">Fig02D_IPPResult_ResponseCurves</a> preview: <br />
<img src="https://user-images.githubusercontent.com/65520358/222964193-64aa7d5a-0fa6-40b2-9a07-70b0f517475a.png" width="750">

<a href="https://github.com/Vperipato/ade2541/blob/main/MainText_figures/Fig03_DomesticatedBoxplot.r" target="_blank">Fig03_DomesticatedBoxplot</a> preview: <br />
<img src="https://user-images.githubusercontent.com/65520358/222964250-eb813699-ab8e-4f65-a8d9-50d061050877.png" width="800">

Other sets of R computer codes from supplementary material figures are inside the "<a href="https://github.com/Vperipato/ade2541/tree/main/SuppMaterial_figures" target="_blank">SuppMaterial_figures</a>" folder.
