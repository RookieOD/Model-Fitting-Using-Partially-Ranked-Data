# Model-Fitting-Using-Partially-Ranked-Data
This file is a summary of realization and experiments of this paper. https://www.tandfonline.com/doi/full/10.1080/10485252.2023.2176180


Background

The importance of models for complete ranking data is well established in the literature. Partial rankings on the other hand arise naturally when the set of objects to be
ranked is relatively large. Partial rankings give rise to classes of compatible order preserving complete rankings. We define an exponential model for complete
rankings and calibrate it on the basis of a random sample of partial rankings data. We appeal to the EM algorithm. 


The code contains the followings:

"Compatible": contains our main algorithm (Algorithm 1) to generate compatible set and two alternative algorithms (Algorithm 2 and 3) 

"examples for small t": contains some examples with t is small

"examples for large t": contains some examples with t is large

"EM for single population": contains the code and implement for single population EM algorithm to determine maximum likelihood estimates

"EM for several populations": contains the code for several population EM algorithm (yet to finish)

"Experiments on running time": contains three algorithms to generate the compatible set and experiments recording the running time

"examples for pseudo compatible set": contains the experiments of pseudo compatible sets to test the robustness

"Tutorial": a R markdown file which gives a tutorial for implementing our algorithm


Implementation

This implementation requires R 4.X. The "gtools" package is required.

To implement our algorithm, one can first open the "EM for single population" file. Input the observations and choose one of the algorithm to generate the compatible set (if not given). 
Then use the EM algorithm to estimate the parameters.
One can also test other examples in "examples for small t" and "examples for large t" then use "EM for single population" for estimation.

