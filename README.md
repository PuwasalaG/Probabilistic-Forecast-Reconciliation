# Probabilistic-Forecast-Reconciliation

Possible Structure.  Things that require significant work of deicsions are in **bold**.  Much of this should be open to discussion and debate.

1. Intro
2. Point Forecasting recap and introduction of notation (to be **shortened** or even **combined** with intro or section 3).  COMPLETED
3. Probabilistic (currently about right in terms of length)
  - Definition of Coherence
  - Definition of Reconciliation
4. Analytical solution / Parametric Solution
  - Describe as change of basis and integratation. COMPLETED - QUITE DIFFERENT
  - Gaussian example but **shorter** or put into **appendix**, in particular page 22 SHORTENED - NO APPENDIX
  - Prove true elliptical distributions can be recovered and conditions for when this is possible NEEDS RETHINK
  - Simulations in section 5.2 to be in an **entirely separate simulations** section COMPLETED
5. Sample based solutions / Non-parametric bootstrapping approach
  - Prove that reconciling a sample is equavialent to reconciling distributions (possibly using empirical cdf)
  - Bootstrapping algorithm. To be **shortened** for instance discussion of reparametrisation can be put in an appendix.
  - Similar to above, simulations to be in an **entirely separate simulations** section
6. Evaluation of forecasts
  - Univariate scoring rules (to be **removed or shortened**) COMPLETED - REMOVED
  - Multivariate log score is improper for coherent v incoherent COMPLETED - PROOF IN APPENIDIX
  - Only basis (bottom) level series needed if log score used to compare coherent v coherent - COMPLETED (IMPROVED)
7. Simulations (currently split between parametric and nonparametric - should be included in a single section)
  - Description of DGP to be shortened and parts to be put in an **appendix** 
8. Application
9. Conclusions

George's comments: 

Simulation discussion is based on scoring rules. But these will be 
introduced in Section 6? Ok after I wrote this I realised that the 
suggestion is that all simulations go to Section 7. I am very happy about 
this. 

The proposed strusture is really good. We definitely need to shorten the 
paper is various sections and place content in the Appendix. I think we 
can have a go at JASA if we want - before we go to JBES. Not sure which 
one is better/more suitable. 

