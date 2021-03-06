

* **PLEASE READ THE FOLLOWING EXPLANATION ABOUT WHAT WE NEED TO OPTIMIZE AND HOW WE WILL GET IT DONE IN 4 PHASES.** 
* **ALL DATA ARE PROVIDED BY A WEB-BASED DATA SIMULATOR WITH IMPOSING LIMITATION OF RESOURCES.**
* **"Netflix_Project.Rmd" IS THE MAIN CODE FOR IMPLICATION OF THE FOLLOWING PHASES.**
## PROJECT SUMMARY
Netflix, one of the largest technology companies in the media, has been identified some of its users experience
choice overload and therefore resulting in a psychological phenomenon known as Decision Paralysis. Their
goal was to reduce the time spent browsing to overcome this problem. There is a suggestion system embedded
in Netflix webpage, and their interest lies in designing the system to result in shortest browsing time for the
Netflix users.
There were four factors selected to be explored in this project among many things that likely influence the
amount of time someone spends browsing Netflix , and, in order to investigate the problem statistically,
an Experimental Design method was used to determine the optimal levels of these factors to obtain the
shortest browsing time. Four phases have been involved in the Experimental Design. In Phase I, we identified
the statistically influential factors, in Phase II, we identified the path toward the optimum and located the
location closest to it, and in Phase III, we calculated the optimum point analytically with RSM. Finally, a
confirmation experiment method was conducted in Phase IV, to ensure that the estimated optimum would
be sustainable.
The optimum location of the factors has been identified to be 70 Seconds and 78 Percent for Preview Length
and Match Score respectively. We also identified the Preview Type of Teaser/Trailer would result in shorter
Browsing Time than Actual Content. Tile Size, however, was not a significant factor, set to 0.2 the same as
default level.
For 100 randomly selected users in this configuration, the average browsing time was 10.72 minutes and the
commensurate probability obtained from Phase IV suggests that with the probability of 53 Percent, the
observations will not exceed 11 minutes. 


## PROJECT DESCRIPTION
We are trying to minimize the browsing time netflix users experience in order to choose a program of interest.  This project will explore four of the many possible influential factors. The tile size, preview length, match score, and preview type are the main factors we would like to experiment with. Except for the Preview type, other factors are continuous factors. There is a different strategy to treat categorical factors. There will be separate experiments performed on each Preview level and the Preview type that provides shorter browsing time would be of interest to us.

In this project, the first phase will be the factor screening phase. For this purpose, a fractional factorial design (FFD) or a factorial design (FD) will be conducted to identify the influential factors. The not-significant factor(s) will be excluded from the next step of our experiment. Significant level would be 5 percent.

The second phase will be done in order to ,first, find the path to the optimum (shortest browsing time) by fitting a first-order regression model plus interaction term(s) with central composite design (CCD) at the initial configuration and then we start to move toward the optimum step by step by collecting information in each step with a specific step length till the results improve. Second, we will conduct a quadratic curve test in the vicinity of the point where we observed the optimum result with regards to whether a significant curve exists. When we find a significant curve near that point, we can move on to phase III, where we will conduct experiments to determine the fitted second-order model.

Phase 3 is where we will fit a second-order model in the vicinity of where we identified a significant curve in proximity in phase II. We require the second-order model as it is capable of modeling concavity and convexity in order to identify the maxima and minima. To fit such a model, we need six conditions. We will use a CCD for the central point and the corner points and also 4 axial points will be considered. A second-order model (second-order response surface) will be fitted by 9 points. Although 6 points is sufficient to fit a second-order surface, having 3 extra points results in a more precise estimation of the true response surface. The stationary point of this model could be the most optimum response. Hence, we will calculate the stationary point analytically by a matrix-based calculation in R and report it with its 95 % confidence interval.

A confirmation phase is required to validate our model. We will conduct a confirmation phase to ensure the sustainability of our model. A comparative probability metric approach is chosen to evaluate the model. When comparing the model with observations, it seems appropriate to use a commensurate probability metric.

The objective is to determine the levels of factors that minimize browsing time. This 4-phase experiment will hopefully help us find the location of the optimum.



