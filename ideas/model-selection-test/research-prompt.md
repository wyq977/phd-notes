# Research brief: confidence certificates after sparse regression

## Role

Act as a rigorous statistical researcher working on sparse linear regression
and safe anytime-valid inference.

Your work must proceed in three stages:

1. Establish what is already known for fixed-sample and asymptotic inference.
2. Establish what is already known for anytime-valid inference in ordinary
   linear regression.
3. Only after completing Stages 1 and 2, attempt a finite-sample construction
   for Lasso and exact sparse regression.

Do not skip directly to a proposed solution.
Do not present a new argument as a theorem until every proof obligation has
been checked.
Use primary sources whenever possible, and verify every title, theorem number,
assumption, year, and DOI.

The phrase "all known results" should be interpreted as a comprehensive,
theorem-level survey of seminal results, major modern developments, surveys,
impossibility results, and directly relevant recent work.
Describe the databases, search terms, citation chains, and date range searched,
and disclose plausible omissions.

## Central goal and computational motivation

The central research goal is to construct a genuinely **finite-sample
confidence set or confidence certificate** after high-dimensional sparse
regression.
For a user-chosen $\alpha\in(0,1)$ and a fixed, finite sample size $n$, the
target must have a nonasymptotic coverage guarantee of at least $1-\alpha$.
An asymptotic statement whose coverage only approaches $1-\alpha$ as
$n\to\infty$ does not by itself solve the problem.

Computational speed is the motivation rather than the unresolved bottleneck.
Heuristic procedures such as Lasso are fast, and exact algorithms for
$\ell_0$-constrained sparse regression can also scale to high-dimensional
problems.
In particular, Bertsimas and Van Parys,
["Sparse High-Dimensional Regression: Exact Scalable Algorithms and Phase
Transitions"](https://doi.org/10.48550/arXiv.1709.10029),
give an exact optimization approach and report solutions with provable
optimality at large scales.
Treat this as an optimization result and motivation for the project, not as an
inferential coverage result.

The inferential question is therefore:

> Once a fast sparse-regression algorithm has retained some features and
> kicked out others, how can one attach a valid finite-sample confidence
> statement to the excluded features?

## Statistical setup

For observations $x_i\in\mathbb R^p$ and responses $y_i\in\mathbb R$, define

$$
X_n=\left(x_1^\top,\ldots,x_n^\top\right)^\top\in\mathbb R^{n\times p},
\qquad
Y_n=\left(y_1,\ldots,y_n\right)^\top\in\mathbb R^n.
$$

Assume initially that

$$
Y_n=X_n\beta^\star+\varepsilon_n,
$$

where $\beta^\star\in\mathbb R^p$ is the true coefficient vector.
Write $[p]=\{1,\ldots,p\}$ and define the true support by

$$
S^\star=\{j\in[p]:\beta_j^\star\neq0\}.
$$

The Lasso estimator is

$$
\widehat\beta_{\mathrm{Lasso},n}
\in
\arg\min_{\beta\in\mathbb R^p}
\left\{
\frac12\lVert Y_n-X_n\beta\rVert_2^2
+\lambda\lVert\beta\rVert_1
\right\}.
$$

The exact $\ell_0$-constrained estimator is

$$
\widehat\beta_{0,n}
\in
\arg\min_{\beta\in\mathbb R^p}
\left\{
\frac12\lVert Y_n-X_n\beta\rVert_2^2:
\lVert\beta\rVert_0\leq k
\right\}.
$$

For theoretical statements, use deterministic tie-breaking whenever either
optimization problem has multiple minimizers.
Treat an approximate best-subset algorithm separately from an exact global
$\ell_0$ solution.

Let $\widehat S_n$ denote the raw feature set retained by a sparse-regression
algorithm.
At online time $t$, let $X_t$ and $Y_t$ contain the first $t$ observations and
let $\widehat S_t$ denote the algorithm's current selected set.
Reserve $n$ for a fixed sample size, $t$ for online time, and $\tau$ for a
data-dependent stopping time.

## Primary inferential target

The main target is a certificate for features declared inactive.
It is not merely a confidence interval for one coefficient, a prediction
interval, or a statement about prediction error.

Distinguish the raw selector $\widehat S_t$ from an inferential certificate.
Let

- $U_t\subseteq[p]$ be an upper confidence set for the true support;
- $Z_t=U_t^c$ be the set of features certified as inactive.

At a fixed sample size, the desired guarantee is

$$
\mathbb P\left(S^\star\subseteq U_n\right)\geq1-\alpha.
$$

Equivalently,

$$
\mathbb P\left(Z_n\subseteq(S^\star)^c\right)\geq1-\alpha.
$$

The anytime-valid version is

$$
\mathbb P\left(\forall t\geq1:\ S^\star\subseteq U_t\right)
\geq1-\alpha.
$$

This implies, for every stopping time $\tau$,

$$
\mathbb P\left(S^\star\subseteq U_\tau\right)\geq1-\alpha.
$$

If the sparse-regression output itself is to be certified, then one must prove
the guarantee with $U_t=\widehat S_t$.
This equality is a substantive requirement and must not be assumed merely
because $\widehat S_t$ was produced by Lasso or best-subset regression.
If it is impossible or too costly, investigate a calibrated enlargement
$U_t\supseteq\widehat S_t$ or a separately constructed support upper set.

This is a one-sided support guarantee.
It certifies that kicked-out features are inactive, but it does not certify
that every retained feature is active.
A trivial choice $U_t=[p]$ has perfect coverage but excludes nothing, so every
method must also report informativeness, such as $|Z_t|$, power to certify
inactive variables, or the size of $U_t$.

## Model-comparison motivation

For $S\subseteq[p]$, let $\mathbb L_S$ denote the class of linear models whose
coefficient vectors have support exactly $S$.
A toy comparison is

$$
y=\beta_1x_1+\beta_2x_2+\varepsilon
\qquad\text{versus}\qquad
y=\beta_1x_1+\beta_3x_3+\varepsilon,
$$

represented by $\mathbb L_{\{1,2\}}$ versus
$\mathbb L_{\{1,3\}}$.

For one feature, the hypotheses

$$
H_0:\beta_3=0,
\qquad
H_1:\beta_3\neq0
$$

correspond to the composite model classes

$$
\{\mathbb L_S:3\notin S\}
\qquad\text{and}\qquad
\{\mathbb L_S:3\in S\}.
$$

Pairwise model comparison does not automatically solve variable selection.
One comparison can change several feature decisions, one feature occurs in
many candidate models, and the hypotheses overlap through nuisance
coefficients and supports.
The research must explain how multiplicity and overlap affect any proposed
feature-level certificate.

## Stage 1: established fixed-sample and asymptotic results

First produce a theorem-level literature survey.
Do not begin an original construction until this survey and its gap summary
are complete.

### 1A. Classical linear regression

Find established results on:

- exact finite-sample confidence regions for $\beta^\star$ under Gaussian
  fixed-design linear regression;
- coordinatewise and simultaneous confidence intervals;
- $t$-tests, $F$-tests, Scheffé-type regions, and inversion of model tests;
- asymptotic confidence regions under non-Gaussian or heteroskedastic noise;
- inference after choosing a model using the same data;
- model-confidence sets or collections of candidate supports.

### 1B. Lasso and related $\ell_1$ methods

Find and clearly separate results on:

- estimation and prediction bounds;
- support recovery and sign consistency;
- the sure-screening property
  $\mathbb P(S^\star\subseteq\widehat S_n)\to1$;
- exact support consistency
  $\mathbb P(\widehat S_n=S^\star)\to1$;
- debiased or desparsified-Lasso coefficient intervals;
- selective or post-selection confidence intervals;
- simultaneous coefficient inference and multiplicity control;
- confidence sets for $\beta^\star$;
- confidence sets or upper confidence sets for $S^\star$;
- impossibility and nonuniformity results for sparse estimators;
- deterministic versus data-dependent or cross-validated $\lambda$.

Do not claim that a coefficient confidence interval, prediction bound, or
support-consistency theorem provides finite-sample support coverage unless the
source proves that exact statement.

### 1C. Exact sparse regression and best-subset selection

Interpret "explicit sparse regression" as exact global
$\ell_0$-constrained least squares.
Find results on:

- exact optimization and deterministic tie-breaking;
- finite-sample and asymptotic support recovery;
- model-selection consistency and beta-min or separation conditions;
- confidence intervals after best-subset selection;
- model-confidence sets and support upper/lower bounds;
- honest confidence sets and impossibility results;
- known $k=|S^\star|$, a known upper bound $k\geq|S^\star|$, and the
  misspecified case $k<|S^\star|$.

### Stage 1 completion gate

Before proceeding, state:

1. Which results give coefficient inference only.
2. Which results give support recovery only in the limit.
3. Which results give honest finite-sample coverage.
4. Which results give a confidence collection of candidate models rather than
   one upper support set.
5. Which result is currently closest to
   $\mathbb P(S^\star\subseteq U_n)\geq1-\alpha$.
6. Which impossibility or identifiability results constrain the target.

## Stage 2: anytime-valid inference for linear regression

Before surveying regression, define and relate:

- e-values;
- e-variables and e-processes;
- test martingales and test supermartingales;
- Ville's inequality;
- always-valid $p$-values;
- confidence sequences;
- optional stopping and optional continuation;
- simple and composite null hypotheses;
- likelihood-ratio, mixture, universal-inference, and self-normalized
  constructions.

Then find established anytime-valid results for linear regression, including:

- sequential analogues of $t$-tests and $F$-tests;
- confidence sequences for coefficients and groups of coefficients;
- known versus unknown noise variance;
- fixed, random, predictable, and adaptively chosen covariates;
- model comparison with nuisance coefficients;
- likelihood mixing and invariant sufficient statistics;
- self-normalized confidence ellipsoids;
- simultaneous testing or confidence sequences over many coordinates;
- results advertised for sparse estimation.

Audit Kirschner, Krause, Meziu, and Mutný (2025) especially carefully because
it explicitly discusses sequential linear regression and sparse estimation.
Determine whether its confidence sets are for the coefficient vector,
prediction, or the support, and whether they imply a simultaneous certificate
for excluded features.

### Stage 2 completion gate

Before proceeding, state:

1. Which results are genuinely time-uniform and which are only fixed-time.
2. Which remain valid for every stopping time.
3. Which handle composite nulls and nuisance coefficients.
4. Which provide coefficient confidence regions but not support certificates.
5. What exact mathematical step is missing between the closest known
   regression result and the desired support upper set.

## Stage 3: finite-sample research attempt

Only after Stages 1 and 2 are complete, attempt a new construction.
Treat Lasso and exact $\ell_0$ regression as separate problems.
Do not silently transfer a construction or proof between them.
The primary deliverable of this stage is a nonasymptotic confidence set or
support certificate with coverage at least $1-\alpha$ for a specified finite
$n$.
Do not replace this target with support consistency, an oracle property, or an
asymptotic confidence interval.

Begin with the following assumption ladder, and say explicitly which level is
being used:

1. Fixed design, Gaussian noise, known variance, and deterministic $\lambda$
   or $k$.
2. Fixed design and Gaussian noise with unknown variance.
3. Random design with stated distributional assumptions.
4. Predictable or adaptive covariates in a sequential design.

For each method:

1. State a precise candidate theorem, including the probability space,
   assumptions, confidence level, and whether coverage is conditional on
   $X$, conditional on selection, or unconditional.
2. Check identifiability and known impossibility results before attempting the
   proof.
3. Decide whether the target is the raw selector
   $U_n=\widehat S_n$ or a calibrated support upper set.
4. Consider, without presuming success, inversion of simultaneous tests,
   mapping a coefficient confidence region to a support certificate,
   sample splitting or universal inference, confidence collections over
   supports, likelihood mixtures, and e-process constructions.
5. State every proof obligation, including multiplicity, dependence between
   selection and inference, nuisance parameters, tuning selection, and
   optimization ties.
6. Report computational complexity and whether exact enumeration over
   $2^p$ supports is required.
7. Quantify both coverage and informativeness; reject the vacuous solution
   $U_n=[p]$ as scientifically unhelpful.
8. If a full theorem fails, give the precise obstruction, a counterexample
   where possible, and the strongest defensible partial result.

First solve the fixed-$n$ problem if possible.
Only then examine whether the construction can be upgraded to the time-uniform
event

$$
\{\forall t\geq1:S^\star\subseteq U_t\}.
$$

Label every unproved statement as a conjecture, heuristic, or proof attempt.

## Assumptions that must be recorded for every result

For every theorem in the survey or derivation, report:

- fixed or random design;
- classical $p<n$ or high-dimensional $p\geq n$ regime;
- the relationship among $n$, $p$, and
  $s^\star=\lvert S^\star\rvert$;
- Gaussian, sub-Gaussian, symmetric, or other noise assumptions;
- known or unknown variance;
- independence, martingale-difference, or other dependence assumptions;
- deterministic, estimated, or cross-validated tuning parameters;
- fixed or growing sparsity;
- beta-min, irrepresentability, restricted-eigenvalue, compatibility,
  separation, and identifiability assumptions;
- conditional-on-design, conditional-on-selection, or unconditional validity;
- finite-sample, asymptotic pointwise, or asymptotic uniform/honest coverage.

Never transfer a result between these regimes without a proof.

## Required output tables

### Established fixed-sample and asymptotic results

| Method | Inferential object | Exact guarantee | Finite or asymptotic | Pointwise or uniform | $n,p,s$ regime | Design and noise assumptions | Tuning assumptions | Post-selection? | Primary source and theorem | Relevance to exclusion target |
|---|---|---|---|---|---|---|---|---|---|---|

### Anytime-valid results

| Reference | Object: e-value, e-process, test supermartingale, or confidence sequence | Null and alternative | Regression target | Fixed or adaptive design | Noise and variance assumptions | Nuisance handling | Time-uniform guarantee | Theorem locator | Remaining mismatch |
|---|---|---|---|---|---|---|---|---|---|

### Gap matrix

| Method | Coefficient inference | Support upper set | Certified inactive set | Certified active set | Fixed-sample | Anytime-valid | Closest result | Missing step |
|---|---|---|---|---|---|---|---|---|

### Derivation ledger

| Candidate method | Assumptions | Proposed certificate | Proof obligations | Current status | Failure mode or counterexample |
|---|---|---|---|---|---|

## Guardrails

- Never equate asymptotic support consistency with finite-sample
  $1-\alpha$ coverage.
- Never infer simultaneous support coverage from marginal coefficient
  intervals without valid multiplicity control.
- Never call a prediction interval a support confidence set.
- Distinguish a confidence region for $\beta^\star$, a collection of candidate
  supports containing $S^\star$, and one upper support set containing
  $S^\star$.
- Distinguish conditional-on-selection, conditional-on-design, and
  unconditional probability statements.
- Distinguish the raw selector $\widehat S_t$ from the certificate $U_t$.
- Do not claim retained features are active from the one-sided exclusion
  guarantee.
- Do not call a fixed-time e-value an e-process without proving the required
  time-uniform property.
- Do not assume theory for deterministic $\lambda$ remains valid under
  cross-validation.
- Report negative and impossibility results as first-class findings.
- Do not invent citations or theorem numbers.

## Starter references

This list is a verified starting point, not an exhaustive bibliography.
Extend it through forward and backward citation searches.

### E-values, test martingales, and anytime-valid inference

1. Ramdas, Grünwald, Vovk, and Shafer (2023),
   ["Game-Theoretic Statistics and Safe Anytime-Valid Inference"](https://doi.org/10.1214/23-STS894).
   Start here for a modern overview of e-processes, confidence sequences,
   optional stopping, composite hypotheses, and test martingales.
2. Shafer, Shen, Vereshchagin, and Vovk (2011),
   ["Test Martingales, Bayes Factors and p-Values"](https://doi.org/10.1214/10-STS347).
   A foundational explanation of nonnegative test martingales as sequential
   evidence.
3. Howard, Ramdas, McAuliffe, and Sekhon (2021),
   ["Time-uniform, nonparametric, nonasymptotic confidence sequences"](https://doi.org/10.1214/20-AOS1991).
   A central construction toolkit for modern confidence sequences.
4. Howard, Ramdas, McAuliffe, and Sekhon (2020),
   ["Time-uniform Chernoff bounds via nonnegative supermartingales"](https://doi.org/10.1214/18-PS321).
   Technical foundations for time-uniform concentration via nonnegative
   supermartingales.
5. Vovk and Wang (2021),
   ["E-values: Calibration, combination, and applications"](https://doi.org/10.1214/20-AOS2020).
   Standard definitions and results for calibrating and combining e-values.
6. Grünwald, de Heide, and Koolen (2024),
   ["Safe testing"](https://doi.org/10.1093/jrsssb/qkae011).
   E-variable testing for simple and composite problems, with attention to
   growth-rate optimality.
7. Wasserman, Ramdas, and Balakrishnan (2020),
   ["Universal inference"](https://doi.org/10.1073/pnas.1922664117).
   Finite-sample tests and confidence sets based on split likelihood ratios.
8. Ville (1939),
   [*Étude critique de la notion de collectif*](https://numdam.org/item/THESE_1939__218__1_0/).
   Historical source for the maximal inequality underlying anytime validity.
9. Darling and Robbins (1967),
   ["Confidence Sequences for Mean, Variance, and Median"](https://doi.org/10.1073/pnas.58.1.66),
   and Lai (1976),
   ["On Confidence Sequences"](https://doi.org/10.1214/aos/1176343406).
   Classical confidence-sequence foundations.
10. Shafer (2021),
    ["Testing by Betting: A Strategy for Statistical and Scientific Communication"](https://doi.org/10.1111/rssa.12647),
    and Johari, Koomen, Pekelis, and Walsh (2022),
    ["Always Valid Inference: Continuous Monitoring of A/B Tests"](https://doi.org/10.1287/opre.2021.2135).
    Accessible conceptual and applied introductions.

### Anytime-valid linear regression and sparse estimation

1. Lindon, Ham, Tingley, and Bojinov (2026),
   ["Anytime-Valid Inference in Linear Models with Applications to Regression-Adjusted Causal Inference"](https://doi.org/10.1080/01621459.2026.2692052),
   with the earlier [arXiv version](https://arxiv.org/abs/2210.08589).
   This is the first paper to audit for sequential $t$-tests, $F$-tests, and
   linear-model confidence sequences.
2. Kirschner, Krause, Meziu, and Mutný (2025),
   ["Confidence Estimation via Sequential Likelihood Mixing"](https://arxiv.org/abs/2502.14689).
   Directly relevant because it treats sequential linear regression and sparse
   estimation; verify whether its parameter sets imply support certification.
3. Abbasi-Yadkori, Pál, and Szepesvári (2011),
   ["Improved Algorithms for Linear Stochastic Bandits"](https://proceedings.neurips.cc/paper_files/paper/2011/hash/e1d5be1c7f2f456670de3d53c7b54f4a-Abstract.html).
   Time-uniform confidence ellipsoids for regularized least squares with
   predictable covariates and conditionally sub-Gaussian noise.
4. de la Peña, Klass, and Lai (2004),
   ["Self-normalized processes: exponential inequalities, moment bounds and iterated logarithm laws"](https://doi.org/10.1214/009117904000000397).
   Self-normalized martingale tools underlying sequential regression bounds.
5. Wang and Ramdas (2025),
   ["Anytime-valid t-tests and confidence sequences for Gaussian means with unknown variance"](https://doi.org/10.1080/07474946.2024.2428245).
   A useful unknown-variance building block and a warning that a confidence
   sequence construction need not itself be an e-process.

### Sparse-regression confidence and selection results

1. Ferrari and Yang (2015),
   ["Confidence Sets for Model Selection by F-Testing"](https://www3.stat.sinica.edu.tw/statistica/j25n4/j25n418/j25n418.html).
   Constructs a confidence collection of regression models containing the
   true model under classical assumptions.
2. Li, Luo, Ferrari, Hu, and Qin (2019),
   ["Model Confidence Bounds for Variable Selection"](https://doi.org/10.1111/biom.13024).
   Provides lower and upper model bounds with asymptotic bootstrap coverage.
3. Fan and Lv (2008),
   ["Sure Independence Screening for Ultrahigh Dimensional Feature Space"](https://doi.org/10.1111/j.1467-9868.2008.00674.x).
   A canonical source for asymptotic support inclusion, which is in the desired
   direction but is not a finite-sample $1-\alpha$ statement.
4. Nickl and van de Geer (2013),
   ["Confidence Sets in Sparse Regression"](https://doi.org/10.1214/13-AOS1170).
   Honest adaptive coefficient confidence sets and associated separation and
   impossibility regimes.
5. Pötscher (2009),
   ["Confidence Sets Based on Sparse Estimators Are Necessarily Large"](https://arxiv.org/abs/0711.1036),
   and Pötscher and Schneider (2010),
   ["Confidence Sets Based on Penalized Maximum Likelihood Estimators in Gaussian Regression"](https://doi.org/10.1214/09-EJS523).
   Essential warnings about uniform validity and the size cost of sparse
   estimators.
6. Zhang and Zhang (2014),
   ["Confidence Intervals for Low Dimensional Parameters in High Dimensional Linear Models"](https://doi.org/10.1111/rssb.12026),
   and van de Geer, Bühlmann, Ritov, and Dezeure (2014),
   ["On Asymptotically Optimal Confidence Regions and Tests for High-Dimensional Models"](https://doi.org/10.1214/14-AOS1221).
   Core debiased or desparsified-Lasso coefficient-inference results, not
   support confidence sets.
7. Lee, Sun, Sun, and Taylor (2016),
   ["Exact Post-Selection Inference, with Application to the Lasso"](https://doi.org/10.1214/15-AOS1371).
   Exact conditional coefficient inference after fixed-$\lambda$ Lasso under
   Gaussian errors.
8. Shen, Pan, Zhu, and Zhou (2013),
   ["On Constrained and Regularized High-Dimensional Regression"](https://doi.org/10.1007/s10463-012-0396-3).
   Selection consistency for constrained $\ell_0$ regression under separation
   conditions.
9. Bertsimas and Van Parys (2017),
   ["Sparse High-Dimensional Regression: Exact Scalable Algorithms and Phase Transitions"](https://doi.org/10.48550/arXiv.1709.10029).
   Demonstrates that exact $\ell_0$-constrained optimization can be scalable;
   this motivates the inferential problem but does not provide a finite-sample
   confidence set.
10. Bertsimas, King, and Mazumder (2016),
   ["Best Subset Selection via a Modern Optimization Lens"](https://doi.org/10.1214/15-AOS1388).
   Exact optimization methodology for best-subset regression; it is not an
   inferential coverage result.
11. Lin and Li (2023),
    ["Valid Confidence Intervals for Regression with Best Subset Selection"](https://arxiv.org/abs/2311.13768).
    A preprint on exact conditional coefficient intervals after AIC-based
    best-subset selection, not a support confidence set.

## Expected final deliverable

Return one coherent report containing:

1. A short executive summary.
2. The search protocol and scope.
3. The completed Stage 1 results table.
4. The completed Stage 2 anytime-valid table.
5. A precise gap statement.
6. The Stage 3 candidate constructions and derivation ledger.
7. Any counterexamples, impossibility results, or unresolved proof obligations.
8. A gap matrix comparing OLS, Lasso, and exact $\ell_0$ regression.
9. A prioritized reading list with theorem or section locators.
10. A complete bibliography with verified links.

Use precise probability statements throughout.
Separate established results, deductions from established results,
conjectures, and original proof attempts.
