Pre-Analysis Plan
# Reproducing with AI Across the Expertise Ladder
Version 0.3 – 12 Jun 2025
# Principal Investigators & Co-authors
Ghina Abdul Baki (University of Ottawa)
Juan P. Aparicio (University of Ottawa)
Bruno Barbarioli (University of Ottawa)Abel Brodeur (University of Ottawa)
Lenka Fiala (University of Ottawa)Derek Mikola (University of Ottawa)
David Valenta (University of Ottawa)et al. (All participants who comply with protocol will be listed after the PIs).
# Funding & Registration
Funded by Open Philanthropy and the Institute for Replication (I4R).
This PAP will be preregistered on OSF and GitHub immediately after pilot power simulations (target September 2025).
# 1. Background & Rationale
Our 2024–25 AI‑Replication Games (Brodeur et al. econstor.eu/bitstream/10419/308508/1/I4R-DP195.pdf) showed that AI assistance changes between‑team performance in reproduction tasks. This study examines the vertical dimension of expertise—undergraduates, graduate students (master’s and PhD), and professors/researchers (postdocs and faculty)—on some of the same tasks. Large‑language models (LLMs) may be a “great equalizer”, narrowing productivity gaps, or could amplify expert advantages through better prompting. Randomizing individuals to AI versus control (human-only) within expertise strata lets us test the following hypotheses.
# 2. Research Questions and Hypotheses
Exploratory: (i) dose–response by years of coding; (ii) learning across events.
The main outcome variables will be achieving computational reproducibility, detecting coding errors, and assessing communication/robustness quality (appropriateness, overall scores, planned checks, and implemented checks).
# 3. Experimental Design
# 3.1 Treatments
Control — Human‑only: participants pledge not to use AI tools.
Treatment — Human + ChatGPT Plus: full subscription (GPT‑4o and successors, code interpreter, vision).
# 3.2 Expertise Strata
Randomization: 1:1 AI vs. control within each stratum.
# 3.3 Sample Size & Power
With a planned sample of 300 individuals, roughly 100 per expertise tier, our 10,000-iteration power simulation starts from three empirical premises: (i) in the control arm, professors/researchers outperform undergraduates by about 15 percentage points in successfully reproducing results; (ii) granting ChatGPT Plus is expected to shrink that gap by at least 40 percent (a six-point improvement for undergraduates); and (iii) hypothesis tests are run at the 5 percent level with standard errors clustered by event-software cells. Under those assumptions, the design attains ≈ 82 percent power to detect the key interaction for undergraduates (δ_UG = –0.06) and over 90 percent power for the joint “compression across all three tiers” test, providing ample sensitivity without exceeding the logistical limits of four one-day events.
# 4. Setting, Participants, & Authorship
# 5. Tasks & Materials
Computational reproduction of a prespecified result.
Error detection (classify major versus minor).
Complete standardized Excel workbook and post-event SurveyMonkey questionnaire (arm, AI usage perceptions or perceived disadvantage, helpful/missing features, perceived performance impact).
One-page referee summary captured via workbook narrative fields.
Materials: PDF, replication package, screenshot, Excel logbook, and ChatGPT transcript template (treatment only). Work window: 7 h (09:00–16:00). Treatment arm completes 1‑h ChatGPT training beforehand.
# 6. Outcome Variables
# 7. Covariates
Years of coding experience (self‑reported).
Preferred software (R, Stata, Python).
Event and article fixed effects.
Prior ChatGPT familiarity (exploratory).
# 8. Statistical Analysis Plan
Our core estimation equation is
where
# Estimation method.
Binary outcomes are estimated with ordinary least squares (linear probability models).
For count outcomes (e.g., number of errors) we use Poisson regression with the same right-hand-side. Standard errors are Huber–White and clustered at the event–software level, reflecting that participants working in the same software environment during the same event share local shocks.
# Secondary analyses.
Replace the three tier dummies with a continuous measure—self-reported years of coding—to estimate a dose–response curve.
Add an interaction between treatment and event order to see whether learning accumulates across events.
Compare planned versus implemented robustness checks as descriptive follow-ups.
Together, these specifications isolate the causal impact of AI access, reveal whether it narrows vertical expertise gaps, and check how robust the findings are to alternative functional forms and behavioral metrics.
# 9. Compliance, Deviations, & Monitoring
Compare assignment with ChatGPT logs and observer notes.
Any protocol change preregistered on OSF and GitHub before data access.
# 10. Data Management & Sharing
Raw logs, transcripts, cleaned data on OSF and GitHub (view‑only until publication).
Replication packages under journal licenses; provide download links.
De‑identified grading sheets included.
# 11. Timeline
# 12. Limitations & Risk Mitigation
Self‑report bias mitigated by continuous experience measure.
Control contamination checked via pledge plus random screen‑shares.
LLM drift documented by logging GPT model names/dates.
# 13. References
1. AI‑Replication Games (2025), Nature.
2. 2024 Pre‑Analysis Plan (OSF).
