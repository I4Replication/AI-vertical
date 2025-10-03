# AI Replication Games Focus Groups — Qualitative Codebook

_Last updated: 2025-xx-xx (update when deployed)_

## Overview
- **Purpose**: Structure analysis of post-pilot focus groups that explore how individuals experienced the AI Replication Games across treatment arms and disciplinary assignments.
- **Sample**: Five focus groups (six participants each). Sessions are stratified by treatment (AI-assisted vs. human-only) and, when feasible, by inside/outside-discipline assignments. Moderator notes the composition at the start of every transcript.
- **Data sources**: Audio recordings (with consent), annotated transcripts, moderator debrief forms.
- **Linkages**: Each coded excerpt should include participant ID, treatment arm, discipline alignment (inside/outside), and focus-group session label.

## Parent Codes and Subcodes

### 1. Motivation and Baseline Context (`MOT`)
- **`MOT_motivation`** – Reasons for volunteering (skill-building, networking, curiosity about AI, institutional incentives).
- **`MOT_replication_experience`** – Prior exposure to replication/reproducibility tasks.
- **`MOT_ai_expectations`** – Expectations about AI support or concerns prior to the pilot (code for both arms).
- **`MOT_training_takeaways`** – Reflections on the pre-game orientation (clarity, usefulness, remaining questions).

### 2. Event Preparation and Setup (`SET`)
- **`SET_materials`** – Feedback on the provided papers, replication bundles, or reporting template.
- **`SET_environment`** – Comments on logistics (workspace, timing, proctoring) that shaped preparation.
- **`SET_support_channels`** – How participants used (or wished for) support from moderators, peers, or documentation before starting.

### 3. Working With AI (AI Arm Only) (`AI_USE`)
- **`AI_USE_tasks`** – Tasks where ChatGPT Plus or related tools were deployed (literature, coding, debugging, interpretation, reporting).
- **`AI_USE_decisions`** – How individuals decided when to prompt, iterate, or stop using AI; guardrails applied.
- **`AI_USE_value`** – Perceived benefits, efficiency gains, or confidence impacts from AI assistance.
- **`AI_USE_limitations`** – Friction points (hallucinations, code errors, latency, interface limitations) and mitigation strategies.

### 4. Working Without AI (Control Arm Only) (`CTL`) 
- **`CTL_strategies`** – Methods used to replace AI functionality (manual debugging, existing scripts, peer lookups).
- **`CTL_perceived_gap`** – Perceptions of equity, fairness, or competitive disadvantages relative to the AI arm.
- **`CTL_support_requests`** – Resources the control group would have valued (extra documentation, guidance, tooling).

### 5. Discipline Alignment and Transfer (`OOD`)
- **`OOD_alignment`** – Experiences when task discipline matched the participant’s background (comfort, leverage of prior knowledge).
- **`OOD_out_of_disc`** – Specific challenges or insights from outside-discipline assignments (terminology, methods, data understanding).
- **`OOD_ai_bridge`** – Ways AI helped or could have helped bridge disciplinary gaps (code for both arms; in control groups capture perceived potential).

### 6. Workflow and Time Management (`FLOW`)
- **`FLOW_planning`** – How individuals structured their seven-hour window (sequencing tasks, checkpoints).
- **`FLOW_turning_points`** – Key moments that accelerated or derailed progress (bug found, prompt breakthrough, data hurdle).
- **`FLOW_collaboration`** – Interactions with proctors, graders, or informal peer exchanges despite the individual assignment.

### 7. Evaluation and Outcomes (`RESULTS`)
- **`RESULTS_success`** – Self-assessment of reproduction success, error detection, or quality of submissions.
- **`RESULTS_time_pressure`** – Perceptions of pacing, stress, or adequacy of the time limit.
- **`RESULTS_rubric`** – Feedback on the replication log, referee rubric, or grading transparency.

### 8. Recommendations and Forward Look (`FUTURE`)
- **`FUTURE_training`** – Suggested adjustments to pre-game training or onboarding materials.
- **`FUTURE_tooling`** – Desired AI features, integrations, or non-AI supports.
- **`FUTURE_scaling`** – Views on how the Games should evolve (team formats, community building, post-event follow-up).
- **`FUTURE_guardrails`** – Governance, ethics, or reproducibility safeguards participants deem necessary.

### 9. Quotable or Ambiguous Content (`META`)
- **`META_quote`** – Memorable statements that illustrate broader themes (capture verbatim with context).
- **`META_clarify`** – Segments needing double-coding or team discussion before final interpretation.

## Coding Guidance
- Apply both parent and child codes when possible (e.g., `AI_USE_tasks` and `AI_USE_value`).
- For mixed-arm sessions (if unavoidable), tag speaker metadata carefully before coding; note cross-arm dialogue where contamination risks emerge.
- When comments touch multiple themes (e.g., AI helping with outside-discipline comprehension), apply multiple codes (`AI_USE_value` + `OOD_ai_bridge`).
- Use memo fields to log emerging patterns, surprising contrasts between arms, and quotes for reporting.

## Analysis Workflow
1. Import transcripts into the qualitative analysis environment (e.g., Atlas.ti, Dedoose, or NVivo) with speaker attributes (ID, treatment, discipline alignment).
2. Conduct an initial pass to apply `MOT`, `SET`, and `FLOW` codes for context.
3. In a second pass, focus on arm-specific codes (`AI_USE` or `CTL`) and cross-discipline insights (`OOD`).
4. Hold weekly inter-coder calibration meetings during pilot analysis to reconcile disagreements.
5. Summarize each focus group using a structured memo (one page) that highlights dominant themes, illustrative quotes, and actionable recommendations for protocol refinement.

## Deliverables
- Coded transcript database with anonymized IDs.
- A synthesis memo for leadership summarizing participant motivations, AI practices, control-group strategies, and discipline-related pain points.
- Appendices with curated quotes aligned to the quantitative findings for future publications.
