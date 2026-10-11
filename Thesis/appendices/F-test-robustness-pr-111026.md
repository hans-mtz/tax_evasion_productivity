# Proof-read: appendix F, text Claude drafted 2026-10-11

Scope: the intro sentence (l. 3) and the section "The test under general detection, cost and technology" (l. 23–59). Audience: thesis/JMP appendix (technical reader, PhD level).

## Pass 1

- **l. 3 (pre-existing sentence, same paragraph):** "re-estimates $\ln\hat\beta$ in every replication, so it carries the sampling error of the first stage" is the ", so" construction. "…a bootstrap test that re-estimates $\ln\hat\beta$ in every replication and thereby carries the sampling error of the first stage."
- **l. 47, notation:** "$H'$" leaves the variable implicit; write $\partial H/\partial M$. "The first term is positive" refers to the last expression of @eq-app-residual-general, which has several; "the first term of the last expression" or "$\ln(H(M^*_{it})/H(M_{it}))$" is unambiguous.
- **l. 51, ", and so":** "not deductible, such as energy, fuels or services, and so is reported truthfully" is the consequence construction. Give the reason its own clause:
  > Let $N_{it}$ be a second flexible input that is not deductible, such as energy, fuels or services. The firm gains nothing by overreporting it and reports it truthfully.
- **l. 51, consistency:** the setup defines $G(K,L,M)$, but $D_N\equiv G_NN/G$ needs $N$ in the technology. Add it: "With $N_{it}$ in the technology, $G(K_{it},L_{it},M_{it},N_{it})$, its share satisfies …". Also define $s^N_{it}$, the log share of $N$, which appears in the display without a definition.
- **l. 41:** the imperative ("Evaluate … and add and subtract") switches from the first person used in the chapters. Acceptable in an appendix derivation; optional: "Evaluating the elasticity at each firm's reported materials, and adding and subtracting the log share of true materials, gives, suppressing $K_{it}$ and $L_{it}$,".
- No other issues: the derivative $\partial H/\partial M=1-GG_{MM}/G_M^2$, the log-concavity condition, the size/power distinction and the sign result for $N$ check out.

## Pass 2

Checked the pass-1 corrections; one needs a fix.

- **l. 51, accuracy of the pass-1 correction (and of the text):** "The firm gains nothing by overreporting it" and "not deductible … reported truthfully" hold for the sales tax only. Energy, fuels and services are costs for the income tax, and overreporting them would lower it. The model ignores the income tax (@sec-model), so the claim holds under the model. Corrected suggestion:
  > Let $N_{it}$ be a second flexible input that is not creditable against the sales tax, such as energy, fuels or services. Under the model, the firm gains nothing by overreporting it and reports it truthfully.
- Nothing else missed.
