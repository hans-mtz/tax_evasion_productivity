---
name: outline
description: Builds the outline of a section, chapter, or paper before any prose is written, so every part provides unique minimum information the reader needs to keep following the argument, and then audits existing text against that outline and provides recommendations for improvement. Use it when the user asks for an outline, a structure, "what should this section contain", or a comparison of existing text with an intended structure, even if they don't say "outline". Also use it when the user asks whether a section answers the reader's questions, or what the reader knows or needs at a given point.
---

# Outline

After reading the title, abstract, and couple of paragraphs of the Introduction, the reader will infer the paper's argument (RAP) and will have follow up questions about  how it is developed and supported in the body of the paper. An outline helps the writer to organizes research details around those questions.

Since we want the reader to be able to navigate through the text and be able to see the development of the argument, how it is supported and find answers to her own questions, the outline should be build by linking the reader's follow up questions to the headings and takeaways, the two most visible elements in the body ot the text.

A heading is a title for a particular chunk of text (section, subsectio, etc.). A takeaway is a mini-paragraph immediately following the heading and offers readers the main message for that section.

The outline lists the full set of headings and takeaways to develop and support the RAP.

An outline is a plan of what the reader must take away, in the order they need it. It is not a table of contents. Each entry is a claim the reader should hold after reading it, with the reason it is there. The author owns the outline: propose it in chat, revise it, and write it to a file only after they approve.

## Before outlining

1. **Reader state, blind.** Read only what precedes the section: the abstract, the introduction, earlier chapters, the chapter opener, and the sections before this one. Do not open the section itself, even if you have read it before. Write down:
   - **What the reader knows** at the section's first line: definitions, equations, results and numbers, each with the line where it was given.
   - **What the reader asks** at that point: the follow-up questions the preceding text raises and leaves open, each with the line that raises it. Phrase them the way the reader would ("you have estimates, now what about the counterfactual?"), not as topics.
   - **Inconsistencies** in the preceding text that the reader will trip over, such as two passages that say different things about the same object.

   Never build a question from what the section contains. A question built that way is always "answered", so the check finds nothing.
2. **The section's job.** From the reader's questions, state in one sentence what this section must answer that no other section can. Mark any question that belongs to a later section (results, scope, robustness) and say which one.

   Present the reader state, the questions and the job, and wait for the author to agree before going on. The author may reframe the job, and the rest of the outline or audit follows from their version.
3. **Dependencies going out.** Read what follows and list what later sections take from this one (definitions, equations, assumptions, labels they cite). Anything a later section uses but this one does not supply is a gap; anything no later section uses is a candidate to cut or move.
4. **Minimum.** For each candidate item ask: can the reader keep up with the rest of the document without it? If yes, it goes to an appendix, a later section, or out. Say where it goes.
5. **The argument the section serves.** Connect the section to the document's research question, answer, and positioning (see the argument-rap skill, if the document has one). The outline opens with that link.

## Building the outline

- **Order: high level to detail.** Start with the idea and how it serves the document's argument, then the ingredients, then the mechanics, then the implications the later sections use. A reader who stops after any level should hold a coherent, correct picture.
- **One entry, one takeaway.** Write each entry as a sentence the reader could say back ("A higher purchase rate raises overreporting for any detection function"), not a topic ("Comparative statics").
- **For each entry give:** the question it answers (why the reader needs it now), what must appear (equation, fact, definition, cite), and what it hands to a later section.
- **Explicit exclusions.** List what looks like it belongs but goes elsewhere, with the destination. This prevents the section from growing back.
- **Open decisions.** List choices the author must make (scope, notation, where a result lives) with a recommendation for each. Do not settle them silently.
- **Length and levels.** Two to three levels are enough. Keep each entry to a line or two; the detail belongs to the prose.

## Presenting

Show the outline in chat, in this order: the section's job and the questions it answers, the outline itself, the exclusions, the open decisions. Then wait. Do not write a file, and do not start comparing or drafting, until the author approves or revises.

## After approval: the audit against existing text

If the author asks only for a check of existing text ("is this section answering the reader's questions?"), still run the blind reader state from step 1 first and get agreement on it. The audit compares the section against those questions, or against an approved outline if one exists.

Run these steps one at a time, each with the author's go-ahead.

1. **Compare.** Read the existing text and mark each outline entry as covered, partial, missing, or misplaced. Also list existing content that matches no entry (cut, move, or add an entry). Present it as a table: entry, status, where it is now, what to change. The questions are fixed by the agreed reader state. Do not add, drop or reword a question because of what the section says. If the section answers something no agreed question asked, list it as content that matches no entry (cut, move, or propose a new question for the author to accept).
2. **Reuse.** For missing and partial entries, search the author's earlier writing (older drafts, notes, slides, approved papers) for text that can be adapted. Report source, what it covers, and what must change (notation, scope, claims that no longer hold). Do not paste it in.
3. **Proof-read** with the proof-read skill, presenting findings in chat.
4. **Author edits.**
5. **Final proof-read** of the edited text.

Save the approved outline and the audit tables where the project keeps working notes so later sessions can find them.

## Style

Follow the document's conventions for spelling, notation, and terms, and any standing rules in the project's CLAUDE.md or memory (for example, no em-dashes). Keep terms fixed once chosen.
