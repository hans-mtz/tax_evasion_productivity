---
name: transcribe-meeting
description: Transcribes a meeting or voice recording locally with whisper.cpp (no upload), checks the transcript for errors, and turns it into a structured summary of the points made, organized by the document or topic discussed. Use it when the user shares an audio file (m4a, mp3, wav) of a supervisor meeting, seminar, or interview, or asks to transcribe, summarize, or extract feedback from a recording. Checks that the software is installed and installs it if the user agrees.
---

# Transcribe a meeting

Claude cannot listen to audio, so the recording is converted to text on the user's machine with whisper.cpp. Nothing is uploaded; only the speech model is downloaded once.

## 1. Check the installation

```bash
.claude/skills/transcribe-meeting/scripts/transcribe.sh check
```

It reports the platform, `whisper-cli`, the model (default `medium.en`), an audio converter (`afconvert` on macOS, else `ffmpeg`), and `git`/`cmake`. Exit code 0 means ready.

If not ready, tell the user what is missing and ask before installing (it builds software and downloads about 1.5 GB). Then run `transcribe.sh install`. Use this script, not `brew install whisper-cpp`: on an Intel Mac with a recent macOS there are no prebuilt Homebrew packages, so Homebrew compiles LLVM from source and upgrades unrelated libraries (seen 2026-09-30). The script builds whisper.cpp directly in `~/.local/share/whisper.cpp` (override with `WHISPER_HOME`).

## 2. Transcribe

```bash
.claude/skills/transcribe-meeting/scripts/transcribe.sh run <audio> <outdir> "<prompt>"
```

Run it in the background for long recordings (about 0.65 times the recording length on an 8-thread Intel i7 with `medium.en`; Apple Silicon is faster). It writes `<name>.txt` (text) and `<name>.srt` (timestamps) in `<outdir>`.

- **Prompt.** Give a prompt of two or three full, punctuated sentences that use the vocabulary of the meeting (names, technical terms, the document under discussion). It steers spelling. A short, unpunctuated prompt produced lowercase, unpunctuated output in part of one run (2026-09-30).
- **GPU.** The script uses the GPU only on Apple Silicon. On an Intel Mac with an AMD GPU the Metal backend crashed mid-run, so it runs on the CPU there.
- **Privacy.** Keep audio and transcripts outside the repository unless the user asks to save them. Put working files in the scratchpad or `~/.local/share/whisper-work/`.

## 3. Check the transcript before using it

Read the whole `.txt`, and look at the `.srt` around anything odd. Common problems:

- **Mishearings of technical terms and names** ("convolution" for "deconvolution", "colonial" for "Colombian", "Ollie and Beggs" for "Olley and Pakes"). Correct them from context and list the corrections for the user.
- **Numbers.** Check against the context ("60%" that was "16%"). Flag any number that cannot be checked.
- **No speaker labels.** Infer the speaker from content (who asks, who explains) and say when it is uncertain.
- **Stalled stretches.** Runs of identical one-word segments ("Yeah." every second), "(Inaudible)", or a switch to lowercase without punctuation mean the decoder struggled, often on overlapping speech or back-channel. The script warns about repeated segments. Say which stretch may have lost content; if it matters, rerun that part.
- **Hypotheticals.** Speakers give made-up numbers to illustrate a point ("say it is five to seven billion"). Record them as illustrations, never as findings.
- **Coverage.** Say which parts of the document or topic the recording does and does not cover.

## 4. Summarize

Organize by the thing the meeting was about (for feedback on a text: by paragraph or section, in document order), not by time. For each point give:

- what the speaker said, in a line;
- why, when they gave a reason;
- whether it was a firm recommendation, a preference ("it's not critical, you decide"), or a question;
- open conflicts with earlier decisions in the project.

Keep the speaker's own words for the few remarks worth quoting. Separate what was said from what Claude infers. End with decisions the user must make and the next steps the speaker mentioned.

Follow the project's conventions for any text written (Canadian spelling, no em-dashes) and confirm with the user before editing a document.
