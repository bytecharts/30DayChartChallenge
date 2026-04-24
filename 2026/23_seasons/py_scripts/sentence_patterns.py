#!/usr/bin/env python3
"""Build per-character sentence-pattern stats from subtitle HTML files."""

from __future__ import annotations

import argparse
import html
import json
import re
import statistics
from collections import Counter, defaultdict
from pathlib import Path


SPEAKER_CUE_RE = re.compile(
    r"(?<![A-Za-z])([A-Z][A-Z]+(?:\s+[A-Z][A-Z]+)*)(?:\s*\([^)]*\))?:\s*"
)

WORD_RE = re.compile(r"[a-z]+(?:'[a-z]+)?")

ALIAS_MAP = {
    "SIOBHAN": "SHIV",
    "SHIV ROY": "SHIV",
    "KENDALL ROY": "KENDALL",
    "ROMAN ROY": "ROMAN",
    "LOGAN ROY": "LOGAN",
    "CONNOR ROY": "CONNOR",
    "TOM WAMBSGANS": "TOM",
    "TOM WAMSGANS": "TOM",
    "GREG HIRSCH": "GREG",
    "MARCIA ROY": "MARCIA",
    "RAVA ROY": "RAVA",
    "GERRI KELLMAN": "GERRI",
    "FRANK VERNON": "FRANK",
    "KARL MULLER": "KARL",
    "KAROLINA NOVOTNEY": "KAROLINA",
    "HUGO BAKER": "HUGO",
    "WILLA FERREYRA": "WILLA",
    "LUKAS MATSSON": "LUKAS",
    "STEWY HOSSEINI": "STEWY",
    "NATE SOFRELLI": "NATE",
    "RHEA JARRELL": "RHEA",
    "CAROLINE COLLINGWOOD": "CAROLINE",
    "CYD PEACH": "CYD",
    "GIL EAVIS": "GIL",
    "JESS JORDAN": "JESS",
}

NON_CHARACTER_RE = re.compile(
    r"^ALL$|ANCHOR|NEWSCASTER|NARRATOR|^WOMAN$|^MAN$|^BOY$|^GIRL$|ON TV|ON VIDEO|"
    r"ON PHONE|SERVER|SOUND TECH"
)


def collect_html_files(input_path: Path) -> list[Path]:
    if input_path.is_file():
        if input_path.suffix.lower() != ".html":
            raise ValueError(f"Input file is not HTML: {input_path}")
        return [input_path]
    if input_path.is_dir():
        return sorted(input_path.rglob("*.html"))
    raise ValueError(f"Input path does not exist: {input_path}")


def html_to_text(raw_html: str) -> str:
    text = re.sub(r"<br\s*/?>", "\n", raw_html, flags=re.IGNORECASE)
    text = re.sub(r"<[^>]+>", " ", text)
    text = html.unescape(text)
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    return text


def normalize_speaker(raw: str) -> str:
    speaker = re.sub(r"\s+", " ", raw.strip().upper())
    return ALIAS_MAP.get(speaker, speaker)


def season_key_from_path(path: Path) -> str:
    for part in path.parts:
        match = re.fullmatch(r"season\s*([0-9]+)", part, flags=re.IGNORECASE)
        if match:
            return f"Season{int(match.group(1))}"

    stem_match = re.search(r"\bS([0-9]{1,2})_E[0-9]{1,2}\b", path.stem, flags=re.IGNORECASE)
    if stem_match:
        return f"Season{int(stem_match.group(1))}"

    return "Unknown"


def season_sort_key(season: str) -> tuple[int, int | str]:
    match = re.fullmatch(r"Season([0-9]+)", season)
    if match:
        return (0, int(match.group(1)))
    return (1, season)


def split_sentences(segment: str) -> list[str]:
    text = re.sub(r"\([^)]*\)", " ", segment)
    text = re.sub(r"\s*\n\s*", "\n", text)
    parts = re.split(r"(?<=[.!?])\s+|\n+", text)

    sentences: list[str] = []
    for part in parts:
        sent = part.strip(" \t-—–\"'“”‘’")
        sent = re.sub(r"\s+", " ", sent)
        if sent:
            sentences.append(sent)
    return sentences


def make_bucket() -> dict[str, object]:
    return {
        "sentences": 0,
        "words": 0,
        "questions": 0,
        "exclamations": 0,
        "ellipsis": 0,
        "interruptions": 0,
        "short_sentences": 0,
        "long_sentences": 0,
        "lengths": [],
        "start_words": Counter(),
        "start_bigrams": Counter(),
        "end_words": Counter(),
    }


def update_bucket(bucket: dict[str, object], sentence: str) -> None:
    words = WORD_RE.findall(sentence.lower())
    if not words:
        return

    sentence_count = int(bucket["sentences"]) + 1
    word_count = int(bucket["words"]) + len(words)
    length = len(words)

    bucket["sentences"] = sentence_count
    bucket["words"] = word_count
    bucket["questions"] = int(bucket["questions"]) + int("?" in sentence)
    bucket["exclamations"] = int(bucket["exclamations"]) + int("!" in sentence)
    bucket["ellipsis"] = int(bucket["ellipsis"]) + int("..." in sentence or "…" in sentence)
    bucket["interruptions"] = int(bucket["interruptions"]) + int(bool(re.search(r"--|—|-$", sentence)))
    bucket["short_sentences"] = int(bucket["short_sentences"]) + int(length <= 4)
    bucket["long_sentences"] = int(bucket["long_sentences"]) + int(length >= 20)
    cast_lengths = bucket["lengths"]
    assert isinstance(cast_lengths, list)
    cast_lengths.append(length)

    cast_start = bucket["start_words"]
    cast_end = bucket["end_words"]
    cast_bigram = bucket["start_bigrams"]
    assert isinstance(cast_start, Counter)
    assert isinstance(cast_end, Counter)
    assert isinstance(cast_bigram, Counter)

    cast_start[words[0]] += 1
    cast_end[words[-1]] += 1
    if len(words) >= 2:
        cast_bigram[f"{words[0]} {words[1]}"] += 1
    else:
        cast_bigram[words[0]] += 1


def finalize_bucket(bucket: dict[str, object], top_n: int) -> dict[str, object]:
    sentences = int(bucket["sentences"])
    words = int(bucket["words"])
    lengths = bucket["lengths"]
    assert isinstance(lengths, list)

    if sentences == 0:
        avg_len = 0.0
        median_len = 0.0
        stdev_len = 0.0
    else:
        avg_len = words / sentences
        median_len = float(statistics.median(lengths))
        stdev_len = float(statistics.pstdev(lengths)) if len(lengths) > 1 else 0.0

    def top_counter(counter_obj: object) -> list[dict[str, object]]:
        assert isinstance(counter_obj, Counter)
        return [
            {"token": token, "count": count}
            for token, count in counter_obj.most_common(top_n)
        ]

    questions = int(bucket["questions"])
    exclamations = int(bucket["exclamations"])
    ellipsis = int(bucket["ellipsis"])
    interruptions = int(bucket["interruptions"])
    short_sentences = int(bucket["short_sentences"])
    long_sentences = int(bucket["long_sentences"])

    return {
        "sentences": sentences,
        "words": words,
        "avg_words_per_sentence": round(avg_len, 3),
        "median_words_per_sentence": round(median_len, 3),
        "stdev_words_per_sentence": round(stdev_len, 3),
        "questions": questions,
        "question_ratio": round((questions / sentences) if sentences else 0.0, 4),
        "exclamations": exclamations,
        "exclamation_ratio": round((exclamations / sentences) if sentences else 0.0, 4),
        "ellipsis": ellipsis,
        "ellipsis_ratio": round((ellipsis / sentences) if sentences else 0.0, 4),
        "interruptions": interruptions,
        "interruption_ratio": round((interruptions / sentences) if sentences else 0.0, 4),
        "short_sentences": short_sentences,
        "short_sentence_ratio": round((short_sentences / sentences) if sentences else 0.0, 4),
        "long_sentences": long_sentences,
        "long_sentence_ratio": round((long_sentences / sentences) if sentences else 0.0, 4),
        "common_sentence_starters": top_counter(bucket["start_words"]),
        "common_opening_bigrams": top_counter(bucket["start_bigrams"]),
        "common_sentence_endings": top_counter(bucket["end_words"]),
    }


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Extract sentence-pattern data for each character from subtitle HTML."
    )
    parser.add_argument(
        "input_path",
        type=Path,
        help="Path to a subtitle HTML file or directory of HTML files",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=Path("sentence_patterns.json"),
        help="Output JSON path",
    )
    parser.add_argument(
        "--top-n",
        type=int,
        default=10,
        help="Top N starters/endings to include per character",
    )
    args = parser.parse_args()

    html_files = collect_html_files(args.input_path)
    if not html_files:
        raise ValueError(f"No HTML files found in: {args.input_path}")

    overall: dict[str, dict[str, object]] = defaultdict(make_bucket)
    by_season: dict[str, dict[str, dict[str, object]]] = defaultdict(lambda: defaultdict(make_bucket))
    excluded_labels: dict[str, dict[str, int]] = defaultdict(lambda: {"segments": 0, "words": 0})
    episodes_without_cues: list[str] = []

    for html_file in html_files:
        raw_html = html_file.read_text(encoding="utf-8")
        text = html_to_text(raw_html)
        matches = list(SPEAKER_CUE_RE.finditer(text))

        rel_path = str(html_file.relative_to(args.input_path)) if args.input_path.is_dir() else str(html_file)
        if not matches:
            episodes_without_cues.append(rel_path)
            continue

        season = season_key_from_path(html_file)
        for idx, match in enumerate(matches):
            speaker = normalize_speaker(match.group(1))
            start = match.end()
            end = matches[idx + 1].start() if idx + 1 < len(matches) else len(text)
            segment = text[start:end]

            sentences = split_sentences(segment)
            if not sentences:
                continue

            if NON_CHARACTER_RE.search(speaker):
                token_count = sum(len(WORD_RE.findall(sent.lower())) for sent in sentences)
                excluded_labels[speaker]["segments"] += 1
                excluded_labels[speaker]["words"] += token_count
                continue

            for sentence in sentences:
                update_bucket(overall[speaker], sentence)
                update_bucket(by_season[season][speaker], sentence)

    characters = []
    for character, bucket in overall.items():
        overall_metrics = finalize_bucket(bucket, top_n=args.top_n)
        season_metrics = []
        for season in sorted(by_season.keys(), key=season_sort_key):
            if character not in by_season[season]:
                continue
            season_metrics.append(
                {
                    "season": season,
                    **finalize_bucket(by_season[season][character], top_n=min(args.top_n, 5)),
                }
            )

        characters.append(
            {
                "character": character,
                **overall_metrics,
                "seasons": season_metrics,
            }
        )

    characters.sort(key=lambda row: (-int(row["sentences"]), -int(row["words"]), str(row["character"])))
    for i, row in enumerate(characters, start=1):
        row["rank_by_sentences"] = i

    by_words = sorted(characters, key=lambda row: (-int(row["words"]), -int(row["sentences"]), str(row["character"])))
    rank_by_words = {row["character"]: idx for idx, row in enumerate(by_words, start=1)}
    for row in characters:
        row["rank_by_words"] = rank_by_words[row["character"]]

    excluded_payload = [
        {"label": label, "segments": payload["segments"], "words": payload["words"]}
        for label, payload in sorted(
            excluded_labels.items(), key=lambda item: (-item[1]["words"], item[0])
        )
    ]

    output = {
        "source_root": str(args.input_path),
        "episode_count": len(html_files),
        "character_count": len(characters),
        "episodes_without_speaker_cues": sorted(episodes_without_cues),
        "excluded_non_character_labels": excluded_payload,
        "characters": characters,
    }

    args.output.write_text(json.dumps(output, indent=2), encoding="utf-8")
    print(f"Wrote sentence-pattern stats for {len(characters)} characters to {args.output}")


if __name__ == "__main__":
    main()
