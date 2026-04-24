#!/usr/bin/env python3
"""Parse speaker-labeled dialogue from subtitle HTML.

Expected cue format in the text: SPEAKER: line of dialogue
Supports parsing a single episode file or a directory of episodes.
"""

from __future__ import annotations

import argparse
import html
import json
import re
from collections import Counter, defaultdict
from pathlib import Path


STOPWORDS = {
    "a",
    "about",
    "above",
    "after",
    "again",
    "against",
    "all",
    "am",
    "an",
    "and",
    "any",
    "are",
    "as",
    "at",
    "be",
    "because",
    "been",
    "before",
    "being",
    "below",
    "between",
    "both",
    "but",
    "by",
    "can",
    "did",
    "do",
    "does",
    "doing",
    "down",
    "during",
    "each",
    "few",
    "for",
    "from",
    "further",
    "had",
    "has",
    "have",
    "having",
    "he",
    "her",
    "here",
    "hers",
    "herself",
    "him",
    "himself",
    "his",
    "how",
    "i",
    "if",
    "in",
    "into",
    "is",
    "it",
    "its",
    "itself",
    "just",
    "me",
    "more",
    "most",
    "my",
    "myself",
    "no",
    "nor",
    "not",
    "of",
    "off",
    "on",
    "once",
    "only",
    "or",
    "other",
    "our",
    "ours",
    "ourselves",
    "out",
    "over",
    "own",
    "s",
    "same",
    "she",
    "should",
    "so",
    "some",
    "such",
    "t",
    "than",
    "that",
    "the",
    "their",
    "theirs",
    "them",
    "themselves",
    "then",
    "there",
    "these",
    "they",
    "this",
    "those",
    "through",
    "to",
    "too",
    "under",
    "until",
    "up",
    "very",
    "was",
    "we",
    "were",
    "what",
    "when",
    "where",
    "which",
    "while",
    "who",
    "whom",
    "why",
    "will",
    "with",
    "you",
    "your",
    "yours",
    "yourself",
    "yourselves",
}


SPEAKER_CUE_RE = re.compile(
    r"(?<![A-Za-z])([A-Z][A-Z]+(?:\s+[A-Z][A-Z]+)*)(?:\s*\([^)]*\))?:\s*"
)


def html_to_text(raw_html: str) -> str:
    text = re.sub(r"<br\s*/?>", "\n", raw_html, flags=re.IGNORECASE)
    text = re.sub(r"<[^>]+>", " ", text)
    text = html.unescape(text)
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    return text


def tokenize(text: str) -> list[str]:
    return re.findall(r"[a-z]+(?:'[a-z]+)?", text.lower())


def normalize_segment(segment: str) -> str:
    segment = re.sub(r"\([^)]*\)", " ", segment)
    segment = segment.replace("-", " ")
    segment = re.sub(r"\s+", " ", segment).strip()
    return segment


def parse_dialogue(text: str) -> tuple[dict[str, list[str]], int]:
    matches = list(SPEAKER_CUE_RE.finditer(text))
    dialogue: dict[str, list[str]] = defaultdict(list)

    for idx, match in enumerate(matches):
        speaker = re.sub(r"\s+", " ", match.group(1)).strip()
        start = match.end()
        end = matches[idx + 1].start() if idx + 1 < len(matches) else len(text)
        segment = normalize_segment(text[start:end])
        if segment:
            dialogue[speaker].append(segment)

    return dialogue, len(matches)


def collect_html_files(input_path: Path) -> list[Path]:
    if input_path.is_file():
        if input_path.suffix.lower() != ".html":
            raise ValueError(f"Input file is not HTML: {input_path}")
        return [input_path]

    if input_path.is_dir():
        return sorted(input_path.rglob("*.html"))

    raise ValueError(f"Input path does not exist: {input_path}")


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


def aggregate_dialogue(target: dict[str, list[str]], source: dict[str, list[str]]) -> None:
    for speaker, lines in source.items():
        target[speaker].extend(lines)


def build_stats(dialogue: dict[str, list[str]], top_n: int) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []

    for speaker, lines in dialogue.items():
        joined = " ".join(lines)
        words = tokenize(joined)
        total_words = len(words)

        all_counter = Counter(words)
        filtered_counter = Counter(w for w in words if w not in STOPWORDS)
        common_source = filtered_counter if filtered_counter else all_counter

        rows.append(
            {
                "character": speaker,
                "words": total_words,
                "common_words": [
                    {"word": word, "count": count}
                    for word, count in common_source.most_common(top_n)
                ],
            }
        )

    rows.sort(key=lambda row: (-int(row["words"]), str(row["character"])))
    for rank, row in enumerate(rows, start=1):
        row["rank"] = rank

    return rows


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Parse speaker dialogue and create per-character stats JSON."
    )
    parser.add_argument(
        "input_path",
        type=Path,
        help="Path to subtitle HTML file or directory containing episode HTML files",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=None,
        help="Output JSON file path",
    )
    parser.add_argument(
        "--top-n",
        type=int,
        default=15,
        help="Number of common words to keep per character",
    )
    args = parser.parse_args()

    html_files = collect_html_files(args.input_path)
    if not html_files:
        raise ValueError(f"No HTML files found in: {args.input_path}")

    is_single_file = len(html_files) == 1 and html_files[0].is_file() and args.input_path.is_file()

    if args.output is None:
        args.output = Path("character_stats.json" if is_single_file else "season_stats.json")

    if is_single_file:
        raw_html = html_files[0].read_text(encoding="utf-8")
        text = html_to_text(raw_html)
        dialogue, cue_count = parse_dialogue(text)
        stats = build_stats(dialogue, top_n=args.top_n)

        payload = {
            "source_file": str(html_files[0]),
            "speaker_cue_count": cue_count,
            "character_count": len(stats),
            "characters": stats,
        }

        args.output.write_text(json.dumps(payload, indent=2), encoding="utf-8")
        print(f"Wrote stats for {len(stats)} characters to {args.output}")
        return

    season_dialogue: dict[str, dict[str, list[str]]] = defaultdict(lambda: defaultdict(list))
    season_episode_count: Counter[str] = Counter()
    season_missing_names: dict[str, list[str]] = defaultdict(list)

    for html_file in html_files:
        raw_html = html_file.read_text(encoding="utf-8")
        text = html_to_text(raw_html)
        dialogue, cue_count = parse_dialogue(text)
        season = season_key_from_path(html_file)

        season_episode_count[season] += 1
        if cue_count == 0:
            rel_path = str(html_file.relative_to(args.input_path))
            season_missing_names[season].append(rel_path)

        aggregate_dialogue(season_dialogue[season], dialogue)

    seasons_payload: list[dict[str, object]] = []
    for season in sorted(season_episode_count.keys(), key=season_sort_key):
        stats = build_stats(season_dialogue[season], top_n=args.top_n)
        missing = sorted(season_missing_names.get(season, []))
        seasons_payload.append(
            {
                "season": season,
                "episode_count": season_episode_count[season],
                "episodes_without_character_name": missing,
                "character_count": len(stats),
                "characters": stats,
            }
        )

    missing_groups = [
        {"season": season, "episodes": sorted(episodes)}
        for season, episodes in sorted(season_missing_names.items(), key=lambda item: season_sort_key(item[0]))
        if episodes
    ]

    payload = {
        "source_root": str(args.input_path),
        "episode_count": len(html_files),
        "season_count": len(seasons_payload),
        "seasons": seasons_payload,
        "episodes_without_character_name": missing_groups,
    }

    args.output.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    print(f"Wrote season stats for {len(seasons_payload)} seasons to {args.output}")


if __name__ == "__main__":
    main()
