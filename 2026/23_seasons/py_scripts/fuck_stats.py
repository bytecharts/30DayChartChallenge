#!/usr/bin/env python3
"""Count occurrences of 'fuck' variants in subtitle transcripts.

Outputs:
- total fuck-variant counts per episode
- fuck-variant counts by character per episode
- overall top speaker by fuck-variant count
"""

from __future__ import annotations

import argparse
import html
import json
import re
from collections import Counter, defaultdict
from pathlib import Path


SPEAKER_CUE_RE = re.compile(
    r"(?<![A-Za-z])([A-Z][A-Z]+(?:\s+[A-Z][A-Z]+)*)(?:\s*\([^)]*\))?:\s*"
)

FUCK_RE = re.compile(
    r"\b(?:fuck(?:er|ers|ed|ing|in'|in|s)?|motherfuck(?:er|ers|ed|ing|in'|in|s)?)\b",
    flags=re.IGNORECASE,
)

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

    stem_match = re.search(r"\bS([0-9]{1,2})_E([0-9]{1,2})\b", path.stem, flags=re.IGNORECASE)
    if stem_match:
        return f"Season{int(stem_match.group(1))}"

    return "Unknown"


def episode_code_from_path(path: Path) -> str:
    stem_match = re.search(r"\bS([0-9]{1,2})_E([0-9]{1,2})\b", path.stem, flags=re.IGNORECASE)
    if stem_match:
        return f"S{int(stem_match.group(1))}_E{int(stem_match.group(2)):02d}"
    return path.stem


def episode_sort_key(path: Path) -> tuple[int, int, str]:
    stem_match = re.search(r"\bS([0-9]{1,2})_E([0-9]{1,2})\b", path.stem, flags=re.IGNORECASE)
    if stem_match:
        return (int(stem_match.group(1)), int(stem_match.group(2)), path.name)
    return (999, 999, path.name)


def count_fucks(text: str) -> tuple[int, Counter[str]]:
    variants = Counter(match.group(0).lower() for match in FUCK_RE.finditer(text))
    return sum(variants.values()), variants


def main() -> None:
    parser = argparse.ArgumentParser(description="Count fuck variants by episode and character.")
    parser.add_argument("input_path", type=Path, help="Path to HTML file or directory")
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=Path("fuck_stats.json"),
        help="Output JSON path",
    )
    args = parser.parse_args()

    html_files = sorted(collect_html_files(args.input_path), key=episode_sort_key)
    if not html_files:
        raise ValueError(f"No HTML files found in: {args.input_path}")

    episodes_payload: list[dict[str, object]] = []
    overall_by_character = Counter()
    overall_variant_counts = Counter()
    episodes_without_cues: list[str] = []

    for html_file in html_files:
        raw_html = html_file.read_text(encoding="utf-8")
        text = html_to_text(raw_html)

        total_fucks, full_variant_counts = count_fucks(text)
        overall_variant_counts.update(full_variant_counts)

        matches = list(SPEAKER_CUE_RE.finditer(text))
        by_character = Counter()
        excluded_or_unattributed = total_fucks

        if not matches:
            rel_path = str(html_file.relative_to(args.input_path)) if args.input_path.is_dir() else str(html_file)
            episodes_without_cues.append(rel_path)
        else:
            attributed = 0
            for idx, match in enumerate(matches):
                speaker = normalize_speaker(match.group(1))
                start = match.end()
                end = matches[idx + 1].start() if idx + 1 < len(matches) else len(text)
                segment = text[start:end]
                segment = re.sub(r"\([^)]*\)", " ", segment)
                segment_count, _ = count_fucks(segment)
                if segment_count == 0:
                    continue
                if NON_CHARACTER_RE.search(speaker):
                    continue

                by_character[speaker] += segment_count
                overall_by_character[speaker] += segment_count
                attributed += segment_count

            excluded_or_unattributed = total_fucks - attributed

        rel_episode = str(html_file.relative_to(args.input_path)) if args.input_path.is_dir() else str(html_file)
        episode_entry = {
            "episode": rel_episode,
            "season": season_key_from_path(html_file),
            "episode_code": episode_code_from_path(html_file),
            "speaker_cue_count": len(matches),
            "total_fucks": total_fucks,
            "unattributed_or_excluded_fucks": excluded_or_unattributed,
            "variants": [
                {"variant": variant, "count": count}
                for variant, count in full_variant_counts.most_common()
            ],
            "by_character": [
                {"character": character, "fucks": count}
                for character, count in by_character.most_common()
            ],
        }
        episodes_payload.append(episode_entry)

    by_character_payload = [
        {"character": character, "fucks": count}
        for character, count in overall_by_character.most_common()
    ]

    top_character = by_character_payload[0] if by_character_payload else None

    payload = {
        "source_root": str(args.input_path),
        "episode_count": len(html_files),
        "total_fucks": sum(overall_variant_counts.values()),
        "count_definition": "Matches fuck, fucks, fucked, fucker(s), fuckin, fucking, and motherfuck* variants (case-insensitive).",
        "top_character": top_character,
        "overall_variants": [
            {"variant": variant, "count": count}
            for variant, count in overall_variant_counts.most_common()
        ],
        "overall_by_character": by_character_payload,
        "episodes_without_speaker_cues": sorted(episodes_without_cues),
        "episodes": episodes_payload,
    }

    args.output.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    print(f"Wrote fuck-variant stats for {len(html_files)} episodes to {args.output}")


if __name__ == "__main__":
    main()
