#!/usr/bin/env python3
"""Extract "fuck off" speaker -> receiver interactions for network/chord charts.

Provides two edge sets:
- strict edges: explicit-name target heuristics only
- inferred edges: strict edges plus turn-based fallback for "you"/unspecified cases
"""

from __future__ import annotations

import argparse
import html
import json
import re
from collections import Counter
from pathlib import Path


SPEAKER_CUE_RE = re.compile(
    r"(?<![A-Za-z])([A-Z][A-Z]+(?:\s+[A-Z][A-Z]+)*)(?:\s*\([^)]*\))?:\s*"
)

FUCK_OFF_RE = re.compile(r"\bfuck(?:ing|in')?\s+(?:right\s+)?off\b", re.IGNORECASE)

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

# Placeholders for unresolved receiving end.
TARGET_UNSPECIFIED = "_UNSPECIFIED_"
TARGET_YOU = "_YOU_"
TARGET_GROUP = "_GROUP_"


MENTION_MAP = {
    "shiv": "SHIV",
    "siobhan": "SHIV",
    "kendall": "KENDALL",
    "ken": "KENDALL",
    "roman": "ROMAN",
    "rome": "ROMAN",
    "romulus": "ROMAN",
    "logan": "LOGAN",
    "dad": "LOGAN",
    "pa": "LOGAN",
    "tom": "TOM",
    "greg": "GREG",
    "gerri": "GERRI",
    "frank": "FRANK",
    "karl": "KARL",
    "karolina": "KAROLINA",
    "hugo": "HUGO",
    "connor": "CONNOR",
    "marcia": "MARCIA",
    "willa": "WILLA",
    "lukas": "LUKAS",
    "matsson": "LUKAS",
    "stewy": "STEWY",
    "rava": "RAVA",
    "caroline": "CAROLINE",
    "jess": "JESS",
    "nate": "NATE",
    "gil": "GIL",
    "rhea": "RHEA",
    "cyd": "CYD",
    "kerry": "KERRY",
    "ebba": "EBBA",
}


def collect_html_files(input_path: Path) -> list[Path]:
    if input_path.is_file():
        if input_path.suffix.lower() != ".html":
            raise ValueError(f"Input file is not HTML: {input_path}")
        return [input_path]
    if input_path.is_dir():
        return sorted(input_path.rglob("*.html"), key=episode_sort_key)
    raise ValueError(f"Input path does not exist: {input_path}")


def episode_sort_key(path: Path) -> tuple[int, int, str]:
    match = re.search(r"\bS([0-9]{1,2})_E([0-9]{1,2})\b", path.stem, flags=re.IGNORECASE)
    if match:
        return (int(match.group(1)), int(match.group(2)), path.name)
    return (999, 999, path.name)


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
    match = re.search(r"\bS([0-9]{1,2})_E([0-9]{1,2})\b", path.stem, flags=re.IGNORECASE)
    if match:
        return f"S{int(match.group(1))}_E{int(match.group(2)):02d}"
    return path.stem


def split_sentences(segment: str) -> list[str]:
    text = re.sub(r"\([^)]*\)", " ", segment)
    text = re.sub(r"\s*\n\s*", "\n", text)
    parts = re.split(r"(?<=[.!?])\s+|\n+", text)
    out: list[str] = []
    for part in parts:
        sentence = re.sub(r"\s+", " ", part).strip()
        if sentence:
            out.append(sentence)
    return out


def token_to_target(token: str) -> str | None:
    clean = token.lower().strip(" \t,.:;!?\"'()[]{}")
    if not clean:
        return None
    return MENTION_MAP.get(clean)


def infer_target(sentence: str, match: re.Match[str], speaker: str) -> str:
    lower = sentence.lower()
    start = match.start()
    end = match.end()
    pre = sentence[max(0, start - 50) : start]
    post = sentence[end : min(len(sentence), end + 50)]

    # Pattern: "Frank, fuck off" or "Frank can fuck off"
    direct_before = re.search(
        r"([A-Za-z][A-Za-z'\-]*)\s*(?:,\s*)?(?:can\s+|could\s+|should\s+|please\s+|just\s+|you\s+can\s+)?$",
        pre,
    )
    if direct_before:
        target = token_to_target(direct_before.group(1))
        if target and target != speaker:
            return target

    # Pattern: "fuck off, Greg"
    direct_after = re.match(r"^\s*(?:,\s*)?([A-Za-z][A-Za-z'\-]*)", post)
    if direct_after:
        target = token_to_target(direct_after.group(1))
        if target and target != speaker:
            return target

    # Pattern: "tell X to fuck off"
    tell_match = re.search(
        r"(?:tell|told|telling)\s+([A-Za-z][A-Za-z'\-]*)\s+(?:to\s+)?(?:just\s+)?fuck(?:ing|in')?\s+(?:right\s+)?off",
        lower,
    )
    if tell_match:
        target = token_to_target(tell_match.group(1))
        if target and target != speaker:
            return target

    # Pattern: "X can fuck off"
    can_match = re.search(
        r"([A-Za-z][A-Za-z'\-]*)\s+(?:can|could|should|needs?\s+to|need\s+to|may)\s+fuck(?:ing|in')?\s+(?:right\s+)?off",
        lower,
    )
    if can_match:
        target = token_to_target(can_match.group(1))
        if target and target != speaker:
            return target

    local = lower[max(0, start - 25) : min(len(lower), end + 25)]
    if re.search(r"\b(you\s+all|you\s+both|you\s+guys|you\s+two|everybody|everyone)\b", local):
        return TARGET_GROUP
    if re.search(r"\byou\b", local):
        return TARGET_YOU

    return TARGET_UNSPECIFIED


def fallback_target_from_turn(raw_target: str, speaker: str, prev_speaker: str | None) -> str:
    """Resolve unresolved target labels using adjacent-speaker turn heuristics."""
    if raw_target not in {TARGET_YOU, TARGET_GROUP, TARGET_UNSPECIFIED}:
        return raw_target
    if prev_speaker and prev_speaker != speaker:
        return prev_speaker
    return raw_target


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Count who says 'fuck off' and likely targets for chord diagrams."
    )
    parser.add_argument("input_path", type=Path, help="Path to HTML file or directory")
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=Path("fuck_off_network.json"),
        help="Output JSON path",
    )
    args = parser.parse_args()

    html_files = collect_html_files(args.input_path)
    if not html_files:
        raise ValueError(f"No HTML files found in: {args.input_path}")

    said_by_character = Counter()

    strict_received_by_target = Counter()
    strict_edge_counts = Counter()

    inferred_received_by_target = Counter()
    inferred_edge_counts = Counter()
    episodes_without_cues: list[str] = []
    episodes_payload: list[dict[str, object]] = []

    for html_file in html_files:
        raw_html = html_file.read_text(encoding="utf-8")
        text = html_to_text(raw_html)
        matches = list(SPEAKER_CUE_RE.finditer(text))

        rel_episode = str(html_file.relative_to(args.input_path)) if args.input_path.is_dir() else str(html_file)

        episode_said = Counter()

        episode_received_strict = Counter()
        episode_edges_strict = Counter()

        episode_received_inferred = Counter()
        episode_edges_inferred = Counter()

        total_fuck_off = 0

        if not matches:
            episodes_without_cues.append(rel_episode)
        else:
            prev_speaker: str | None = None
            for idx, cue in enumerate(matches):
                speaker = normalize_speaker(cue.group(1))
                if NON_CHARACTER_RE.search(speaker):
                    continue

                start = cue.end()
                end = matches[idx + 1].start() if idx + 1 < len(matches) else len(text)
                segment = text[start:end]
                for sentence in split_sentences(segment):
                    for fo_match in FUCK_OFF_RE.finditer(sentence):
                        total_fuck_off += 1
                        strict_target = infer_target(sentence, fo_match, speaker)
                        inferred_target = fallback_target_from_turn(strict_target, speaker, prev_speaker)

                        episode_said[speaker] += 1
                        episode_received_strict[strict_target] += 1
                        episode_edges_strict[(speaker, strict_target)] += 1

                        episode_received_inferred[inferred_target] += 1
                        episode_edges_inferred[(speaker, inferred_target)] += 1

                        said_by_character[speaker] += 1
                        strict_received_by_target[strict_target] += 1
                        strict_edge_counts[(speaker, strict_target)] += 1

                        inferred_received_by_target[inferred_target] += 1
                        inferred_edge_counts[(speaker, inferred_target)] += 1

                prev_speaker = speaker

        episode_entry = {
            "episode": rel_episode,
            "season": season_key_from_path(html_file),
            "episode_code": episode_code_from_path(html_file),
            "speaker_cue_count": len(matches),
            "total_fuck_off": total_fuck_off,
            "said_by_character": [
                {"character": ch, "count": count} for ch, count in episode_said.most_common()
            ],
            "received_by_target_strict": [
                {"target": target, "count": count}
                for target, count in episode_received_strict.most_common()
            ],
            "received_by_target_inferred": [
                {"target": target, "count": count}
                for target, count in episode_received_inferred.most_common()
            ],
            "edges_strict": [
                {"source": source, "target": target, "count": count}
                for (source, target), count in sorted(
                    episode_edges_strict.items(), key=lambda item: (-item[1], item[0][0], item[0][1])
                )
            ],
            "edges_inferred": [
                {"source": source, "target": target, "count": count}
                for (source, target), count in sorted(
                    episode_edges_inferred.items(), key=lambda item: (-item[1], item[0][0], item[0][1])
                )
            ],
        }
        episodes_payload.append(episode_entry)

    said_payload = [
        {"character": ch, "count": count} for ch, count in said_by_character.most_common()
    ]
    strict_received_payload = [
        {"target": target, "count": count}
        for target, count in sorted(strict_received_by_target.items(), key=lambda item: (-item[1], item[0]))
    ]
    strict_edge_payload = [
        {"source": source, "target": target, "count": count}
        for (source, target), count in sorted(
            strict_edge_counts.items(), key=lambda item: (-item[1], item[0][0], item[0][1])
        )
    ]

    inferred_received_payload = [
        {"target": target, "count": count}
        for target, count in sorted(inferred_received_by_target.items(), key=lambda item: (-item[1], item[0]))
    ]
    inferred_edge_payload = [
        {"source": source, "target": target, "count": count}
        for (source, target), count in sorted(
            inferred_edge_counts.items(), key=lambda item: (-item[1], item[0][0], item[0][1])
        )
    ]

    explicit_received = [row for row in strict_received_payload if not row["target"].startswith("_")]
    top_receiver = explicit_received[0] if explicit_received else None
    top_speaker = said_payload[0] if said_payload else None

    payload = {
        "source_root": str(args.input_path),
        "episode_count": len(html_files),
        "total_fuck_off": sum(said_by_character.values()),
        "count_definition": "Counts phrase-level matches of 'fuck off' and 'fucking off' variants (case-insensitive).",
        "target_inference": {
            "explicit_name_rules": [
                "NAME, fuck off",
                "fuck off, NAME",
                "tell NAME to fuck off",
                "NAME can/should/could/needs to fuck off",
            ],
            "fallback_labels": [TARGET_GROUP, TARGET_YOU, TARGET_UNSPECIFIED],
            "note": "Targets are heuristic and may include unresolved placeholders.",
        },
        "top_speaker": top_speaker,
        "top_receiver_explicit": top_receiver,
        "said_by_character": said_payload,
        "strict": {
            "received_by_target": strict_received_payload,
            "edges": strict_edge_payload,
        },
        "inferred": {
            "received_by_target": inferred_received_payload,
            "edges": inferred_edge_payload,
        },
        "episodes_without_speaker_cues": sorted(episodes_without_cues),
        "episodes": episodes_payload,
    }

    args.output.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    print(f"Wrote fuck-off network stats for {len(html_files)} episodes to {args.output}")


if __name__ == "__main__":
    main()
