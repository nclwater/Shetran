#!/usr/bin/env python3
"""Audit SHETRAN FORD documentation integration.

This script is intentionally read-only. It finds documentation clean-up work
that should be reviewed by an agent before editing source:

* legacy ordinary-comment header blocks left inside a documented procedure;
* dummy arguments with INTENT(...) declarations but no FORD inline comment.

For legacy blocks, the report includes terms that occur in the stale block but
not in the main FORD block. Those terms are only a review aid: the agent still
needs to decide whether the information is active, already covered, obsolete,
or should be moved into the FORD documentation before deleting the old block.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Iterator


SOURCE_SUFFIXES = {".f90", ".F90", ".f", ".F", ".f03", ".F03", ".f95", ".F95"}

ENTITY_RE = re.compile(
    r"^\s*(?!end\b)"
    r"(?:(?:pure|recursive|elemental|impure|module)\s+)*"
    r"(?:(?:integer|real|logical|character|complex|double\s+precision|"
    r"type\s*\([^)]*\)|class\s*\([^)]*\))"
    r"(?:\s*\([^)]*\))?(?:\s*,\s*[^:]+)?\s+)?"
    r"(subroutine|function)\s+([a-z_]\w*)\b",
    re.IGNORECASE,
)
END_ENTITY_RE = re.compile(r"^\s*end\s*(subroutine|function)\b", re.IGNORECASE)
MODULE_RE = re.compile(r"^\s*(?!end\b)(module|program)\s+([a-z_]\w*)\b", re.IGNORECASE)

LEGACY_MARKERS = (
    "version:",
    "modifications:",
    "commons",
    "common ",
    "entry conditions",
    "limited ranges",
    "assumed external module dependencies",
    "imported constants",
    "input common",
    "output common",
)

TOKEN_STOPWORDS = {
    "and",
    "are",
    "array",
    "arrays",
    "call",
    "code",
    "common",
    "constants",
    "data",
    "dependencies",
    "dummy",
    "external",
    "file",
    "for",
    "from",
    "global",
    "input",
    "intent",
    "line",
    "local",
    "module",
    "none",
    "not",
    "open",
    "output",
    "routine",
    "subroutine",
    "the",
    "this",
    "unit",
    "units",
    "used",
    "variables",
    "with",
}


@dataclass(frozen=True)
class Entity:
    path: Path
    name: str
    kind: str
    start: int
    end: int
    doc_start: int | None
    doc_end: int | None
    doc_text: str


@dataclass(frozen=True)
class LegacyBlockFinding:
    path: Path
    entity: str
    kind: str
    start: int
    end: int
    summary: str
    missing_terms: tuple[str, ...]


@dataclass(frozen=True)
class IntentFinding:
    path: Path
    entity: str
    kind: str
    line: int
    variables: tuple[str, ...]
    issue: str
    declaration: str


def iter_source_files(root: Path) -> Iterator[Path]:
    for path in sorted(root.rglob("*")):
        if path.is_file() and path.suffix in SOURCE_SUFFIXES:
            yield path


def is_ford_doc_line(line: str) -> bool:
    stripped = line.strip()
    return stripped.startswith("!>") or stripped.startswith("!!") or stripped.startswith("!<")


def is_ordinary_comment(line: str) -> bool:
    stripped = line.strip()
    return stripped.startswith("!") and not is_ford_doc_line(line)


def entity_on_line(line: str) -> tuple[str, str] | None:
    stripped = line.strip().lower()
    if stripped.startswith("end "):
        return None
    if re.match(r"^\s*module\s+procedure\b", line, re.IGNORECASE):
        return None
    match = MODULE_RE.match(line)
    if match:
        return match.group(1).lower(), match.group(2).lower()
    match = ENTITY_RE.match(line)
    if match:
        return match.group(1).lower(), match.group(2).lower()
    return None


def docblock_before(lines: list[str], idx0: int) -> tuple[int, int, list[str]] | None:
    """Return one-based start/end and lines for the FORD block before idx0."""
    j = idx0 - 1
    while j >= 0 and lines[j].strip() == "":
        j -= 1
    if j < 0 or not is_ford_doc_line(lines[j]):
        return None

    k = j
    while k >= 0:
        if is_ford_doc_line(lines[k]):
            k -= 1
            continue
        if lines[k].strip() == "":
            kk = k - 1
            while kk >= 0 and lines[kk].strip() == "":
                kk -= 1
            if kk >= 0 and is_ford_doc_line(lines[kk]):
                k -= 1
                continue
        break

    block = lines[k + 1 : idx0]
    while block and block[0].strip() == "":
        block = block[1:]
        k += 1
    while block and block[-1].strip() == "":
        block = block[:-1]
    if not block:
        return None
    return k + 2, idx0, block


def find_entities(path: Path, lines: list[str]) -> list[Entity]:
    entities: list[Entity] = []
    stack: list[tuple[str, str, int, int | None, int | None, str]] = []

    for idx0, line in enumerate(lines):
        entity = entity_on_line(line)
        if entity and entity[0] in {"subroutine", "function"}:
            doc = docblock_before(lines, idx0)
            if doc:
                doc_start, doc_end, doc_lines = doc
                doc_text = "\n".join(doc_lines)
            else:
                doc_start = doc_end = None
                doc_text = ""
            stack.append((entity[1], entity[0], idx0 + 1, doc_start, doc_end, doc_text))

        if END_ENTITY_RE.match(line) and stack:
            name, kind, start, doc_start, doc_end, doc_text = stack.pop()
            entities.append(
                Entity(
                    path=path,
                    name=name,
                    kind=kind,
                    start=start,
                    end=idx0 + 1,
                    doc_start=doc_start,
                    doc_end=doc_end,
                    doc_text=doc_text,
                )
            )

    return entities


def comment_payload(line: str) -> str:
    stripped = line.strip()
    if stripped.startswith("!"):
        return stripped[1:].strip()
    return stripped


def legacy_block_score(block: Iterable[str]) -> int:
    block_lines = list(block)
    text = "\n".join(comment_payload(line).lower() for line in block_lines)
    score = 0
    score += sum(1 for marker in LEGACY_MARKERS if marker in text)
    score += sum(1 for line in block_lines if re.fullmatch(r"\s*!\s*[-*]{6,}\s*", line))
    score += len(re.findall(r"^\s*!\s*[A-Z0-9 /._()-]{12,}\s*$", "\n".join(block_lines), flags=re.MULTILINE))
    return score


def extract_terms(text: str) -> set[str]:
    terms: set[str] = set()
    for raw in re.findall(r"[A-Za-z_][A-Za-z0-9_]{2,}", text):
        token = raw.strip("_").lower()
        if len(token) < 3 or token in TOKEN_STOPWORDS:
            continue
        if token.isdigit():
            continue
        terms.add(token)
    return terms


def summarize_block(block: list[str]) -> str:
    payloads = [comment_payload(line) for line in block]
    payloads = [line for line in payloads if line and not re.fullmatch(r"[-*]+", line)]
    if not payloads:
        return "(separator-only block)"
    summary = payloads[0]
    return summary[:110] + ("..." if len(summary) > 110 else "")


def find_leading_legacy_blocks(entity: Entity, lines: list[str]) -> list[LegacyBlockFinding]:
    findings: list[LegacyBlockFinding] = []
    idx0 = entity.start  # first line after SUBROUTINE/FUNCTION, zero-based
    limit = min(entity.end, entity.start + 80)

    while idx0 < limit:
        line = lines[idx0]
        stripped = line.strip()

        if stripped == "":
            idx0 += 1
            continue
        if stripped.lower().startswith("implicit none"):
            idx0 += 1
            continue
        if is_ordinary_comment(line):
            block_start = idx0
            block: list[str] = []
            while idx0 < entity.end and (is_ordinary_comment(lines[idx0]) or lines[idx0].strip() == ""):
                block.append(lines[idx0])
                idx0 += 1
            block_end = idx0 - 1
            if legacy_block_score(block) > 0:
                block_text = "\n".join(block)
                missing = sorted(extract_terms(block_text) - extract_terms(entity.doc_text))
                findings.append(
                    LegacyBlockFinding(
                        path=entity.path,
                        entity=entity.name,
                        kind=entity.kind,
                        start=block_start + 1,
                        end=block_end + 1,
                        summary=summarize_block(block),
                        missing_terms=tuple(missing[:20]),
                    )
                )
            continue

        # Stop once ordinary code/declarations start. Later implementation
        # comments are not stale leading documentation.
        break

    return findings


def split_fortran_comment(line: str) -> tuple[str, str | None]:
    in_single = False
    in_double = False
    idx = 0
    while idx < len(line):
        char = line[idx]
        if char == "'" and not in_double:
            if in_single and idx + 1 < len(line) and line[idx + 1] == "'":
                idx += 2
                continue
            in_single = not in_single
        elif char == '"' and not in_single:
            if in_double and idx + 1 < len(line) and line[idx + 1] == '"':
                idx += 2
                continue
            in_double = not in_double
        elif char == "!" and not in_single and not in_double:
            return line[:idx].rstrip(), line[idx:].rstrip()
        idx += 1
    return line.rstrip(), None


def split_vars(rhs: str) -> list[str]:
    parts: list[str] = []
    current: list[str] = []
    paren_depth = 0
    bracket_depth = 0
    in_single = False
    in_double = False
    idx = 0
    while idx < len(rhs):
        char = rhs[idx]
        if char == "'" and not in_double:
            in_single = not in_single
        elif char == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if char == "(":
                paren_depth += 1
            elif char == ")" and paren_depth:
                paren_depth -= 1
            elif char == "[":
                bracket_depth += 1
            elif char == "]" and bracket_depth:
                bracket_depth -= 1
            elif char == "," and paren_depth == 0 and bracket_depth == 0:
                item = "".join(current).strip()
                if item:
                    parts.append(item)
                current = []
                idx += 1
                continue
        current.append(char)
        idx += 1

    item = "".join(current).strip()
    if item:
        parts.append(item)
    return parts


def variable_name(item: str) -> str | None:
    left = item.split("=>", 1)[0].split("=", 1)[0].strip()
    match = re.match(r"([A-Za-z_]\w*)\b", left)
    return match.group(1).lower() if match else None


def find_intent_findings(entity: Entity, lines: list[str]) -> list[IntentFinding]:
    findings: list[IntentFinding] = []
    continuation = ""
    start_line = 0

    for line_no in range(entity.start + 1, entity.end):
        raw = lines[line_no - 1]
        if raw.lstrip().startswith("!"):
            continue

        code, comment = split_fortran_comment(raw)
        if continuation:
            code_for_parse = continuation + " " + code.strip().lstrip("&").strip()
        else:
            code_for_parse = code
            start_line = line_no

        if code.rstrip().endswith("&"):
            continuation = code_for_parse.rstrip("&").strip()
            continue

        continuation = ""

        if "intent" not in code_for_parse.lower() or "::" not in code_for_parse:
            continue

        rhs = code_for_parse.split("::", 1)[1]
        names = tuple(name for item in split_vars(rhs) if (name := variable_name(item)))
        if not names:
            continue

        if comment is None or not comment.strip().startswith("!!"):
            findings.append(
                IntentFinding(
                    path=entity.path,
                    entity=entity.name,
                    kind=entity.kind,
                    line=start_line,
                    variables=names,
                    issue="missing-inline-ford-comment",
                    declaration=raw.strip(),
                )
            )
        elif len(names) > 1:
            findings.append(
                IntentFinding(
                    path=entity.path,
                    entity=entity.name,
                    kind=entity.kind,
                    line=start_line,
                    variables=names,
                    issue="shared-comment-for-multiple-dummies",
                    declaration=raw.strip(),
                )
            )

    return findings


def collect_findings(root: Path) -> tuple[list[LegacyBlockFinding], list[IntentFinding]]:
    legacy_findings: list[LegacyBlockFinding] = []
    intent_findings: list[IntentFinding] = []

    for source in iter_source_files(root):
        lines = source.read_text(encoding="utf-8", errors="replace").splitlines()
        for entity in find_entities(source, lines):
            legacy_findings.extend(find_leading_legacy_blocks(entity, lines))
            intent_findings.extend(find_intent_findings(entity, lines))

    return legacy_findings, intent_findings


def rel(path: Path) -> str:
    try:
        return path.as_posix()
    except ValueError:
        return str(path)


def print_markdown(legacy: list[LegacyBlockFinding], intents: list[IntentFinding], limit: int | None) -> None:
    print("# FORD documentation audit findings")
    print()
    print("Generated by `scripts/audit_ford_docs.py`.")
    print()
    print(f"- Legacy leading comment blocks: {len(legacy)}")
    print(f"- Dummy INTENT documentation findings: {len(intents)}")
    print()

    print("## Legacy leading ordinary-comment blocks")
    print()
    if not legacy:
        print("No findings.")
    else:
        print("| File | Entity | Lines | Summary | Terms not seen in FORD block |")
        print("| --- | --- | ---: | --- | --- |")
        for finding in legacy[:limit]:
            terms = ", ".join(f"`{term}`" for term in finding.missing_terms) or "-"
            print(
                f"| `{rel(finding.path)}` | `{finding.entity}` | "
                f"{finding.start}-{finding.end} | {finding.summary.replace('|', '\\|')} | {terms} |"
            )
        if limit is not None and len(legacy) > limit:
            print(f"\nShowing {limit} of {len(legacy)} findings.")
    print()

    print("## Dummy INTENT documentation findings")
    print()
    if not intents:
        print("No findings.")
    else:
        print("| File | Entity | Line | Issue | Variables | Declaration |")
        print("| --- | --- | ---: | --- | --- | --- |")
        for finding in intents[:limit]:
            variables = ", ".join(f"`{name}`" for name in finding.variables)
            declaration = finding.declaration.replace("|", "\\|")
            print(
                f"| `{rel(finding.path)}` | `{finding.entity}` | {finding.line} | "
                f"`{finding.issue}` | {variables} | `{declaration}` |"
            )
        if limit is not None and len(intents) > limit:
            print(f"\nShowing {limit} of {len(intents)} findings.")


def print_text(legacy: list[LegacyBlockFinding], intents: list[IntentFinding], limit: int | None) -> None:
    print(f"Legacy leading comment blocks: {len(legacy)}")
    for finding in legacy[:limit]:
        terms = ", ".join(finding.missing_terms) or "-"
        print(f"{rel(finding.path)}:{finding.start}-{finding.end}: {finding.entity}: {finding.summary}")
        print(f"  terms not seen in FORD block: {terms}")
    if limit is not None and len(legacy) > limit:
        print(f"  ... showing {limit} of {len(legacy)}")

    print()
    print(f"Dummy INTENT documentation findings: {len(intents)}")
    for finding in intents[:limit]:
        variables = ", ".join(finding.variables)
        print(f"{rel(finding.path)}:{finding.line}: {finding.entity}: {finding.issue}: {variables}")
        print(f"  {finding.declaration}")
    if limit is not None and len(intents) > limit:
        print(f"  ... showing {limit} of {len(intents)}")


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path("src"), help="source root to scan")
    parser.add_argument("--format", choices=("text", "markdown"), default="text", help="output format")
    parser.add_argument("--limit", type=int, default=None, help="maximum rows per finding type")
    parser.add_argument(
        "--entity",
        action="append",
        default=[],
        help="case-insensitive procedure name to include; may be supplied more than once",
    )
    parser.add_argument(
        "--file",
        action="append",
        default=[],
        help="case-insensitive path substring to include; may be supplied more than once",
    )
    parser.add_argument(
        "--fail-on-findings",
        action="store_true",
        help="exit with status 1 when any finding is reported",
    )
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    legacy, intents = collect_findings(args.root)
    entity_filters = {name.lower() for name in args.entity}
    file_filters = [item.lower() for item in args.file]

    if entity_filters:
        legacy = [finding for finding in legacy if finding.entity.lower() in entity_filters]
        intents = [finding for finding in intents if finding.entity.lower() in entity_filters]

    if file_filters:
        legacy = [
            finding
            for finding in legacy
            if any(fragment in rel(finding.path).lower() for fragment in file_filters)
        ]
        intents = [
            finding
            for finding in intents
            if any(fragment in rel(finding.path).lower() for fragment in file_filters)
        ]

    if args.format == "markdown":
        print_markdown(legacy, intents, args.limit)
    else:
        print_text(legacy, intents, args.limit)

    if args.fail_on_findings and (legacy or intents):
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
