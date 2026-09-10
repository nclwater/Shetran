#!/usr/bin/env python3
"""One-shot: wire the unformatted restart/result writes in FRmod's FRRESC and
FRRESP through the module-local ``res_write_check`` helper (which forwards to
``mod_error::errstat_write``).

Every ``WRITE (RES) ...`` / ``WRITE (IORES(ISET)) ...`` statement gains
``IOSTAT=ios, IOMSG=emsg`` and is followed by ``CALL res_write_check(ios, emsg)``.
The single guarded ``IF (.NOT. COLUMN) WRITE (IORES(ISET)) ...`` line is expanded
into a ``THEN`` block so the check only runs when the write happened.
"""
import re

PATH = "src/modules/FRmod.f90"


def stmt_end(lines, i):
    """Index of the last physical line of the statement starting at i."""
    while lines[i].rstrip().endswith("&"):
        i += 1
    return i


def process():
    with open(PATH, encoding="utf-8") as fh:
        lines = fh.readlines()

    out = []
    i = 0
    n_res = n_ires = 0
    while i < len(lines):
        ln = lines[i]
        s = ln.strip()

        m_res = re.match(r"^WRITE \(RES\) ", s)
        m_ires = re.match(r"^WRITE \(IORES\(ISET\)\) ", s)
        m_guard = re.match(r"^IF \(\.NOT\. COLUMN\) WRITE \(IORES\(ISET\)\) (.*)$", s)
        indent = ln[: len(ln) - len(ln.lstrip())]

        if m_guard:
            body = m_guard.group(1)
            out.append(f"{indent}IF (.NOT. COLUMN) THEN\n")
            out.append(f"{indent}   WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) {body}\n")
            out.append(f"{indent}   CALL res_write_check(ios, emsg)\n")
            out.append(f"{indent}END IF\n")
            n_ires += 1
            i += 1
            continue

        if m_res or m_ires:
            end = stmt_end(lines, i)
            first = lines[i]
            if m_res:
                first = first.replace("WRITE (RES) ", "WRITE (RES, IOSTAT=ios, IOMSG=emsg) ", 1)
                n_res += 1
            else:
                first = first.replace(
                    "WRITE (IORES(ISET)) ", "WRITE (IORES(ISET), IOSTAT=ios, IOMSG=emsg) ", 1
                )
                n_ires += 1
            out.append(first)
            for j in range(i + 1, end + 1):
                out.append(lines[j])
            out.append(f"{indent}CALL res_write_check(ios, emsg)\n")
            i = end + 1
            continue

        out.append(ln)
        i += 1

    with open(PATH, "w", encoding="utf-8") as fh:
        fh.writelines(out)
    print(f"{PATH}: wired {n_res} WRITE (RES) and {n_ires} WRITE (IORES(ISET)) sites")


if __name__ == "__main__":
    process()
