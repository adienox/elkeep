#!/usr/bin/env python3
import argparse
import json
import logging
import os
import sys
import uuid
from datetime import datetime
from pathlib import Path

import gkeepapi
import keyring
import pypandoc

data_home = Path(os.getenv("XDG_DATA_HOME"))
logger = logging.getLogger(__name__)

if data_home.exists():
    state_file = data_home / "gkeep_state"
else:
    state_file = Path(os.getenv("HOME")) / ".local/share"


def make_pathsafe(s: str, replacement: str = "_") -> str:
    # Replace '/' and '\0' only
    return (
        s.replace("/", replacement)
        .replace("\0", replacement)
        .replace(" ", replacement)
        .replace("?", replacement)
    )


def save_state(keep: gkeepapi.Keep) -> None:
    state = keep.dump()
    fh = Path.open(state_file, "w")
    json.dump(state, fh)


def load_state() -> dict:
    fh = Path.open(state_file, "r")
    return json.load(fh)


def store_token(token: str) -> None:
    mail = input("Enter email: ")
    keyring.set_password("google-keep-token", mail, token)


def login_keep() -> gkeepapi.Keep:
    keep = gkeepapi.Keep()
    try:
        mail = keyring.get_credential("google-keep-token", None).username
        master_token = keyring.get_password("google-keep-token", mail)
    except Exception:
        logger.exception("Error getting credential from keyring.")
        sys.exit(1)

    try:
        if state_file.exists():
            state = load_state()
            keep.authenticate(mail, master_token, state=state)
        else:
            keep.authenticate(mail, master_token)

    except Exception:
        logger.exception("Error logging in to keep.")
        sys.exit(1)

    return keep


def list_notes(keep: gkeepapi.Keep) -> list[dict]:
    try:
        notes = keep.find(archived=False, trashed=False)

    except Exception:
        logger.exception("Error finding note.")

    else:
        names = []

        for note in notes:
            if note.title != "":
                name = note.title.strip()
                notitle = False
            else:
                name = note.text.strip()
                notitle = True

            labels = [str(x) for x in note.labels.all()]

            names.append(
                {
                    "name": name,
                    "id": note.id,
                    "labels": labels,
                    "ctime": str(note.timestamps.created.astimezone()),
                    "notitle": notitle,
                },
            )

        return names


def output_file_name(note: gkeepapi.node, title: str | None, output_path: Path) -> Path:
    date = note.timestamps.created.astimezone()

    timestamp = date.strftime("%Y%m%d%H%M%S")

    if title:
        filename = f"{timestamp}-{make_pathsafe(title)}.org"
    else:
        filename = f"{timestamp}-{make_pathsafe(note.title)}.org"

    return Path(output_path) / filename if output_path else filename



def convert_file(input_file: Path, output_file: Path) -> None:
    # convert the file into an org file
    pypandoc.convert_file(
        input_file,
        "org",
        outputfile=output_file,
        extra_args=["--wrap=none"],
    )

    # remove original file
    if input_file.exists():
        Path.unlink(input_file)


def combine_file(dst: Path, src: Path) -> None:

    with Path.open(dst, "a") as f_dst, Path.open(src, "r") as f_src:
        f_dst.write(f_src.read())

    Path.unlink(src)


def prepend_org_uuid(path: Path, title: str) -> None:
    # Generate a random UUID4
    uid = str(uuid.uuid4())

    # Build property drawer string
    header = f":PROPERTIES:\n:ID:\t{uid}\n:END:\n#+title: {title}\n\n"

    # Read file content
    original = path.read_text(encoding="utf-8")

    # Prepend header
    path.write_text(header + original, encoding="utf-8")


def get_note(
    keep: gkeepapi.Keep, note_id: str, output_path: Path, title: str | None = None,
) -> None:
    try:
        note = keep.get(note_id)

    except Exception:
        logger.exception("Error retrieving note.")

    else:
        if note:
            input_file = Path("untitled.md") if note.title == "" else Path(f"{note.title}.md")

            if output_path and output_path.suffix == ".org":
                output_file = output_path
            else:
                output_file = output_file_name(note, title, output_path)

            # write content to a md file
            with Path.open(input_file, "w") as file:
                file.write(note.text)

            if output_file.exists():
                convert_file(input_file, f"{output_file}.temp")
                combine_file(output_file, f"{output_file}.temp")
            else:
                convert_file(input_file, output_file)
                prepend_org_uuid(output_file, title if title else note.title)

            note.archived = True
            keep.sync()
        else:
            logger.error("No note found with the given ID.")


def get_journals(keep: gkeepapi.Keep, journal_path: str) -> None:

    notes = list_notes(keep)

    for note in notes:
        if "JOURNAL" in note["labels"]:
            title = datetime.fromisoformat(note["ctime"]).strftime("%Y-%m-%d")
            filename = Path(journal_path) / f"{title}.org"

            get_note(keep, note["id"], filename, title)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="A simple CLI tool to interact with gkeep",
    )

    parser.add_argument("-l", "--list", action="store_true", help="List all notes")
    parser.add_argument(
        "-j", "--journal", type=str, metavar="path", help="Get all journal files",
    )
    parser.add_argument("-g", "--get", type=str, metavar="ID", help="Get a note by ID")
    parser.add_argument("-o", "--output", type=str, metavar="path", help="Output path")
    parser.add_argument("-t", "--token", type=str, metavar="token", help="Master token")
    parser.add_argument(
        "-T", "--title", type=str, metavar="title", help="Title of note",
    )

    args = parser.parse_args()

    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(0)

    if args.output and not Path(args.output).exists():
        logger.error("Output path doesn't exist")
        sys.exit(1)

    if args.journal and not Path(args.journal).exists():
        logger.error("Journal path doesn't exist")
        sys.exit(1)

    keep = login_keep()
    if args.token:
        store_token(args.token)
    elif args.list:
        notes = list_notes(keep)
        print(json.dumps(notes)) # noqa: T201
    elif args.get:
        get_note(keep, args.get, Path(args.output), args.title)
    elif args.journal:
        get_journals(keep, args.journal)
    else:
        parser.print_help()
        sys.exit(1)

    save_state(keep)


if __name__ == "__main__":
    main()
