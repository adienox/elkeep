#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "gkeepapi>=0.16.0",
#     "keyring>=25.6.0",
#     "pypandoc>=1.15",
# ]
# ///

"""CLI tool for interacting with Google Keep and exporting notes to Org mode.

This module provides commands to:
- List notes
- Retrieve a specific note by ID
- Export journal entries to Org files
- Store and reuse authentication tokens securely

It uses the gkeepapi library for communication with Google Keep,
keyring for secure token storage, and pypandoc for file conversion.

Typical usage:
    elkeep-cli -l
    elkeep-cli -g NOTE_ID -o ./notes
    elkeep-cli -j ./journals
"""

import argparse
import json
import logging
import os
import re
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
    r"""Convert a string into a path-safe format by replacing problematic characters.

    This function replaces characters such as '/', '\0', ' ', and '?'
    with a specified replacement string (default is '_'). It also
    collapses multiple consecutive replacements into a single one and
    removes any trailing replacement characters.

    Args:
        s (str): The input string to be made path-safe.
        replacement (str): The string to replace problematic characters with.
                           Defaults to '_'.

    Returns:
        str: A path-safe version of the input string.

    """
    # Replace problematic characters
    s = (
        s.replace("/", replacement)
        .replace("\0", replacement)
        .replace(" ", replacement)
        .replace("?", replacement)
        .strip()
    )
    # Collapse multiple consecutive replacements
    s = re.sub(f"{re.escape(replacement)}+", replacement, s)

    # Remove trailing replacement if present
    return s.removesuffix(replacement)


def save_state(keep: gkeepapi.Keep) -> None:
    """Save the current state of the Google Keep instance to a JSON file.

    Args:
        keep (gkeepapi.Keep): The Google Keep API client instance.

    """
    state = keep.dump()
    fh = Path.open(state_file, "w")
    json.dump(state, fh)


def load_state() -> dict:
    """Load the application state from a JSON file.

    Returns:
        dict: The contents of the state file as a dictionary.

    Raises:
        FileNotFoundError: If the state file does not exist.
        json.JSONDecodeError: If the state file contains invalid JSON.

    """
    fh = Path.open(state_file, "r")
    return json.load(fh)


def store_token(token: str) -> None:
    """Store the provided token in the keyring associated with the specified email.

    Args:
        token (str): The token to be stored.

    """
    mail = input("Enter email: ")
    keyring.set_password("google-keep-token", mail, token)


def login_keep() -> gkeepapi.Keep:
    """Authenticate and log into Google Keep using stored credentials.

    Returns:
        gkeepapi.Keep: An authenticated instance of the Google Keep API client.

    Raises:
        SystemExit: Exits the program in case of credential retrieval or login failure.

    """
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


def get_labels(note: gkeepapi._node.TopLevelNode) -> list[str]:
    """Retrieve all labels associated with a given note.

    Args:
        note (gkeepapi._node.TopLevelNode): The note from which to retrieve labels.

    Returns:
        list[str]: A list of label names as strings.

    """
    return [str(label) for label in note.labels.all()]


def get_files_list(keep: gkeepapi.Keep) -> list[dict]:
    """Retrieve notes list from google keep.

    Args:
        keep (gkeepapi.Keep): The Google Keep API client instance.

    Returns:
        list[dict]: list of notes with its properties

    """
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

            labels = get_labels(note)

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


def output_file_name(
    note: gkeepapi._node.TopLevelNode,
    title: str | None,
    output_path: Path | None,
) -> Path:
    """Generate a filename based on a note's creation timestamp and title.

    Args:
        note (gkeepapi._node.TopLevelNode): The note object from gpkeepapi.
        title (str | None): Optional custom title to use for the filename;
                            if None, uses the note's title.

        output_path (Path | None): Optional path where the file should be saved;
                                   if None, uses the current directory.

    Returns:
        Path: The complete file path as a Path object, including the formatted filename.

    """
    date = note.timestamps.created.astimezone()

    timestamp = date.strftime("%Y%m%d%H%M%S")

    if title:
        filename = f"{timestamp}-{make_pathsafe(title)}.org"
    else:
        filename = f"{timestamp}-{make_pathsafe(note.title)}.org"

    return Path(output_path) / filename if output_path else Path(filename)


def convert_file(input_file: Path, output_file: Path) -> None:
    """Convert a file to Org mode format and remove the original file.

    Args:
        input_file (Path): The path of the file to be converted.
        output_file (Path): The path where the converted file will be saved.

    """
    org_text = pypandoc.convert_file(
        input_file,
        "org",
        extra_args=["--wrap=none"],
    )
    cleaned = re.sub(r":PROPERTIES:\n[\s\S]*?:END:\n?", "", org_text)

    with Path.open(output_file, "w") as f:
        f.write(cleaned)

    if input_file.exists():
        Path.unlink(input_file)


def combine_file(dst: Path, src: Path) -> None:
    """Combine the source file and destination file and deletes the source file.

    Args:
        dst (Path): The path to the destination file.
        src (Path): The path to the source file.

    Raises:
        FileNotFoundError: If the source file does not exist.
        IOError: If an I/O error occurs during file operations.

    """
    with Path.open(dst, "a") as f_dst, Path.open(src, "r") as f_src:
        f_dst.write("\n")
        f_dst.write(f_src.read())

    Path.unlink(src)


def prepend_org_uuid(
    path: Path, title: str, note: gkeepapi._node.TopLevelNode, is_journal: bool = False
) -> None:
    """Prepend a new header containing a id, title, and tags to an Org mode file.

    Args:
        path (Path): The path to the Org mode file.
        title (str): The title to be added in the header.
        note (gkeepapi._node.TopLevelNode): The note whose labels will be used as tags.

    """
    # Generate a random UUID4
    uid = str(uuid.uuid4())

    # Build property drawer string
    header = f":PROPERTIES:\n:ID:\t{uid}\n:END:\n#+title: {title}\n"

    labels = get_labels(note)

    tags = None
    if not is_journal:
        tags = f":{':'.join(labels)}:" if labels else None

    if tags:
        header = header + f"#+filetags: {tags}\n"

    # Read file content
    original = path.read_text(encoding="utf-8")

    # Prepend header
    path.write_text(header + "\n" + original, encoding="utf-8")


def get_note(
    keep: gkeepapi.Keep,
    note_id: str,
    output_path: Path | None,
    title: str | None = None,
    is_journal: bool = False,
) -> None:
    """Retrieve a note by its ID, save its content to a file.

    Args:
        keep (gkeepapi.Keep): The Google Keep API client instance.
        note_id (str): The ID of the note to be retrieved.
        output_path (Path | None): The path where the output file should be saved.
        title (str | None): The title to use for the output file, if provided.

    """
    try:
        note = keep.get(note_id)

    except Exception:
        logger.exception("Error retrieving note.")

    else:
        if note:
            input_file = "untitled.md" if note.title == "" else f"{note.title}.md"
            input_file = Path(input_file)

            if output_path and output_path.suffix == ".org":
                output_file = output_path
            else:
                output_file = output_file_name(note, title, output_path)

            # write content to a md file
            with Path.open(input_file, "w") as file:
                if is_journal:
                    file.write(f"# {note.title}\n")
                file.write(note.text)

            if output_file.exists():
                convert_file(input_file, f"{output_file}.temp")
                combine_file(output_file, f"{output_file}.temp")
            else:
                convert_file(input_file, output_file)
                prepend_org_uuid(output_file, title if title else note.title, note)

            print(f"File saved: {output_file}") # noqa: T201
            note.archived = True
            keep.sync()
        else:
            logger.error("No note found with the given ID.")


def get_journals(keep: gkeepapi.Keep, journal_path: str, label: str) -> None:
    """Retrieve journal notes from Google Keep and save them as Org files.

    Args:
        keep (gkeepapi.Keep): The Google Keep API client instance.
        journal_path (str): The directory path where the journal files will be saved.
        journal (str): The label to use to identify note as journal.

    """
    notes = get_files_list(keep)

    for note in notes:
        if label in note["labels"]:
            title = datetime.fromisoformat(note["ctime"]).strftime("%Y-%m-%d")
            filename = Path(journal_path) / f"{title}.org"

            get_note(keep, note["id"], filename, title, is_journal=True)


def list_notes(files: list[dict]) -> None:
    """Print a list of notes that do not have the label "JOURNAL".

    Args:
        files (list[dict]): A list of files where each dictionary represents a file
                            with potential labels.

    """
    exclude = {"JOURNAL", "NOSHOW"}
    notes = [
        file for file in files if all(label not in file["labels"] for label in exclude)
    ]

    print(json.dumps(notes))  # noqa: T201


def main() -> None:
    """Entry point for the program.

    Parses command-line arguments and logs into google keep.

    """
    parser = argparse.ArgumentParser(
        description="A simple CLI tool to interact with gkeep",
    )

    parser.add_argument("-l", "--list", action="store_true", help="List all notes")
    parser.add_argument(
        "-j",
        "--journal",
        type=str,
        metavar="path",
        help="Get all journal files",
    )
    parser.add_argument("-g", "--get", type=str, metavar="ID", help="Get a note by ID")
    parser.add_argument("-o", "--output", type=str, metavar="path", help="Output path")
    parser.add_argument("-t", "--token", type=str, metavar="token", help="Master token")
    parser.add_argument("-L", "--label", type=str, metavar="label", help="Label")
    parser.add_argument(
        "-T",
        "--title",
        type=str,
        metavar="title",
        help="Title of note",
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

    if args.journal and not args.label:
        logger.error("Label to identify journal not specified")
        sys.exit(1)

    keep = login_keep()

    if args.token:
        store_token(args.token)
    elif args.list:
        files = get_files_list(keep)
        list_notes(files)
    elif args.get:
        output_path = Path(args.output) if args.output else None
        get_note(keep, args.get, output_path, args.title)
    elif args.journal:
        get_journals(keep, args.journal, args.label)
    else:
        parser.print_help()
        sys.exit(1)

    save_state(keep)


if __name__ == "__main__":
    main()
