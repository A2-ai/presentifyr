import os
import json
from py_logger import get_logger

## Placeholder types that can accept images
## 7 = OBJECT (Content Placeholder) - universal content placeholder
## 18 = PICTURE (Picture Placeholder) - image-specific placeholder
USABLE_PLACEHOLDER_TYPES = {7, 18}

## Placeholder indices to preserve (not clear) when adding images
## 11 = Footer Placeholder
## 12 = Slide Number Placeholder
PRESERVE_PLACEHOLDER_INDICES = {11, 12}


def find_footer_placeholder(slide):
    """Find the footer placeholder on a slide, if it exists.

    Footer placeholders have placeholder index 11 in the PowerPoint spec.
    Only returns shapes that have a text_frame (guards against placeholders
    that were converted to PlaceholderPicture by insert_picture).
    """
    for shape in slide.placeholders:
        if shape.placeholder_format.idx == 11 and shape.has_text_frame:
            return shape
    return None


def load_metadata_for_image(image_path):
    """Load metadata JSON for an image file.

    Args:
        image_path: Path to the image file

    Returns:
        dict with metadata, or None if not found
    """
    logger = get_logger()

    # Construct metadata filename: {name}_{ext}_metadata.json
    dir_name = os.path.dirname(image_path)
    file_name = os.path.basename(image_path)
    name, ext = os.path.splitext(file_name)
    ext = ext.lstrip('.')  # Remove leading dot

    metadata_filename = f"{name}_{ext}_metadata.json"
    metadata_path = os.path.join(dir_name, metadata_filename)

    logger.debug(f"Looking for metadata at: {metadata_path}")

    if not os.path.exists(metadata_path):
        logger.warning(f"Metadata file not found: {metadata_path}")
        return None

    try:
        with open(metadata_path, 'r') as f:
            metadata = json.load(f)
        logger.debug(f"Loaded metadata from: {metadata_path}")
        return metadata
    except Exception as e:
        logger.error(f"Error reading metadata file: {metadata_path} - {e}")
        return None


def decode_abbreviations(abbrev_list, definitions=None):
    """Decode abbreviation keys into 'KEY: full form' strings.

    Matches reportifyr's footnote formatting convention.

    Args:
        abbrev_list: list of abbreviation key strings
        definitions: dict mapping keys to full forms

    Returns:
        Formatted string like "CI: confidence interval, HR: hazard ratio."

    Raises:
        KeyError: if any abbreviation key is not in definitions.
    """
    filtered = [a for a in abbrev_list if a]
    if not filtered:
        return "N/A"

    definitions = definitions or {}
    parts = []
    for key in filtered:
        if key not in definitions:
            raise KeyError(
                f"Abbreviation '{key}' not found in abbreviations "
                f"section of footnotes YAML"
            )
        full_form = definitions[key].rstrip('.')
        parts.append(f"{key}: {full_form}")
    return ', '.join(parts) + '.'


def format_slide_notes(metadata, abbreviation_definitions=None):
    """Format slide notes with metadata.

    Args:
        metadata: dict containing the metadata (from load_metadata_for_image)
        abbreviation_definitions: dict mapping abbreviation keys to
            their full forms. If None, raw keys are displayed.

    Returns:
        Formatted string for slide notes
    """
    lines = []

    # Source: source_meta.path + source_meta.latest_time
    source_meta = metadata.get('source_meta', {})
    source_path = source_meta.get('path', '')
    source_time = source_meta.get('latest_time', '')

    if source_path and source_time:
        lines.append(f"Source: {source_path} {source_time}")
    elif source_path:
        lines.append(f"Source: {source_path}")
    else:
        lines.append("Source: N/A")

    # Notes: object_meta.footnotes.notes (joined with ". ")
    object_meta = metadata.get('object_meta', {})
    footnotes = object_meta.get('footnotes', {})
    notes_list = footnotes.get('notes', [])

    if notes_list and any(n for n in notes_list if n):
        notes_text = ' '.join(
            n if n.endswith('.') else f"{n}."
            for n in notes_list if n
        )
        lines.append(f"Notes: {notes_text}")
    else:
        lines.append("Notes: N/A")

    # Abbreviations: decode keys using definitions
    abbrev_list = footnotes.get('abbreviations', [])

    if abbrev_list and any(a for a in abbrev_list if a):
        abbrev_text = decode_abbreviations(
            abbrev_list, abbreviation_definitions
        )
        lines.append(f"Abbreviations: {abbrev_text}")
    else:
        lines.append("Abbreviations: N/A")

    # Trailing blank line separator
    lines.append("")

    return '\n'.join(lines)
