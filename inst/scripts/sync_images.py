import os
import re
import argparse
import json
import hashlib
from pptx import Presentation
from pptx.util import Pt
from pptx.dml.color import RGBColor
from py_logger import get_logger


def compute_file_hash(file_path):
    """Compute MD5 hash of a file on disk."""
    hash_md5 = hashlib.md5()
    with open(file_path, 'rb') as f:
        for chunk in iter(lambda: f.read(8192), b''):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


def compute_blob_hash(blob):
    """Compute MD5 hash of binary data (image blob from PPTX)."""
    return hashlib.md5(blob).hexdigest()


def compute_text_hash(text):
    """Compute MD5 hash of text content."""
    return hashlib.md5(text.encode('utf-8')).hexdigest()


def should_replace_image(shape, new_image_path, logger):
    """Determine if image should be replaced based on hash comparison."""
    try:
        existing_hash = compute_blob_hash(shape.image.blob)
        new_hash = compute_file_hash(new_image_path)

        if existing_hash == new_hash:
            return False, "unchanged (hashes match)"
        else:
            return True, "content changed"
    except Exception as e:
        logger.warning(f"Could not compute hash: {e}. Will replace image.")
        return True, f"hash computation failed ({e})"


def find_footer_placeholder(slide):
    """Find the footer placeholder on a slide, if it exists.

    Footer placeholders have placeholder index 11 in the PowerPoint spec.
    """
    for shape in slide.placeholders:
        if shape.placeholder_format.idx == 11:
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


def format_slide_notes(metadata):
    """Format slide notes with metadata.

    Args:
        metadata: dict containing the metadata (from load_metadata_for_image)

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

    # Abbreviations: object_meta.footnotes.abbreviations (comma-separated)
    abbrev_list = footnotes.get('abbreviations', [])

    if abbrev_list and any(a for a in abbrev_list if a):
        lines.append(f"Abbreviations: {', '.join(a for a in abbrev_list if a)}")
    else:
        lines.append("Abbreviations: N/A")

    # Trailing blank line separator
    lines.append("")

    return '\n'.join(lines)


def load_font_settings(font_settings_path):
    """Load font settings from a JSON file.

    Args:
        font_settings_path: Path to JSON file with font settings

    Returns:
        dict with font settings, or empty dict if not found/invalid
    """
    logger = get_logger()
    if not font_settings_path or not os.path.exists(font_settings_path):
        logger.debug("No font settings file provided or file not found")
        return {}

    try:
        with open(font_settings_path, 'r') as f:
            settings = json.load(f)
        logger.debug(f"Loaded font settings: {settings}")
        return settings
    except Exception as e:
        logger.warning(f"Could not load font settings: {e}")
        return {}


def apply_font_to_run(run, font_settings):
    """Apply font settings to a python-pptx Run object.

    Args:
        run: A python-pptx Run object
        font_settings: dict with font_name, font_size, bold, italic, underline, font_color
    """
    font = run.font

    font_name = font_settings.get('font_name')
    if font_name:
        font.name = font_name

    font_size = font_settings.get('font_size')
    if font_size is not None:
        font.size = Pt(font_size)

    bold = font_settings.get('bold')
    if bold is not None:
        font.bold = bold

    italic = font_settings.get('italic')
    if italic is not None:
        font.italic = italic

    underline = font_settings.get('underline')
    if underline is not None:
        font.underline = underline

    font_color = font_settings.get('font_color')
    if font_color:
        hex_color = font_color.lstrip('#')
        font.color.rgb = RGBColor(
            int(hex_color[0:2], 16),
            int(hex_color[2:4], 16),
            int(hex_color[4:6], 16)
        )


def set_formatted_text(text_frame, text, font_settings):
    """Set text in a text_frame with font formatting applied.

    Clears existing content, splits text by newlines, and applies font
    settings to each run.

    Args:
        text_frame: A python-pptx TextFrame object
        text: The text content to set
        font_settings: dict with font settings to apply
    """
    # Clear existing paragraphs
    text_frame.clear()

    lines = text.split('\n')

    for i, line in enumerate(lines):
        if i == 0:
            p = text_frame.paragraphs[0]
        else:
            p = text_frame.add_paragraph()

        run = p.add_run()
        run.text = line
        apply_font_to_run(run, font_settings)


def font_settings_fingerprint(font_settings):
    """Compute a hash fingerprint of font settings for change detection.

    Args:
        font_settings: dict with font settings

    Returns:
        MD5 hex digest string
    """
    serialized = json.dumps(font_settings, sort_keys=True)
    return hashlib.md5(serialized.encode('utf-8')).hexdigest()


def should_update_footer(footer_shape, new_text, logger, font_settings=None):
    """Determine if footer text should be updated based on hash comparison."""
    try:
        existing_text = footer_shape.text or ""
        existing_hash = compute_text_hash(existing_text)

        # Include font settings in hash so formatting changes trigger updates
        combined = new_text
        if font_settings:
            combined = new_text + font_settings_fingerprint(font_settings)

        new_hash = compute_text_hash(combined)

        # Also hash existing text with font fingerprint for fair comparison
        existing_combined = existing_text
        if font_settings:
            existing_combined = existing_text + font_settings_fingerprint(font_settings)
        existing_hash = compute_text_hash(existing_combined)

        if existing_hash == new_hash:
            return False, "unchanged (hashes match)"
        else:
            return True, "content changed"
    except Exception as e:
        logger.warning(f"Could not compare footer: {e}. Will update.")
        return True, f"comparison failed ({e})"


def should_update_notes(slide, new_notes_text, logger, font_settings=None):
    """Determine if slide notes should be updated based on hash comparison."""
    try:
        notes_slide = slide.notes_slide
        text_frame = notes_slide.notes_text_frame
        if text_frame is None:
            return True, "no existing notes"

        existing_text = text_frame.text or ""
        existing_hash = compute_text_hash(existing_text)

        combined = new_notes_text
        if font_settings:
            combined = new_notes_text + font_settings_fingerprint(font_settings)

        new_hash = compute_text_hash(combined)

        existing_combined = existing_text
        if font_settings:
            existing_combined = existing_text + font_settings_fingerprint(font_settings)
        existing_hash = compute_text_hash(existing_combined)

        if existing_hash == new_hash:
            return False, "unchanged (hashes match)"
        else:
            return True, "content changed"
    except Exception as e:
        logger.warning(f"Could not compare notes: {e}. Will update.")
        return True, f"comparison failed ({e})"


def sync_images(input_pptx, output_pptx, image_dict, font_settings=None):
    logger = get_logger()
    logger.debug(f"Starting sync images Python function")

    presentation = Presentation(input_pptx)
    logger.debug(f"Using input .pptx file: {input_pptx}")

    start_pattern = r'\{prfy\}\:'
    end_pattern = r'\.[^.]+$'
    magic_pattern = re.compile(start_pattern + '.*?' + end_pattern)

    # Track statistics for summary
    stats = {
        'images_replaced': 0,
        'images_skipped': 0,
        'images_not_found': 0,
        'notes_updated': 0,
        'notes_skipped': 0,
        'footer_updated': 0,
        'footer_skipped': 0
    }

    logger.info("Scanning slides for image and footnote sync")

    for slide_index, slide in enumerate(presentation.slides, start=1):
        matched_images = []  # All images with {prfy}: marker and valid paths
        replacements = []    # Images that need replacement (hash differs)

        for shape in slide.shapes:

            # Extract alt_text based on shape type
            if shape.shape_type == 14:  # GROUP
                alt_text = shape._element._nvXxPr.cNvPr.attrib.get("descr", "")
            elif shape.shape_type == 13:  # PICTURE
                alt_text = shape._element.nvPicPr.cNvPr.attrib.get("descr", "")
            else:
                continue

            logger.debug(f"Slide {slide_index}: Checking image alt-text: {alt_text}")

            match = magic_pattern.match(alt_text)
            if match:
                figure_name = match.group(0).split(':')[1]
                # Try exact match first (for relative paths), then basename match
                image_path = image_dict.get(figure_name)
                if not image_path:
                    # Fall back to matching by basename (for backward compatibility)
                    for key, path in image_dict.items():
                        if os.path.basename(key) == figure_name or os.path.basename(path) == figure_name:
                            image_path = path
                            break

                if image_path and os.path.exists(image_path):
                    # Track all matched images for footnote processing
                    matched_images.append((shape, image_path, alt_text))

                    # Check if image content has changed using hash comparison
                    should_replace, reason = should_replace_image(shape, image_path, logger)

                    if should_replace:
                        logger.info(f"Slide {slide_index}: Queuing replacement for {figure_name} ({reason})")
                        replacements.append((shape, image_path, alt_text))
                    else:
                        logger.debug(f"Slide {slide_index}: Skipping figure {figure_name} - {reason}")
                        stats['images_skipped'] += 1
                else:
                    logger.warning(f"Slide {slide_index}: No matching image found for {figure_name} or file does not exist")
                    stats['images_not_found'] += 1

        # Process figure replacements
        for shape, image_path, alt_text in replacements:
            left = shape.left
            top = shape.top
            width = shape.width
            height = shape.height

            slide.shapes._spTree.remove(shape._element)
            new_pic = slide.shapes.add_picture(image_path, left, top, width, height)
            new_pic._element.nvPicPr.cNvPr.set("descr", alt_text)

            logger.debug(f"Slide {slide_index}: Replaced image from {image_path}")
            stats['images_replaced'] += 1

        # Process footnotes independently (using ALL matched images, not just replacements)
        if matched_images:
            # Collect metadata from all matched images on this slide
            slide_metadata_list = {}
            for _, image_path, _ in matched_images:
                metadata = load_metadata_for_image(image_path)
                if metadata:
                    slide_metadata_list[os.path.basename(image_path)] = metadata

            if slide_metadata_list:
                # Combine notes from all images (matching add_images.R behavior)
                combined_notes = "\n\n".join(
                    f"## {name}\n{format_slide_notes(meta)}"
                    for name, meta in slide_metadata_list.items()
                )

                use_formatted = bool(font_settings)

                # Check for footer placeholder first; fall back to slide notes
                footer_shape = find_footer_placeholder(slide)

                if footer_shape is not None:
                    should_update, reason = should_update_footer(
                        footer_shape, combined_notes, logger, font_settings
                    )
                    if should_update:
                        try:
                            if use_formatted:
                                set_formatted_text(footer_shape.text_frame, combined_notes, font_settings)
                            else:
                                footer_shape.text = combined_notes
                            logger.info(f"Slide {slide_index}: Updated footer ({reason})")
                            stats['footer_updated'] += 1
                        except Exception as e:
                            logger.warning(f"Slide {slide_index}: Could not update footer - {e}")
                    else:
                        logger.debug(f"Slide {slide_index}: Skipping footer - {reason}")
                        stats['footer_skipped'] += 1
                else:
                    # No footer placeholder — fall back to slide notes
                    should_update, reason = should_update_notes(
                        slide, combined_notes, logger, font_settings
                    )
                    if should_update:
                        try:
                            notes_slide = slide.notes_slide
                            text_frame = notes_slide.notes_text_frame
                            if text_frame is not None:
                                if use_formatted:
                                    set_formatted_text(text_frame, combined_notes, font_settings)
                                else:
                                    text_frame.text = combined_notes
                                logger.info(f"Slide {slide_index}: Updated notes ({reason})")
                                stats['notes_updated'] += 1
                            else:
                                logger.warning(f"Slide {slide_index}: No notes text frame available")
                        except Exception as e:
                            logger.warning(f"Slide {slide_index}: Could not update notes - {e}")
                    else:
                        logger.debug(f"Slide {slide_index}: Skipping notes - {reason}")
                        stats['notes_skipped'] += 1

        if not matched_images:
            logger.warning(f"Slide {slide_index}: No matching alt-text found")

    logger.info(f"Sync complete: {stats['images_replaced']} images replaced, "
                f"{stats['images_skipped']} images unchanged, "
                f"{stats['footer_updated']} footers updated, "
                f"{stats['footer_skipped']} footers unchanged, "
                f"{stats['notes_updated']} notes updated, "
                f"{stats['notes_skipped']} notes unchanged")

    presentation.save(output_pptx)
    logger.debug(f"PowerPoint saved as {output_pptx}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Sync images within input pptx file by monkey-patching placeholders.")
    parser.add_argument('-i', '--input_pptx', type=str, required=True, help="Input pptx file path")
    parser.add_argument('-o', '--output_pptx', type=str, required=True, help="Output pptx file path")
    parser.add_argument('-d', '--image_dict', type=str, required=True, help="Path to JSON file containing image dictionary")
    parser.add_argument('-f', '--font_settings', type=str, required=False, default=None,
                        help="Path to JSON file containing font settings")

    args = parser.parse_args()

    with open(args.image_dict, 'r') as f:
        image_dict = json.load(f)

    fs = load_font_settings(args.font_settings) if args.font_settings else None

    sync_images(
        input_pptx=args.input_pptx,
        output_pptx=args.output_pptx,
        image_dict=image_dict,
        font_settings=fs
    )
