import os
import re
import argparse
import json
import hashlib
from pptx import Presentation
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


def normalize_text_for_comparison(text):
    """Normalize text for hash comparison.

    Strips trailing whitespace from each line and trailing blank lines,
    so officer-created styled content (which may have extra empty paragraphs)
    matches the Python-generated text.
    """
    lines = text.split('\n')
    # Strip trailing whitespace per line
    lines = [line.rstrip() for line in lines]
    # Strip trailing blank lines
    while lines and lines[-1] == '':
        lines.pop()
    return '\n'.join(lines)


def should_update_footer(footer_shape, new_text, logger):
    """Determine if footer text should be updated based on hash comparison."""
    try:
        existing_text = normalize_text_for_comparison(footer_shape.text or "")
        new_normalized = normalize_text_for_comparison(new_text)

        existing_hash = compute_text_hash(existing_text)
        new_hash = compute_text_hash(new_normalized)

        if existing_hash == new_hash:
            return False, "unchanged (hashes match)"
        else:
            return True, "content changed"
    except Exception as e:
        logger.warning(f"Could not compare footer: {e}. Will update.")
        return True, f"comparison failed ({e})"


def should_update_notes(slide, new_notes_text, logger):
    """Determine if slide notes should be updated based on hash comparison."""
    try:
        notes_slide = slide.notes_slide
        text_frame = notes_slide.notes_text_frame
        if text_frame is None:
            return True, "no existing notes"

        existing_text = normalize_text_for_comparison(text_frame.text or "")
        new_normalized = normalize_text_for_comparison(new_notes_text)

        existing_hash = compute_text_hash(existing_text)
        new_hash = compute_text_hash(new_normalized)

        if existing_hash == new_hash:
            return False, "unchanged (hashes match)"
        else:
            return True, "content changed"
    except Exception as e:
        logger.warning(f"Could not compare notes: {e}. Will update.")
        return True, f"comparison failed ({e})"


def split_into_blocks(lines):
    """Split a list of lines into blocks delimited by '## ' headers.

    Returns a list of blocks, where each block is a list of lines.
    The first line of each block is the '## ' header.
    """
    blocks = []
    current = []
    for line in lines:
        if line.startswith('## ') and current:
            blocks.append(current)
            current = []
        current.append(line)
    if current:
        blocks.append(current)
    return blocks


# Expected line count per block: header, Source, Notes, Abbreviations, blank
BLOCK_SIZE = 5


def update_text_preserve_formatting(text_frame, new_text):
    """Update text frame content while preserving existing run formatting.

    Uses the known block structure (## header, Source, Notes, Abbreviations,
    blank) to align existing paragraphs with new lines by index within each
    block.  Lines present in both old and new get their text updated while
    keeping the existing rPr.  Lines the user deleted (missing paragraph)
    are re-inserted with no styling.  Extra paragraphs are removed.
    """
    from copy import deepcopy
    from lxml import etree

    ns = '{http://schemas.openxmlformats.org/drawingml/2006/main}'

    new_lines = new_text.split('\n')
    while new_lines and new_lines[-1].strip() == '':
        new_lines.pop()

    existing_paras = list(text_frame.paragraphs)
    existing_texts = [(p.text or '').rstrip() for p in existing_paras]

    # Split both sides into blocks by '## ' header
    new_blocks = split_into_blocks(new_lines)
    old_blocks = split_into_blocks(existing_texts)

    # Build a map: block_index -> list of existing paragraph objects
    old_para_blocks = []
    idx = 0
    for block_lines in old_blocks:
        old_para_blocks.append(existing_paras[idx:idx + len(block_lines)])
        idx += len(block_lines)

    txBody = text_frame._txBody

    # Remove all existing <a:p> elements — we'll re-insert them in order
    for p_elem in txBody.findall(f'{ns}p'):
        txBody.remove(p_elem)

    for block_i, new_block in enumerate(new_blocks):
        # Get the matching old block (by index), if it exists
        old_paras = old_para_blocks[block_i] if block_i < len(old_para_blocks) else []

        for line_i, new_line in enumerate(new_block):
            if line_i < len(old_paras):
                # Existing paragraph at this position — reuse it
                p = old_paras[line_i]
                p_elem = p._p

                if p.runs:
                    p.runs[0].text = new_line
                    # Remove extra runs beyond the first
                    for run in p.runs[1:]:
                        p_elem.remove(run._r)
                else:
                    # Paragraph exists but has no runs — add a plain one
                    r_elem = etree.SubElement(p_elem, f'{ns}r')
                    t_elem = etree.SubElement(r_elem, f'{ns}t')
                    t_elem.text = new_line

                # Re-attach to txBody in correct order
                txBody.append(p_elem)
            else:
                # Missing paragraph (user deleted it) — insert plain, unstyled
                p_elem = etree.SubElement(txBody, f'{ns}p')
                r_elem = etree.SubElement(p_elem, f'{ns}r')
                t_elem = etree.SubElement(r_elem, f'{ns}t')
                t_elem.text = new_line


def sync_images(input_pptx, output_pptx, image_dict):
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

                # Check for footer placeholder first; fall back to slide notes
                footer_shape = find_footer_placeholder(slide)

                if footer_shape is not None:
                    should_update, reason = should_update_footer(footer_shape, combined_notes, logger)
                    if should_update:
                        try:
                            update_text_preserve_formatting(footer_shape.text_frame, combined_notes)
                            logger.info(f"Slide {slide_index}: Updated footer ({reason})")
                            stats['footer_updated'] += 1
                        except Exception as e:
                            logger.warning(f"Slide {slide_index}: Could not update footer - {e}")
                    else:
                        logger.debug(f"Slide {slide_index}: Skipping footer - {reason}")
                        stats['footer_skipped'] += 1
                else:
                    # No footer placeholder — fall back to slide notes
                    should_update, reason = should_update_notes(slide, combined_notes, logger)
                    if should_update:
                        try:
                            notes_slide = slide.notes_slide
                            text_frame = notes_slide.notes_text_frame
                            if text_frame is not None:
                                update_text_preserve_formatting(text_frame, combined_notes)
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

    args = parser.parse_args()

    with open(args.image_dict, 'r') as f:
        image_dict = json.load(f)

    sync_images(
        input_pptx=args.input_pptx,
        output_pptx=args.output_pptx,
        image_dict=image_dict
    )
