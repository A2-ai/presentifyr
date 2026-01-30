import os
import re
import argparse
import json
from pptx import Presentation
from py_logger import get_logger


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


def format_slide_notes(metadata, github_url):
    """Format slide notes with metadata and GitHub URL.

    Args:
        metadata: dict containing the metadata (from load_metadata_for_image)
        github_url: The GitHub URL for the image

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

    # Add blank line and GitHub URL
    lines.append('')
    lines.append(github_url)

    return '\n'.join(lines)


def sync_images(input_pptx, output_pptx, image_dict, repo_url=None):
    logger = get_logger()
    logger.debug(f"Starting sync images Python function")
    
    presentation = Presentation(input_pptx)
    logger.debug(f"Using input .pptx file: {input_pptx}")

    start_pattern = r'\{prfy\}\:'
    end_pattern = r'\.[^.]+$'
    magic_pattern = re.compile(start_pattern + '.*?' + end_pattern)

    logger.info("Scanning slides for image replacements")

    for slide_index, slide in enumerate(presentation.slides, start=1):  
        match_found = False 
        replacements = [] ## Track replacements

        for shape in slide.shapes:
            if shape.shape_type == 14:
                alt_text = shape._element._nvXxPr.cNvPr.attrib.get("descr", "")
            elif shape.shape_type == 13: 
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
                    logger.info(f"Slide {slide_index}: Queuing replacement for {figure_name} with {image_path}")
                    replacements.append((shape, image_path, alt_text))
                    match_found = True
                else:
                    logger.warning(f"Slide {slide_index}: No matching image found for {figure_name} or file does not exist")

        for shape, image_path, alt_text in replacements:
            left = shape.left
            top = shape.top
            width = shape.width
            height = shape.height

            slide.shapes._spTree.remove(shape._element)
            new_pic = slide.shapes.add_picture(image_path, left, top, width, height)
            new_pic._element.nvPicPr.cNvPr.set("descr", alt_text)

            logger.debug(f"Slide {slide_index}: Inserted new picture from {image_path} with original dimensions and cropping maintained")

        # Update slide notes if replacements were made
        if replacements and repo_url:
            # Use the last replaced image for notes (typically one per slide)
            _, last_image_path, last_alt_text = replacements[-1]

            # Extract relative path from alt_text for GitHub URL
            figure_name = last_alt_text.split(':')[1] if ':' in last_alt_text else os.path.basename(last_image_path)
            github_url = f"{repo_url}/{figure_name}"

            # Load metadata and format notes
            metadata = load_metadata_for_image(last_image_path)
            if metadata:
                notes_text = format_slide_notes(metadata, github_url)
                logger.debug(f"Slide {slide_index}: Formatted notes with metadata")
            else:
                notes_text = github_url
                logger.debug(f"Slide {slide_index}: Using URL-only notes (no metadata)")

            # Set slide notes
            try:
                notes_slide = slide.notes_slide
                text_frame = notes_slide.notes_text_frame
                if text_frame is not None:
                    text_frame.text = notes_text
                    logger.debug(f"Slide {slide_index}: Updated slide notes")
                else:
                    logger.warning(f"Slide {slide_index}: No notes text frame available")
            except Exception as e:
                logger.warning(f"Slide {slide_index}: Could not update notes - {e}")

        if not match_found:
            logger.warning(f"Slide {slide_index}: No matching alt-text for replacement")

    presentation.save(output_pptx)
    logger.debug(f"PowerPoint saved as {output_pptx}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Sync images within input pptx file by monkey-patching placeholders.")
    parser.add_argument('-i', '--input_pptx', type=str, required=True, help="Input pptx file path")
    parser.add_argument('-o', '--output_pptx', type=str, required=True, help="Output pptx file path")
    parser.add_argument('-d', '--image_dict', type=str, required=True, help="Path to JSON file containing image dictionary")
    parser.add_argument('-r', '--repo_url', type=str, required=False, default=None, help="Repository URL for GitHub links in notes")

    args = parser.parse_args()

    with open(args.image_dict, 'r') as f:
        image_dict = json.load(f)

    sync_images(
        input_pptx=args.input_pptx,
        output_pptx=args.output_pptx,
        image_dict=image_dict,
        repo_url=args.repo_url
    )
