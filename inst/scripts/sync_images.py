import os
import re
import argparse
import json
from pptx import Presentation
from pptx.shapes.shapetree import PicturePlaceholder, PlaceholderPicture
from py_logger import get_logger

def sync_images(pptx_in, output_pptx, image_dict):
    ## This needs a better solution
    if PlaceholderPicture:
        PlaceholderPicture.insert_picture = PicturePlaceholder.insert_picture
        PlaceholderPicture._new_placeholder_pic = PicturePlaceholder._new_placeholder_pic
        PlaceholderPicture._get_or_add_image = PicturePlaceholder._get_or_add_image
        PlaceholderPicture._replace_placeholder_with = PicturePlaceholder._replace_placeholder_with

    logger = get_logger()
    logger.debug(f"Starting image sync process.")
    presentation = Presentation(pptx_in)

    start_pattern = r'\{prfy\}\:'
    end_pattern = r'\.[^.]+$'
    magic_pattern = re.compile(start_pattern + '.*?' + end_pattern)

    logger.info("Scanning slides for image replacements...")

    for slide_index, slide in enumerate(presentation.slides, start=1):  
        match_found = False  # Track if at least one image was replaced

        for shape in slide.shapes:
            if shape.shape_type == 14:  # Shape type 14 = Picture
                alt_text = shape._element._nvXxPr.cNvPr.attrib.get("descr", "")
                logger.debug(f"Slide {slide_index}: Checking image alt-text: {alt_text}")

                match = magic_pattern.match(alt_text)
                if match:
                    figure_name = match.group(0).split(':')[1]
                    image_path = image_dict.get(figure_name)

                    if image_path and os.path.exists(image_path):
                        logger.info(f"Slide {slide_index}: Replacing image {figure_name} with {image_path}")

                        new_pic = shape.insert_picture(image_path)
                        new_pic._element.nvPicPr.cNvPr.set("descr", alt_text)

                        logger.debug(f"Slide {slide_index}: Inserted new picture from {image_path}")
                        match_found = True
                    else:
                        logger.warning(f"Slide {slide_index}: No matching image found for {figure_name} or file does not exist.")

        if not match_found:
            logger.warning(f"Slide {slide_index}: No matching alt-text for replacement.")

    presentation.save(output_pptx)
    logger.info(f"PowerPoint saved as {output_pptx}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Sync images within input pptx file by monkey-patching placeholders.")
    parser.add_argument('-i', '--input', type=str, required=True, help="Input pptx file path")
    parser.add_argument('-o', '--output', type=str, required=True, help="Output pptx file path")
    parser.add_argument('-d', '--image_dict', type=str, required=True, help="Path to JSON file containing image dictionary")
    args = parser.parse_args()

    with open(args.image_dict, 'r') as f:
        image_dict = json.load(f)

    sync_images(args.input, args.output, image_dict)
