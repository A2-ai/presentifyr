import os
import re
import argparse
import json
from pptx import Presentation
from pptx.util import Inches
from pptx.shapes.shapetree import PicturePlaceholder, PlaceholderPicture

def sync_images(pptx_in, pptx_out, image_dict):
    if PlaceholderPicture:
        PlaceholderPicture.insert_picture = PicturePlaceholder.insert_picture
        PlaceholderPicture._new_placeholder_pic = PicturePlaceholder._new_placeholder_pic
        PlaceholderPicture._get_or_add_image = PicturePlaceholder._get_or_add_image
        PlaceholderPicture._replace_placeholder_with = PicturePlaceholder._replace_placeholder_with

    presentation = Presentation(pptx_in)

    # Regex to match something like '{prfy}:my_image.png'
    start_pattern = r'\{prfy\}\:'
    end_pattern = r'\.[^.]+$'
    magic_pattern = re.compile(start_pattern + '.*?' + end_pattern)

    for slide in presentation.slides:
        for shape in slide.shapes:
            if shape.shape_type == 14:
                alt_text = shape._element._nvXxPr.cNvPr.attrib.get("descr", "")
                print(f"Checking image alt-text: {alt_text}")

                match = magic_pattern.match(alt_text)
                if match:
                    figure_name = match.group(0).split(':')[1]
                    image_path = image_dict.get(figure_name)

                    if image_path and os.path.exists(image_path):
                        print(f"Attempting shape.insert_picture({image_path})...")

                        new_pic = shape.insert_picture(image_path)
                        new_pic._element.nvPicPr.cNvPr.set("descr", alt_text)

                        print(f"Inserted new picture from {image_path}")
                    else:
                        print(f"No image or file not found: {image_path}")
                else:
                    print("No matching alt-text for replacement.")

    presentation.save(pptx_out)
    print(f"Presentation saved at '{pptx_out}'.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Sync images within input pptx file by monkey-patching placeholders.")
    parser.add_argument('-i', '--input', type=str, required=True, help="Input pptx file path")
    parser.add_argument('-o', '--output', type=str, required=True, help="Output pptx file path")
    parser.add_argument('-d', '--image_dict', type=str, required=True, help="Path to JSON file containing image dictionary")
    args = parser.parse_args()

    with open(args.image_dict, 'r') as f:
        image_dict = json.load(f)

    sync_images(args.input, args.output, image_dict)
