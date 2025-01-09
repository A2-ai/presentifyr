import os
import re
import argparse
import json
from pptx import Presentation
from pptx.util import Inches

def sync_images(pptx_in, pptx_out, image_dict):
    presentation = Presentation(pptx_in)

    # Define magic string pattern
    start_pattern = r'\{prfy\}\:'   # Matches "{prfy}:" and any directory structure following it
    end_pattern = r'\.[^.]+$'
    magic_pattern = re.compile(start_pattern + '.*?' + end_pattern)

    for slide in presentation.slides:
        for shape in slide.shapes:
            if shape.shape_type == 14:  # 13 is the type for images (pictures)
                alt_text = shape._element._nvXxPr.cNvPr.attrib.get("descr", "")
                print(f"Checking image alt-text: {alt_text}")

                match = magic_pattern.match(alt_text)
                if match:
                    # Get the full file path from the alt-text
                    figure_name = match.group(0).split(':')[1]  # Extract path after "{prfy}:"
                    image_path = image_dict.get(figure_name)
                    print(f"Found matching alt-text. Figure_name: '{figure_name}'.")

                    if image_path and os.path.exists(image_path):
                        left = shape.left
                        top = shape.top
                        width = shape.width
                        height = shape.height

                        # Remove the existing image
                        shape._element.getparent().remove(shape._element)
                        print(f"Removed image with alt-text '{alt_text}'.")

                        # Insert the new image at the same position and size
                        new_shape = slide.shapes.add_picture(image_path, left=left, top=top, width=width, height=height)
                        
                        # Reapply the original alt-text
                        new_shape._element._nvXxPr.cNvPr.set("descr", alt_text)  
                        print(f"Replaced image with new image from {image_path} and retained alt-text '{alt_text}'")
                    else:
                        print(f"Image file {image_path} not found.")
                else:
                    print("No matching alt-text for replacement.")

    presentation.save(pptx_out)
    print(f"Presentation saved at '{pptx_out}'.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Sync images within input pptx file")
    parser.add_argument('-i', '--input', type = str, required = True, help = "Input pptx file path")
    parser.add_argument('-o', '--output', type = str, required = True, help = "Output pptx file path")
    parser.add_argument('-d', '--image_dict', type = str, required = True, help = "Path to temp JSON file containing image dictionary")

    args = parser.parse_args()
    
    with open(args.image_dict, 'r') as f:
        image_dict = json.load(f)

    sync_images(args.input, args.output, image_dict)
