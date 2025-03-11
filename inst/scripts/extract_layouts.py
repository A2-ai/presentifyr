import os
import argparse
from pptx import Presentation
from PIL import Image, ImageDraw
from py_logger import get_logger

def generate_layout_images(base_pptx, output_dir):
    logger = get_logger()
    logger.debug("Starting generate layout images Python function")

    prs = Presentation(base_pptx)
    
    max_width_px=800 ## Currently hardcoded based on testing

    slide_width_emu = prs.slide_width
    slide_height_emu = prs.slide_height
    logger.debug(f"Slide master dimensions (EMUs): width={slide_width_emu}, height={slide_height_emu}")

    ## Fix thumbnail's width to max_width_px and compute the height by preserving aspect ratio.
    width_px = max_width_px
    aspect_ratio = slide_height_emu / float(slide_width_emu)
    logger.debug(f"Slide master aspect ratio: {aspect_ratio}")
    height_px = int(width_px * aspect_ratio)

    layout_count = 0
    for i, layout in enumerate(prs.slide_layouts):
        has_type_18 = any(
            shape.is_placeholder and shape.placeholder_format.type == 18
            for shape in layout.shapes
        )
        logger.debug(f"Checking if layout index {i} has any picture placeholders (type=18) - {has_type_18}")

        if not has_type_18:
            logger.debug(f"Layout index {i} has no picture placeholder (type=18); skipping")
            continue

        logger.info(f"Layout index {i} has at least one picture placeholder (type=18); generating thumbnail")
        img = Image.new("RGB", (width_px, height_px), "white")
        draw = ImageDraw.Draw(img)

        draw.text((10, 10), f"Layout {i}", fill="black")

        x_scale = width_px / float(slide_width_emu)
        y_scale = height_px / float(slide_height_emu)

        for shape in layout.shapes:
            if shape.is_placeholder:
                left_px = int(shape.left * x_scale)
                top_px = int(shape.top * y_scale)
                right_px = int((shape.left + shape.width) * x_scale)
                bottom_px = int((shape.top + shape.height) * y_scale)

                fill_color = None

                if shape.placeholder_format.type == 18:
                    fill_color = (255, 200, 200)  ## Soft red/pink

                draw.rectangle(
                    [(left_px, top_px), (right_px, bottom_px)],
                    outline="black",
                    fill=fill_color,
                    width=2
                )

        img_path = os.path.join(output_dir, f"layout_{i}.png")
        logger.debug(f"Saving thumbnail to '{img_path}'")
        img.save(img_path)
        layout_count += 1

    logger.info(f"Generated {layout_count} layout image(s) in '{output_dir}'")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Extract and visualize PowerPoint slide layouts if they have a picture placeholder (type 18)")
    parser.add_argument('-b', '--base_pptx', type=str, required=True, help="Base PowerPoint template")
    parser.add_argument('-o', '--output_dir', type=str, required=True, help="Directory to save layout images")
    
    args = parser.parse_args()

    generate_layout_images(base_pptx = args.base_pptx, output_dir = args.output_dir)
