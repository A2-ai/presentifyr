import os
import argparse
from pptx import Presentation
from PIL import Image, ImageDraw
from py_logger import get_logger

def generate_layout_images(base_pptx, output_dir, max_width_px=800):
    logger = get_logger()
    logger.debug(f"Starting generate_layout_images with base_pptx='{base_pptx}', output_dir='{output_dir}', max_width_px={max_width_px}")

    prs = Presentation(base_pptx)
    os.makedirs(output_dir, exist_ok=True)

    # Slide dimensions in EMUs
    slide_width_emu = prs.slide_width
    slide_height_emu = prs.slide_height

    # We'll fix the thumbnail's width to max_width_px
    # and compute the height by preserving aspect ratio.
    width_px = max_width_px
    aspect_ratio = slide_height_emu / float(slide_width_emu)
    height_px = int(width_px * aspect_ratio)

    layout_count = 0
    for i, layout in enumerate(prs.slide_layouts):
        # Check for a placeholder of type=18
        has_type_18 = any(
            shape.is_placeholder and shape.placeholder_format.type == 18
            for shape in layout.shapes
        )
        logger.debug(f"Layout index {i}: Checking placeholders for type=18 -> {has_type_18}")

        if not has_type_18:
            logger.debug(f"Layout index {i} has no placeholder of type=18; skipping.")
            continue

        # Create the thumbnail
        logger.info(f"Layout index {i} has placeholder type=18, generating thumbnail.")
        img = Image.new("RGB", (width_px, height_px), "white")
        draw = ImageDraw.Draw(img)

        draw.text((10, 10), f"Layout {i}", fill="black")

        # Scale factors for drawing rectangles
        x_scale = width_px / float(slide_width_emu)
        y_scale = height_px / float(slide_height_emu)

        # Draw rectangles for placeholder shapes
        for shape in layout.shapes:
            if shape.is_placeholder:
                left_px = int(shape.left * x_scale)
                top_px = int(shape.top * y_scale)
                right_px = int((shape.left + shape.width) * x_scale)
                bottom_px = int((shape.top + shape.height) * y_scale)

                draw.rectangle([(left_px, top_px), (right_px, bottom_px)],
                               outline="black", width=2)

        # Save the thumbnail .png
        img_path = os.path.join(output_dir, f"layout_{i}.png")
        img.save(img_path)
        layout_count += 1

    logger.info(f"Generated {layout_count} layout image(s) in '{output_dir}'.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Extract and visualize PowerPoint slide layouts if they have a placeholder of type 18")
    parser.add_argument('-b', '--base_pptx', type=str, required=True,
                        help="Path to the PowerPoint file")
    parser.add_argument('-o', '--output_dir', type=str, required=True,
                        help="Directory to save layout images")
    parser.add_argument('--width_px', type=int, default=800,
                        help="Max width in pixels for the generated images (default: 800)")
    args = parser.parse_args()

    generate_layout_images(args.base_pptx, args.output_dir, args.width_px)
