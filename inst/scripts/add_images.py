import os
import argparse
from pptx import Presentation
from py_logger import get_logger

def add_images_to_ppt(files, repo_url, output_pptx, base_pptx=None):
    logger = get_logger()
    logger.debug(f"Starting add_images py function. Total images: {len(files)}.")
  
    if base_pptx is not None and os.path.exists(base_pptx):
        logger.info(f"Using base PowerPoint template: {base_pptx}")
        my_pres = Presentation(base_pptx) 
    else:
        logger.info("No valid PowerPoint template. Creating a blank presentation.")
        my_pres = Presentation()

    slide_layout_index = None
    for idx, layout in enumerate(my_pres.slide_layouts):
        for shape in layout.shapes:
            if shape.is_placeholder and shape.placeholder_format.type == 18:
                slide_layout_index = idx  ## Set the layout index where the placeholder type 18 is found
                logger.debug(f"Found layout {slide_layout_index} with placeholder type 18.")
                break
        if slide_layout_index is not None:
            break

    ## If no suitable layout is found, raise an error instead of defaulting
    if slide_layout_index is None:
        logger.error("No slide layout with placeholder type 18 found.")
        raise ValueError("No slide layout with the correct placeholder type (18) found in the PowerPoint template or blank presentation.")

    for i, file in enumerate(files, start=1):
        file_url = f"{repo_url}/{file}"
        sentinel_val = "{prfy}:"
        alt_text = f"{sentinel_val}{os.path.basename(file)}"

        logger.info(f"Creating slide {i}/{len(files)} with image: {file}.")

        slide = my_pres.slides.add_slide(my_pres.slide_layouts[slide_layout_index])
        
        for shape in slide.shapes:
            if shape.is_placeholder:
                logger.debug(f"Shape: {shape.name}, Type: {shape.placeholder_format.type}, ID: {shape.placeholder_format.idx}.")
            else:
                logger.debug(f"Shape: {shape.name}, Type: {shape.shape_type}")

        placeholder = None
        for shape in slide.placeholders:
            if shape.is_placeholder and shape.placeholder_format.type == 18:
                placeholder = shape
                logger.debug(f"Found placeholder for slide {i}: {shape.name} (Type: {shape.placeholder_format.type}).")
                break

        if placeholder:
            placeholder.insert_picture(file) ## Insert the image into the placeholder
            logger.info(f"Image inserted into placeholder for slide {i}.")

            ## Locate the new <p:pic> element created after image insertion
            try:
                namespaces = {
                    'p': 'http://schemas.openxmlformats.org/presentationml/2006/main',
                    'a': 'http://schemas.openxmlformats.org/drawingml/2006/main'
                }
                pic_element = slide._element.findall('.//p:pic', namespaces)[-1]

                ## Find the <p:cNvPr> element and set the alt text
                nv_cNvPr = pic_element.find('.//p:nvPicPr/p:cNvPr', namespaces)
                if nv_cNvPr is not None:
                    nv_cNvPr.set("descr", alt_text)
                    logger.debug(f"Alt text set for slide {i}: {alt_text}.")
                else:
                    logger.warning(f"Failed to find <p:cNvPr> in <p:pic> for slide {i}.")
            except Exception as e:
                logger.error(f"Error locating or updating <p:pic> for slide {i}: {e}.")

            notes_slide = slide.notes_slide
            notes_text_frame = notes_slide.notes_text_frame
            notes_text_frame.text = file_url
        else:
            logger.warning(f"No content placeholder found on slide {i}. Skipping image placement.")

    my_pres.save(output_pptx)
    logger.info(f"PowerPoint saved as {output_pptx}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Add images within input pptx file")
    parser.add_argument('-f', '--files', nargs='+', type=str, required=True, help="Files")
    parser.add_argument('-r', '--repo_url', type=str, required=True, help="Repo URL")
    parser.add_argument('-o', '--output', type=str, required=True, help="Output pptx file path")
    parser.add_argument('-b', '--base_pptx', type=str, required=False, help="Base PowerPoint template (optional)")

    args = parser.parse_args()

    add_images_to_ppt(args.files, args.repo_url, args.output, base_pptx=args.base_pptx)
