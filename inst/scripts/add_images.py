import os
import argparse
from pptx import Presentation
from py_logger import get_logger

def add_images_to_ppt(files, repo_url, output_pptx, slide_layout_index, base_pptx=None):
    logger = get_logger()
    logger.debug(f"Starting add images Python function")
  
    if base_pptx and os.path.exists(base_pptx):
        logger.debug(f"Using base PowerPoint template: {base_pptx}")
        my_pres = Presentation(base_pptx) 
    else:
        logger.info("No valid PowerPoint template. Creating a blank presentation")
        my_pres = Presentation()

    logger.debug(f"Total images to insert: {len(files)}")
    
    for i, file in enumerate(files, start=1):
        file_url = f"{repo_url}/{file}"
        sentinel_val = "{prfy}:"
        alt_text = f"{sentinel_val}{os.path.basename(file)}"

        logger.info(f"Creating slide {i}/{len(files)} with image: {file}")
        logger.debug(f"Using slide layout index: {slide_layout_index} for all slides")

        slide = my_pres.slides.add_slide(my_pres.slide_layouts[slide_layout_index])

        placeholder = None
        for shape in slide.placeholders:
            if shape.is_placeholder and shape.placeholder_format.type == 18:
                placeholder = shape
                break

        if placeholder:
            placeholder.insert_picture(file) ## Insert the image into the placeholder
            logger.info(f"Image inserted into placeholder for slide {i}")

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
                    logger.debug(f"Alt text set for slide {i}: {alt_text}")
                else:
                    logger.warning(f"Failed to find <p:cNvPr> in <p:pic> for slide {i}")
            except Exception as e:
                logger.error(f"Error locating or updating <p:pic> for slide {i}: {e}")

            notes_slide = slide.notes_slide
            notes_text_frame = notes_slide.notes_text_frame
            notes_text_frame.text = file_url
        else:
            logger.warning(f"No content placeholder found on slide {i}. Skipping image placement")

    my_pres.save(output_pptx)
    logger.debug(f"PowerPoint saved as {output_pptx}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Add images within input pptx file")
    parser.add_argument('-f', '--files', nargs='+', type=str, required=True, help="Files")
    parser.add_argument('-r', '--repo_url', type=str, required=True, help="Repo URL")
    parser.add_argument('-o', '--output_pptx', type=str, required=True, help="Output pptx file path")
    parser.add_argument('-b', '--base_pptx', type=str, default=None, help="Base PowerPoint template")
    parser.add_argument('-l', '--slide_layout_index', type=int, required=True, help="Slide layout index to use")

    args = parser.parse_args()

    add_images_to_ppt(files = args.files, repo_url = args.repo_url, output_pptx = args.output_pptx, base_pptx = args.base_pptx, slide_layout_index = args.slide_layout_index)
