import os
import argparse
from pptx import Presentation

def log_message(message, log_file="script.log"):
    with open(log_file, "a") as log:
        log.write(f"{message}\n")

def add_images_to_ppt(files, repo_url, output_pptx, base_pptx=None, log_file="script.log"):
    if base_pptx is not None and os.path.exists(base_pptx):
        # If base_pptx is provided and valid
        log_message(f"Using base PowerPoint template: {base_pptx}", log_file)
        my_pres = Presentation(base_pptx)  # Load user-uploaded template
    else:
        # If no base_pptx is provided or it's invalid, create a blank PowerPoint presentation
        log_message("No PowerPoint template provided or invalid template. Creating a blank PowerPoint presentation.", log_file)
        my_pres = Presentation()  # Create a blank PowerPoint

    # Always scan for slide layout with placeholder type 18
    slide_layout_index = None
    for idx, layout in enumerate(my_pres.slide_layouts):
        for shape in layout.shapes:
            if shape.is_placeholder and shape.placeholder_format.type == 18:
                slide_layout_index = idx  # Set the layout index where the placeholder type 18 is found
                log_message(f"Found layout {slide_layout_index} with placeholder type 18.", log_file)
                break
        if slide_layout_index is not None:
            break

    # If no suitable layout is found, raise an error instead of defaulting
    if slide_layout_index is None:
        log_message("No slide layout with placeholder type 18 found.", log_file)
        raise ValueError("No slide layout with the correct placeholder type (18) found in the PowerPoint template or blank presentation.")

    total_files = len(files)
    log_message(f"Total files to process: {total_files}", log_file)

    for i, file in enumerate(files, start=1):
        file_url = f"{repo_url}/{file}"
        sentinel_val = "{prfy}:"
        alt_text = f"{sentinel_val}{os.path.basename(file)}"

        log_message(f"Creating slide {i} of {total_files}...", log_file)

        # Add a new slide with the appropriate layout (determined dynamically)
        slide = my_pres.slides.add_slide(my_pres.slide_layouts[slide_layout_index])
        
        for shape in slide.shapes:
            if shape.is_placeholder:
                log_message(f"Shape Name: {shape.name}, Type: {shape.placeholder_format.type}, ID: {shape.placeholder_format.idx}", log_file)
            else:
                log_message(f"Shape Name: {shape.name}, Not a Placeholder, Type: {shape.shape_type}", log_file)

        placeholder = None
        for shape in slide.placeholders:
            if shape.is_placeholder and shape.placeholder_format.type == 18:
                placeholder = shape
                log_message(f"Found placeholder for slide {i}: {shape.name} (Type: {shape.placeholder_format.type})", log_file)
                break

        if placeholder:
            log_message(f"Before image insertion: Placeholder XML: {placeholder._element}", log_file)
            
            # Insert the image into the placeholder
            placeholder.insert_picture(file)
            log_message(f"Image inserted into placeholder for slide {i}.", log_file)

            # Locate the new <p:pic> element created after image insertion
            try:
                namespaces = {
                    'p': 'http://schemas.openxmlformats.org/presentationml/2006/main',
                    'a': 'http://schemas.openxmlformats.org/drawingml/2006/main'
                }
                pic_element = slide._element.findall('.//p:pic', namespaces)[-1]

                # Find the <p:cNvPr> element and set the alt text
                nv_cNvPr = pic_element.find('.//p:nvPicPr/p:cNvPr', namespaces)
                if nv_cNvPr is not None:
                    nv_cNvPr.set("descr", alt_text)
                    log_message(f"Alt text set for slide {i}: {alt_text}", log_file)
                else:
                    log_message(f"Failed to find <p:cNvPr> in <p:pic> for slide {i}.", log_file)
            except Exception as e:
                log_message(f"Error locating or updating <p:pic> for slide {i}: {e}", log_file)

            # Add notes
            notes_slide = slide.notes_slide
            notes_text_frame = notes_slide.notes_text_frame
            notes_text_frame.text = file_url
        else:
            log_message(f"No content placeholder found on slide {i}. Skipping image placement.", log_file)

    log_message("Saving PowerPoint file...", log_file)
    my_pres.save(output_pptx)
    log_message(f"PowerPoint saved as {output_pptx}", log_file)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Add images within input pptx file")
    parser.add_argument('-f', '--files', nargs='+', type=str, required=True, help="Files")
    parser.add_argument('-r', '--repo_url', type=str, required=True, help="Repo URL")
    parser.add_argument('-o', '--output', type=str, required=True, help="Output pptx file path")
    parser.add_argument('-b', '--base_pptx', type=str, required=False, help="Base PowerPoint template (optional)")

    args = parser.parse_args()

    add_images_to_ppt(args.files, args.repo_url, args.output, base_pptx=args.base_pptx)
